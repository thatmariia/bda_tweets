# ==========================================================
# == FUNCTIONS FOR FEATURE ENGINEERING
# ==========================================================

#' Settings for one feature set
feature_config <- function(
    ngram_max = 1, shingle_min = 0, shingle_max = 0,
    weighting = c("count", "binary", "log_count", "tf", "tf_idf"),
    surprise_pct = 0,
    length = FALSE, lexicon = FALSE, nb_scaling = FALSE
) {
    config <- list(
        ngram_max = ngram_max,
        shingle_min = shingle_min,
        shingle_max = shingle_max,
        weighting = match.arg(weighting),
        surprise_pct = surprise_pct,
        length = length,
        lexicon = lexicon,
        nb_scaling = nb_scaling
    )

    return(config)
}

#' Turn tweets into a feature matrix
#' Call it with `config` on training data and with `recipe` on validation / test data
#'
#' The tweets are processed in chunks, so the tokens of all tweets never have to fit
#' in memory at the same time. Fitting goes over the chunks twice: first to count the
#' tokens (for the vocabulary, IDF and naive Bayes ratios), then to build the matrix.
#'
#' @param data A data frame with columns `id` and `tweet` (and `label` for training data)
#' @param config Settings from feature_config(); only when fitting on training data
#' @param recipe The `recipe` returned when fitting on training data
#' @param chunk_size Number of tweets per chunk
#' @return A list with `x` (sparse matrix), `y` (labels or NULL), and `recipe`
prepare_features <- function(data, config = NULL, recipe = NULL, chunk_size = 20000) {
    stopifnot(all(c("id", "tweet") %in% colnames(data)))
    stopifnot(xor(is.null(config), is.null(recipe))) # either config or recipe must be provided

    fitting <- is.null(recipe)
    if (!fitting) config <- recipe$config

    y <- if ("label" %in% colnames(data)) data$label else NULL # test data doesn't have labels

    chunks <- split(data, ceiling(seq_len(nrow(data)) / chunk_size))
    tokenize <- \(chunk) {
        tokens <- tokenize_tweets(chunk, config$ngram_max, config$shingle_min, config$shingle_max)
        return(tokens)
    }

    # vocabulary, IDF and naive Bayes ratios, learned on training data only
    if (fitting) {
        chunk_counts <- map(chunks, \(chunk) count_tokens(tokenize(chunk), chunk))
        n_docs <- sum(map_int(chunk_counts, \(counts) counts$n_docs))
        token_counts <- chunk_counts |>
            map(\(counts) counts$tokens) |>
            bind_rows() |>
            group_by(token) |>
            summarise(across(everything(), sum))

        recipe <- list(
            config = config,
            vocab = fit_vocabulary(token_counts, n_docs, config$surprise_pct)
        )
        if (config$nb_scaling) recipe$nb_ratios <- fit_nb_ratios(token_counts, recipe$vocab)
    }

    # weighted tokens -> sparse matrix, one chunk at a time
    chunk_matrices <- map(chunks, \(chunk) {
        values <- tokenize(chunk) |> weight_tokens(recipe$vocab, config$weighting)
        if (config$nb_scaling) values <- nb_scale(values, recipe$nb_ratios)
        return(tokens_to_sparse_matrix(values, chunk$id, recipe$vocab))
    })
    x <- do.call(rbind, unname(chunk_matrices))

    # extra features from raw tweets
    if (config$length) x <- x |> add_columns(length_features(data))
    if (config$lexicon) x <- x |> add_columns(lexicon_features(data))

    return(list(x = x, y = y, recipe = recipe))
}

#' Count the tweets that contain each token, in one chunk of training tweets
#' (and how many of them are offensive or not, for the naive Bayes ratios)
#' @return A list with `tokens` (one row per token) and `n_docs` (tweets with any token)
count_tokens <- function(tokens, chunk) {
    token_counts <- tokens |>
        inner_join(select(chunk, id, label), by = "id") |>
        group_by(token) |>
        summarise(
            doc_count = n(), # tokens has one row per tweet and token
            n_offensive = sum(label == 1),
            n_other = sum(label == 0)
        )

    return(list(tokens = token_counts, n_docs = n_distinct(tokens$id)))
}

#' Get the vocabulary and IDF from the token counts of the training tweets
fit_vocabulary <- function(token_counts, n_docs, surprise_pct = 0) {
    kept_tokens <- token_counts |>
        mutate(idf = log(n_docs / doc_count)) |>
        filter(idf <= -log(surprise_pct / 100)) |>
        select(token, idf)

    return(kept_tokens)
}

#' Assign each token a weighting value per tweet
weight_tokens <- function(tokens, vocab, weighting) {
    values <- tokens |>
        group_by(id) |>
        mutate(tf = n / sum(n)) |> # before dropping tokens: share of all tokens in the tweet
        ungroup() |>
        inner_join(vocab, by = "token") |>
        mutate(value = switch(weighting,
            # nr of times token occurs in the tweet
            count = n,
            # 1 if token occurs in the tweet
            binary = 1,
            # log count so repeats add less than first occurrence
            log_count = 1 + log(n),
            # n / number of tokens in the tweet
            tf = tf,
            # tf * idf from the training data
            tf_idf = tf * idf
        )) |>
        select(id, token, value)

    return(values)
}

#' Turn weighted (values) tokens into a sparse matrix
tokens_to_sparse_matrix <- function(values, ids, vocab) {
    x <- Matrix::sparseMatrix(
        i = match(values$id, ids),
        j = match(values$token, vocab$token),
        x = values$value,
        dims = c(length(ids), nrow(vocab)),
        dimnames = list(ids, vocab$token)
    )

    return(x)
}

#' Learn naive Bayes log-count ratios from the token counts of the training tweets
#'
#' For each token: log of a ratio:
#' numerator: share of offensive tweets that contain the token
#' denominator: share of non-offensive tweets that contain the token
fit_nb_ratios <- function(token_counts, vocab, alpha = 1) {
    ratios <- token_counts |>
        semi_join(vocab, by = "token") |>
        mutate(
            n_offensive = n_offensive + alpha,
            n_other = n_other + alpha
        ) |>
        mutate(nb_ratio = log(
            (n_offensive / sum(n_offensive)) / (n_other / sum(n_other))
        )) |>
        select(token, nb_ratio)

    return(ratios)
}

#' Replace token values by 0/1 times the token's naive Bayes ratio
nb_scale <- function(values, ratios) {
    scaled <- values |>
        inner_join(ratios, by = "token") |>
        mutate(value = nb_ratio) |> # 1 (token present) * ratio
        select(id, token, value)

    return(scaled)
}

#' Add per-tweet columns to the matrix, matching rows by id
#' @param x Sparse matrix with tweet ids as row names
#' @param extra A data frame with column `id` and one column per feature
add_columns <- function(x, extra) {
    extra_matrix <- tibble(id = rownames(x)) |>
        left_join(extra, by = "id") |>
        mutate(across(-id, \(v) replace_na(v, 0))) |> # e.g. tweets without lexicon words
        select(-id) |>
        as.matrix()

    return(cbind(x, Matrix::Matrix(extra_matrix, sparse = TRUE)))
}

#' Compute features related to tweet length
length_features <- function(data) {
    lengths <- data |>
        mutate(
            n_words = str_count(tweet, "\\S+"),
            n_letters = str_length(str_remove_all(tweet, "\\s"))
        ) |>
        transmute(
            id,
            log_n_words = log1p(n_words),
            mean_word_length = n_letters / pmax(n_words, 1)
        )

    return(lengths)
}

#' Compute features from a sentiment lexicon
lexicon_features <- function(data) {
    sentiment_counts <- data |>
        select(id, tweet) |>
        unnest_tokens(word, tweet) |>
        inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") |>
        group_by(id) |>
        summarise(
            lex_negative = sum(sentiment == "negative"),
            lex_positive = sum(sentiment == "positive")
        )

    return(sentiment_counts)
}
