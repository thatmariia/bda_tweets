# ==========================================================
# == FUNCTIONS FOR FEATURE ENGINEERING
# ==========================================================

#' Settings for one feature set
feature_config <- function(ngram_max = 1, shingle_min = 0, shingle_max = 0,
                           weighting = c("count", "binary", "log_count", "tf", "tf_idf"),
                           surprise_pct = 0,
                           length = FALSE, lexicon = FALSE, nb_scaling = FALSE) {
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
#' @param data A data frame with columns `id` and `tweet` (and `label` for training data)
#' @param config Settings from feature_config(); only when fitting on training data
#' @param recipe The `recipe` returned when fitting on training data
#' @return A list with `x` (sparse matrix), `y` (labels or NULL), and `recipe`
prepare_features <- function(data, config = NULL, recipe = NULL) {
    stopifnot(all(c("id", "tweet") %in% colnames(data)))
    stopifnot(xor(is.null(config), is.null(recipe))) # either config or recipe must be provided

    fitting <- is.null(recipe)
    if (!fitting) config <- recipe$config

    y <- if ("label" %in% colnames(data)) data$label else NULL # test data doesn't have labels

    # tokenize
    tokens <- tokenize_tweets(data, config$ngram_max, config$shingle_min, config$shingle_max)

    # vocabulary and IDF, learned on training data only
    if (fitting) {
        recipe <- list(config = config, vocab = fit_vocabulary(tokens, config$surprise_pct))
    }

    # add weights to tokens
    values <- tokens |> weight_tokens(recipe$vocab, config$weighting)

    # naive Bayes scaling, learned on training data only
    if (config$nb_scaling) {
        if (fitting) recipe$nb_ratios <- fit_nb_ratios(values, data)
        values <- nb_scale(values, recipe$nb_ratios)
    }

    # turn into a sparse matrix
    x <- values |> tokens_to_sparse_matrix(data$id, recipe$vocab)

    # extra features from raw tweets
    if (config$length) x <- x |> add_columns(length_features(data))
    if (config$lexicon) x <- x |> add_columns(lexicon_features(data))

    return(list(x = x, y = y, recipe = recipe))
}

#' Get the vocabulary and IDF from training tokens
fit_vocabulary <- function(tokens, surprise_pct = 0) {
    n_docs <- n_distinct(tokens$id)

    kept_tokens <- tokens |>
        count(token, name = "doc_count") |> # number of tweets containing the token
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

#' Learn naive Bayes log-count ratios from training data
#'
#' For each token: log of a ratio:
#' numerator: share of offensive tweets that contain the token
#' denominator: share of non-offensive tweets that contain the token
fit_nb_ratios <- function(values, data, alpha = 1) {
    stopifnot("label" %in% colnames(data))

    ratios <- values |>
        distinct(id, token) |>
        inner_join(select(data, id, label), by = "id") |>
        group_by(token) |>
        summarise(
            n_offensive = sum(label == 1) + alpha,
            n_other = sum(label == 0) + alpha
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
