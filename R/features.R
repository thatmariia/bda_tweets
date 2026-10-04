# ==========================================================
# == FUNCTIONS FOR FEATURE ENGINEERING
# ==========================================================

#' Settings for one feature set
#' @param extras Add the extra per-tweet columns (length, lexicon, question and offensive words)
feature_config <- function(
  ngram_max = 1, shingle_min = 0, shingle_max = 0,
  weighting = c("count", "binary", "log_count", "tf", "tf_idf"),
  surprise_pct = 0,
  stopwords = c("none", "snowball", "smart"),
  extras = FALSE
) {
    config <- list(
        ngram_max = ngram_max,
        shingle_min = shingle_min,
        shingle_max = shingle_max,
        weighting = match.arg(weighting),
        surprise_pct = surprise_pct,
        stopwords = match.arg(stopwords),
        extras = extras
    )

    return(config)
}

#' Turn tweets into a feature matrix
#' Call it with `config` on training data and with `recipe` on validation / test data
#'
#' The tweets are processed in chunks, so the tokens of all tweets never have to fit
#' in memory at the same time. Fitting goes over the chunks twice: first to count the
#' tokens (for the vocabulary and IDF), then to build the matrix.
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
        tokens <- tokenize_tweets(
            chunk, config$ngram_max, config$shingle_min, config$shingle_max, config$stopwords
        )
        return(tokens)
    }

    # vocabulary and IDF, learned on training data only
    if (fitting) {
        chunk_counts <- map(chunks, \(chunk) count_tokens(tokenize(chunk)))
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
    }

    # weighted tokens -> sparse matrix, one chunk at a time
    chunk_matrices <- map(chunks, \(chunk) {
        values <- tokenize(chunk) |> weight_tokens(recipe$vocab, config$weighting)
        return(tokens_to_sparse_matrix(values, chunk$id, recipe$vocab))
    })
    x <- do.call(rbind, unname(chunk_matrices))

    # extra features from raw tweets
    if (config$extras) x <- x |> add_columns(extra_features(data))

    return(list(x = x, y = y, recipe = recipe))
}

#' Get the vocabulary and IDF from the token counts of the training tweets
fit_vocabulary <- function(token_counts, n_docs, surprise_pct = 0) {
    kept_tokens <- token_counts |>
        mutate(idf = log(n_docs / doc_count)) |>
        filter(idf <= -log(surprise_pct / 100)) |>
        select(token, idf)

    return(kept_tokens)
}

#' Count the tweets that contain each token
#' @return A list with `tokens` (one row per token) and `n_docs` (tweets with any token)
count_tokens <- function(tokens) {
    token_counts <- tokens |>
        count(token, name = "doc_count") # tokens has one row per tweet and token

    return(list(tokens = token_counts, n_docs = n_distinct(tokens$id)))
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

#' Add per-tweet columns to the matrix, matching rows by id
#' @param x Sparse matrix with tweet ids as row names
#' @param extra A data frame with column `id` and one column per feature
add_columns <- function(x, extra) {
    extra_matrix <- tibble(id = rownames(x)) |>
        left_join(extra, by = "id") |>
        mutate(across(-id, \(v) replace_na(v, 0))) |>
        select(-id) |>
        as.matrix()

    return(cbind(x, Matrix::Matrix(extra_matrix, sparse = TRUE)))
}

#' Compute the extra per-tweet features from the words of each tweet
#' @param long_word Minimum number of letters of a long word (default 7)
extra_features <- function(data, long_word = 7) {
    bing <- get_sentiments("bing")
    negative_words <- bing$word[bing$sentiment == "negative"]
    positive_words <- bing$word[bing$sentiment == "positive"]

    extras <- data |>
        select(id, tweet) |>
        unnest_tokens(word, tweet) |>
        group_by(id) |>
        summarise(
            # length
            log_n_words = log1p(n()),
            mean_word_length = mean(nchar(word)),
            long_word_share = mean(nchar(word) >= long_word),
            unique_word_share = n_distinct(word) / n(),
            # sentiment lexicon
            lex_negative = sum(word %in% negative_words),
            lex_positive = sum(word %in% positive_words),
            lex_negative_share = lex_negative / n(),
            lex_positive_share = lex_positive / n(),
            lex_more_negative = as.numeric(lex_negative > lex_positive),
            # question and offensive words
            question_word_share = mean(word %in% question_words),
            n_offensive_words = sum(word %in% offensive_words)
        )

    return(extras)
}
