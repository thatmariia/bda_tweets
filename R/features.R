# ==========================================================
# == FUNCTIONS FOR BUILDING FEATURE MATRICES
# ==========================================================

#' Settings for one feature set
#' @param replace_was Replace "what a slut" by "was" in the tweets first (see replace_what_a_slut())
#' @param extras Add the extra per-tweet columns (length, lexicon, question and offensive words)
feature_config <- function(
  ngram_max = 1, shingle_min = 0, shingle_max = 0,
  weighting = c("count", "binary", "log_count", "tf", "tf_idf"),
  surprise_pct = 0,
  stopwords = c("none", "snowball", "smart"),
  replace_was = FALSE,
  extras = FALSE
) {
    config <- list(
        ngram_max = ngram_max,
        shingle_min = shingle_min,
        shingle_max = shingle_max,
        weighting = match.arg(weighting),
        surprise_pct = surprise_pct,
        stopwords = match.arg(stopwords),
        replace_was = replace_was,
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
#' @param n_cores Number of chunks processed at the same time
#'      (keep 1 when prepare_features() itself already runs in parallel, as in the studies)
#' @return A list with `x` (sparse matrix), `y` (labels or NULL), and `recipe`
prepare_features <- function(
  data, config = NULL, recipe = NULL, chunk_size = 20000, n_cores = 1
) {
    stopifnot(all(c("id", "tweet") %in% colnames(data)))
    stopifnot(xor(is.null(config), is.null(recipe))) # either config or recipe must be provided

    fitting <- is.null(recipe)
    if (!fitting) config <- recipe$config

    y <- if ("label" %in% colnames(data)) data$label else NULL # test data doesn't have labels

    if (config$replace_was) data <- data |> mutate(tweet = replace_what_a_slut(tweet))

    chunks <- split(data, ceiling(seq_len(nrow(data)) / chunk_size))
    tokenize <- \(chunk) {
        tokens <- tokenize_tweets(
            chunk, config$ngram_max, config$shingle_min, config$shingle_max, config$stopwords
        )
        return(tokens)
    }

    # vocabulary and IDF, learned on training data only (chunks in parallel)
    if (fitting) {
        chunk_counts <- parallel::mclapply(
            chunks, \(chunk) count_tokens(tokenize(chunk)),
            mc.cores = n_cores
        )
        stopifnot(!map_lgl(chunk_counts, \(counts) inherits(counts, "try-error"))) # a chunk failed
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
        # in how many training tweets each word occurs, for the share of rare words
        if (config$extras) recipe$word_counts <- count_words(data)
    }

    # weighted tokens -> sparse matrix per chunk (chunks in parallel)
    chunk_matrices <- parallel::mclapply(chunks, \(chunk) {
        values <- tokenize(chunk) |> weight_tokens(recipe$vocab, config$weighting)
        return(tokens_to_sparse_matrix(values, chunk$id, recipe$vocab))
    }, mc.cores = n_cores)
    stopifnot(!map_lgl(chunk_matrices, \(matrix) inherits(matrix, "try-error"))) # a chunk failed
    x <- do.call(rbind, unname(chunk_matrices))

    # extra features from raw tweets
    if (config$extras) x <- x |> add_columns(extra_features(data, recipe$word_counts))

    return(list(x = x, y = y, recipe = recipe))
}
