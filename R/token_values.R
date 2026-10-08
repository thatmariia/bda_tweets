# ==========================================================
# == FUNCTIONS FOR TOKEN VALUES
# ==========================================================

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
