# ==========================================================
# == FUNCTIONS FOR EXTRA PER-TWEET FEATURES
# ==========================================================

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

#' Count in how many tweets each word occurs (learned on the training tweets)
count_words <- function(data) {
    word_counts <- data |>
        select(id, tweet) |>
        unnest_tokens(word, tweet) |>
        distinct(id, word) |>
        count(word, name = "word_count")

    return(word_counts)
}

#' Compute the extra per-tweet features from the words of each tweet
#' @param word_counts Output of count_words() on the training tweets
#' @param long_word Minimum number of letters of a long word (default 7)
#' @param rare_count A word is rare if it occurs in at most this many training tweets (default 5)
extra_features <- function(data, word_counts, long_word = 7, rare_count = 5) {
    bing <- get_sentiments("bing")
    negative_words <- bing$word[bing$sentiment == "negative"]
    positive_words <- bing$word[bing$sentiment == "positive"]

    extras <- data |>
        select(id, tweet) |>
        unnest_tokens(word, tweet) |>
        left_join(afinn, by = "word") |>
        left_join(word_counts, by = "word") |>
        group_by(id) |>
        summarise(
            # length
            log_n_words = log1p(n()),
            mean_word_length = mean(nchar(word)),
            long_word_share = mean(nchar(word) >= long_word),
            unique_word_share = n_distinct(word) / n(),
            rare_word_share = mean(replace_na(word_count, 0) <= rare_count),
            # sentiment lexicon
            lex_negative = sum(word %in% negative_words),
            lex_positive = sum(word %in% positive_words),
            lex_negative_share = lex_negative / n(),
            lex_positive_share = lex_positive / n(),
            lex_more_negative = as.numeric(lex_negative > lex_positive),
            afinn_mean = if_else(
                sum(!is.na(afinn_score)) > 0,
                mean(afinn_score, na.rm = TRUE),
                0
            ),
            afinn_variance = if_else(
                sum(!is.na(afinn_score)) > 1,
                var(afinn_score, na.rm = TRUE),
                0
            ),
            afinn_negative_mean = if_else(
                sum(!is.na(afinn_score) & afinn_score < 0) > 0,
                mean(afinn_score[!is.na(afinn_score) & afinn_score < 0], na.rm = TRUE),
                0
            ),
            negation_before_positive_share = {
                prev_word <- lag(word, 1)
                mean((word %in% positive_words) & (replace_na(prev_word, "") %in% negation_words))
            },
            # question, offensive, group and hateful words
            question_word_share = mean(word %in% question_words),
            n_offensive_words = sum(word %in% offensive_words),
            n_group_words = sum(word %in% group_words),
            n_hateful_words = sum(word %in% hateful_words)
        )

    return(extras)
}
