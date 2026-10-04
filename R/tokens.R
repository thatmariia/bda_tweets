# ==========================================================
# == FUNCTIONS FOR DATA PREPROCESSING
# ==========================================================

#' Tokenize text one way and count the tokens per tweet
tokenize_data <- function(data, text_col, id_col, token_type = "words", ...) {
    data <- data |>
        unnest_tokens(output = token, {{ text_col }}, token = token_type, ...) |>
        filter(!is.na(token)) |>
        count({{ id_col }}, token)
    return(data)
}

#' Tokenize tweets into word n-grams and character n-grams
#' @param data A data frame with columns `id` and `tweet`
#' @param ngram_max Word n-grams of size 1 to `ngram_max`
#'      (default = 1 aka single words)
#' @param shingle_min,shingle_max Character n-grams of sizes `shingle_min` to `shingle_max`
#'      (default = 0 so none)
#' @param stopwords Stop word list whose words are removed from the single words (not from n-grams):
#'      "none" (default), "snowball", or "smart"
#' @return One row per tweet and token: `id`, `token`, `n`
tokenize_tweets <- function(
    data, ngram_max = 1, shingle_min = 0, shingle_max = 0, stopwords = "none"
) {
    stopifnot(all(c("id", "tweet") %in% colnames(data)), ngram_max >= 1)
    removed_words <- stopword_list(stopwords)

    ngrams_tokens <- map(1:ngram_max, \(size) {
        tokens <- data |>
            tokenize_data(
                tweet, id,
                token_type = "ngrams", n = size
            ) |>
            mutate(token = if (size == 1) token else paste0(size, "gram_", token))
        if (size == 1) tokens <- tokens |> filter(!token %in% removed_words)
        return(tokens)
    })

    shingle_sizes <- if (shingle_max > 0) shingle_min:shingle_max else integer(0)
    char_tokens <- map(shingle_sizes, \(size) {
        tokens <- data |>
            tokenize_data(
                tweet, id,
                token_type = "character_shingles", n = size, strip_non_alphanum = FALSE
            ) |>
            mutate(token = paste0(size, "shingle_", token))
        return(tokens)
    })

    return(bind_rows(ngrams_tokens, char_tokens))
}

#' Words of a stop word list from tidytext ("none" = no words)
stopword_list <- function(stopwords = c("none", "snowball", "smart")) {
    stopwords <- match.arg(stopwords)
    if (stopwords == "none") {
        return(character(0))
    }

    lexicon_name <- c(snowball = "snowball", smart = "SMART")[[stopwords]]
    words <- stop_words |>
        filter(lexicon == lexicon_name) |>
        pull(word)

    return(words)
}
