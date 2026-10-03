# ==========================================================
# == FUNCTIONS FOR PREPARING DATA FOR MODELS
# ==========================================================

#' Split tweets into training and validation sets
#'
#' Splits the raw data, before computing features, so that everything learned for
#' the features (vocabulary, IDF) comes from the training part only.
#'
#' @param data A data frame of tweets
#' @param target_col The label column; the split keeps its class balance
#' @param partition Share of tweets that goes into the training set
#' @return A list with data frames `train` and `val`
split_train_validation <- function(data, target_col = "label", partition = 0.8) {
    train_idx <- caret::createDataPartition(data[[target_col]], p = partition)$Resample1

    split <- list(
        train = data[train_idx, ],
        val = data[-train_idx, ]
    )
    return(split)
}
