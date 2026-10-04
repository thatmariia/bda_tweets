# ==========================================================
# == FUNCTIONS FOR PREPARING DATA FOR MODELS
# ==========================================================

#' Split tweets into training and validation sets
split_train_validation <- function(data, target_col = "label", partition = 0.8) {
    train_idx <- caret::createDataPartition(data[[target_col]], p = partition)$Resample1

    split <- list(
        train = data[train_idx, ],
        val = data[-train_idx, ]
    )
    return(split)
}
