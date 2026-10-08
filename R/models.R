# ==========================================================
# == FUNCTIONS FOR TRAINING MODELS
# ==========================================================

#' Fit one model on a feature set and score it on the validation tweets
fit_option <- function(feature_set, method, feature_sets, foldid) {
    start <- Sys.time()
    data <- feature_sets[[feature_set]]

    model <- model_methods[[method]]$fit(data$train$x, data$train$y, foldid)

    model$method <- method
    model$feature_set <- feature_set

    model$val_pred <- predict_model(model, data$val$x)

    model$val_auc <- glmnet::assess.glmnet(
        model$val_pred,
        newy = data$val$y, family = "binomial"
    )$auc
    model$seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))

    return(model)
}

#' Fit all models in the options table
#' @param options A data frame with columns `key`, `feature_set` and `method`
#' @param feature_sets Named list of feature sets
#' @return A list with the fitted models and a table of their results
fit_all <- function(options, feature_sets) {
    stopifnot(all(options$feature_set %in% names(feature_sets)))
    stopifnot(all(options$method %in% names(model_methods)))

    # same 3 folds for every model
    n_train <- length(feature_sets[[1]]$train$y)
    foldid <- sample(rep_len(1:3, n_train))

    fits <- map(transpose(select(options, feature_set, method)), \(option) {
        fit <- try(
            fit_option(option$feature_set, option$method, feature_sets, foldid),
            silent = TRUE
        )
        return(fit)
    })

    # report any failed fits, keep the others
    failed <- map_lgl(fits, \(fit) inherits(fit, "try-error"))
    for (i in which(failed)) {
        cat("Could not fit", options$key[i], ":", fits[[i]], "\n")
    }

    models <- set_names(fits[!failed], options$key[!failed])
    results <- options[!failed, ] |>
        mutate(
            val_auc = map_dbl(models, \(model) model$val_auc),
            seconds = map_dbl(models, \(model) model$seconds)
        )

    return(list(models = models, results = results))
}

#' Refit a model on all labelled tweets (training + validation)
refit_best <- function(best_key, options, feature_sets, models) {
    option <- options |> filter(key == best_key)
    features <- feature_sets[[option$feature_set]]

    x <- rbind(features$train$x, features$val$x)
    y <- c(features$train$y, features$val$y)

    model <- model_methods[[option$method]]$refit(x, y, models[[best_key]])
    model$method <- option$method
    model$feature_set <- option$feature_set

    return(list(key = best_key, model = model))
}
