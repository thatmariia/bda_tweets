# ==========================================================
# == FUNCTIONS FOR TRAINING MODELS
# ==========================================================

#' Fit a glmnet model, tuning lambda by cross-validation
#' @param alpha 1 = lasso, 0 = ridge, in between = elastic net
#' @return A list with the fitted model and its tuning results
fit_glmnet <- function(x, y, foldid, alpha) {
    fit <- glmnet::cv.glmnet(
        x, y,
        family = "binomial", alpha = alpha, foldid = foldid, type.measure = "auc"
    )

    tuning <- tibble(
        parameter = "log10(lambda)",
        value = log10(fit$lambda),
        cv_auc = fit$cvm,
        cv_se = fit$cvsd
    )

    return(list(fit = fit, lambda = fit$lambda.min, tuning = tuning))
}

#' Refit a glmnet model on new data with the lambda chosen by its cross-validation
#' @param model A model from fit_glmnet()
#' @return A list like the one from fit_glmnet()
refit_glmnet <- function(x, y, model, alpha) {
    # glmnet works best on a sequence of lambdas, so the path is fitted down to the chosen one
    lambdas <- model$fit$lambda[model$fit$lambda >= model$lambda]
    fit <- glmnet::glmnet(x, y, family = "binomial", alpha = alpha, lambda = lambdas)

    return(list(fit = fit, lambda = model$lambda, tuning = model$tuning))
}

#' Predict probabilities with a glmnet model
predict_glmnet <- function(model, x) {
    pred <- predict(model$fit, x, s = model$lambda, type = "response") |> drop()
    return(pred)
}


#' Fit, refit, and predict functions of each method
model_methods <- list(
    lasso = list(
        fit = \(x, y, foldid) fit_glmnet(x, y, foldid, alpha = 1),
        refit = \(x, y, model) refit_glmnet(x, y, model, alpha = 1),
        predict = predict_glmnet
    ),
    ridge = list(
        fit = \(x, y, foldid) fit_glmnet(x, y, foldid, alpha = 0),
        refit = \(x, y, model) refit_glmnet(x, y, model, alpha = 0),
        predict = predict_glmnet
    ),
    elastic_net = list(
        fit = \(x, y, foldid) fit_glmnet(x, y, foldid, alpha = 0.5),
        refit = \(x, y, model) refit_glmnet(x, y, model, alpha = 0.5),
        predict = predict_glmnet
    )
)

#' Predict with a model from any method
predict_model <- function(model, x) {
    return(model_methods[[model$method]]$predict(model, x))
}

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

    # fir models
    fits <- parallel::mclapply(
        transpose(select(options, feature_set, method)),
        \(option) {
            fit <- try(
                fit_option(option$feature_set, option$method, feature_sets, foldid),
                silent = TRUE
            )
            return(fit)
        },
        mc.cores = n_cores,
        mc.preschedule = FALSE # the next model starts as soon as a core is free
    )

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

#' Bar chart of the validation AUC of every model
plot_model_auc <- function(results) {
    plot <- results |>
        mutate(key = fct_inorder(key)) |>
        ggplot(aes(x = key, y = val_auc)) +
        geom_col() +
        ylim(0, 1) +
        labs(title = "Validation AUC per model", x = NULL, y = "Validation AUC") +
        theme_minimal()

    return(plot)
}

#' Plot how every model was tuned: cross-validated AUC per value of its tuning parameter
plot_tuning <- function(models) {
    tuning <- imap_dfr(models, \(model, key) mutate(model$tuning, key = key)) |>
        mutate(panel = fct_inorder(paste0(key, "\n", parameter)))

    plot <- ggplot(tuning, aes(value, cv_auc)) +
        geom_errorbar(
            aes(ymin = cv_auc - cv_se, ymax = cv_auc + cv_se),
            width = 0, colour = "grey"
        ) +
        geom_line() +
        geom_point(size = 1) +
        facet_wrap(~panel, scales = "free_x", strip.position = "bottom") +
        labs(title = "Tuning per model", x = NULL, y = "Cross-validated AUC") +
        theme_bw() +
        theme(strip.placement = "outside", strip.background = element_blank())

    return(plot)
}

#' Plot the ROC curve of every model on the validation tweets
plot_roc <- function(models, feature_sets) {
    curves <- imap_dfr(models, \(model, key) {
        y <- feature_sets[[model$feature_set]]$val$y
        curve <- glmnet::roc.glmnet(model$val_pred, newy = y) |>
            as_tibble() |>
            mutate(key = key)
        return(curve)
    })

    plot <- ggplot(curves, aes(FPR, TPR, colour = key)) +
        geom_abline(linetype = "dashed", colour = "grey60") +
        geom_line() +
        coord_equal() +
        labs(
            title = "ROC curves on the validation tweets",
            x = "False positive rate", y = "True positive rate", colour = NULL
        ) +
        theme_bw()

    return(plot)
}
