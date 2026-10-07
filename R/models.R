# ==========================================================
# == FUNCTIONS FOR TRAINING MODELS
# ==========================================================

#' Alpha of each glmnet method: 1 = lasso, 0 = ridge, in between = elastic net
glmnet_alphas <- c(lasso = 1, ridge = 0, elastic_net = 0.5)

#' ==> START LLM src=https://aichat.uva.nl/share/weTlMy8A3kMHqVeDf8TeVWwynGm3NCpI1Vtj
#' Conservative settings for tree-based xgboost:
xgboost_params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.05,
    max_depth = 4,
    min_child_weight = 5,
    subsample = 0.8,
    colsample_bytree = 0.5,
    gamma = 0.1,
    lambda = 1,
    alpha = 0,
    tree_method = "hist",
    nthread = n_cores
)
#' ==> END LLM

#' Fit a glmnet model
#' @param alpha See glmnet_alphas
#' @return A list with the fitted model, its alpha and chosen lambda, and its tuning results
fit_glmnet <- function(x, y, foldid, alpha) {
    # the folds are fitted in parallel, on the backend registered in setup.R
    fit <- glmnet::cv.glmnet(
        x, y,
        family = "binomial", alpha = alpha, foldid = foldid, type.measure = "auc",
        parallel = TRUE
    )

    tuning <- tibble(
        parameter = "log10(lambda)",
        value = log10(fit$lambda),
        cv_auc = fit$cvm,
        cv_se = fit$cvsd
    )

    return(list(fit = fit, alpha = alpha, lambda = fit$lambda.min, tuning = tuning))
}

#' Refit a glmnet model on new data with the alpha and lambda chosen before
#' @param model A model from fit_glmnet()
#' @return A list like the one from fit_glmnet()
refit_glmnet <- function(x, y, model) {
    lambdas <- model$fit$lambda[model$fit$lambda >= model$lambda]
    fit <- glmnet::glmnet(x, y, family = "binomial", alpha = model$alpha, lambda = lambdas)

    return(list(fit = fit, alpha = model$alpha, lambda = model$lambda, tuning = model$tuning))
}

#' Predict probabilities with a glmnet model
predict_glmnet <- function(model, x) {
    pred <- predict(model$fit, x, s = model$lambda, type = "response") |> drop()
    return(pred)
}

#' ==> START LLM src=https://aichat.uva.nl/share/weTlMy8A3kMHqVeDf8TeVWwynGm3NCpI1Vtj
#' Fit a tree-based xgboost model
#' The number of boosting rounds is selected by CV AUC on the training tweets
fit_xgboost <- function(x, y, foldid) {
    dtrain <- xgboost::xgb.DMatrix(data = x, label = y)
    folds <- split(seq_along(y), foldid)

    cv <- xgboost::xgb.cv(
        params = xgboost_params,
        data = dtrain,
        nrounds = 500,
        folds = folds,
        early_stopping_rounds = 25,
        verbose = 0
    )

    nrounds <- cv$early_stop$best_iteration

    if (is.null(nrounds)) {
        nrounds <- cv$best_iteration
    }

    if (is.null(nrounds)) {
        nrounds <- which.max(cv$evaluation_log$test_auc_mean)
    }

    fit <- xgboost::xgb.train(
        params = xgboost_params,
        data = dtrain,
        nrounds = nrounds,
        verbose = 0
    )

    tuning <- cv$evaluation_log |>
        transmute(
            parameter = "nrounds",
            value = iter,
            cv_auc = test_auc_mean,
            cv_se = test_auc_std
        )

    return(list(
        fit = fit,
        params = xgboost_params,
        nrounds = nrounds,
        tuning = tuning
    ))
}

#' Refit an xgboost model using its chosen settings and number of rounds
refit_xgboost <- function(x, y, model) {
    dtrain <- xgboost::xgb.DMatrix(data = x, label = y)

    fit <- xgboost::xgb.train(
        params = model$params,
        data = dtrain,
        nrounds = model$nrounds,
        verbose = 0
    )

    return(list(
        fit = fit,
        params = model$params,
        nrounds = model$nrounds,
        tuning = model$tuning
    ))
}

#' Predict probabilities with an xgboost model
predict_xgboost <- function(model, x) {
    dtest <- xgboost::xgb.DMatrix(data = x)
    pred <- predict(model$fit, dtest)
    return(pred)
}
# ==> END LLM


#' Fit one model on a feature set and score it on the validation tweets
fit_option <- function(feature_set, method, feature_sets, foldid) {
    start <- Sys.time()
    data <- feature_sets[[feature_set]]

    model <- if (method %in% names(glmnet_alphas)) {
        fit_glmnet(data$train$x, data$train$y, foldid, glmnet_alphas[[method]])
    } else if (method == "xgboost_tree") {
        fit_xgboost(data$train$x, data$train$y, foldid)
    } else {
        stop("Unknown method: ", method)
    }

    model$method <- method
    model$feature_set <- feature_set

    model$val_pred <- if (method %in% names(glmnet_alphas)) {
        predict_glmnet(model, data$val$x)
    } else {
        predict_xgboost(model, data$val$x)
    }

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
    allowed_methods <- c(names(glmnet_alphas), "xgboost_tree")
    stopifnot(all(options$method %in% allowed_methods))

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

    model <- if (option$method %in% names(glmnet_alphas)) {
        refit_glmnet(x, y, models[[best_key]])
    } else if (option$method == "xgboost_tree") {
        refit_xgboost(x, y, models[[best_key]])
    } else {
        stop("Unknown method: ", option$method)
    }
    
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

#' Standardized coefficients of a glmnet model
#' @param x The training matrix the model was fitted on (for the standard deviations)
glmnet_importance <- function(model, x) {
    coefs <- coef(model$fit, s = model$lambda)[-1, 1] # without the intercept
    column_sd <- sqrt(pmax(Matrix::colMeans(x^2) - Matrix::colMeans(x)^2, 0))

    importance <- tibble(
        column = names(coefs),
        coefficient = coefs,
        standardized = coefs * column_sd[names(coefs)]
    ) |>
        filter(standardized != 0)

    return(importance)
}

#' Plot the features that influence the prediction towards offensive or not offensive
plot_important_words <- function(model, x, most_n = 15, least_n = 15) {
    importance <- glmnet_importance(model, x)

    top <- bind_rows(
        importance |> slice_max(standardized, n = most_n),
        importance |> slice_min(standardized, n = least_n)
    ) |>
        filter(standardized != 0) |>
        mutate(
            direction = if_else(standardized > 0, "towards offensive", "towards not offensive"),
            label = fct_reorder(column, standardized)
        )

    plot <- ggplot(top, aes(x = standardized, y = label, fill = direction)) +
        geom_col() +
        geom_vline(xintercept = 0, colour = "grey") +
        scale_fill_manual(values = c(
            "towards offensive" = "#e34948",
            "towards not offensive" = "#2a78d6"
        )) +
        labs(
            title = "Feature impacts of the best model",
            x = "Standardized coefficient", y = NULL, fill = NULL
        ) +
        theme_minimal() +
        theme(legend.position = "top")

    return(plot)
}
