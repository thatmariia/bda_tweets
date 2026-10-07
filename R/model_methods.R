# ==========================================================
# == FUNCTIONS FOR MODEL METHODS
# ==========================================================

#' Fit a glmnet model
#' @param alpha 1 = lasso, 0 = ridge, in between = elastic net
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

#' Fit, refit and predict functions of a glmnet method with the given alpha
glmnet_method <- function(alpha) {
    method <- list(
        fit = \(x, y, foldid) fit_glmnet(x, y, foldid, alpha),
        refit = refit_glmnet,
        predict = predict_glmnet,
        submittable = TRUE
    )
    return(method)
}

#' The methods that can be used in the model options
#' `submittable`: whether the method may be used for the leaderboard submission
model_methods <- list(
    lasso = glmnet_method(alpha = 1),
    ridge = glmnet_method(alpha = 0),
    elastic_net = glmnet_method(alpha = 0.5),
    # allowed in the code, but not for the submission (see the models notebook)
    xgboost_tree = list(
        fit = fit_xgboost,
        refit = refit_xgboost,
        predict = predict_xgboost,
        submittable = FALSE
    )
)

#' Predict probabilities with a model of any method
predict_model <- function(model, x) {
    return(model_methods[[model$method]]$predict(model, x))
}
