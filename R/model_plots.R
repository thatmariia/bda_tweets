# ==========================================================
# == FUNCTIONS FOR PLOTTING MODEL RESULTS
# ==========================================================

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
