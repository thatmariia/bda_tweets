# ==========================================================
# == FUNCTIONS FOR COMPARING FEATURE SETS
# ==========================================================

#' Validation AUC of one feature set
#' @param foldid Cross-validation fold of each training tweet, the same for every feature set
#' @return A one-row data frame with `n_features`, `val_auc` and `seconds`
evaluate_features <- function(config, data_split, foldid) {
    start <- Sys.time()

    train <- prepare_features(data_split$train, config)
    val <- prepare_features(data_split$val, recipe = train$recipe)

    fit <- glmnet::cv.glmnet(
        train$x, train$y,
        family = "binomial", foldid = foldid, type.measure = "auc"
    )
    pred <- predict(fit, val$x, s = "lambda.min", type = "response") |> drop()

    result <- tibble(
        n_features = ncol(train$x),
        val_auc = glmnet::assess.glmnet(pred, newy = val$y, family = "binomial")$auc,
        seconds = as.numeric(difftime(Sys.time(), start, units = "secs"))
    )

    return(result)
}

#' Validate AUC of every feature set in a table
evaluate_feature_options <- function(options, data_split) {
    configs <- options |>
        select(-key) |>
        pmap(feature_config)

    # same 3 folds for every feature set
    foldid <- sample(rep_len(1:3, nrow(data_split$train)))

    scores <- map_parallel(
        configs,
        \(config) evaluate_features(config, data_split, foldid),
        n_cores = n_cores
    )

    # failed jobs return their error instead of stopping, so stop here
    failed <- map_lgl(scores, \(score) inherits(score, "try-error"))
    if (any(failed)) {
        stop("Feature set ", options$key[which(failed)[1]], " failed: ", scores[failed][[1]])
    }

    results <- options |>
        mutate(result = scores) |>
        unnest(result)

    return(results)
}

#' Apply `fun` to each item on several cores, with a progress bar in interactive sessions
#'
#' A new item is started as soon as a core is free. Errors are returned as "try-error"
#' objects instead of stopping, like parallel::mclapply().
#'
#' @param items A list
#' @param fun A function of one item
#' @param n_cores Number of items to run at the same time
#' @return A list with the result of `fun` for each item, in the same order as `items`
map_parallel <- function(items, fun, n_cores) {
    show_progress <- !on_kaggle
    if (show_progress) progress <- txtProgressBar(max = length(items), style = 3)

    results <- vector("list", length(items))

    if (n_cores == 1) {
        for (i in seq_along(items)) {
            results[[i]] <- try(fun(items[[i]]), silent = TRUE)
            if (show_progress) setTxtProgressBar(progress, i)
        }
        if (show_progress) close(progress)
        return(results)
    }

    running <- list() # jobs that are still running
    item_of_job <- c() # item index per job, named by the job's process id
    n_started <- 0
    n_done <- 0

    while (n_done < length(items)) {
        # start new jobs while there are free cores
        while (length(running) < n_cores && n_started < length(items)) {
            n_started <- n_started + 1
            job <- parallel::mcparallel(fun(items[[n_started]]))
            running <- c(running, list(job))
            item_of_job[as.character(job$pid)] <- n_started
        }

        # collect the jobs that finished within the last second
        finished <- parallel::mccollect(running, wait = FALSE, timeout = 1)
        for (pid in names(finished)) {
            results[item_of_job[pid]] <- finished[pid]
            n_done <- n_done + 1
        }
        running <- running |> discard(\(job) as.character(job$pid) %in% names(finished))

        if (show_progress) setTxtProgressBar(progress, n_done)
    }

    if (show_progress) close(progress)

    return(results)
}

#' Settings of the feature set with the given key
config_from_options <- function(options, chosen_key) {
    settings <- options |>
        filter(key == chosen_key) |>
        select(-key)
    stopifnot(nrow(settings) == 1)

    return(do.call(feature_config, as.list(settings)))
}

#' Plot the validation AUC of each feature set against the full DTM baseline
#' @param results Output of evaluate_feature_options()
#' @param baseline_key Key of the feature set to compare against; skipped if it isn't in `results`
#' @param title Title of the plot
plot_feature_results <- function(results, baseline_key = "full_dtm_counts",
                                 title = "Validation AUC per feature set") {
    baseline_auc <- results |>
        filter(key == baseline_key) |>
        pull(val_auc)
    has_baseline <- length(baseline_auc) == 1
    if (!has_baseline) baseline_auc <- NA_real_

    plot <- results |>
        mutate(
            baseline_rel = case_when(
                !has_baseline ~ "no baseline",
                key == baseline_key ~ "baseline",
                val_auc < baseline_auc ~ "worse",
                val_auc > baseline_auc ~ "better",
                TRUE ~ "same"
            ),
            # single out the best val in baseline_rel
            baseline_rel = ifelse(val_auc == max(val_auc), "best", baseline_rel),
            key = fct_rev(fct_inorder(key)) # first option at the top
        ) |>
        ggplot(aes(x = val_auc, y = key, colour = baseline_rel))

    if (has_baseline) {
        plot <- plot +
            # vertical line for baseline
            geom_vline(xintercept = baseline_auc, linetype = "dashed") +
            # horizontal segments from baseline to each feature set
            geom_segment(aes(x = baseline_auc, xend = val_auc, yend = key))
    }

    plot <- plot +
        geom_point(size = 3) +
        labs(
            title = title,
            x = "Validation AUC",
        ) +
        theme_bw()

    return(plot)
}
