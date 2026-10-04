library(tidyverse)
library(tidytext)
library(Matrix)
library(glmnet)

options(width = 100)
options(repr.matrix.max.rows = 8)
set.seed(2025)

on_kaggle <- dir.exists("/kaggle/input")
final_submission <- FALSE
run_studies <- TRUE

n_cores <- if (.Platform$OS.type == "windows") {
    1
} else if (on_kaggle) {
    parallel::detectCores()
} else {
    max(1, parallel::detectCores() - 1)
}
if (n_cores > 1) doMC::registerDoMC(cores = n_cores) else foreach::registerDoSEQ()

# tweets per chunk when building features; smaller locally, so many cores fit in memory
chunk_size <- if (on_kaggle) 20000 else 5000

data_dir <- if (on_kaggle) {
    list.files("/kaggle/input/competitions", full.names = TRUE)[1]
} else {
    here::here("data")
}
output_dir <- if (on_kaggle) "/kaggle/working" else here::here("output")
dir.create(output_dir, showWarnings = FALSE)
