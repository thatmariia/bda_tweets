# Detection of Offensive Tweets

Team repository for the BDA Kaggle competition.

## Structure

```
notebook.Rmd        main notebook: includes everything below
analysis/           sub-notebooks, included in alphabetical order
R/                  functions, all included in the notebook
setup.R             packages and data paths
data/               Kaggle data (not in Git)
output/             generated files (not in Git)
build_notebook.R    creates output/notebook.ipynb for Kaggle
check_style.R       formats code and checks style
```

New files in `analysis/` and `R/` are picked up automatically.

## Setup

1. Clone the repository and open `bda_tweets.Rproj`.
2. Install the packages:
   ```r
   install.packages("renv")
   renv::restore()
   ```
3. Download the competition data into `data/` (see `data/README.md`).

## Workflow

Work in your own branch and merge through a pull request; don't commit to `main` directly.

```bash
git switch main
git pull
git switch -c feature/my-feature
# ... make changes ...
git add analysis/02_features.Rmd R/features.R
git commit -m "Add my feature"
git push -u origin feature/my-feature
```

Then open a pull request on GitHub and ask a teammate to review it.

### Running and previewing

Each sub-notebook in `analysis/` works like a normal notebook. Its first chunk loads the setup, the functions, and the results of earlier sub-notebooks; run it, then run any chunk with ▶. After changing a file in `R/`, rerun that first chunk.

Sub-notebooks pass results on with `saveRDS()` in their last chunk and `readRDS()` in the first chunk of the next one. These chunks use `include = FALSE`, so they don't appear in the final notebook. If you change an earlier sub-notebook, run it again so later ones load up-to-date results.

Knit `notebook.Rmd` to preview the whole notebook (this also refreshes all saved results).

Keep reusable code in `R/` as documented functions, and keep the analysis steps visible in the notebook as commented `dplyr` pipelines. Follow the [tidyverse style guide](https://style.tidyverse.org). Before committing, run `check_style.R` (click **Source**): it formats your code with styler and lists remaining issues from lintr.

If you add a package, run `renv::snapshot()` and commit `renv.lock`.

## Submitting to Kaggle

1. Run `build_notebook.R` (click **Source** in RStudio).
2. On Kaggle, create a notebook, choose **File → Import Notebook**, and upload `output/notebook.ipynb`.
3. Add the competition data, then **Save Version** and submit `submission.csv` from the output.

Don't edit the generated notebook on Kaggle; change the files in the repository and build again.
