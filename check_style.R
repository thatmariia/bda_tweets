# Formats all R and R Markdown files, then lists remaining style issues.
# Both follow the tidyverse style guide.

styler::style_dir(filetype = c("R", "Rmd"))
lintr::lint_dir()
