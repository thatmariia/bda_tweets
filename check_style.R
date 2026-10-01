# Formats all R and R Markdown files, then lists remaining style issues.
# Both follow the tidyverse style guide.

styler::style_dir(filetype = c("R", "Rmd"), transformers = styler::tidyverse_style(indent_by = 4))
lintr::lint_dir()
