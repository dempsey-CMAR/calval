# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# other technical solution here:
# https://dplyr.tidyverse.org/articles/programming.html


utils::globalVariables(
  c(
    # helpers
    "val_id",

    # cv_read_calval_tracking
    "validation event id",
    "sensor model",
    "serial number",

    "validation start date",
    "validation start time (AST)",

    "validation end date",
    "validation end time (AST)",

    "validation variable",
    "validation status (CMAR USE)",
    "percent bad do readings",
    "percent bad temp readings",
    "percent bad sal readings",

    "name of calibration attendant",
    "notes"
  ))







