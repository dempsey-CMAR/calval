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

    "val_start_date",
    "val_end_date",
    "val_start_time",
    "val_end_time",

    "validation variable",
    "validation status (CMAR USE)",
    "percent bad do readings",
    "percent bad temp readings",
    "percent bad sal readings",

    "name of calibration attendant",
    "name of validation attendant",
    "notes",

    # cv_create_val_log
    "validation_id",
    "Deployment_Waterbody",
    "Location_Description",
    "val_start_date",
    "val_end_date",
   "Logger_Latitude",
    "Logger_Longitude",
    "sensor_model",
    "sensor_serial_number",
    "Sensor_Depth",
    "Configuration",

   "Deployment",
   "Retrieval",
   "Lease#",
   "deployment_can",
   "deployment_utc",
   "retrieval_can",
   "retrieval_utc",
   "Fail",
   "Pass",
   "med",
   "n_percent",
   "round_timestamp",
   "sensor_type",
   "timestamp_utc",
   "tol_lower",
   "tol_upper",
   "tolerance",
   "qc_flag",
   "value",
   "variable"
  ))







