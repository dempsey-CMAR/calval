#' Create metadata log from calval tracking sheet
#'
#' @param path File path to the Log folder.
#'
#' @param tracking calval tracking sheet, as downloaded from
#'   \code{cv_read_calval_tracking()}.
#'
#' @return Exports csv file to /log folder in the log format required for
#'   compiling data with \code{sensorstrings}.
#'
#' @importFrom data.table fwrite
#' @importFrom dplyr %>% distinct mutate rename
#' @importFrom lubridate as_date as_datetime
#'
#' @export
#'

cv_create_val_log <- function(path, tracking) {

  val_id <- unique(tracking$validation_id)

  if(length(val_id) > 1) {
    warning("More than 1 validation id found in tracking: ",
      paste(val_id, collapse = " "))
  }

  tracking %>%
    distinct(sensor_serial_number, .keep_all = TRUE) %>%
    mutate(
      Logger_Latitude = 44.66,
      Logger_Longitude = -63.56,
      Sensor_Depth = 999,
      Deployment_Waterbody = "bucket",
      `Lease#` = NA,
      Configuration = NA,
      Location_Description = "validation",
      Deployment = min(val_start_date),
      Retrieval = max(val_end_date),
    ) %>%
    select(
      validation_id,
      Deployment_Waterbody,
      Location_Description,
      `Lease#`,
      Deployment,
      Retrieval ,
      Logger_Latitude,
      Logger_Longitude,
      Logger_Model = sensor_model,
      `Serial#` = sensor_serial_number,
      Sensor_Depth,
      Configuration
    ) %>%
    fwrite(
      file = paste0(path, "/", "Log", "/", val_id, "_log.csv"),
      na = "NA",
      showProgress = TRUE,
      col.names = TRUE
    )
}




