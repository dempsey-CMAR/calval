#' Create metadata log from calval tracking sheet
#'
#' @param tracking calval tracking sheet, as downloaded from
#'   \code{cv_read_calval_tracking()}.
#'
#' @return Returns data frame in the log format required for compiling data with
#'   \code{sensorstrings}
#'
#' @author Nicole Torrie
#'
#' @importFrom dplyr %>% mutate rename
#' @importFrom lubridate as_date as_datetime
#'
#' @export
#'

cv_create_val_log <- function(tracking) {

  trimtime <- tracking %>%
    group_by(variable) %>%
    summarise(
      deployment_utc = min(deployment_utc), retrieval_utc = max(retrieval_utc)
    )

 x <-  tracking %>%
   distinct(sensor_serial_number, .keep_all = TRUE)

    mutate(
      Logger_Latitude = NA,
      Logger_Longitude = NA,
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
      Deployment = val_start_date,
      Retrieval = val_end_date,
      Logger_Latitude,
      Logger_Longitude,
      Logger_Model = sensor_model,
      `Serial#` = sensor_serial_number,
      Sensor_Depth,
      Configuration
    )

}




