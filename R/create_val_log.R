#' Create metadata log from Tracking Google sheet
#'
#' @param Tracking Dataframe created from downloaded CalVal Tracking Google sheet
#'
#' @return Returns metadata log with updated column names and new columns with deployment and retrieval datetime in Atlantic timezone
#' 
#' @author Nicole Torrie
#' 
#' @importFrom dplyr %>% mutate rename
#' @importFrom lubridate as_date as_datetime
#' 
#' @export
#'




create_val_log <- function(Tracking){
  Tracking %>%
    mutate(Logger_Latitude = 44.66) %>%
    mutate(Logger_Longitude = -63.56) %>%
    mutate(Sensor_Depth = 1) %>%
    mutate(Deployment_Waterbody = "bucket") %>%
    mutate(`Lease#` = "bucket") %>%
    mutate(configuration = "attached to fixed structure") %>%
    mutate(Location_Description = "validation") %>%
    rename(
      "ID" = `validation event id`,
      "Deployment" = `validation start date`,
      "validation_start_time" = `validation start time (AST)`,
      "Retrieval" = `validation end date`,
      "validation_end_time" = `validation end time (AST)`,
      "Logger_Model" = `sensor model`,
      "Serial#" = `serial number`,
      "firmware_version" = `firmware version`,
      "calibration_attendant" = `name of calibration attendant`,
      "calibration_date" = `calibration date`,
      "calibration_room_temp_c" = `calibration room temperature (C)`,
      "calibration_water_temp_c" = `calibration water temperature (C)`,
      "calibration_pressure_kpa" = `calibration pressure (kPa)`,
      "validation_variable" = `validation variable`,
      "validation_attendant" = `name of validation attendant`,
      "validation_start_room_temp_c" = `validation room temperature (C) at start`,
      "validation_start_pressure_kpa" = `validation pressure (kPa) at start`,
      "validation_end_room_temp_c" = `validation room temperature (C) at end`,
      "validation_end_pressure_kpa" = `validation pressure (kPa) at end`,
      "validation_status" = `validation status (CMAR USE)`,
      "percent_bad_temp" = `percent bad temp readings`,
      "percent_bad_do" = `percent bad do readings`,
      "percent_bad_sal" = `percent bad sal readings`)  %>%
    mutate(Deployment =  as_date(Deployment, format = "%d-%b-%y")) %>%
    mutate(validation_start_time = paste0(validation_start_time, ":00")) %>%
    mutate(deployment_datetime = as_datetime(paste0(Deployment, sep = " ", validation_start_time), tz = "Canada/Atlantic")) %>%
    
    mutate(Retrieval =  as_date(Retrieval, format = "%d-%b-%y")) %>%
    mutate(validation_end_time = paste0(validation_end_time, ":00")) %>%
    mutate(retrieval_datetime = as_datetime(paste0(Retrieval, sep = " ", validation_end_time), tz = "Canada/Atlantic")) 
  

  
  
}


