#' Create variable for Hobo DO test start time in UTC 
#'
#' @param Log Metadata log with deployment_datetime column
#'
#' @return Returns HDOstarttime_utc
#' 
#' @author Nicole Torrie
#' 
#' @importFrom dplyr filter %>% row_number
#' @importFrom lubridate as_datetime
#' 
#' @export
#'


#Hobo DO
#Convert the test start/end times to UTC 
#Add 3 hrs to convert from ADT (dst true). Daylight savings (ADT) runs roughly from March 12- November 5.
#Add 4 hrs to convert from AST (dst false). Roughly Nov 6-March 11.

assign_trim_start_hdo <- function(Log){
  HDOLog <- filter(Log, validation_variable == "DO", Logger_Model == "HOBO DO") %>%
    filter(row_number()==1)
  HDOstarttime_utc <- as_datetime(HDOLog$deployment_datetime, tz = "UTC")
}


#' Create variable for HOBO DO test end time in UTC
#'
#' @param Log Metadata log with deployment_datetime column
#'
#' @return Returns HDOendtime_utc
#' 
#' @author Nicole Torrie
#' 
#' @importFrom dplyr filter %>% row_number
#' @importFrom lubridate as_datetime
#' 
#' @export
#'


assign_trim_end_hdo <- function(Log){
  HDOLog <- filter(Log, validation_variable == "DO", Logger_Model == "HOBO DO") %>%
    filter(row_number()==1)
  HDOendtime_utc <- as_datetime(HDOLog$retrieval_datetime, tz = "UTC")
}

