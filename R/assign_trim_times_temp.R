#' Create variable for TEMP test start time in UTC 
#'
#' @param Log Metadata log with deployment_datetime column
#'
#' @return Returns TEMPstarttime_utc
#' 
#' @author Nicole Torrie
#' 
#' @importFrom dplyr filter %>% row_number
#' @importFrom lubridate as_datetime
#' 
#' @export
#'


#Temperature
#Convert the test start/end times to UTC 
#Add 3 hrs to convert from ADT (dst true). Daylight savings (ADT) runs roughly from March 12- November 5.
#Add 4 hrs to convert from AST (dst false). Roughly Nov 6-March 11.

assign_trim_start_temp <- function(Log){
  TEMPLog <- filter(Log, validation_variable == "Temp") %>%
    filter(row_number()==1)
  TEMPstarttime_utc <- as_datetime(TEMPLog$deployment_datetime, tz = "UTC")
}


#' Create variable for TEMP test end time in UTC
#'
#' @param Log Metadata log with deployment_datetime column
#'
#' @return Returns TEMPendtime_utc
#' 
#' @author Nicole Torrie
#' 
#' @importFrom dplyr filter %>% row_number
#' @importFrom lubridate as_datetime
#' 
#' @export
#'


assign_trim_end_temp <- function(Log){
  TEMPLog <- filter(Log, validation_variable == "Temp") %>%
    filter(row_number()==1)
  TEMPendtime_utc <- as_datetime(TEMPLog$retrieval_datetime, tz = "UTC")
}

