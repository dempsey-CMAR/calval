#' Create variable for SAL test start time in UTC 
#'
#' @param Log Metadata log with deployment_datetime column
#'
#' @return Returns SALstarttime_utc
#' 
#' @author Nicole Torrie
#' 
#' @importFrom dplyr filter %>% row_number
#' @importFrom lubridate as_datetime
#' 
#' @export
#'


#Salinity
#Convert the test start/end times to UTC 
#Add 3 hrs to convert from ADT (dst true). Daylight savings (ADT) runs roughly from March 12- November 5.
#Add 4 hrs to convert from AST (dst false)

assign_trim_start_sal <- function(Log){
  SALLog <- filter(Log, validation_variable == "SAL") %>%
    filter(row_number()==1)
  SALstarttime_utc <- as_datetime(SALLog$deployment_datetime, tz = "UTC")
}


#' Create variable for SAL test end time in UTC 
#'
#' @param Log Metadata log with deployment_datetime column
#'
#' @return Returns SALendtime_utc
#' 
#' @author Nicole Torrie
#' 
#' @importFrom dplyr filter %>% row_number
#' @importFrom lubridate as_datetime
#' 
#' @export
#'

assign_trim_end_sal <- function(Log){
  SALLog <- filter(Log, validation_variable == "SAL") %>%
    filter(row_number()==1)
  SALendtime_utc <- as_datetime(SALLog$retrieval_datetime, tz = "UTC")
}

