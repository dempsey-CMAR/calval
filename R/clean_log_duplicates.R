#' Clean duplicate rows out of metadata log
#'
#' @param Log Metadata log
#'
#' @return Returns cleaned log with no duplicate rows from multi-variable sensors
#' 
#' @author Nicole Torrie
#' 
#' @importFrom dplyr distinct rename %>%
#' 
#' @export
#'



#clean_log_duplicates
#remove duplicate rows from multi variable sensors in Log
clean_log_duplicates <- function(Log) {
  cleaned_log <-
    data.frame(distinct(Log, `Serial#`, .keep_all = TRUE)) %>%
    rename("Serial#" = `Serial.`,
           "Lease#" = `Lease.`)
}


