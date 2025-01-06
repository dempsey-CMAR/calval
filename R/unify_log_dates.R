#' Unify dates in metadata log
#'
#' @param Log Metadata log
#'
#' @return Returns cleaned log with unified deployment and retrieval dates for each sensor
#' 
#' @author Nicole Torrie
#' 
#' @importFrom dplyr %>% mutate
#' 
#' @export
#'


#Function to set the deployment and retrieval dates for each sensor to match
# Deployment is the earliest deployment date of all sensors in the validation test
# Retrieval is the latest retrieval data of all sensors in the validation test

unify_log_dates <- function(Log = Log) {
  Log %>%
    mutate(Deployment = min(Deployment)) %>%
    mutate(Retrieval = max(Retrieval)) 
}


