#' Clean log for export
#'
#' @param Log Metadata log 
#'
#' @return Returns cleaned log which sensorstrings can import
#' 
#' @author Nicole Torrie
#' 
#' @export
#'
#' 


#unify log dates and remove duplicate log items in order to create a log which sensorstrings can import

clean_log_all <- function(Log = Log) {
  Log <- unify_log_dates(Log)
  cleaned_log <- clean_log_duplicates(Log) 
}


