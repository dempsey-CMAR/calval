#' Create table of test start and end times for each variable
#'
#' @param Temp TRUE if Temperature data is present in the dataset
#' @param HDO TRUE if Hobo DO mg/l Dissolved Oxygen data is present in the dataset
#' @param DO TRUE if Dissolved Oxygen percent saturation data is present in the dataset
#' @param SAL TRUE if Salinity data is present in the dataset
#'
#' @return a data frame of all test start and end times in utc
#' 
#' @author Nicole Torrie
#' 
#' @export 
#'
#' 
#' 
#' 

#to assign trim times for all variables at once
assign_trim_times_all <- function(Temp = TRUE,
                                  DO = TRUE,
                                  HDO = FALSE,
                                  SAL = FALSE){
  
  if(Temp == TRUE){
    TEMPstarttime_utc <- assign_trim_start_temp(Log)
    TEMPendtime_utc <- assign_trim_end_temp(Log)
  } else{
    TEMPstarttime_utc <- NA
    TEMPendtime_utc <- NA
  }
  
  if(HDO == TRUE){
    HDOstarttime_utc <- assign_trim_start_hdo(Log)
    HDOendtime_utc <- assign_trim_end_hdo(Log)
  } else{
    HDOstarttime_utc <- NA
    HDOendtime_utc <- NA
  }
  
  if(DO == TRUE){
    DOstarttime_utc <- assign_trim_start_do(Log)
    DOendtime_utc <- assign_trim_end_do(Log)
  } else{
    DOstarttime_utc <- NA
    DOendtime_utc <- NA
  }
  
  if(SAL == TRUE){
    SALstarttime_utc <- assign_trim_start_sal(Log)
    SALendtime_utc <- assign_trim_end_sal(Log)
  } else{
    SALstarttime_utc <- NA
    SALendtime_utc <- NA
  }
  
  trimtime_list <-
    list(
      TimeVariable = c(
        'TEMPstarttime_utc',
        'TEMPendtime_utc',
        'HDOstarttime_utc',
        'HDOendtime_utc',
        'DOstarttime_utc',
        'DOendtime_utc',
        'SALstarttime_utc',
        'SALendtime_utc'
      ),
      DateTime = c(
        TEMPstarttime_utc,
        TEMPendtime_utc,
        HDOstarttime_utc,
        HDOendtime_utc,
        DOstarttime_utc,
        DOendtime_utc,
        SALstarttime_utc,
        SALendtime_utc
      )
    ) 
    
  
  trimtime_table <- data.frame(trimtime_list) 
  
  print(trimtime_table)
  
}






