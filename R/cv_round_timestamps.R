#' Round timestamps to given intervals
#'
#' @param dat Data frame of compiled validation data in a long format.
#'
#' @param do_percent_sat_int Character string indicating the time interval for
#'   binning the dissolved oxygen (percent saturation) timestamps. Default is
#'   \code{"10 minutes"}.
#'
#' @param do_mg_l_int Character string indicating the time interval for
#'   binning the dissolved oxygen (mg/L) timestamps. Default is \code{"10
#'   minutes"}.
#'
#' @param ph_int Character string indicating the time interval for binning the
#'   pH timestamps. Default is \code{"15 minutes"}.
#'
#' @param sal_int Character string indicating the time interval for binning the
#'   salinity timestamps. Default is \code{"10 mintues"}.
#'
#' @param temp_int Character string indicating the time interval for binning the
#'   temperature timestamps. Default is \code{"15 minutes"}.
#'
#' @return Returns dat with an additional column \code{round_timestamp}.
#'
#' @importFrom dplyr case_when filter mutate
#' @importFrom lubridate is.POSIXct round_date
#'
#' @export

cv_round_timestamps <- function(
    dat,
    do_percent_sat_int = "10 minutes",
    do_mg_l_int = "15 minutes",
    ph_int = "10 minutes",
    sal_int = "10 minutes",
    temp_int = "15 minutes"
) {

  if(!is.POSIXct(dat$timestamp_utc)) {
    stop("timestamp_utc must be class POSIXct")
  }

  dat %>%
    mutate(
      round_timestamp = case_when(
        variable == "dissolved_oxygen_percent_saturation" ~
          round_date(timestamp_utc, do_percent_sat_int),

        variable == "dissolved_oxygen_uncorrected_mg_per_l" ~
          round_date(timestamp_utc, do_mg_l_int),

        variable == "ph_ph" ~
          round_date(timestamp_utc, ph_int),

        variable == "salinity_psu" ~ round_date(timestamp_utc, sal_int),

        variable == "temperature_degree_c" ~ round_date(timestamp_utc, temp_int),

        TRUE ~ NA
      )
    )
}
