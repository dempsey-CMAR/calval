#' Round timestamps to given intervals
#'
#' @param dat Data frame of compiled validation data in a long format.
#'
#' @param do_percent_sat_int Character string indicating the time interval for
#'   binning the dissolved oxygen (percent saturation) timestamps. Default is
#'   \code{"10 mintues"}.
#'
#' @param do_mg_l_int Character string indicating the time interval for
#'   binning the dissolved oxygen (mg/L) timestamps. Default is \code{"10
#'   mintues"}.
#'
#' @param sal_int Character string indicating the time interval for binning the
#'   salinity timestamps. Default is \code{"10 mintues"}.
#'
#' @param temp_int Character string indicating the time interval for binning the
#'   temperature timestamps. Default is \code{"15 mintues"}.
#'
#' @return Returns dat with an additional column \code{round_timestamp}.
#'
#' @importFrom dplyr case_when filter mutate
#' @importFrom lubridate round_date
#'
#' @export

cv_round_timestamps <- function(
    dat,
    do_percent_sat_int = "10 minutes",
    do_mg_l_int = "10 minutes",
    sal_int = "10 minutes",
    temp_int = "15 minutes"
) {

  # if(is.null(round_ints)) {
  #   round_ints <- data.frame(
  #     variable = c(
  #       "dissolved_oxygen_percent_saturation",
  #       "dissolved_oxygen_uncorrected_mg_l",
  #       "salinity_psu",
  #       "temperature_degree_c"),
  #     round_ints = c("10 minutes", "10 minutes", "10 minutes", "15 minutes")
  #   )
  # }
  #
  # do_percent_sat_int <- filter(
  #   round_ints, variable == "dissolved_oxygen_percent_saturation")$round_ints
  #
  # do_mg_l_int <- filter(
  #   round_ints, variable == "dissolved_oxygen_uncorrected_mg_l")$round_ints
  #
  # sal_int <- filter(round_ints, variable == "salinity_psu")$round_ints
  #
  # temp_int <- filter(round_ints, variable == "temperature_degree_c")$round_ints


  dat %>%
    mutate(
      round_timestamp = case_when(
        variable == "dissolved_oxygen_percent_saturation" ~
          round_date(timestamp_utc, do_percent_sat_int),

        variable == "dissolved_oxygen_uncorrected_mg_l" ~
          round_date(timestamp_utc, do_mg_l_int),

        variable == "salinity_psu" ~ round_date(timestamp_utc, sal_int),

        variable == "temperature_degree_c" ~ round_date(timestamp_utc, temp_int),

        TRUE ~ NA
      )
    )
}
