#' Round timestamps to given intervals
#'
#' @param dat Data frame of compiled validation data in a long format.
#'
#' @param round_ints Data frame indicating what intervals to use when rounding
#'   the timestamps for each variable. Must have two columns: \code{variable},
#'   which has an entry for each variable in \code{dat}, and \code{round_ints},
#'   a character string of the corresponding interval, e.g., "10 minutes". The
#'   default interval is 10 minutes for dissolved oxygen and salinity, and 15
#'   minutes for temperature.
#'
#' @return Returns dat with an additional column \code{round_timestamp}.
#'
#' @export


cv_round_timestamps <- function(dat, round_ints = NULL) {

  if(is.null(round_ints)) {
    round_ints <- data.frame(
      variable = c(
        "dissolved_oxygen_percent_saturation",
        "dissolved_oxygen_uncorrected_mg_l",
        "salinity_psu",
        "temperature_degree_c"),
      round_ints = c("10 minutes", "10 minutes", "10 minutes", "15 minutes")
    )
  }

  do_percent_sat_int <- filter(
    round_ints, variable == "dissolved_oxygen_percent_saturation")$round_ints

  do_mg_l_int <- filter(
    round_ints, variable == "dissolved_oxygen_uncorrected_mg_l")$round_ints

  sal_int <- filter(round_ints, variable == "salinity_psu")$round_ints

  temp_int <- filter(round_ints, variable == "temperature_degree_c")$round_ints


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
