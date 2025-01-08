#' Trim compiled data to the start and end of each test
#'
#' @param dat Data frame of compiled validation data in wide format.
#'
#' @param trimtimes Data frame with the start and end timestamps for each
#'   variable. Must have three columns: variable, deployment_utc, retrieval_utc.
#'
#'
#' @return Returns dat in a long format, trimmed to the start and end of the
#'   test for each variable.
#'
#' @importFrom sensorstrings ss_pivot_longer
#' @importFrom dplyr case_when filter if_else left_join join_by mutate
#'
#' @export

cv_trim_dat <- function(dat, trimtimes) {

  dat %>%
    ss_pivot_longer() %>%
    # because DO can be measured in two units
    mutate(
      units = case_when(
        variable == "dissolved_oxygen_percent_saturation" ~ "percent_saturation",
        variable == "dissolved_oxygen_uncorrected_mg_l" ~ "mg_l",
        TRUE ~ NA
      ),
      variable = if_else(
        str_detect(variable, "dissolved_oxygen"), "dissolved_oxygen", variable
      )
    ) %>%
    left_join(trimtimes, by = join_by(variable)) %>%
    filter(timestamp_utc >= deployment_utc, timestamp_utc <= retrieval_utc) %>%
    mutate(
      variable = case_when(
        !is.na(units) ~ paste(variable, units, sep = "_"),
        TRUE ~ variable
      )
    ) %>%
    select(-c(units, deployment_utc, retrieval_utc))

}
