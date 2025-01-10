#' Create table of validation test start and end times for each variable
#'
#' @inheritParams cv_create_log
#'
#' @return Returns a data frame of the start and end times for each variable, in
#'   UTC.
#'
#' @importFrom dplyr case_when group_by mutate summarise ungroup


cv_val_trimtimes <- function(tracking) {

  tracking %>%
    group_by(variable) %>%
    summarise(
      deployment_utc = min(deployment_utc),
      retrieval_utc = max(retrieval_utc)
    ) %>%
    mutate(
      variable = case_when(
        variable == "DO" ~ "dissolved_oxygen",
        variable == "SAL" ~ "salinity_psu",
        variable == "Temp" ~ "temperature_degree_c",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
}
