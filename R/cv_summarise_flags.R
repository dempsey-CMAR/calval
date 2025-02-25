#' Validation results for each sensor and variable
#'
#' @param dat Data frame of compiled and flagged validation data, as returned
#'   from \code{cv_assign_tolerance_flag()}.
#'
#' @param wide Logical argument indicating whether to return a wide table of
#'   percentage values, or a long table including counts and percents.
#'
#' @param dt Logical argument indicating whether to open as an interactive
#'   datatable.
#'
#' @return Returns a summary of observations that passed and failed for each
#'   variable and sensor.
#'
#' @importFrom DT datatable
#' @importFrom dplyr arrange case_when desc group_by n ungroup summarise
#' @importFrom tidyr pivot_wider
#' @importFrom qaqcmar qc_assign_flag_labels
#'
#' @export


cv_summarise_flags <- function(dat, wide = TRUE, dt = FALSE) {

  dat <- dat %>%
    group_by(variable, sensor_type, sensor_serial_number, qc_flag) %>%
    summarise(n = n()) %>%
    group_by(variable, sensor_type, sensor_serial_number) %>%
    mutate(n_percent = 100 * round(n / sum(n), digits = 2)) %>%
    ungroup() %>%
    select(-n) %>%
    qc_assign_flag_labels()

  if(isTRUE(wide)) {
    dat <- dat %>%
      pivot_wider(names_from = qc_flag, values_from = n_percent) #%>%

    if(!("Fail" %in% colnames(dat))) dat <- dat %>% mutate(Fail = NA)
    if(!("Pass" %in% colnames(dat))) dat <- dat %>% mutate(Pass = NA)

    dat <- dat %>%
      mutate(
        percent_pass = if_else(is.na(Pass), 0, Pass),
        percent_fail = if_else(is.na(Fail), 0, Fail)
      ) %>%
      select(-c(Pass, Fail))
  }

  # dat <- dat %>%
  #   arrange(desc(Fail))

  if(isTRUE(dt)) {
    datatable(
      dat,
      rownames = FALSE,
      options = list(
        dom = 'ft',
        paging = FALSE,
        searching = TRUE,
        scrollY = "500px",
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  } else dat

}
