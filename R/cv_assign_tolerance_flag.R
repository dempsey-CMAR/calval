#' Assign a flag to each observation
#'
#' The tolerance flag is based on the median value of the time interval
#' plus/minus to tolerance (accuracy) for each variable.
#'
#' TODO: Add option to overwrite tol_min and tol_max with threshold values,
#' e.g., for dissolved oxygen.
#'
#' @param dat Data frame of compiled validation data with column
#'   \code{round_timestamp}, as returned from \code{cv_round_timestamps()}.
#'
#' @param tolerance A data frame with the expected accuracy for each variable.
#'   Must have two columns: \code{variable}, which has an entry for each
#'   variable in \code{dat}, and \code{tol}, with the accuracy. Note this
#'   assumes only one type of sensor for each variable, or that the sensors have
#'   the same tolerance. Exception: the tolerance for the VR2AR temperature
#'   sensors may be set in \code{vr2ar_tol} argument.
#'
#' @param vr2ar_tol The tolerance for the VR2AR temperature sensors. **The
#'   default will overwrite the temperature tolerance in \code{tolerance}.** Set
#'   \code{vr2ar_tol = NULL} to use the same value for all temperature sensors.
#'
#' @return Returns dat with several additional columns: tolerance, med,
#'   tol_lower, tol_upper, and qc_flag.
#'
#' @importFrom dplyr if_else group_by left_join mutate ungroup
#' @importFrom stats median
#'
#' @export


cv_assign_tolerance_flag <- function(dat, tolerance = NULL, vr2ar_tol = 0.5) {

  if(is.null(tolerance)) {
    tolerance <- data.frame(
      variable = c(
        "dissolved_oxygen_percent_saturation",
        "dissolved_oxygen_uncorrected_mg_per_l",
        "ph_ph",
        "salinity_psu",
        "temperature_degree_c"),
      tolerance = c(5, 0.2, 0.1, 1, 0.2)
    )
  }

  dat <- dat %>%
    left_join(tolerance, by = join_by(variable))

  if(!is.null(vr2ar_tol)) {
    dat <- dat %>%
      mutate(tolerance = if_else(
        str_detect(sensor_type, "vr2ar"), vr2ar_tol, tolerance)
      )
  }

  dat %>%
    group_by(variable, round_timestamp) %>%
    mutate(
      med = median(value),
      tol_lower = med - tolerance,
      tol_upper = med + tolerance,
      qc_flag = if_else(value < tol_lower | value > tol_upper, 4, 1),
      qc_flag = ordered(qc_flag, levels = c(1, 4))
    ) %>%
    ungroup()
}
