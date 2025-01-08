#' Plot sensor data coloured by flag value
#'
#' @param dat Data frame of flagged validation data in long or wide format. Must
#'   include at least one column name with the string "qc_flag".
#'
#' @param vars Character vector of variables to plot. Default is \code{vars =
#'   "all"}, which will make a plot for each recognized variable in \code{dat}.
#'
#' @param plotly_friendly Logical argument. If \code{TRUE}, the legend will be
#'   plotted when \code{plotly::ggplotly} is called on \code{p}. Default is
#'   \code{FALSE}, which makes the legend look better in a static figure.
#'
#' @return Returns a list of ggplot objects; one figure for each variable in
#'   \code{vars}. Points are coloured by the flag
#'
#' @importFrom lubridate as_datetime
#'
#' @export
#'

cv_plot_flags <- function(dat, vars = "all", plotly_friendly = FALSE) {

  p <- list()
  p_out <- list()

  if (!("variable" %in% colnames(dat))) {
    dat <- ss_pivot_longer(dat)
  }

  dat <- dat %>% qc_assign_flag_labels()

  if (vars == "all") vars <- unique(dat$variable)

  # plot for each variable
  for (i in seq_along(vars)) {
    var_i <- vars[i]

    dat_i <- filter(dat, variable == var_i)

    # if nrow is 0 don't make a plot
    if (nrow(dat_i) == 0) {
      stop("No data for variable << ", var_i, " >>")
    }

    p[[var_i]] <- cv_ggplot_flags(
      dat_i,
      var = var_i,
      plotly_friendly = plotly_friendly
    )

  }

  p
}


#' Create ggplot for one variable
#'
#' @param dat Data frame of flagged validation data in long format. Must include
#'   a column named with the string "qc_flag".
#'
#' @param var variable to plot.
#'
#' @return Returns a ggplot object; a figure of validation results coloured by
#'   flag for \code{var}.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes geom_point geom_ribbon ggplot guides guide_legend
#'   scale_colour_manual scale_x_datetime scale_y_continuous theme_light theme
#'


cv_ggplot_flags <- function(
    dat,
    valour,
    colour_col = "qc_flag",
    flag_title = TRUE,
    plotly_friendly = FALSE
) {

  flag_colours <- c("chartreuse4", "#DB4325", NA, NA)

  if(var == "dissolved_oxygen_percent_saturation") {
    y_limits <- c(80, 110)
  } else y_limits <- NULL

  #browser()

  sensors <- unique(dat$sensor_type)

  #if(length(sensors[-which(str_detect(sensors, "vr2ar"))]) > 0) {
  if(nrow(filter(dat, !str_detect(sensor_type, "vr2ar"))) > 0) {
    var_ribbon <- geom_ribbon(
      data = filter(dat, !str_detect(sensor_type, "vr2ar")),
      aes(ymin = med - tolerance, ymax = med + tolerance),
      alpha = 0.3, col = "grey20", fill = "grey75"
    )
  } else var_ribbon <- NULL

  if(any(str_detect(sensors, "vr2ar"))) {

    temp_vr <- dat %>%
      filter(str_detect(sensor_type, "vr2ar"))

    vr2_ribbon <- geom_ribbon(
      data = temp_vr,
      aes(ymin = med - tolerance, ymax = med + tolerance),
      alpha = 0.3, col = "grey20", fill = "grey87"
    )
  } else vr2_ribbon <- NULL

  #p <-

    dat %>%
    ggplot(aes(round_timestamp, value, color = qc_flag)) +
    var_ribbon +
    vr2_ribbon +
    geom_point(show.legend = TRUE) +
    scale_y_continuous(var, limits = y_limits) +
    scale_x_datetime("Date") +
    scale_colour_manual("Flag Value", values = flag_colours, drop = TRUE) +
    theme_light() +
    theme(
      strip.text = element_text(colour = "black", size = 10),
      strip.background = element_rect(fill = "white", colour = "darkgrey")
    )

  if(isFALSE(plotly_friendly)) {
    p <- p + guides(color = guide_legend(override.aes = list(size = 4)))
  }

  p
}
