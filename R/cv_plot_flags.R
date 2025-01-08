#' Plot sensor data coloured by flag value
#'
#' @param dat Data frame of flagged validation data in long or wide format.
#'
#' @param vars Character vector of variables to plot. Default is \code{vars =
#'   "all"}, which will make a plot for each recognized variable in \code{dat}.
#'
#' @param colour_col Character string indicating the column to use to colour the
#'   observations.
#'
#' @param pal Vector of colours onto which \code{colour_col} will be mapped.
#'   Default is c("chartreuse4", "#DB4325") when \code{colour_col = "qc_flag"},
#'   and Dark2 from RColourBrewer otherwise.
#'
#' @param plotly_friendly Logical argument. If \code{TRUE}, the legend will be
#'   plotted when \code{plotly::ggplotly} is called on \code{p}. Default is
#'   \code{FALSE}, which makes the legend look better in a static figure.
#'
#' @return Returns a list of ggplot objects; one figure for each variable in
#'   \code{vars}.
#'
#' @importFrom dplyr case_when mutate
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
#'

cv_plot_flags <- function(
    dat,
    vars = "all",
    colour_col = "qc_flag",
    pal = NULL,
    plotly_friendly = FALSE) {

  p <- list()

  if (!("variable" %in% colnames(dat))) {
    dat <- ss_pivot_longer(dat)
  }

  dat <- dat %>%
    mutate(
      qc_flag = case_when(qc_flag == 1 ~ "Pass", qc_flag == 4 ~ "Fail"),
      qc_flag = ordered(qc_flag, levels = c("Pass", "Fail"))
    )

  if (vars == "all") vars <- unique(dat$variable)

  if(is.null(pal)) {
    if(colour_col == "qc_flag") {
      pal <- c("chartreuse4", "#DB4325")
    } else {
      n_colours <- nrow(unique(dat[, colour_col]))
      if(n_colours <= 8) {
        pal <- brewer.pal(8, "Dark2")
      } else {
        pal <- colorRampPalette(brewer.pal(8, "Dark2"))(n_colours)
      }
    }
  }

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
      colour_col = colour_col,
      var = var_i,
      pal = pal,
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
#' @param var Caharacter string of the variable to plot.
#'
#' @param pal Colour palette assigned to the observations.
#'
#' @inheritParams cv_plot_flags
#'
#' @return Returns a ggplot object; a figure of validation results coloured by
#'   flag for \code{var}.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes geom_point geom_ribbon ggplot guides guide_legend
#'   scale_colour_manual scale_x_datetime scale_y_continuous theme_light theme
#' @importFrom rlang sym
#'

cv_ggplot_flags <- function(
    dat,
    var,
    colour_col = "qc_flag",
    pal = NULL,
    plotly_friendly = FALSE
) {

  if(var == "dissolved_oxygen_percent_saturation") {
    y_limits <- c(80, 110)
  } else y_limits <- NULL


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

  p <- dat %>%
    ggplot(aes(round_timestamp, value, colour = !!sym(colour_col))) +
    var_ribbon +
    vr2_ribbon +
    geom_point(show.legend = TRUE) +
    scale_y_continuous(var, limits = y_limits) +
    scale_x_datetime("Date") +
    scale_colour_manual(colour_col, values = pal, drop = FALSE) +
    theme_light()



  if(isFALSE(plotly_friendly)) {
    p <- p + guides(color = guide_legend(override.aes = list(size = 4)))
  }

  p
}
