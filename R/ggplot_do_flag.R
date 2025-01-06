#' Plot DO validation data, colorize points by DO flag
#'
#' @param DO Dataframe filtered for dissolved oxygen percent saturation test data
#' @param point_size to set size of ggplot points
#'
#' @return Returns a ggplot object of Dissolved Oxygen validation data colorized based on DO flag
#' 
#' @author Nicole Torrie
#' 
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual geom_hline geom_ribbon labs
#' 
#' @export
#'



ggplot_do_flag <- function(DO,
                           point_size = 0.75) {
  q <-
    ggplot(
      DO,
      aes(
        x = timestamp_utc,
        y = dissolved_oxygen_percent_saturation,
        color = as.factor(FLAG)
      )
    ) +
    geom_point(size = point_size) +
    scale_color_manual(values = c('Green', 'Red')) +
    geom_hline(yintercept = 95) +
    geom_hline(yintercept = 105) +
    geom_ribbon(aes(ymin = threshold - 5, ymax = threshold + 5),
                alpha = 0.1,
                color = NA)
  q <- q  + labs(color = 'FLAG')
  q
}

