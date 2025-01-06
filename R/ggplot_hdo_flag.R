#' Plot HOBO DO validation data, colorize points by HOBO DO flag
#'
#' @param final_HDO Dataframe filtered for HDO test data
#' @param point_size to set size of ggplot points
#'
#' @return Returns a ggplot object of HOBO DO validation data colorized based on HOBO DO flag
#' 
#' @author Nicole Torrie
#' 
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual geom_line geom_ribbon labs
#' 
#' @export
#'


ggplot_hdo_flag <- function(final_HDO,
                            point_size = 0.75) {
  v <-
    ggplot(final_HDO,
           aes(
             x = timestamp_utc,
             y = dissolved_oxygen_uncorrected_mg_per_l,
             color = as.factor(FLAG)
           )) +
    geom_point(size = point_size) +
    scale_color_manual(values = c('Green', 'Red')) +
    geom_line(aes(y = median), color = "black") +
    geom_ribbon(aes(ymin = median - 0.2, ymax = median + 0.2),
                alpha = 0.3,
                color = NA)
  
  v <- v  + labs(color = 'FLAG')
  v
}

