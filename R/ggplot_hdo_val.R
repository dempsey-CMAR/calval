#' Plot HOBO DO validation data, colorize points by sensor serial #
#'
#' @param final_HDO Dataframe filtered for HOBO DO test data
#' @param point_size to set size of ggplot points
#'
#' @return Returns a ggplot object of HOBO DO validation data colorized based on sensor serial #
#' 
#' @author Nicole Torrie
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon labs guides guide_legend
#' 
#' @export
#'

ggplot_hdo_val <- function(final_HDO,
                           point_size = 0.75) {
  w <-
    ggplot(final_HDO,
           aes(
             x = timestamp_utc,
             y = dissolved_oxygen_uncorrected_mg_per_l,
             color = as.factor(sensor_serial_number)
           )) +
    geom_point(size = point_size) +
    geom_line(aes(y = median), color = "black") +
    geom_ribbon(aes(ymin = median - 0.2, ymax = median + 0.2),
                alpha = 0.3,
                color = NA) 
  
  
  w <- w  + labs(color = 'sensor_serial_number') +
    guides(fill = guide_legend(ncol = 2))
  w
}

