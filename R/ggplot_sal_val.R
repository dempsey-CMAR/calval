#' Plot SAL validation data, colorize points by sensor serial #
#'
#' @param final_SAL Dataframe filtered for salinity test data
#' @param point_size to set size of ggplot points
#'
#' @return Returns a ggplot object of Salinity validation data colorized based on sensor serial #
#' 
#' @author Nicole Torrie
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon labs guides guide_legend
#' 
#' @export
#'


ggplot_sal_val <- function(final_SAL,
                           point_size = 0.75) {
  u <-
    ggplot(final_SAL,
           aes(
             x = timestamp_utc,
             y = salinity_psu,
             color = as.factor(sensor_serial_number)
           )) +
    geom_point(size = point_size) +
    geom_line(aes(y = median), color = "black") +
    geom_ribbon(aes(ymin = median - 1, ymax = median + 1),
                alpha = 0.3,
                color = NA) 
  
  
  u <- u  + labs(color = 'sensor_serial_number') +
    guides(fill = guide_legend(ncol = 2))
  u
}
