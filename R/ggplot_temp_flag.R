#' Plot TEMP validation data, colorize points by Temp flag
#'
#' @param final_temp Dataframe filtered for temperature test data
#' @param point_size Argument to set size of ggplot points
#'
#' @return Returns a ggplot object of Temperature validation data colorized based on temp flag
#' 
#' @author Nicole Torrie
#' 
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual geom_line geom_ribbon labs
#' 
#' @export
#'



ggplot_temp_flag <- function(final_temp, 
                             point_size = 0.75) {
  r <-
    ggplot(final_temp,
           aes(
             x = timestamp_utc,
             y = temperature_degree_c,
             color = as.factor(FLAG)
           )) +
    geom_point(size = point_size) +
    scale_color_manual(values = c('Green', 'Red')) +
    geom_line(aes(y = median), color = "black") +
    geom_ribbon(aes(ymin = median - 0.2, ymax = median + 0.2),
                alpha = 0.3,
                color = "grey87") +
    geom_ribbon(aes(ymin = median - 0.5, ymax = median + 0.5),
                 alpha = 0.3,
                 color = "grey87")
  
  r <- r  + labs(color = 'FLAG')
  r
}
