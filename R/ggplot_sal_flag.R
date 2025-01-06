#' Plot SAL validation data, colorize points by SAL flag
#'
#' @param final_SAL Dataframe filtered for salinity test data
#' @param point_size to set size of ggplot points
#'
#' @return Returns a ggplot object of Salinity validation data colorized based on SAL flag
#' 
#' @author Nicole Torrie
#' 
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual geom_line geom_ribbon labs
#' 
#' @export
#'



ggplot_sal_flag <- function(final_SAL,
                            point_size = 0.75) {
  t <-
    ggplot(final_SAL,
           aes(
             x = timestamp_utc,
             y = salinity_psu,
             color = as.factor(FLAG)
           )) +
    geom_point(size = point_size) +
    scale_color_manual(values = c('Green', 'Red')) +
    geom_line(aes(y = median), color = "black") +
    geom_ribbon(aes(ymin = median - 1, ymax = median + 1),
                alpha = 0.3,
                color = NA)
  
  t <- t  + labs(color = 'FLAG')
  t
}


