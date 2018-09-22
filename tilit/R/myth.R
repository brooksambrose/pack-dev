#' My Theme
#'
#' @param ggplot
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
myth<-function(ggplot){
  ggplot +
    theme_bw() +
    scale_fill_brewer(type = 'qual',palette='Dark2') +
    scale_color_brewer(type = 'qual',palette='Dark2') +
    scale_y_continuous(labels = scales::comma) +
    theme(
      legend.position = c(.98, .98),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(0, 0, 0, 0)
    )
}
