#' My Theme
#'
#' @param ggplot
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
myth<-function(ggplot,pal=c('Dark2','Paired')){
  ggplot +
    theme_bw() +
    scale_fill_brewer(type = 'qual',palette=pal[1]) +
    scale_color_brewer(type = 'qual',palette=pal[1]) +
#    scale_y_continuous(labels = scales::comma) +
    theme(
      legend.position = c(.98, .98),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(0, 0, 0, 0)
    )
}
