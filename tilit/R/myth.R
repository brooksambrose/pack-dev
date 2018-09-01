#' My Theme
#'
#' @param ggplot
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
myth<-function(ggplot){
  ggplot + theme_bw() + scale_fill_brewer(type = 'qual',palette='Dark2') + scale_color_brewer(type = 'qual',palette='Dark2')
}
