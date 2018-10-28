#' Plot ggplot item as static in latex and plotly interactive in html
#'
#' @param ggplot
#' @param ... Arguments to plotly::ggplotly
#'
#' @return
#' @export
#'
#' @examples
plt<-function(ggplot,...) {
  if(knitr::is_latex_output()) return(ggplot) else {
    p<-plotly::ggplotly(ggplot,...)
    for(i in 1:length(p$x$data)) {
      for(j in c('name','legendgroup')) p$x$data[[i]][[j]]<-sub('^\\((.+),1(,NA)?\\)','\\1',p$x$data[[i]][[j]])
      p$x$data[[i]][['hovertext']]<-NULL
    }
  }
  p
}
