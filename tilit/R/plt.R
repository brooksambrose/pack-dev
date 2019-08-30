#' Plot ggplot item as static in latex and plotly interactive in html
#'
#' @param ggplot
#' @param output
#' @param hovinf One of 'x+y','all','text','none','skip'. Defaults to 'x+y'.
#' @param ... Arguments to plotly::ggplotly
#'
#' @return
#' @export
#'
#' @examples
plt<-function(ggplot,output,hovinf='x+y',...) {
  if(missing(output)) output<-if(knitr::is_latex_output()) 'latex' else 'html'
  if(output=='latex') return(ggplot) else {
    p<-plotly::ggplotly(ggplot,...) %>% plotly::style(hoverinfo=hovinf[1])
    for(i in 1:length(p$x$data)) {
      for(j in c('name','legendgroup')) p$x$data[[i]][[j]]<-sub('^\\((.+),1(,NA)?\\)','\\1',p$x$data[[i]][[j]])
      p$x$data[[i]][['hovertext']]<-NULL
    }
  }
  #attr(p,'package')<-'ggplotly'
  p
}
