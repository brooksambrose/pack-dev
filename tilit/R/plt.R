#' Plot ggplot item as static in latex and plotly interactive in html
#'
#' @param p
#'
#' @return
#' @export
#'
#' @examples
plt<-function(ggplot) {
  if(knitr::is_latex_output()) ggplot else plotly::ggplotly(ggplot)
}
