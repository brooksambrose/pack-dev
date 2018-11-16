#' Suppress a plot but return useful values
#'
#' https://stackoverflow.com/questions/20363266/how-can-i-suppress-the-creation-of-a-plot-while-calling-a-function-in-r
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
suppressPlot <- function(...){
  ff <- tempfile()
  png(filename=ff)
  res <- plot(...)
  dev.off()
  unlink(ff)
  res
}
