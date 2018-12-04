#' All duplicated, including first
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
adup<-function(x){
  duplicated(x)|duplicated(x,fromLast = T)
}
