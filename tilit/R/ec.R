#' Easy string concatenation
#'
#' @param x
#' @param sp
#'
#' @return
#' @export
#'
#' @examples
ec<-function(x,sp=',') {
	strsplit(x,sp)[[1]]
}
