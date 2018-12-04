#' Fast convert matrix to list of columns
#'
#' https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
mlist<-function(x) {
  split(x, rep(1:ncol(x), each = nrow(x)))
}
