#' Desaturate a color
#'
#' @param cols
#' @param sat
#'
#' @return
#' @export
#'
#' @examples
desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}
