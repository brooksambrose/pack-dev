#' Inverse hyperbolic sine (IHS) transformation
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
ihs <- function(x) {log10(x + sqrt(x ^ 2 + 1))}
