#' Inverse Hyperbolic Sine axis formatting breaks
#' http://wresch.github.io/2013/03/08/asinh-scales-in-ggplot2.html
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
asinh_breaks <- function(x) {
  br <- function(r) {
    lmin <- round(log10(r[1]))
    lmax <- round(log10(r[2]))
    lbreaks <- seq(lmin, lmax, by = 1)
    breaks <- 10 ^ lbreaks
  }
  p.rng <- range(x[x > 0], na.rm = TRUE)
  breaks <- br(p.rng)
  if (min(x) <= 0) {breaks <- c(0, breaks)}
  if (sum(x < 0) > 1) { #more negative values that expected from expanding scale that includes zero
    n.rng <- -range(x[x < 0], na.rm = TRUE)
    breaks <- c(breaks, -br(n.rng))
  }
  return(sort(breaks))
}
#' Inverse Hyperbolic Sine axis transformation for ggplot2
#'
#' @return
#' @export
#'
#' @examples
asinh_trans <- function() {
  trans_new("asinh",
            transform = asinh,
            inverse   = sinh,
            breaks = asinh_breaks)
}
