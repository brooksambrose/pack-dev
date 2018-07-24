#' https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
#'
#' @param x
#' @param pos
#'
#' @return
#' @export
#'
#' @examples
splitAt <- function(x, pos) {
  unname(split(x, cumsum(seq_along(x) %in% pos)))
}
