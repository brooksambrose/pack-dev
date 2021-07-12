#' Letter Grade 2 Score
#'
#' @param ltr letter grade
#' @param ... passed to ltrgrd
#'
#' @return
#' @export
#'
#' @examples
ltr2scr<-function(ltr,...) {
  tilit::ltrgrd(...)[ltr,on='let',mid]
}
