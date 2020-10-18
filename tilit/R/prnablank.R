#' Print NA as Blank for data.table
#'
#' @param x
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
prnablank<-function(x) {
  data.table(x)[,prmatrix(.SD,na.print='',quote=F,rowlab = as.character(1:.N))]
  invisible(NULL)
}
