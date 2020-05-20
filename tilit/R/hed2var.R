#' Heading to Variable
#'
#' @return
#' @export
#'
#' @ import data.table
#'
#' @examples
hed2var<-function(x,p='[A-Za-z0-9]$'){
  hed<-grep(p,x$txt)
  n<-diff(c(hed,length(x$txt)+1))-1
  data.table(x[-hed],hed=rep(x$txt[hed],n))
}
