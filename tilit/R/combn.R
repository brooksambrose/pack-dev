#' Fast combinations - Mask utils::combn with Rfast::comb_n
#'
#' @param ...
#' @param x
#' @param m
#'
#' @return
#' @export
#'
#' @examples
combn<-function(x,m,...){
  l<-Rfast::comb_n(1:length(x),k=m,...)
  if(is.list(l)) lapply(l,function(y) x[y]) else matrix(data=x[l],nrow=m)
}
