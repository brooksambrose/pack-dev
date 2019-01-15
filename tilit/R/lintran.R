#' Linear transformation of vector into new range
#'
#' @param x
#' @param s1
#' @param s2
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @examples
lintran<-function(x,s2=c(0,1),s1=range(na.omit(x))) {
  w<-is.na(x)
  a=diff(s2)/diff(s1)
  b=s2[1]-a*s1[1]
  if(is.infinite(a)) x[!w]<-mean(s2) else x[!w]<-a*x[!w]+b
  x
}
