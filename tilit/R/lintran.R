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
lintran<-function(x,s1=c(0,1),s2=c(0,1)) {a=diff(s2)/diff(s1);b=s2[1]-a*s1[1];return(a*x+b)}
