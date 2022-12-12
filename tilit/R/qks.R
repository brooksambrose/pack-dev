#' Quick Score to Letter
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
qks<-function(memo='',x,points=10,digits=5,...){
  if(missing(x)) x<-clipr::read_clip() %>% as.numeric
  r<-scr2ltr(x,points=points,sigdif=digits,...)
  r<-paste0(memo,r,' ',x)
  cat(r)
  clipr::write_clip(r)
}
