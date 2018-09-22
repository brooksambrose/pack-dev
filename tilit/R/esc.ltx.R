#' Escape Latex
#'
#' @param y
#' @param tikzs
#' @param tikzr
#'
#' @return
#' @export
#'
#' @examples
esc.ltx<-function(
  y
  ,tikzs=ec("%,$,},{,^,_,#,&,~")
  ,tikzr=ec("\\%,\\$,\\},\\{,\\^{},\\_{},\\#,\\&,\\char`\\~")
) {
  w<-sapply(tikzs,grep,x=y,fixed=T)
  for(i in which(!!sapply(w,length))) y<-gsub(tikzs[i],tikzr[i],y,fixed = T)
  y
}
