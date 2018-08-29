#' JSTOR Master list to Table output
#'
#' @param jstorm
#' @param lan
#' @param pat
#' @param beg.bef
#' @param end.aft
#'
#' @return
#' @export
#' @import data.table
#' @importFrom magrittr %>%
#' @examples
jstorm2tab.f<-function(jstorm,beg.bef=1900,end.aft=2000,lan='ENGLISH',pat='(anth)|(soci[oa])|([^a-z]poli)|(econ)'){
  long<-jstorm[start<(beg.bef)&stop>(end.aft)]
  long[,lang:=list(lapply(title,cld2::detect_language_mixed))]
  long[,`:=`(
    bg=sapply(lang,function(x) x[[1]][1,1])
    ,pg=sapply(lang,function(x) x[[1]][1,4])
  )]
  setkey(long,bg,start,stop)
  long[lan][grep(pat,title,ignore.case = T),.(title,start,stop)]
}
