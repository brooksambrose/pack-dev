#' JSTOR Master list to Table output
#'
#' @param jstorm
#' @param beg
#' @param end
#' @param lan
#' @param pat
#'
#' @return
#' @export
#'
#' @examples
jstorm2tab.f<-function(jstorm,beg.bef=1900,end.aft=2000,lan='ENGLISH',pat='(anth)|(soci[oa])|([^a-z]poli)|(econ)'){
  library(data.table)
  library(magrittr)
  library(knitr)
  library(cld2)
  long<-jstorm[start<(beg.bef)&stop>(end.aft)]
  long[,lang:=list(lapply(title,cld2::detect_language_mixed))]
  long[,`:=`(
    bg=sapply(lang,function(x) x[[1]][1,1])
    ,pg=sapply(lang,function(x) x[[1]][1,4])
  )]
  setkey(long,bg,start,stop)
  long[lan][grep(pat,title,ignore.case = T),.(title,start,stop)] %>% knitr::kable(.)
}
