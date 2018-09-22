#' JSTOR Master list to Table output
#'
#' @param jstorm
#' @param lan
#' @param beg.bef
#' @param end.aft
#' @param jclu
#' @param sup
#'
#' @return
#' @export
#' @import data.table
#' @importFrom magrittr %>%
#' @examples
jstorm2tab.f<-function(jstorm,jclu,beg.bef=1900,end.aft=2000,lan='ENGLISH',sup='Social Sciences'){
  jstorm[,`:=`(start=sapply(fc,min))]
  jstorm %>% setorder(start)
  long<-jstorm[
    sapply(discipline,function(x) sup%in%x)&sapply(lang,function(x) lan%in%x),
    .(publication_title=paste0(publication_title,collapse='; '),discipline=paste0(setdiff(discipline %>% unlist %>% unique,sup),collapse='; '),fc)
    ,by=title_history
    ][
      mapply(FUN = function(b,e,y,d) all(c(b,e) %in% y),b=beg.bef,e=end.aft,y=fc)
      ]
  long[,`:=`(start=sapply(fc,min),stop=sapply(fc,max),fc=NULL)]
  setkey(long,start,stop)
  long
}
