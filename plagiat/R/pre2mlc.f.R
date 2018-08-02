#' Pre-process to Multi Level Corpus
#'
#' @param pre
#' @param tdd
#'
#' @return
#' @export
#'
#' @examples
pre2mlc.f<-function(pre){
  library(data.table)
  setkey(pre,doc,lin,cha)
  f<-function(sd) pre[,.(list({x<-stm %>% as.numeric %>% table;x<-rbind(names(x) %>% as.numeric,x);attr(x,'dimnames')<-NULL;x})),by=sd]$V1
  mlc<-list(
    doc=f('doc')
    ,hed=f(c('doc','hed'))
    ,lin=f(c('doc','hed','lin'))
    ,sen=f(c('doc','hed','lin','sen'))
    ,voc=pre[,levels(stm)]
  )
  mlc
}
