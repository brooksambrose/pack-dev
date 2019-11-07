#' Pre-process to Multi Level Corpus
#'
#' @param pre
#' @param lev
#'
#' @return
#' @export
#' @import data.table magrittr
#'
#' @examples
pre2mlc.f<-function(pre,lev=c('doc','par','lin','sen')){
  setkey(pre,doc,lin,cha)
  f<-function(sd) {
    r<-pre[,.(spm=list({x<-stm %>% as.numeric %>% table;x<-rbind(names(x) %>% as.numeric,x);attr(x,'dimnames')<-NULL;x})),by=sd]
    r[sapply(spm,length) %>% as.logical]
    }
  mlc<-list(
    doc=if('doc'%in%lev) f('doc')
    ,par=if('par'%in%lev) f(c('doc','par'))
    ,lin=if('lin'%in%lev) f(c('doc','par','lin'))
    ,sen=if('sen'%in%lev) f(c('doc','par','lin','sen'))
    ,voc=pre[,levels(stm)]
  )
  mlc
}
