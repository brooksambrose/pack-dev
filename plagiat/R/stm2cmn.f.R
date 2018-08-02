#' Structural Topic Model to Commensuration
#'
#' @param stm1
#' @param stm2
#' @param pre
#'
#' @return
#' @export
#'
#' @examples
stm2cmn.f<-function(stm1,stm2,pre){
  # convert doc.top to term frequencies/word counts
  dtc<-pre[,.(stm %>% is.na %>% `!` %>% sum),by=.(doc,lin)]$V1 # document term count
  dtc<-sweep(stm1$theta,1,dtc,'*')
  # convert top.ter to term frequencies/word counts
  ttc<-apply(dtc,2,sum)
  ttp<-prop.table(ttc)
  tso<-data.table(`%`=(ttp*100) %>% round(1))[,T:=.I] %>% setcolorder(2:1) %>% setorder(-`%`)
  ttc<-sweep(stm1$beta$logbeta %>% `[[`(1) %>% exp,1,ttc,'*')

}
