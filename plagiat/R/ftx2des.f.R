#' Full text 2 Descriptives
#'
#' @param ftx
#' @param thr
#' @param rnd
#'
#' @return
#' @export
#'
#' @examples
ftx2des.f<-function(ftx,thr=9,rnd=2){
  des<-ftx[,table(com) %>% unclass %>% factor(.,levels=c(1:min(.),.) %>% unique %>% sort) %>% table %>% cbind(.,prop.table(.)*100) %>% data.table(keep.rownames = T)] %>% setnames(c('frq','cnt','per'))
  des<-rbindlist(
    list(
      des[as.numeric(frq)<thr,.(frq=as.character(frq),cnt,per)]
      ,des[as.numeric(frq)>=thr,.(frq=paste0('[',thr,',',des[,max(cnt)],']'),cnt=sum(cnt),per=sum(per))]
      ,des[,.(frq='tot',cnt=sum(cnt),per=sum(per))]
    )
  )[,per:=round(per,rnd)][.N,per:=des[!.N,sum(per) %>% round(rnd)]]
  des
}
