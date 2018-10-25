#' Corpus 2 Audit Table
#'
#' @param cor
#' @param step.name
#'
#' @return
#' @export
#' @import data.table
#' @examples
cor2aud.f<-function(cor,step.name){
  iferr<-function(x){x<-try(x,silent = T);ifelse(inherits(x,'try-error'),NA,x)}
  data.table(
    step=step.name
    ,doc=iferr(unique(cor[,.(doc)])[,.N])
    ,pag=iferr(unique(cor[,.(doc,pag)])[,.N])
    ,par=iferr(unique(cor[,.(doc,par)])[,.N])
    ,sen=iferr(unique(cor[,.(doc,sen)])[,.N])
    ,tok=ifelse(cor[1:10,any(grepl(' ',txt))],NA,cor[,.N])
    ,ter=ifelse(cor[1:10,any(grepl(' ',txt))],NA,unique(cor[,.(txt)])[,.N])
    ,lem=iferr(unique(cor[,.(lem)])[,.N])
  )
}
