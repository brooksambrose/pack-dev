#' Gram Matrix 2 Bootstrapped Random PCA
#'
#' @param Q
#' @param D
#' @param crs
#' @param lev
#'
#' @return
#' @export
#' @import data.table magrittr
#'
#' @examples
grm2brpca.f<-function(Q,lev=ec('doc,par,sen')[1],D=4,crs=1){
  cat("\t Bootstrap PCA model...\n \t")
  of<-paste0('d/p/bpcai-',lev,'-D',D,'.RData')
  if(!'bpcai'%in%ls()) {
    try(load(of),silent = T)
    if(!'bpcai'%in%ls()) bpcai<-list()
  }
  bpcai<-bpcai[!bpcai %>% sapply(function(x) x %>% dim %>% is.null)]
  s<-try(round((1000-sum({j<-sapply(bpcai,dim);if(!length(j)) 0 else j}[3,]))/100),silent=T)
  if(inherits(s,'try-error')) s<-10
  if(s!=0) for(i in (10-s+1):10){
    cat('Batch:',i,'\n')
    bpcai[[length(bpcai)+1]] <- pbapply::pbreplicate(n = 100,rsvd::rpca(Q[[lev]], D, center = TRUE,scale = FALSE, retx = TRUE)$rotation[, 1:min(D, ncol(Q[[lev]]))],cl = crs)
    save(bpcai,file=of)
  }
  bpcai<-bpcai[!bpcai %>% sapply(function(x) x %>% dim %>% is.null)]
  bpca<-apply(do.call(abind::abind,bpcai,3),c(1,2),function(x) ifelse(mean(x>0)>.5,1,-1)*(x %>% abs %>% mean))
  rownames(bpca)<-rownames(Q[[lev]])
  bpca<-list(list(mbd=bpca))
  names(bpca)<-lev
  bpca
}
