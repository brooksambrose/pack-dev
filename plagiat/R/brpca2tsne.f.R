#' Bootstrapped Random PCA 2 T-SNE
#'
#' @param Q
#' @param lev
#' @param D
#' @param thta
#' @param nc
#' @param tD
#' @param plx
#' @param mit
#' @param brpca
#' @param skip.pca
#'
#' @return
#' @export
#' @import data.table magrittr
#'
#' @examples
brpca2tsne.f<-function(Q,brpca,D=50,lev=c('doc','par','sen')[1],tD=c(4:2),plx=c(10,30,50),thta=0.5,mit=1000,skip.pca=F,nc=1){

  if(skip.pca){
    brpca<-Q[[lev]]
    D<-ncol(Q[[lev]])
    } else {
  if(missing(brpca)){
    l<-try(load('d/p/bpcai.RData'))
    if(!inherits(l,'try-error')) {
      d<-dim(bpcai[[1]])[2]
      if(d!=D) stop(paste('Saved bpscai iteration has Di =',d,'compared to initialization of D =',D))
    }
    brpca<-grm2brpca.f(Q = Q[[lev]],D = D,crs = nc)
  }
  }
  of<-paste0('d/p/brpca2tsne-',lev,'-D',D,'.RData')
  save(brpca,file=ifelse(skip.pca,sub('brpca','grm',of),of))

  of<-paste0('d/p/tsne-',lev,'-D',D,'.RData')
  l<-try(load(of),silent = T)
  if(inherits(l,'try-error')) tsne<-list()
  bch<-data.table(expand.grid(d=tD,p=plx))[,f:=paste0('d',d,'p',p)]
  n<-bch$f
  bch %<>% split(f=1:nrow(bch))
  names(bch)<-n
  if(length(tsne)) bch<-bch[n%in%names(tsne[[lev]])]
  for(b in bch) {
    cat('\n',b$f,'\n',sep='')
    system.time(
      tsne[[lev]][[b$f]] <- Rtsne::Rtsne(
        brpca, max_iter=mit, pca = FALSE, dims = b$d,initial_dims = D
        , perplexity = b$p,check_duplicates=F,theta=thta
        ,verbose=T)
    ) %>% print
    save(tsne,file=ifelse(skip.pca,sub('tsne','grm2tsne',of),of))
  }
  tsne
}
