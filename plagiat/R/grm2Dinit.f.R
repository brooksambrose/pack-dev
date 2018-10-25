#' Gram Matrix 2 Dimension Initialization
#'
#' @param Q
#' @param D
#' @param lev
#' @param Dover
#' @param Gover
#' @param sparse
#' @param ...
#' @param p
#'
#' @return
#' @export
#' @import data.table magrittr ggplot2 tilit
#'
#' @examples
grm2Dinit.f<-function(Q,D=50,lev=c('doc','par','sen')[1],Dover,Gover,sparse=F,p,...){
  Dinit<-D
  cat('\nAnalysing',lev,'level at initial D =',D,'\n')

  # reduce dimensionality
  if(missing(p)) {if(sparse){
    x<-sparsepca::rspca(Q$par,D,center = TRUE,scale = FALSE)
  } else {
    x<-rsvd::rpca(Q[[lev]], D, center = TRUE,scale = FALSE, retx = TRUE)
  }
    p<-ov2chpt.f(x[[ifelse(sparse,'eigenvalues','eigvals')]],min.period = 2,drv = 2,...)[,i:=.I]
  }
  if(p[,levels(g) %>% length]>1) D<-p[g!=tail(levels(g),1),max(i)]
  cat('\nD =',D,'chosen from eigenvalues\n')
  if(missing(Gover)) {Gover<-1} else {if(missing(Dover)) Dover<-p[g==as.character(Gover),max(i)]}
  if(!missing(Dover)) {
    cat(paste('Manual override of automatic selection D =',D,'to manual selection Dm =',Dover,'\n'))
    D<-Dover
  } else {
    Dover<-NA_integer_
  }

  gp1<-ggplot(p,aes(x=i,y=x,color=g,label=i)) +
    geom_vline(xintercept=D,linetype='dashed',color='darkgray') +
    geom_point(size=3)+
    geom_text(color='white',size=2)+
    annotate(geom='label',x=D,label=paste0('D=',D),y=max(p$x %>% na.omit)) +
    ylab('Eigenvalue') + xlab('Principal Component')
  ggsave(paste0('d/b/bspca-scree-',lev,ifelse(sparse,'-sparse',''),'.pdf'),gp1)
  gp2<-ggplot(na.omit(p)[,m:=mean(d),by=g],aes(x=i,y=d,color=g,label=i))+
    geom_line(aes(y=m))+
    geom_hline(yintercept = 0,color='red',size=.1)+
    geom_vline(xintercept=D,linetype='dashed',color='darkgray') +
    geom_point(size=3)+
    geom_text(color='white',size=2)+
    annotate(geom='label',x=D,label=paste0('D=',D),y=max(p$d %>% na.omit)) +
    ylab('2nd Derivative of Eigenvalue') + xlab('Principal Component')
  ggsave(paste0('d/b/bspca-2drv-',lev,ifelse(sparse,'-sparse',''),'.pdf'),gp2)

  list(lev=lev,D=D,Dinit=Dinit,Dover=Dover,pe=gp1,pe2=gp2,dat=p)
}

# ABORTED parallel analysis to choose dims
# lp<-ov2chpt.f(x$eigvals %>% log,min.period = 2)[,i:=.I]
#
#
# if(!'bspcai'%in%ls()) {
#   try(load('d/p/bspcai.RData'),silent = T)
#   if(!'bspcai'%in%ls()) bspcai<-list()
# }
# s<-try(round((1000-sum({j<-sapply(bspcai,dim);if(!length(j)) 0 else j}[3,]))/100),silent=T)
# if(inherits(s,'try-error')) s<-10
# D=50
# for(i in 1:s){
#   bspcai[[length(bspcai)+1]] <-pbapply::pbreplicate(
#     100,data.table(i=1:D,e=
#                    # data.table(Q$doc)[,lapply(.SD,function(x) rnorm(x,mean(x),sd(x)))] %>%
#                    data.table(Q$doc)[,lapply(.SD,sample,replace = F)] %>%
#                    # matrix(rnorm(nrow(Q$doc)*ncol(Q$doc)),nrow=nrow(Q$doc)) %>%
#     rsvd::rpca(D, center = TRUE,scale = FALSE) %>% `[[`('eigvals')
#      )
#     ,cl = 2,simplify = F) %>% rbindlist
#   save(bspcai,file='d/p/bspcai.RData')
#   myth(ggplot() + geom_point(data=lp,aes(x=i,y=x,color=g)) + geom_smooth(
#     #    method=lm,formula=y~x,
#     data=rbindlist(bspcai),aes(x=i,y=e)) + geom_point(data=rbindlist(bspcai),aes(x=i,y=e),size=.1)
#   )# + scale_y_log10()
#
#   }
# bspca<-rbindlist(bspcai)
# pbs<-merge(p,bspca[,.(m=mean(e),sd=sd(e)),by=i],by='i')[,t:=(x-sd)/m]
# myth(ggplot() + geom_point(data=pbs,aes(x=i,y=t,color=g))+geom_hline(yintercept = qt(.95,df = nrow(Q$doc)),color=gray(.5,.5)))
#
