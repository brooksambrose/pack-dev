#' Multi Level K 2 Simulation
#'
#' @param mlk
#' @param bw
#' @param bh
#' @param conf
#' @param tail
#' @param rep
#' @param theme
#'
#' @return
#' @export
#' @import data.table ggplot2
#'
#' @examples
mlk2sim.f<-function(mlk,bw=1,bh=.002,conf=1/100,tail=2,rep=1000,theme=theme_bw()){

  conf<-1-conf/tail

  fit<-mlk[,.(fit=list(vcd::goodfit(K,'poisson'))),by=level]
  mlk2<-fit[,do.call(rbind,fit),by=level] %>% setnames(c('level','observed','count','fitted','type','method','df','par'))

  fit<-fit[,.(lam=sapply(fit,function(x) x$par$lambda)),keyby=level]
  psim<-function(x) {
    pbapply::pbreplicate(
      rep
      ,rpois(mlk[x,.N],fit[x,lam]) %>%
        table %>% prop.table %>% data.table,simplify = F
    ) %>% rbindlist %>% setnames(c('c','d')) %>% data.table(level=x,.)
  }
  sim<-lapply(fit$level %>% levels,psim) %>% rbindlist
  sim[,c:=as.numeric(c)]

  mlk2<-mlk2[,.(count,observed=prop.table(observed),fitted=prop.table(fitted)),by=level][count>=min(sim$c)]



  # figure ------------------------------------------------------------------
  ggplot2::theme_set(theme)

  f<-ggplot(mlk2,aes(x=count,fill=level)) +
    geom_bin2d(data=sim,aes(x=c,y=d,group=level,fill=level,alpha=..count..),binwidth=c(bw,bh)) +
    geom_step(aes(y=fitted,color=level),size=1,direction = 'vh') +
    geom_text(aes(y=observed,label=ifelse(observed==0,'',count),color=level),size=3) +
    geom_label(aes(mK,y=1e-10,label=mK),mlk[,.(mK=median(K) %>% round(1)),by=level],color='white') +
    scale_color_brewer(type = 'qual',palette='Dark2') +
    scale_fill_brewer(type = 'qual',palette='Dark2') + theme(legend.position = c(.9,.5))


  # table -------------------------------------------------------------------

  setkey(sim,level,c)
  t<-sim[,.(m=mean(d),se=sd(d)),keyby=.(level,c)][mlk[,.N,keyby=.(level,K)][,.(level,c=K,d=prop.table(N))] %>% setkey(level,c)]
  t<-merge(t,mlk2,by.x=c('level','c'),by.y = c('level','count'),all = T)[,.(t=(observed-fitted)/se),keyby=.(level,c)]
  w<-t[is.na(t),.N,by=c][c(0,diff(N)) %>% `as.logical` %>% which][1,c]
  t<-dcast(t[t>qt(conf,rep)|t< -qt(conf,rep)],c~level,value.var = 't')[c>=w,]
  t<-split(t,t[,c(1,diff(c))>1] %>% cumsum)
  t<-lapply(t,function(x) rbind(x,data.table(c='...'),fill=T)) %>% rbindlist %>% `[`(!.N)

  list(fig=f,tab=t)
}
