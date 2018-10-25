#' Multi Level K 2 Simulation
#'
#' @param mlk
#' @param bw
#' @param bh
#' @param conf
#' @param tail
#' @param rep
#' @param crs
#'
#' @return
#' @export
#' @import data.table ggplot2 tilit
#'
#' @examples
mlk2sim.f<-function(mlk,bw=1,bh=.002,conf=1/100,tail=2,rep=1000,crs=1){

  conf<-1-conf/tail
  conf<-qt(conf,rep)*c(-1,1)

  if('factor'!=mlk[,class(level)]) mlk[,level:=factor(level)]
  setkey(mlk,level)
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



  # figure1 ------------------------------------------------------------------

  f1<-myth(
    ggplot(mlk2,aes(x=count,fill=level)) +
      geom_bin2d(data=sim,aes(x=c,y=d,group=level,fill=level,alpha=..count..),binwidth=c(bw,bh)) +
      geom_step(aes(y=fitted,color=level),size=1,direction = 'vh') +
      geom_text(aes(y=observed,label=ifelse(observed==0,'',count),color=level),size=3) +
      geom_label(aes(mK,y=1e-10,label=mK),mlk[,.(mK=median(K) %>% round(1)),by=level],color='white')
  ) + xlab('K') + ylab('Density') + guides(alpha=FALSE)


  # table -------------------------------------------------------------------

  setkey(sim,level,c)
  t<-sim[,.(m=mean(d),se=sd(d)),keyby=.(level,c)][mlk[,.N,keyby=.(level,K)][,.(level,c=K,d=prop.table(N))] %>% setkey(level,c)]
  t<-merge(t,mlk2,by.x=c('level','c'),by.y = c('level','count'),all = T)[,.(t=(observed-fitted)/se),keyby=.(level,c)]
  w<-t[is.na(t),.N,by=c][c(0,diff(N)) %>% `as.logical` %>% which][1,c]
  r<-dcast(t[t<conf[1]|t>conf[2]],c~level,value.var = 't')[c>=w,]
  r<-split(r,r[,c(1,diff(c))>1] %>% cumsum)
  r<-lapply(r,function(x) rbind(x,data.table(c='...'),fill=T)) %>% rbindlist %>% `[`(!.N)
  t<-t[,.(c=c[which(!is.na(t))[1]:.N],t=t[which(!is.na(t))[1]:.N]),by=level]

  # figure2 -----------------------------------------------------------------

  f2<-
    myth(
      ggplot(data=t,aes(x=c,y=t,color=level)) +
        annotate('rect',xmin=-Inf,xmax=Inf,ymin=conf[1],ymax=conf[2],alpha=.1) +
        geom_line() +
        # geom_point(fill='white',shape=21,size=3) +
        geom_label(data=t[t<conf[1]|t>conf[2]],aes(label=c),alpha=.5,label.size = 0,size=3,label.padding = unit(0, "lines")) +
        xlab('K') + ylab('t-score')
    ) + theme(legend.direction = 'horizontal') + guides(alpha=FALSE)
  list(fig1=f1,tab=t,fig2=f2,res=r)
}
