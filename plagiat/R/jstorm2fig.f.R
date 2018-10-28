#' JSTOR Master 2 Figure
#'
#' https://stackoverflow.com/questions/50887710/ggplotly-with-tooltip-has-problems-using-geom-rect
#'
#' @param jstorm
#' @param series
#' @param jclu
#' @param min.period
#' @param ann
#'
#' @return
#' @export
#' @import data.table magrittr ggplot2 gridExtra igraph tilit
#' @examples
jstorm2fig.f<-function(jstorm,jclu,series,min.period=2,ann){
  if(missing(series)) series<-jclu$tab[[1]][1]
  j<-jclu$super
  j<-j[jstorm[,.(year=fc %>% unlist %>% unique %>% sort),keyby=title_history]]
  j<-j[,.N,by=.(year,super)]
  j<-j[!is.na(super)]
  j[,p:=prop.table(N),by=year]
  setkey(j,super,year)

  # truncate by journal birth and die-off rates, guess at JSTOR coverage
  by<-j[,.(N=sum(N)),keyby=year][,g:=ov2chpt.f(N,drv = 2,min.period = 5,inc.ov = T)$g] %>% setkey(g) %>% .['1',max(year)]
  ey<-jstorm[,.(title_history,last=sapply(fc,max))][,.N,keyby=last][,g:=ov2chpt.f(N,drv=1,inc.ov = F,min.period = 5)$g][which.max(g),last]

  j<-j[between(year,by,ey)]
  lv<-j[,.(N=sum(N)),by=super] %>% setorder(-N) %>% .[,super %>% as.character]
  j[,super:=factor(super,levels=lv)]
  j[,c('g1','g2'):=lapply(1:2,function(y) ov2chpt.f(x=N,drv=y,min.period = 9,inc.ov = F)$g),by=super]
  setkey(j,super,year)

  y<-j[series][,year[1],by=g1]$V1
  r<-j[series,range(N)]
  n<-diff(r)*.1
  if(!missing(ann)) ann[,x:=mean(c(xmin,xmax)),by=text][,N:=text][,year:='']
  p1<-ggplot(mapping=aes(text=paste(year,N,sep='\n') %>% sub('^\n','',.)))
  if(!missing(ann)) p1<-p1+geom_rect(data=ann,aes(xmin=xmin,xmax=xmax,ymin=r[1],ymax=r[2]),fill=ann$col,alpha=.1) +
    geom_text(data=ann,aes(x=x,y=ann$dodge*r[2],label=text),color=ann$col)
    p1<-p1+geom_line(data=j[series],aes(x=year,y=N,linetype=super,color=g1,group=1),size=1)
    if(j[series,!all(g1=='1')]) p1<-p1+geom_text(data = j[series][!duplicated(g1),.(year,N,g1)][-1],aes(year,N,label=year,color=g1,angle=80,fontface='bold'),nudge_y = n)
    if(j[series,!all(g2=='1')]) p1<-p1+geom_text(data=j[series][!duplicated(g2),.(year,N,g2)][-1],aes(year,N,label=paste0('|\n',year),fontface='bold'),nudge_y = -n,color=gray(.3))
  list(
    p=myth(p1) + theme(legend.position="none",axis.title.x=element_blank()) + guides(color=F,fill=F) + ylab('N')
    ,d=j
  )
}
