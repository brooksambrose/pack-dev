#' JSTOR Master 2 Figure
#'
#' @param jstorm
#' @param series
#' @param jclu
#'
#' @return
#' @export
#' @import data.table magrittr ggplot2 gridExtra igraph tilit
#' @examples
jstorm2fig.f<-function(jstorm,jclu,series='Social Sciences'){
  j<-jclu$super
  j<-j[jstorm[,.(year=fc %>% unlist %>% unique %>% sort),keyby=title_history]]
  j<-j[,.N,by=.(year,super)]
  j<-j[!is.na(super)]
  j[,super:=factor(super,levels=
                     j[year==2000,.(super,N)] %>% setorder(N) %>% '[['('super')
                   #  j[,table(super) %>% sort %>% names]
  )]
  j[,p:=prop.table(N),by=year]
  setorder(j,super,year)
  s<-j[,super==series]
  #jstorm[,.(my=fc %>% unlist %>% max),by=title_history][,table(my)] %>% cbind
  d<-j[,between(year,1800,2000)]

  j[,dif1:=c(NA,diff(N)),by=super]
  j[s&d,dif1g:=ecp::e.divisive(dif1 %>% cbind,min.size = 9)$cluster %>% factor]
  j[,dif2:=c(NA,NA,diff(N) %>% diff),by=super]
  j[s&d,dif2g:=ecp::e.divisive(dif2 %>% cbind,min.size = 9)$cluster %>% factor,by=super]

  y<-j[s&d][!duplicated(dif1g),year]

  r<-j[s&d,range(N)]
  n<-diff(r)*.1
  p1<-ggplot(j[s&d],aes(year,N)) +
    annotate('rect',xmin=1888,xmax=1922,ymin=r[1]-n,ymax=r[2],alpha=.1) +
    geom_line(aes(color=dif1g),size=1) +
    geom_text(data = j[s&d][!duplicated(dif1g),.(year,N,dif1g)][-1],aes(year,N,label=year,color=dif1g,angle=45,fontface='bold'),nudge_y = n) +
    geom_text(data=j[s&d][!duplicated(dif2g),.(year,N,dif2g)][-1],aes(year,N,label=paste0('|\n',year),fontface='bold'),nudge_y = -n,color=gray(.3))

  list(
    p=myth(p1) + theme(legend.position="none")
    ,d=j
  )
}
