#' JSTOR Master 2 Figure
#'
#' @param jstorm
#' @param jclu
#'
#' @return
#' @export
#' @import data.table magrittr ggplot2 gridExtra igraph
#' @examples
jstorm2fig.f<-function(jstorm,jclu){
  jstormr<-jstorm[,.(discipline=discipline %>% unlist %>% unique),keyby=title_history]
  jstormr<-jstormr[data.table(th=jclu$title_history$names,m=jclu$title_history %>% membership) %>% setkey(th)]
  jstormr<-jstormr[,.N,by=.(m,discipline)] %>% setorder(m,-N)
  jstormr<-jstormr[,.(super=discipline[1]),by=m]

  j<-data.table(title_history=jclu$title_history$names,super=jstormr[jclu$title_history %>% membership,super]) %>% setkey(title_history)
  j<-j[jstorm[,.(year=fc %>% unlist %>% unique %>% sort),keyby=title_history]]
  j<-j[,.N,by=.(year,super)]
  j<-j[!is.na(super)]
  j[,super:=factor(super,levels=
                     j[year==2000,.(super,N)] %>% setorder(N) %>% '[['('super')
                   #  j[,table(super) %>% sort %>% names]
  )]
  j[,p:=prop.table(N),by=year]
  setorder(j,super,year)
  s<-j[,super=='Social Sciences']
  #jstorm[,.(my=fc %>% unlist %>% max),by=title_history][,table(my)] %>% cbind
  d<-j[,between(year,1800,2000)]

  j[,dif1:=c(NA,diff(N)),by=super]
  j[s&d,dif1g:=ecp::e.divisive(dif1 %>% cbind,min.size = 9)$cluster %>% factor]
  j[,dif2:=c(NA,NA,diff(N) %>% diff),by=super]
  j[s&d,dif2g:=ecp::e.divisive(dif2 %>% cbind,min.size = 9)$cluster %>% factor,by=super]
  j[,dif3:=c(NA,NA,NA,diff(N) %>% diff %>% diff),by=super]
  j[s&d,dif3g:=ecp::e.divisive(dif3 %>% cbind,min.size = 9)$cluster %>% factor,by=super]

  r<-j[s,range(N)]
  p1<-ggplot(j[s&d],aes(year,N)) + annotate('rect',xmin=1888,xmax=1922,ymin=r[1]-50,ymax=r[2],alpha=.1) +
    geom_line(aes(color=dif1g),size=1) +
    geom_line(aes(y=N-50,color=dif2g),size=1) +
    theme_bw() + scale_fill_brewer(type = 'qual',palette='Dark2') + scale_color_brewer(type = 'qual',palette='Dark2')
  p1
  r<-j[s,range(p)]
  p2<-ggplot(j[s],aes(year,p)) + annotate('rect',xmin=1888,xmax=1922,ymin=r[1],ymax=r[2],alpha=.1) + geom_line(size=1) + theme_bw() + scale_fill_brewer(type = 'qual',palette='Dark2') + scale_color_brewer(type = 'qual',palette='Dark2')
  r<-j[s,range(dif1)]
  p3<-ggplot(j[s],aes(year,dif1)) + annotate('rect',xmin=1888,xmax=1922,ymin=r[1],ymax=r[2],alpha=.1) + geom_line(size=1) + theme_bw() + scale_fill_brewer(type = 'qual',palette='Dark2') + scale_color_brewer(type = 'qual',palette='Dark2')
  p3
  r<-j[s,range(dif2)]
  p4<-ggplot(j[s],aes(year,dif2)) + annotate('rect',xmin=1888,xmax=1922,ymin=r[1],ymax=r[2],alpha=.1) + geom_line(size=1) + theme_bw() + scale_fill_brewer(type = 'qual',palette='Dark2') + scale_color_brewer(type = 'qual',palette='Dark2')
  p4
  r<-j[s,range(pif1)]
  p5<-ggplot(j[s],aes(year,pif1)) + annotate('rect',xmin=1888,xmax=1922,ymin=r[1],ymax=r[2],alpha=.1) + geom_line(size=1) + theme_bw() + scale_fill_brewer(type = 'qual',palette='Dark2') + scale_color_brewer(type = 'qual',palette='Dark2')
  p5
  r<-j[s,range(pif2)]
  p6<-ggplot(j[s],aes(year,pif2)) + annotate('rect',xmin=1888,xmax=1922,ymin=r[1],ymax=r[2],alpha=.1) + geom_line(size=1) + theme_bw() + scale_fill_brewer(type = 'qual',palette='Dark2') + scale_color_brewer(type = 'qual',palette='Dark2')
  p6

  grid.arrange(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
}
