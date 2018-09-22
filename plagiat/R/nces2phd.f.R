#' NCES Historical Table 2 PhD Time Series
#'
#' @param xls
#' @param out
#' @param interpolation
#'
#' @return
#' @export
#' @import data.table ggplot2 magrittr tilit
#'
#' @examples
nces2phd.f<-function(xls='https://nces.ed.gov/programs/digest/d14/tables/xls/tabn301.20.xls',out='d/q/nces.xls',interpolation=c('linear','smooth')){
  system(paste('wget -qO',out,xls))
  nces<-readxl::read_xls(out)
  phd<-nces[c(1,grep('Doctor',nces[[1]])),grep('[0-9]{4}',nces[1,])] %>% t %>% data.table %>% setnames(c('year','N'))
  phd[,year:=sub('-.+','',year) %>% as.integer %>% `+`(1)]
  phd[,N:=as.numeric(N)]
  spl<-phd[,.(year=year[match(do.call(seq,range(year) %>% as.list),year,nomatch = NA)])]
  spl[!is.na(year),N:=phd$N]
  spl[,`:=`(
    year=zoo::na.approx(year)
    ,N=switch(interpolation[1],linear=zoo::na.approx(N),smooth=zoo::na.spline(N,method='hyman'))
  )]
  r<-phd[,range(N)]
  list(
    raw=nces
    ,tab=phd
    ,int=spl
    ,fig=myth(qplot(x = year,y = N,geom='line',data=spl) +
                annotate('rect',xmin=1888,xmax=1922,ymin=r[1]
                         #-n
                         ,ymax=r[2],alpha=.1) +
                geom_point(data=phd,aes(x=year,y=N),shape=21,size=1.5,stroke=1,fill='white')) + theme(axis.title.x =element_blank())
  )
}

