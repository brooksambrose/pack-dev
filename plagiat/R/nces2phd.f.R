#' NCES Historical Table 2 PhD Time Series
#'
#' @param xls
#' @param out
#' @param interpolation
#'
#' @return
#' @export
#'
#' @examples
nces2phd.f<-function(xls='https://nces.ed.gov/programs/digest/d14/tables/xls/tabn301.20.xls',out='d/q/nces.RData',interpolation=c('linear','smooth')){

  system(paste('wget -qO',out,xls))
  nces<-readxl::read_xls(out)
  save(nces,file=out)

  d<-nces[c(1,grep('Doctor',nces[[1]])),grep('[0-9]{4}',nces[1,])] %>% t %>% data.table %>% setnames(c('year','N'))
  phd[,year:=sub('-.+','',year) %>% as.integer %>% `+`(1)]
  phd[,N:=as.numeric(N)]
  spl<-phd[,.(year=year[match(do.call(seq,range(year) %>% as.list),year,nomatch = NA)])]
  spl[!is.na(year),N:=phd$N]
  spl[,`:=`(
    year=zoo::na.approx(year)
    ,N=switch(interpolation[1],linear=zoo::na.approx(N),smooth=zoo::na.spline(N,method='hyman'))
  )]

  list(
    fig=qplot(x = year,y = N,geom='line',data=spl) + geom_point(data=phd,aes(x=year,y=N),shape=21,size=1.5,stroke=1,fill='white')
    ,tab=phd
    ,int=spl
  )


}

