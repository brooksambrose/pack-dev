#' Ordered Vector 2 Changepoints (now accepting matrix)
#'
#' @param x
#' @param drv list all derivatives you want to calculate; multiples will be analyzed simultaneously.
#' @param min.period
#' @param warn
#' @param inc.ov include original variable
#' @param drv.only derivative only, no grouping
#' @param ... other args to ecp::e.divisive
#'
#' @return
#' @export
#' @import data.table
#' @examples
ov2chpt.f<-function(x,drv=1,min.period=2,inc.ov=F,warn=T,drv.only=F,...){
  d<-length(dim(x))
  if(!is.null(d)) if(d>2) stop('\ndim(x) > 2')
  m<-'\nComputing long vectors is inefficient, warn=F to disable warning.'
  x<-data.table(x) %>% {.[,.SD,.SDcols=sapply(.,is.numeric)]}
  if(warn) if(is.null(d)) {if(length(x)>1000) stop(m)} else {if(nrow(x)>1000) stop(m)}
  dif<-function(y) {
    lapply(drv,function(i) {
      for(j in 1:i) y<-diff(y)
      y<-data.table(c(rep(NA,i),y)) %>% setnames(paste0(names(x),'_d',i))
      y
    }
    ) %>% do.call(data.table,.)}
  r<-lapply(x,dif) %>% unname %>% {do.call(data.table,c(list(x),.))} # breaking change
  if(inc.ov) j<-1:ncol(r) else j<-2:ncol(r)
  if(!drv.only) r[,g:=factor(c(rep(1,max(drv)-1),{k<-ecp::e.divisive(as.matrix(r[-(1:max(drv)),.SD,.SDcols=j]),min.size = min.period,...)$cluster;c(k,max(k))}))]
  r
}
# TODO add this to SO answer at ?
# find appropriate SO like https://stats.stackexchange.com/questions/111140/is-it-a-good-idea-to-use-log-scale-on-scree-plots-for-pca-ica-fa
# ggplot(na.omit(p),aes(x=i,y=d,color=g,label=i))+geom_point(size=3)+geom_text(color='white',size=2)
#myth(ggplot(na.omit(p)[,m:=mean(d),by=g],aes(x=i,y=d,color=g,label=i))+geom_line(aes(y=m))+geom_hline(yintercept = 0,color='red',size=.1)+geom_point(size=3)+geom_text(color='white',size=2))
