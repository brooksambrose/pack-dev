#' Ordered Vector 2 Changepoints
#'
#' @param x
#' @param drv
#' @param min.period
#' @param warn
#'
#' @return
#' @export
#' @import data.table
#' @examples
ov2chpt.f<-function(x,drv=1,min.period=2,inc.ov=T,warn=T){
  if(length(x)>1000) stop('Computing long vectors is inefficient, warn=T to disable warning.')
  dif<-function(y){for(i in 1:drv) y<-diff(y);y}
  r<-data.table(x,d=c(rep(NA,drv),dif(x)))
  if(inc.ov) j<-1:2 else j<-2
  r[,g:=c(rep(1,drv),ecp::e.divisive(cbind(x,d)[-(1:drv),j] %>% cbind,min.size = min.period)$cluster) %>% factor]
  r
}
# TODO add this to SO answer at ?
# find appropriate SO like https://stats.stackexchange.com/questions/111140/is-it-a-good-idea-to-use-log-scale-on-scree-plots-for-pca-ica-fa
# ggplot(na.omit(p),aes(x=i,y=d,color=g,label=i))+geom_point(size=3)+geom_text(color='white',size=2)
#myth(ggplot(na.omit(p)[,m:=mean(d),by=g],aes(x=i,y=d,color=g,label=i))+geom_line(aes(y=m))+geom_hline(yintercept = 0,color='red',size=.1)+geom_point(size=3)+geom_text(color='white',size=2))
