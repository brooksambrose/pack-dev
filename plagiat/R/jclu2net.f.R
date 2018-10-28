#' JSTOR Journal Clustering 2 Network Visualization
#'
#' @param jclu
#' @param proj
#' @param warn
#' @param layout
#' @param ... Arguments to network::network.layout
#' @param latent
#' @param isolates
#'
#' @return
#' @export
#' @import igraph ggplot2 ggnetwork
#'
#' @examples
jclu2net.f<-function(jclu,proj=c('gt','gd')[2],warn=T,layout='fruchtermanreingold',latent=F,isolates=F,...){
  if((V(jclu[[proj]]) %>% length)>100) if(warn) stop('Graphs over 100 nodes are slow and ugly.')
  m<-V(jclu[[proj]])$memb
  k<-!(table(m)-1)
  k<-names(k)[k]
  net<-induced_subgraph(jclu[[proj]],which(!V(jclu[[proj]])$name%in%ifelse(isolates,unique(m) %>% setdiff(k),unique(m))),impl="create_from_scratch")
  net<-asNetwork(net)
  if(latent){
      lnet<-pbapply::pblapply(1:6,function(x) ergmm(
        net~euclidean(d=2,G=x)+
          #nodemix('memb')+
          edges
        ,response='weight'
        ,family='Poisson.log'
        ,verbose=2
        ,control=control.ergmm(
          sample.size=1e4
          ,burnin=1e6
          ,interval=1e2
        ))
      ,cl = 6)
  }
  pd <- ggnetwork(
    net
    ,layout=layout,...)
  pd$memb<-factor(pd$memb,levels=levels(jclu[[sub('g','f',proj)]]))
  p <- ggplot(pd, aes(x = x, y = y, xend = xend, yend = yend,text=vertex.names)) +
    geom_edges(aes(alpha=cross),size=.1) +
    geom_nodes(aes(color=memb)) +
    theme_blank() +
    theme(legend.title = element_blank())
  p
}
# pd<-split(pd,c('e','n')[is.na(pd$weight)+1])
# pd$e<-pd$e[,c('x','y','xend','yend','cross')]
# pd$n<-pd$n[,c('x','y','memb','vertex.names')]
# pd$n$memb<-factor(pd$n$memb,levels=levels(jclu[[sub('g','f',proj)]]))

if(F){
lnet<-pbapply::pblapply(1:6,function(x) ergmm(
net~euclidean(d=2,G=x)+
#nodemix('memb')+
edges
,response='weight'
,family='Poisson.log'
,verbose=2
,control=control.ergmm(
sample.size=1e4
,burnin=1e6
,interval=1e2
))
,cl = 6)

}
