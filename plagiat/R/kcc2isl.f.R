#' K-clique Communities to Island plot
#'
#' @param bel2mel
#' @param cos2kcc
#' @param type
#' @param co
#' @param ordinal
#' @param border
#' @param nodes
#'
#' @return
#' @export
#' @import data.table magrittr igraph
#'
#' @examples
kcc2isl.f<-function(bel2mel,cos2kcc,type=c('utel','crel')[2],co,ordinal=T,border=T,nodes=T){

  net<-graph_from_edgelist(bel2mel[[type]][,.(cr1,cr2)] %>% as.matrix,F)
  E(net)$weight<-bel2mel[[type]]$ew
  str<-which(igraph::degree(net)>0)

  kdo<-cos2kcc[[type]]$orig %>% do.call(what=c)
  kdo<-lapply(kdo,function(x) which(V(net)$name%in%attr(cos2kcc[[type]],'levels')[x]))
  kl<-names(kdo) %>% sub('k([0-9]+).+','\\1',.) %>% as.integer
  clr<-kl %>% `-`(min(.)) %>% `+`(1)
  if(ordinal){
    clrl<-terrain.colors(length(unique(clr)))
    mp<-data.table(c=unique(clr))[,o:=1:.N] %>% setkey(c)
    clr<-clrl[mp[.(c(1:2,clr)),o]]
  } else{
    clrl<-terrain.colors(max(clr))
    clr<-clrl[clr]
  }
  top<-topo.colors(2)
  kdo<-c(list(str),kdo)
  clr<-c(top,clr)
  clrl<-c(top,clrl)

  clrs<-desat(clr,.6)
  clr<-desat(clr,.3)
  clrl<-desat(clrl,.3)

  if(missing(co)){
    #t1<-system.time(co<-layout_with_fr(net,niter=5e4,grid = 'grid'))/60
    nnet<-intergraph::asNetwork(net)
    area<-gorder(net)
    cat('\n calculated layout in...')
    t1<-system.time(co<-network::network.layout.fruchtermanreingold(
      nnet,layout.par = list(niter=1e5,repulse.rad=log(area)*area^1.9,area=area*2/3)
    )
    )
    cat(round(t1/60,2),'mins')
    attr(co,'layout-time')<-t1
  }
  par(bg=clr[1],mar=rep(0,4))
  plot(
    net
    ,vertex.size=.5
    ,vertex.label=NA
    ,vertex.frame.color=NA #'gray'
    ,vertex.color='lightgray'
    ,vertex.shape=if(nodes) 'square' else 'none'
    ,mark.groups = kdo
    ,mark.col = clr[-1]
    ,mark.border = if(border) clrs else NA
    ,edge.color=NA
    ,mark.expand = 2
    ,mark.shape = 0
    ,layout=co
    ,margin=c(0,-.5,0,0)
  )
  #shape::colorlegend(col=clrl,main='K',zval=sort(unique(kl)),main.col='white',lab.col='white',zlim = c(1,max(kl)),posy = c(0.01,.99),zlevels=10,cex=.25,left=T)
  uc<-par("usr")
  plotrix::color.legend(xl=uc[2]*.7,xr=uc[2]*.8,yb=uc[3]*.9,yt=uc[4]*.9,legend = c(1:2,unique(kl)),rect.col = unique(clrs),gradient = 'y',col='white',cex=.75)
  co
}

