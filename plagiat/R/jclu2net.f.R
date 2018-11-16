#' JSTOR Journal Clustering 2 Network Visualization
#'
#' @param jclu
#' @param proj
#' @param warn
#' @param layout
#' @param ... Arguments to network::network.layout
#' @param isolates
#' @param grp
#' @param dim
#' @param zref
#' @param zkref
#'
#' @return
#' @export
#' @import igraph ggplot2 ggnetwork tilit
#' @importFrom network network.dyadcount
#' @importFrom network network.edgecount
#' @importFrom network network.size
#' @importFrom network %e%
#' @importFrom network %v%
#' @importFrom network %v%<-
#'
#' @examples
jclu2net.f<-function(jclu,proj=c('gt','gd')[2],warn=T,layout=c('fruchtermanreingold','latent')[1],isolates=T,grp=1,dim=2,zref=NULL,zkref=NULL,...){
  if((V(jclu[[proj]]) %>% length)>100) if(warn) stop('Graphs over 100 nodes are slow and ugly.')
  m<-V(jclu[[proj]])$memb %>% unique
  net<-induced_subgraph(jclu[[proj]],which(!V(jclu[[proj]])$name%in%m))
  if(!isolates) {
    iso<-igraph::degree(net) %>% {which(.==0)} %>% names
    net<-induced_subgraph(net,which(!V(net)$name%in%iso))
  }
  net<-asNetwork(net)
  co<-layout %>% dim %>% is.null %>% `!`

  if(co) if(network.size(net)!=nrow(layout)) stop('Layout and network do not match, did you forget to set isolates?')

    if(!co) if('latent'==layout[1]){
    # permute poisson
    pp<-pbapply::pbreplicate(1e3,1:network.dyadcount(net) %>% sample(size=net%e%'weight' %>% sum,replace = T) %>% table %>% {table(c(rep(0,network.dyadcount(net)-length(.)),.))} %>% data.table,simplify = F) %>% rbindlist %>% setnames('V1','c') %>% .[,c:=as.integer(c)]
    op<-c(rep(0,network.dyadcount(net)-network.edgecount(net)),net%e%'weight')
    lam<-mean(op)
    sp<-pbapply::pbreplicate(1e3,rpois(network.dyadcount(net),lambda = lam) %>% table %>% data.table,simplify = F) %>% rbindlist %>% setnames('.','c') %>% .[,c:=as.integer(c)]
    op %<>% table %>% data.table %>% setnames('.','c') %>% .[,c:=as.integer(c)]
    pp[is.na(pp)]<-0

    op[,sim:='observed']
    pp[,sim:='simulated']
    sp[,sim:='parametric']

    sim<-rbindlist(list(op,pp,sp))[,sim:=factor(sim,levels=ec('observed,simulated,parametric'))]
    setkey(sim,sim)
    pl<-ggplot() +
      geom_bin2d(data=sim['simulated']
                 ,aes(x=ihs(c),y=N,fill=sim,alpha=..count..
                      ,text=paste0(
                        'edge weight = ',x,'\n'
                        ,'count ≈ ',floor(10^y),'\n'
                        ,..count../1e2,'%'
                      )
                 ),bins=30
                 ,show.legend = F) + scale_alpha(range=c(.5,1))  +
      geom_point(data=sim['observed'],aes(x=ihs(c),y=N,text=paste0('edge weight: ',c,'\nobserved: ',N)),show.legend = F) +
      scale_y_log10() + theme(legend.position='bottom')

    withr::with_package('latentnet',{
      t1<-system.time(lnet<-ergmm(
        net~euclidean(d=dim,G=grp) +
          rsociality +
          edges
        ,response='weight'
        ,family='Poisson.log'
        ,verbose=2
        ,Z.ref=zref
        ,Z.K.ref=zkref
        ,control=control.ergmm(...)
      ))
      attr(lnet,'layout-time')<-t1
    })
    layout<-lnet$mkl$Z
    net%v%'rs'<-lnet$mkl$sociality %>% exp

  } else {
    layout<-network::network.layout.fruchtermanreingold(net,layout.par =list(...))
    net%v%'rs'<-sna::degree(net)
  }
  pd <- ggnetwork(net,layout=layout)
  pd$memb<-factor(pd$memb,levels=levels(jclu[[sub('g','f',proj)]]))
  p <- ggplot(pd, aes(x = x, y = y, xend = xend, yend = yend,text=vertex.names)) +
    geom_edges(aes(alpha=cross,size=weight)) + scale_alpha_ordinal(range=c(.1,.5)) + scale_size_continuous(range=c(.1,2)) +
    geom_nodes(aes(color=memb,stroke=1+rs/max(rs)*5),size=2,shape=21,fill='white') +
    theme_blank() + scale_color_brewer(type = 'qual',palette='Dark2') +
    theme(legend.title = element_blank())
  if('net'%in%ls()) attr(p,'net')<-net
  attr(p,'zref')<-layout %>% apply(2,scale)
  attr(p,'zkref')<-net%v%'memb' %>% factor(levels=levels(jclu[[sub('g','f',proj)]])) %>% droplevels %>% as.integer
  if('lnet'%in%ls()) attr(p,'lnet')<-lnet
  if('pl'%in%ls()) attr(p,'sim')<-pl
  if('iso'%in%ls()) attr(p,'iso')<-iso
  attr(p,'ggnet')<-pd
  p
}

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
if(F){
  inet<-asIgraph(net)
  layout<-lnet$mkl$Z
    graphjs(inet
          ,layout =
     #list(layout_with_fr(inet,dim=3),)
          ,vertex.color = RColorBrewer::brewer.pal(4,'Dark2')[factor(V(inet)$memb) %>% as.numeric]
          ,vertex.size =1
          ,vertex.label = network.vertex.names(net)[V(inet)]
          ,edge.width = 1
        ,vertex.shape = 'sphere'
          ,edge.alpha = 1
          ,brush=T
          #,fpl=10
          #,main=ec('force,latent')
          )
  }

if(F){
  V(jclu$gd)$lmem<-jclu$gd %>% cluster_louvain() %>% {.$memberships[1,]}
  mem<-data.table(v=V(jclu$gd)$name,mb=V(jclu$gd)$lmem)
  setkey(mem,v)
}

if(F){
  v<-data.table(factor(net%v%'memb'),lnet2d$mkl$Z,network.vertex.names(net)
  #              ,lnet2d$mkl$sociality %>% exp
                ) %>% setnames(ec('m,x,y,name,rs')[-5])
  v[,m:=mem[.(name),mb]]
  e<-net %>% as.edgelist
  vn<-attributes(e)$vnames
  e<-data.table(e) %>% setnames(ec('s,r'))
  e[,`:=`(s=vn[s],r=vn[r])]
  e[,weight:=net%e%'weight']
  e[,weight:=net%e%'weight']
  e[,cross:=net%e%'cross']
  setkey(v,name)
  v[e$s]
  ct<-RColorBrewer::brewer.pal(unique(v$m) %>% length,'Dark2')
  names(ct)<-table(v$m) %>% sort(decreasing=T) %>% names

  v[,clr:=ct[m]]
  e[,`:=`(
    x=v[s,x]
    ,xend=v[r,x]
    ,y=v[s,y]
    ,yend=v[r,y]
    ,xm=v[s,m]
    ,ym=v[r,m]
  )]
  e[,clr:=mapply(function(s,r) colorRampPalette(c(s,r))(3)[2],s=ct[xm],r=ct[ym])]
  v[,rs:=degree(net)]
  setorder(v,-rs)

  cp<-ggplot() +
    geom_segment(data=e,mapping=aes(x=x,y=y,xend=xend,yend=yend,group=clr),alpha=.1,color=e$clr,size=e$weight/max(e$weight)) +
    geom_point(data=v,mapping=aes(x=x,y=y,text=name,size=rs),shape=21,color=desat(v$clr,.01),fill=v$clr)
cp

  plt(
    myth(
cp
    )+theme_blank(),tooltip='text')
}

