#' K-clique Communities to Island plot
#'
#' @param bel2mel
#' @param cos2kcc
#' @param type
#' @param co
#' @param ordinal
#' @param border
#' @param nodes
#' @param rad
#' @param nit
#' @param minew
#' @param wok2dbl
#' @param dbl2bel
#' @param area
#' @param res
#' @param pins
#' @param hex hull expansion
#'
#' @return
#' @export
#' @import data.table magrittr igraph sp methods
#'
#' @examples
kcc2isl.f<-function(bel2mel,cos2kcc,wok2dbl,dbl2bel,type=c('utel','crel')[2],minew=1,co,ordinal=T,border=T,nodes=T,rad=.8,area=.8,nit=1e4,res=100,pins=T,hex=.001){
  n1<-sub('el','1',type)
  n2<-sub('el','2',type)
  bel2mel[[type]]<-bel2mel[[type]][ew>=minew]

  net<-graph_from_edgelist(bel2mel[[type]][,.(get(n1),get(n2))] %>% as.matrix,F)
  E(net)$weight<-bel2mel[[type]][,ew]
  str<-which(igraph::degree(net)>0)

  kdo<-cos2kcc[[type]]$orig %>% do.call(what=c)
  kdo<-lapply(kdo,function(x) which(V(net)$name%in%attr(cos2kcc[[type]],'levels')[x]))
  kcc<-kdo

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
    cat('\n calculated layout in...')
    n<-gorder(net)
    t1<-system.time(co<-network::network.layout.fruchtermanreingold(
      nnet,layout.par = list(niter=nit,repulse.rad=log(n)*n^rad,area=n^area)
    )
    )
    cat(round(max(t1)/60,2),'mins')

    co<-apply(co,2,scale,center=T,scale=F)
    com<-components(net)
    com<-com$membership==which.max(com$csize)
    ex<-apply(co[com,],2,function(x) c(which(x<quantile(x,.05)),which(x>quantile(x,.95)))) %>% c %>% sort
    vm<-varimax(co[ex,],normalize = F)
    co<-co%*%vm$rotmat
    if(diff(range(co[com,1]))<diff(range(co[com,2]))) co<-cbind(co[,2],co[,1])

    attr(co,'layout-time')<-t1
  } else {
    if(pins) if(!is.null(attr(co,'surface'))) pin<-attr(co,'surface')
    setorder(co,v)
    co<-as.matrix(co[,.(x,y)])
  }

  # plot 2d island
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

  # try to get correct group ids
  kdg<-cos2kcc[[type]]$orig %>% do.call(what=c)
  kdg<-lapply(kdg,function(x) which(V(net)$name%in%attr(cos2kcc[[type]],'levels')[x]))
  kg<-names(kdg) %>% sub('k([0-9]+).+','\\1',.) %>% as.integer

  kd<-data.table(g=kdg,k=kg)
  kd<-kd[,.(g=lapply(1:.N,function(i){
    setdiff(g[[i]],unlist(g[-i]))
  })),by=k]
  kd[,id:=names(kdg)]
  setkey(kd,k)
  kd<-data.table(k=kd[,rep(k,sapply(g,length))],g=kd[,rep(id,sapply(g,length))],v=kd[,unlist(g)])
  kd<-kd[!duplicated(v,fromLast = T)]

  sfc<-data.table(co,V(net)$name) %>% setnames(ec('x,y,names'))
  sfc[,v:=.I]
  sfc[kd$v,k:=kd$k]
  sfc[kd$v,g:=kd$g]
  d<-igraph::degree(net) %>% .[.<3]
  d<-d[sfc[is.na(k),names]] %>% na.omit
  setkey(sfc,names)
  sfc[names(d),k:=d %>% as.integer]

  str<-cos2kcc[[type]]$strict %>% do.call(what=c)
  str<-lapply(str,function(x) which(V(net)$name%in%attr(cos2kcc[[type]],'levels')[x]))
  stl<-names(str) %>% sub('k([0-9]+).+','\\1',.) %>% as.integer
  str<-data.table(v=unlist(str),k=stl)
  str<-str[!duplicated(v,fromLast = T)]
  setkey(str,v)
  sfc[is.na(k),k:=str[.(sfc[is.na(k),v]),k]]
  sfc[is.na(k),k:=0]
  sfc[is.na(g)&k==1,g:='k1.star']
  sfc[is.na(g),g:='hinge']
  setorder(sfc,v)

  cr<-rbindlist(list(
    bel2mel[[type]][ew>=minew,.(ut=unlist(ut)),by=cr1] %>% setnames('cr1','cr')
    ,bel2mel[[type]][ew>=minew,.(ut=unlist(ut)),by=cr2] %>% setnames('cr2','cr')
  )) %>% unique
  cr<-cr[,.N,cr] %>% setkey(cr)
  setkey(sfc,names)
  sfc[cr$cr,cc:=cr$N] # citation count
  sfc[,text:='']
  if(!missing(wok2dbl)) {
    if(!missing(dbl2bel)) {
      setkey(wok2dbl,id,field)
      wok2dbl %<>% .[expand.grid(dbl2bel$ut %>% unique,ec('J9,PY,CR')),!'o']
      setkey(dbl2bel,cr)
      if('zcr'%in%names(dbl2bel)) {
        wok<-rbindlist(list(wok2dbl[field!='CR'],dbl2bel[,.(id=ut,field='CR',val=zcr)] %>% unique))
        setkey(dbl2bel,zcr)
      }
      sfc[,mut:=dbl2bel[names,table(ut) %>% mean %>% as.double],by=g]
      sfc[,m2ut:=dbl2bel[names,table(ut) %>% sort(T) %>% .[2]],by=g]
      sfc[,gini:=dbl2bel[names,table(ut) %>% ineq::Gini(.) %>% as.double],by=g]
      sfc[,text:=paste0('mut:',round(mut,2),' m2ut:',m2ut,' gini:',round(gini,3),'\n')]
    } else {
      setkey(wok2dbl,field)
      wok2dbl %<>% .[ec('J9,PY,CR'),!'o']
      wok<-wok2dbl
    }
    so<-wok2mrg.f(wok,ec('CR,J9'))
    setkey(so,cr)
    so<-so[sfc$names]
    so<-so[,.(so=j9 %>% table %>% names %>% list,sop=j9 %>% table %>% prop.table %>% {round(.*100)} %>% list),by=cr][,.(so=unlist(so),sop=unlist(sop)),by=cr]
    so<-dcast(so,cr~so,value.var = 'sop',fill=0)

    setkey(sfc,names)
    sfc[so$cr,names(so)[-1]:=so[,!'cr']]

    py<-wok2mrg.f(wok,ec('CR,PY'))
    setkey(py,cr)
    py<-py[sfc$names]
    py[,ca:=as.integer(py)-{cr %>% strsplit(', ') %>% sapply(function(x) x[grepl('^[0-9]{4}$',x)][1] %>% as.integer)}]
    py<-py[,.(py=mean(py %>% as.integer),ca=ca %>% na.omit %>% mean),by=cr]

    setkey(sfc,names)
    sfc[py$cr,names(py)[-1]:=py[,!'cr']]

    sfc[,text:=paste0(text,'py:',round(py),' ca:',round(ca))]

  }

  sfc[,gn:=rep(.N,.N),by=g]

  el<-data.table(as_edgelist(net)) %>% setnames(ec('s,r'))
  el<-merge(el,sfc[,.(names,k)],by.x='s',by.y='names') %>% setnames('k','sk')
  el<-merge(el,sfc[,.(names,k)],by.x='r',by.y='names') %>% setnames('k','rk')
  el[,inf:=rk<sk]
  inf<-rbindlist(list(el[,.(names=s,inf)],el[,.(names=r,inf=!inf)]))
  g<-merge(sfc[,.(names,g)],inf,by='names')[,.(minf=mean(inf)),by=g]
  sfc<-merge(sfc,inf[,.(inf=mean(inf)),by=names],by='names')
  sfc<-merge(sfc,g,by='g')

  sfc[,text:=paste0('k',k,'.',sub('^[^.]+\\.','',g),' (',gn,') minf:',round(minf,3),'\n',names,'\ncc:',cc,' inf:',round(inf,3),' ',text)]

  # construct 3d surface
  ## TODO alpha hull radius
  ## sapply(kcc,function(x) dist(sfc[x,.(x,y)]) %>% max) %>% max
  setorder(sfc,v)

  if(pins) if(!exists('pin')) {
    cat('\ncalculating surface')

    wm<-setdiff(1:gorder(net),kcc %>% unlist %>% unique)
    wmk<-degree(net)[wm]

    xr<-range(co[,1])
    yr<-range(co[,2])
    cl<-min(diff(xr),diff(yr))/res
    rd<-hex*res*cl
    pin<-expand.grid(x=seq(xr[1],xr[2]+cl,cl),y=seq(yr[1],yr[2]+cl,cl)) %>% SpatialPoints # https://en.wikipedia.org/wiki/Pin_Art
    kcc<-c(list(1:gorder(net)),kcc)
    names(kcc)[1]<-'k1.all'
    k<-kcc %>% names %>% sub('\\..+','',.) %>% unique
    hul<-list()

    for(i in k) hul[[i]]<-lapply(kcc[grep(paste0(i,'\\.'),names(kcc))],function(z) {
      chull(sfc[z,.(x,y)]) %>% sfc[z][.,.(x,y)] %>% tilit::schull(.) %>%
      {rbindlist(list(.,.[,.(x=x+rd,y)],.[,.(x=x-rd,y)],.[,.(x,y=y+rd)],.[,.(x,y=y-rd)]))} %>%
        tilit::schull(.) %>% Polygon(hole=F)
    }) %>% Polygons(ID = i)
    hul<-SpatialPolygons(hul,1:length(hul))

    ov<-pin %over% hul
    pin <- data.table(as.data.frame(pin),k=as.integer(sub('k([0-9]+).*','\\1',k))[ov])[!is.na(k)]
    setattr(sfc,'radius',rd)
  }

  setattr(sfc,'colors',clrl)
  setattr(sfc,'surface',pin)
  sfc
}

