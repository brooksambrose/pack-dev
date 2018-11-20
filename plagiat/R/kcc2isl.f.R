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
#'
#' @return
#' @export
#' @import data.table magrittr igraph
#'
#' @examples
kcc2isl.f<-function(bel2mel,cos2kcc,wok2dbl,dbl2bel,type=c('utel','crel')[2],minew=1,co,ordinal=T,border=T,nodes=T,rad=.8,nit=1e4){

  net<-graph_from_edgelist(bel2mel[[type]][ew>=minew,.(cr1,cr2)] %>% as.matrix,F)
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
      nnet,layout.par = list(niter=nit,repulse.rad=log(area)*area^rad,area=area*2/3)
    )
    )
    cat(round(max(t1)/60,2),'mins')
    attr(co,'layout-time')<-t1
  } else {
    setorder(co,v)
    co<-as.matrix(co[,.(x,y)])
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
  sfc[,text:=paste0('k',k,'.',sub('^[^.]+\\.','',g),' (',gn,')\n',names,'\ncc:',cc,' ',text)]
  setorder(sfc,v)
  sfc<-sfc[!is.na(k)]
  setattr(sfc,'colors',clrl)
  sfc
}

