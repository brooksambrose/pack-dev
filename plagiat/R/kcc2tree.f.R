#' K-clique Communities 2 Tree Map
#'
#' @param cos2kcc
#' @param bel2mel
#' @param type
#' @param seed
#' @param wok2dbl
#'
#' @return
#' @export
#' @import data.table ggplot2 magrittr viridis
#'
#' @examples
kcc2tree.f<-function(cos2kcc,bel2mel,wok2dbl,type=c('crel','utel')[1],sides=8,seed=12345,transform=F,clr='D'){
  mel<-copy(bel2mel[[type]]) %>% setnames(ec('from,to,weight,id')) %>% setkey(from,to)
  kcc<-cos2kcc[[type]]$orig
  lev<-cos2kcc[[type]] %>% attr('levels')
  hng<-lapply(kcc,function(x) {
    l<-length(x)
    if(l>1){
      h<-ij(l)
      lapply(h,function(y) {
        y<-intersect(x[[y[1]]],x[[y[2]]])
        if(length(y)>1) y %>% lev[.] %>% sort %>% combn(2) %>% mlist
      })}
  })
  hng<-do.call(c, unlist(hng, recursive=FALSE)) %>% unique %>% do.call(what=rbind) %>% data.table %>% setnames(ec('from,to'))

  kcc %<>% do.call(what = c)
  kcc<-c(list('k1.k1c1'=1:length(lev)),kcc)
  # TODO some kcc's lost during unzip
  unz<-lapply(names(kcc)[-1],function(x) try(data.table(mel[lev[kcc[[x]]] %>% sort %>% combn(2) %>% t %>% data.table %>% setnames(ec('from,to')),.(from,to),nomatch=0][!hng,on=.(from,to)],g=x),silent = T)) %>% .[!sapply(.,inherits,'try-error')]
  unz %<>% rbindlist %>% setkey(from,to)

  kel<-strsplit(names(kcc),'[k.c-]') %>%
    lapply(function(x) data.table(rbind(x %>% as.integer))) %>%
    rbindlist(fill=T) %>%
    .[,4:6] %>%
    setnames(ec('k,c,i')) %>%
    data.table(name=names(kcc),.)

  setkey(kel,c,k,i)
  mel[unz[,.(g=g[which.max(sub('k([0-9]+)\\..+','\\1',g) %>% as.integer)]),by=.(from,to)],on=.(from,to),g:=g]

  unz<-unz[,{
    kl<-kel[name%in%g]
    f<-from
    t<-to
    kl[,.(p=paste(c('k1.k1c1',name),collapse=' '),weight=mel[.(f,t),weight])]
  },by=.(from,to)]
  unz<-unz[,.(weight=sum(weight)),by=p][,weight:=nodew(weight)] %>% setkey(p)

  for(i in 1:nrow(unz)) unz[grep(paste(unz$p[i],''),p),p:=sub(unz[i,p] %>% sub('[^ ]+$','',.),'',p)]
  unz[,p:=sub(' .+ ',' ',p)]

  # covariates
  dbl<-wok2dbl[mel$id %>% unlist %>% unique] %>% setnames('id','did')
  setkey(dbl,field)
  dbl['TI',val:=stringr::str_to_title(val)]
  cr<-mel[!is.na(g),.(
    cr=dbl['CR',.N,keyby=val
           ][unique(c(from,to)),nomatch=0
             ][sample(1:.N)] %>%
      setorder(-N) %>%
      .[1:min(5,.N)]  %>%
      apply(1,function(x) paste(rev(x),collapse=' ')) %>%
      paste(collapse='\n')
  ),by=g]
  setkey(dbl,did,field)
  ut<-mel[!is.na(g),.(
    ut=dbl[CJ(unlist(id) %>% unique,ec('PY,J9,AU,TI,BP,EP,UT'),sorted = F),.(
      ut=val %>% {
        # some titles repeated themselves
        t<-c(substr(.,0,floor(nchar(.)/2)),substr(.,ceiling(nchar(.)/2),nchar(.))) %>% sub('^ +','',.) %>% sub(' +$','',.)
        if(identical(t[1],t[2])) t[1] else .
      } %>% paste(collapse='\t'))
      ,by=did,nomatch=0
      ][,N:=unlist(id) %>% table %>% nodew %>% round(2) %>% c
        ] %>% setorder(-N) %>%
      .[1:min(5,.N),.(N,ut)] %>%
      apply(1,paste,collapse=' ') %>%
      stringr::str_wrap(exdent=2) %>%
      paste(collapse='\n')
  ),by=g]
  sd<-mel[!is.na(g),.(
    sd=dbl[CJ(unlist(id) %>% unique,ec('AU')),stringdist::stringdistmatrix(val %>% na.omit,method = 'jw',p=.1) %>% mean %>% {.*.N}]
  ),by=g]

  txt<-ut[cr,on='g'][,.(utcr=paste(c(ut,cr),collapse='\n\n')),by=g]
  setkey(mel,g)
  txt<-txt[!is.na(g),mel[g,.(txt=c(g %>% unique,' ut(',id %>% unlist %>% unique %>% length,')cr(',c(from,to) %>% unlist %>% unique %>% length,')\n\n',utcr) %>% paste(collapse=''))],by=g]

  # layout
  # browser()
  unl<-unz[,strsplit(p,' ')[[1]] %>% {cbind(.[-length(.)],.[-1])} %>% data.table %>% setnames(ec('from,to')),by=p][,!'p']
  vtx<-unz[,.(name=sub('.+ ','',p),weight)]
  vtx<-rbindlist(list(vtx,unl[,.(name=c(from,to) %>% unique %>% setdiff(vtx$name),weight=0)]))[!duplicated(name)][,k:=sub('k([0-9]+)\\..+','\\1',name) %>% as.integer]

  vtx[sd,on='name==g',sd:=sd]
  vtx[,n:=sapply(kcc[name],length)]

  # vtx[,cut:=cut(n,breaks = 2,include.lowest = T),by=k]
  # vtx[is.na(sd),cut:=NA]
  vtx[,sdo:=sd][,depth:=graph_from_data_frame(unl,T,vtx) %>% bfs('k1.k1c1','all',order=F,dist=T) %>% .$dist]

  # layout order
  vtx %>% setorder(depth,-sd,na.last=T) # setorder(-depth,-sdo,-k,na.last = T) #setorder(-k,-cut,-sd,na.last = T)
  gph<-graph_from_data_frame(unl,T,vtx)
  od<-degree(gph,mode = 'out')
  res<-vtx[names(od[od!=0]),on='name'][weight>0]
  unl<-rbindlist(list(unl,res[,.(from=name,to=paste0(name,'.r'))]))
  vtx[res$name,on='name',name:=paste0(name,'.r')]
  res[,weight:=0]
  vtx<-rbindlist(list(vtx,res))

  gph<-graph_from_data_frame(unl,T,vtx)

  set.seed(seed)
  cp<-ggraph:::layout_igraph_circlepack(gph,weight='weight') %>% data.table
  cp[grep('\\.r$',name),depth:=depth-1]
  cp[,depth:=factor(depth)]
  cp[txt,on='name==g',txt:=txt]
  cp[is.na(txt),txt:=name]
  cp[sd,on='name==g',sd:=sd]
  cp[is.na(sd),sd:=NA]
  if(transform) {
    cp[!is.na(sd),sdo:=sd]
    cp[!is.na(sd),sd:=ihs(sd)]
  }
  cp %>% setorder(depth,-r)

  vts<-packcircles::circleLayoutVertices(cp[,.(x,y,r)],npoints = sides) %>% data.table
  vts[,k:=cp$k[id]][,level:=cp$depth[id]]
  vts[,text:=cp$txt[id]]
  vts[,sd:=cp$sd[id]]
  vts[,r:=cp$r[id]]
  vts[,depth:=cp$depth[id]]
  vts %>% setorder(depth,-r)

  map<-aes(fill=depth,color=k)

  setkey(vts,level)
  p<-ggplot()
  for(i in levels(vts$level)[-1]) p<-p+geom_polygon(data=vts[i],aes(x,y,group=id,fill=sd,color=level,text=text),size=.25)

  thm<-function(x) x + theme_void() +
    scale_color_viridis(option='E',discrete = T,na.value = 'red') +
    scale_fill_viridis(option=clr,discrete = F,na.value='lightgray',alpha=.5,direction = 1)
  #scale_color_gradient(low='black',high='red',na.value = 'white')


  set.seed(seed)


  cp[,`:=`(x=scale(x,center = F),y=scale(y,center = F))]
  list(
    top=thm(ggraph::ggraph(gph, 'circlepack',weight='weight') +
              ggraph::geom_node_circle(map,size=.25,n = sides)) + coord_fixed()
    ,side=thm(ggraph::ggraph(gph, 'partition', weight = 'weight') +
                ggraph::geom_node_tile(map,size=.5)) + theme(legend.position = 'right')
    ,int=plt(thm(p) + coord_fixed(),tooltip='text') %>% plotly::config(displaylogo=F,collaborate=F)
    ,mel=mel
    ,kdb=cp
    ,gph=gph
  )
}
