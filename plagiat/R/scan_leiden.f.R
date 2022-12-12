#' Leiden Community Detection solutions across range of resolution parameters
#'
#' @param y
#' @param iter
#' @param rp
#' @param precision
#' @param bas
#' @param verbose
#'
#' @return
#' @export
#' @import data.table magrittr igraph withr ggplot2 plotly
#'
#' @examples
scan_leiden.f<-function(y,iter=1000,rp=0,precision=3,bas=1,verbose=F){
  ocl<-withr::with_seed(seed,{cluster_leiden(y,'modularity',resolution_parameter = rp)})
  icl<-ocl
  c<-0
  rs<-data.table() #data.table(rp=rp,ng=ng,mod=mod,vr=vr,mx=mx,mn=mn,c=c,cl=list(icl))
  inc<-bas
  pr<-inc %>% as.character %>% strsplit('\\.') %>% unlist %>% last %>% nchar
  dg<-list(rp)
  different<-F
  wd<-F
  while(
    c<=iter&
    !((membership(icl) %>% table %>% {all(.==1)})&pr>=precision)
  ) {
    c<-c+1
    pr<-rp %>% as.character %>% strsplit('\\.') %>% unlist %>% last %>% nchar
    nicl<-withr::with_seed(seed,{cluster_leiden(y,'modularity',resolution_parameter = rp)})
    if(exists('start')&&is.logical(start)&&start) icl<-nicl;start<-F
    different<-!identical(icl %>% communities,nicl %>% communities)
    if(different){
      if(!wd) inc<-inc/2
      rp<-rp-inc
      wd<-T
    } else {
      #same
      if(wd) inc<-inc/2
      rp<-rp+inc
      wd<-F
    }
    prdf<-round(last(dg),precision)-round(rp,precision)
    if(verbose) cat(c,ifelse(different,' F ',' T '),rp,'\t',ifelse(wd,'-',' '),inc,'\t',prdf,'\n',sep='')
    if((!different)&!(prdf)){
      ng<-icl$nb_clusters
      tb<-membership(icl) %>% table
      vr<-var(tb)
      mx<-max(tb)
      mn<-min(tb)
      mod<-modularity(y,membership(icl))
      rs<-list(rs,data.table(rp=rp,ng=ng,mod=mod,vr=vr,mx=mx,mn=mn,c=c,cl=list(nicl))) %>% rbindlist(fill=T)
      inc<-bas
      rp<-sort(unlist(dg)) %>% {.[min(which(.>rp))]}
      icl<-nicl
      start<-T
      different<-F
      wd<-F
    }
    dg[[length(dg)+1]]<-rp
  }

# browser()
  list(ig=y,rs=rs#,pck=pck,p=p1,pg=p2,pl=plx,dif
       )
}
