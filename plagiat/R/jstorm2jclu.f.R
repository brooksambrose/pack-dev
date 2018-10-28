#' JSTOR Master 2 Journal Cluster
#'
#' @param jstorm
#'
#' @return
#' @export
#' @import igraph data.table ggnetwork
#' @importFrom magrittr %>%
#'
#' @examples
jstorm2jclu.f<-function(jstorm){
  el<-jstorm[,.(discipline=unlist(discipline) %>% unique),by=title_history] %>% as.matrix
  g<-graph_from_edgelist(el)
  V(g)$type<-V(g)$name %in% el[,2]
  g<-bipartite_projection(g)
  cw1<-cluster_walktrap(g$proj1)
  cw2<-cluster_walktrap(g$proj2)
  jclu<-list(gt=g$proj1,title_history=cw1,gd=g$proj2,discipline=cw2)


  # label membership for disciplines with super
  k<-data.table(d=igraph::degree(g$proj2),m=membership(cw2),name=V(g$proj2)$name)[,.(super=name[which.max(d)]),by=m] %>% setorder(m)
  f2<-factor(k$super[membership(jclu$discipline)],levels = k$super)
  V(jclu$gd)$memb<-as.character(f2)

  jstormr<-jstorm[,.(discipline=discipline %>% unlist %>% unique),keyby=title_history]
  jstormr<-jstormr[data.table(th=jclu$title_history$names,m=jclu$title_history %>% membership) %>% setkey(th)]
  jstormr<-jstormr[,.N,by=.(m,discipline)] %>% setorder(m,-N)
  j<-jstormr[,.(super=discipline[1],N=sum(N)),by=m][,!'m']
  f1<-factor(j$super[membership(jclu$title_history)],levels=j$super[order(j$N,decreasing = T)])
  V(jclu$gt)$memb<-as.character(f1)
  w<-j[,which(duplicated(super)|duplicated(super,fromLast = T))]
  if(length(w)) {
    s<-jstormr[,.(super=discipline[1:2] %>% paste(collapse=' (') %>% paste0(')')),by=m][w,super]
    j[w,super:=s]
    }
  jstormr<-j

  j<-data.table(title_history=jclu$title_history$names,super=jstormr[jclu$title_history %>% membership,super]) %>% setkey(title_history)
  jstormr %>% setorder(-N)
  jstormr[,Pct:=round(prop.table(N)*100,1)]
  jstormr<-rbindlist(
    list(jstormr,data.table('Total',jstormr[,2:3][,lapply(.SD,sum)]))
  )

  # crossings
  E(jclu$gd)$cross<-rev(c("within","between"))[(!crossing(jclu$discipline,jclu$gd)) + 1]
  E(jclu$gt)$cross<-rev(c("within","between"))[(!crossing(jclu$title_history,jclu$gt)) + 1]


  c(jclu,ft=list(f1),fd=list(f2),tab=list(jstormr),super=list(j))
}
