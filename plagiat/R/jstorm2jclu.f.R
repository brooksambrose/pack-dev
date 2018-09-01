#' JSTOR Master 2 Journal Cluster
#'
#' @param jstorm
#'
#' @return
#' @export
#' @import igraph data.table
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
  jclu<-list(title_history=cw1,discipline=cw1)

  jstormr<-jstorm[,.(discipline=discipline %>% unlist %>% unique),keyby=title_history]
  jstormr<-jstormr[data.table(th=jclu$title_history$names,m=jclu$title_history %>% membership) %>% setkey(th)]
  jstormr<-jstormr[,.N,by=.(m,discipline)] %>% setorder(m,-N)
  jstormr<-jstormr[,.(super=discipline[1],N=sum(N)),by=m][,!'m']

  j<-data.table(title_history=jclu$title_history$names,super=jstormr[jclu$title_history %>% membership,super]) %>% setkey(title_history)
  c(jclu,tab=list(jstormr),super=list(j))
}
