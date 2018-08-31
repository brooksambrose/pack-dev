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
  list(title_history=cw1,discipline=cw1)
}
