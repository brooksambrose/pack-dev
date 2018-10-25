#' Term Edge List 2 Cluster
#'
#' @param fam
#'
#' @return
#' @export
#' @import igraph
#'
#' @examples
tel2clu.f<-function(tel){
  g<-graph_from_edgelist(tel[,.(s,r)] %>%  as.matrix,directed = F)
  E(g)$weight<-tel[,ew]
  cluster_louvain(g)
}
