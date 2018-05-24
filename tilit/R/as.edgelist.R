#' Title
#'
#' @param net
#'
#' @return
#' @export
#'
#' @examples
as.edgelist<-function(
  net
)
{
  require(network)
  el<-matrix(unlist(do.call(rbind,net$mel)[,2:1]),ncol=2)
  el<-cbind(
    s=network.vertex.names(net)[el[,1]]
    ,r=network.vertex.names(net)[el[,2]]
  )
  el
}
