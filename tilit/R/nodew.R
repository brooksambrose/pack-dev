#' Nodeweight from edgeweight of undirected, unlooped graph
#'
#' @param x sum of edgeweights of subnetwork
#'
#' @return
#' @export
#'
#' @examples
nodew<-function(x){
  (1+sqrt(1+8*x))/2
}
