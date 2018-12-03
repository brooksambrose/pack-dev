#' Sort and Close Vertices of Convex Hull
#'
#' https://stackoverflow.com/questions/48249540/plot-convex-hull-given-by-quickhull-algorithm-in-r-convhulln-function
#'
#' @param chull
#' @param cw
#'
#' @return
#' @export
#'
#' @examples
schull<-function(v,cw=T){
  v <- v[order(ifelse(cw,-1,1) * atan2(v$y - mean(range(v$y)), v$x - mean(range(v$x)))),]
  v <- rbind(v, v[1,])
  v
}
