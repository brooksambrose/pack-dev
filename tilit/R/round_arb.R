#' Round vector to set of arbtirary numbers
#'
#' @param x
#' @param snaps
#' @param range
#'
#' @return
#' @export
#'
#' @examples
round_arb<-function(x,snaps,range=NULL){
  snaps<-sort(snaps)          # need them sorted
  if(is.null(range)) range<-snaps %>% diff %>% mean %>% `/`(2)
  range <- range*(1+.Machine$double.eps)         # avoid rounding issues
  nearest <- findInterval(x, snaps - range) # index of nearest
  nearest <- c(-Inf, snaps)[nearest + 1]  # value of nearest
  diff <- x - nearest                           # compute errors
  snap <- diff <= range                               # only snap near numbers
  x[snap] <- nearest[snap]                      # snap values to nearest
  x
}
