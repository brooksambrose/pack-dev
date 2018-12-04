#' List of row and column indices for accessing 2D square matrix upper triangle
#'
#' @param n square matrix or length of one of its dimensions
#'
#' @return
#' @export
#'
#' @examples
ij<-function(n) {
  if(!is.vector(n)) {
    if(length(dim(n))!=2) stop('Too many dimensions')
    if(diff(dim(n))) stop('Not square')
    n<-nrow(n)
    }
    mlist(combn(1:n,2))
}
