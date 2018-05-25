#' interchangeable names for operational molecule
#'
#' @param sp
#' @param n
#'
#' @return
#' @export
#'
#' @examples
opm <- function(sp=1,n=NULL){
	r<-rbind(
	kcc=c(s='k-clique community',p='k-clique communities')
	,kca=c('kcc','kccs')
	)
	if(is.null(n)) n<-sample(rownames(r),1)
	r[n,sp]
}
