#' interchangeable names for ontological element as event
#'
#' @param sp
#' @param n
#'
#' @return
#' @export
#'
#' @examples
one <- function(sp=1,n=NULL){
	r<-rbind(
		act=c(s='action',p='actions')
		,beh=c('behavior','behaviors')
		,per=c('performance','performances')
	)
	if(is.null(n)) n<-sample(rownames(r),1)
	r[n,sp]
}
