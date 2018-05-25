#' interchangeable names for ontological element as person
#'
#' @param sp
#' @param n
#'
#' @return
#' @export
#'
#' @examples
ones <- function(sp=1,n=NULL){
	r<-rbind(
		act=c(s='actor',p='actors')
		,beh=c('behavior','behaviors')
		,per=c('performer','performers')
		,sub=c('subject','subjects')
	)
	if(is.null(n)) n<-sample(rownames(r),1)
	r[n,sp]
}
