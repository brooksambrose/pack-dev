#' interchangeable names for ontological molecule
#'
#' @param sp
#' @param n
#'
#' @return
#' @export
#'
#' @examples
onm <- function(sp=1,n=NULL){
	r<-rbind(
		sf=c(s='subfield',p='subfields')
		,sd=c('subdiscipline','subdisciplines')
		,ic=c('invisible college','invisible colleges')
		,fp=c('field position','field positions')
		,sp=c('specialty','specialties')
		,fi=c('field of inquiry','fields of inquiry')
		,sc=c('subculture','subcultures')
		,mi=c('milieu','milieus')
	)
	if(is.null(n)) n<-sample(rownames(r),1)
	r[n,sp]
}
