#' Title
#'
#' @param a
#' @param b
#' @param cull
#' @param noanon
#'
#' @return
#' @export
#'
#' @examples
cull.f<-function(
	a
	,b,cull=.2
	,noanon=F
)
{
	if(any(is.na(b))) {b<-na.omit(b);attributes(b)<-NULL}
	if(noanon) {
		if(grepl("\\[ANONYMOUS\\],? ?",a)) stop("k = \"",a,"\"\n",call.=F)
		b<-sub("\\[ANONYMOUS\\],? ?","",b)
	}
	b<-b[nchar(b)>13]
	if(!is.null(cull)) b<-b[stringdist(a=a,b=b,method="jw",p=.1)<cull]
	b
}
