#' Title
#'
#' @param wok2dbl
#' @param form
#'
#' @return
#' @export
#'
#' @examples
dbl2dbw.f<-function(wok2dbl,form='id~field'){
	dbl2dbw<-dcast.data.table(
		data=wok2dbl
		,formula=as.formula(form)
		,value.var='val'
		,fun.aggregate = list
		,fill='NA'
	)
	ord<-copy(colnames(dbl2dbw))
	cat(ord)
	for(i in ord) {
		if(any(sapply(dbl2dbw[[i]],length)>1)) next
		rep<-type.convert(as.character(unlist(dbl2dbw[[i]])))
		dbl2dbw[,(i):=NULL,with=F]
		dbl2dbw[,(i):=rep,with=F]
		rm(rep)
	}
	if('col.ord'%in%names(attributes(wok2dbl))) setcolorder(dbl2dbw,c('id',intersect(attributes(wok2dbl)$col.ord,colnames(dbl2dbw))))
	dbl2dbw
}
