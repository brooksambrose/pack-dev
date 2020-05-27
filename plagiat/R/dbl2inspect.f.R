#' Title
#'
#' @param wok2dbl
#' @param reps
#' @param drop.miss.vars
#' @import data.table igraph
#'
#' @return
#' @export
#'
#' @examples
dbl2inspect.f<-function(wok2dbl,reps=100,drop.miss.vars=F){
	setkey(wok2dbl,field)
	pw<-combn(unique(wok2dbl[,field]),m=2,simplify = FALSE)
	el<-list()
	ew<-list()
	for(i in 1:length(pw)){
		dbw<-wok2dbl[pw[[i]]]
		if(anyDuplicated(dbw,by=c('id','field'))) next
		dbw<-dcast.data.table(
			data=dbw
			,formula=id~field
			,value.var='val'
		)[,2:3,with=F]
		if(any(sapply(dbw,function(x) any(is.na(x))))) next
		dbf<-dbw[,.N,keyby=eval(colnames(dbw))]
		if(any(dbf$N==1)) next
		ew[[length(ew)+1]]<-summary(table(dbw))$statistic
		#ew[[length(ew)+1]]<-summary(xtabs(as.formula(paste('N',paste(names(dbf),collapse='+'),sep='~')),data=dbf))$statistic
		l<-sort(sapply(dbf[,-3,with=F],function(x) length(unique(x))))
		el[[length(el)+1]]<-paste(names(l),l,sep=' ')
	}
	el<-do.call(rbind,el)
	ew<-scale(unlist(ew),center=F)
	g<-graph.edgelist(el,directed=T)
	E(g)$weight<-ew
	V(g)$weight<-as.integer(sub('^[^ ]+ ','',V(g)$name))
	u<-graph.edgelist(el,directed=F)
	E(u)$weight<-ew
	par(mfrow=c(2,2))
	wu<-walktrap.community(u)
	plot.communities(wu,u,edge.width=E(u)$weight*3,edge.arrow.size=.25,edge.arrow.width=.25)
	g$layout<-layout.sugiyama(g,layers=V(g)$weight)$layout
	plot(g,layout=g$layout,edge.width=E(g)$weight*3,edge.arrow.size=.25,edge.arrow.width=.25,mark.groups=split(1:length(membership(wu)),membership(wu)))
	dendPlot(wu,use.modularity = T)
	f<-wok2dbl[,.N,by=field]
	h<-cut(f$N,breaks=hist(f$N,main='Variables by observation count')$breaks)
	drop<-f$field[as.numeric(h)<which.max(table(h))]
	cat('Recommend dropping:',paste('c(\'',paste(drop,collapse='\',\''),'\')\n',collapse='',sep=''))
	if(drop.miss.vars) {wok2dbl<-wok2dbl[!drop];warning('wok2dbl altered in memory, variables not included in modal bin automatically dropped',immediate. = TRUE)}
	f<-wok2dbl[,.N,by=field]
	setkey(f,N)
	print(f)
	list(g=g,f=f,drop=drop)
}
