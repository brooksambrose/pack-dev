#' Negative community detection, or finding groups of people who've worked together the least.
#'
#' @param el a two column data.frame where first column is sender and second reciever
#' @param out.dir output directory
#' @param mx.grp upper limit on the number of groups. In many situations you can use this to define the number of groups you want, unless there is an "optimal solution" that works with fewer groups.
#' @param type do you want to find similar or different groups?
#' @param reps increase if precision is super important, will also increase running time
#' @param cex expansion factor for vertex labels
#' @param lab.col color of vertex labels
#' @param count.last.twice discourage the most recent groups from happening again
#' @return Non, but prints community results and saves plot to output directory.
#' @importFrom igraph
#' @author Brooks Ambrose, \email{brooksambrose@berkeley.edu}
#' @references \url{https://github.com/brooksambrose/MIDS}
#' @seealso \code{\link{[igraph]{spinglass.community}}}
#'
dif.groups<-function(
	el
	,out.dir
	,mx.grp
	,type=c('different','same')
	,reps=10
	,cex=1.25
	,lab.col='black'
	,count.last.twice=F
)
{

	require(igraph) # had used "statnet" but this is faster and more updated
	require(data.table) # just for concise printing of edgelists

	cat('Bipartite edgelist\n')
	print(data.table(el))

	### Parameters and data for both types ###
	bnet<-graph.edgelist(as.matrix(el),directed=F) # converts edgelist into a igraph network object
	V(bnet)$type<-V(bnet)$name%in%el[,2]   # adds a "type" vertex attribute defining the first and second mode membership, as expected by the bipartite.projection function
	mnet<-bipartite.projection(bnet)
	for(i in which(!sapply(mnet,function(x) length(E(x))))) E(mnet[[i]])$weight<-NA
	for(i in names(mnet)) mnet[[i]]$name<-i

	cat('\nMonopartite edgelists\n')
	lapply(mnet, function(x) {print(data.table(get.edgelist(x),ew=E(x)$weight));cat('\n')})

	### Define estimation and plotting function ###
	groups.f<-function(
		net
		,title # name of pdf excluding extension, e.g. "plot" not "plot.pdf". Extension will be added automatically.
		,out=out.dir
		,typ
		,lyt
	)
	{
		if(!length(E(net))) return('Graph is empty.')
		enet<-graph_from_adjacency_matrix(as.matrix(!as_adj(net)),mode='undirected',diag=F,weighted=T)
		E(enet)$weight<-0
		net<-net%u%enet
		E(net)$weight_1[is.na(E(net)$weight_1)]<-0
		E(net)$weight<-E(net)$weight_1
		net<-delete_edge_attr(net,'weight_1')
		net<-delete_edge_attr(net,'weight_2')
		if(typ=='different') E(net)$weight<- E(net)$weight*-1 # reverse weights

		sg<-replicate(reps,spinglass.community(net,spins=mx.grp,implementation='neg',weights=E(net)$weight),simplify=F) # run community detection 100 times so we can take "best" result
		mod<-sapply(sg,modularity,weights=E(net)$weight)

		if(typ=='same') {
			sgb<-sg[[which.max(mod)]]
			bmod<-max(mod) # double check, but I think this is the right way to treat modularity
		}
		if(typ=='different') {
			sgb<-sg[[which.min(mod)]]
			bmod<-min(mod) # double check, but I think this is the right way to treat modularity
		}
		net<-delete_edges(net,which(!E(net)$weight))
		par(
			omi=rep(0,4)
			,mai=rep(0,4) # c(bottom, left, top, right)
			,mfrow=c(1,2)
		)
		fo<-gsub(paste(.Platform$file.sep,'+',sep=''),.Platform$file.sep,paste(out,.Platform$file.sep,title,'-',typ,'.pdf',sep=''))
		dir.create(out,recursive = T,showWarnings = FALSE)
		pdf(fo)
		plot(
			sgb
			,net
			,layout=lyt
			,edge.width=abs(E(net)$weight)/max(abs(E(net)$weight))*3
			,vertex.label.cex=cex
			,vertex.label.color=lab.col
			,vertex.frame.color='white'
			,col=rainbow(length(sgb))[membership(sgb)]
			,mark.groups=NULL
		)
		title(main=paste(title,typ,'\nmod',round(bmod,3)),cex=cex,outer=F)
		plot(density(mod,bw=.01),xlim=c(0,1))
		abline(v=bmod,col='red')
		plot(
			net
			,layout=lyt
			,edge.width=abs(E(net)$weight)/max(abs(E(net)$weight))*3
			,vertex.label.cex=cex
			,vertex.label.color=lab.col
			,vertex.frame.color='white'
		)
		dev.off()
		par(mfrow=c(1,1))

		list(net=net,com=sgb)
	}

	### Plot each projection and type specified ###

	ret<-lapply(mnet,function(x) {
		l<-layout.fruchterman.reingold(x,weights=E(x)$weight)
		lapply(type, function(y) groups.f(x,typ=y,title=x$name,lyt=l))
	})
	for(i in names(ret)) for(j in 1:length(ret[[i]])){
		if(!is.atomic(ret[[i]][[j]])) {
			ret[[i]][[c('diff','same')[j]]]$com$membership<-ret[[i]][[j]]$com$membership %>% sample(unique(.))[.]
			ret[[i]][[c('diff','same')[j]]]$com$csize<-ret[[i]][[j]]$com$membership %>% table %>% as.numeric
		}
	}
	ret
}
