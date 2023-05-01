#' Scan across resolution parameter of Leiden Community Detection
#'
#' @param y
#' @param iter
#' @param rp
#' @param precision
#' @param bas
#' @param verbose
#' @param mx.grp
#' @param n.grp
#'
#' @return
#' @export
#'
#' @examples
scan_leiden.f<-function(y,iter=1000,rp=0,precision=3,bas=1,verbose=F,mx.grp,n.grp){
	ocl<-withr::with_seed(seed,{cluster_leiden(y,'modularity',resolution_parameter = rp)})
	icl<-ocl
	c<-0
	rs<-data.table() #data.table(rp=rp,ng=ng,mod=mod,vr=vr,mx=mx,mn=mn,c=c,cl=list(icl))
	inc<-bas
	pr<-inc %>% as.character %>% strsplit('\\.') %>% unlist %>% last %>% nchar
	dg<-list(rp)
	different<-F
	wd<-F
	while(
		c<=iter&
		!((membership(icl) %>% table %>% {all(.==1)})&pr>=precision)
	) {
		c<-c+1
		# if(c==43) browser()
		# cat(c,'')
		if(is.na(rp)) browser()
		pr<-rp %>% as.character %>% strsplit('\\.') %>% unlist %>% last %>% nchar
		nicl<-withr::with_seed(seed,{cluster_leiden(y,'modularity',resolution_parameter = rp)})
		if(exists('start')&&is.logical(start)&&start) icl<-nicl;start<-F
		different<-!identical(icl %>% communities,nicl %>% communities)
		if(different){
			if(!wd) inc<-inc/2
			rp<-rp-inc
			wd<-T
		} else {
			#same
			if(wd) inc<-inc/2
			rp<-rp+inc
			wd<-F
		}
		prdf<-round(last(dg),precision)-round(rp,precision)
		if(verbose) cat(c,ifelse(different,' F ',' T '),rp,'\t',ifelse(wd,'-',' '),inc,'\t',prdf,'\n',sep='')
		tT<-(!different)&!(prdf)
		# if(is.na(tT)) browser()
		if(tT){
			ng<-icl$nb_clusters
			tb<-membership(icl) %>% table
			vr<-var(tb)
			if(is.na(vr)) vr<-Inf
			mx<-max(tb)
			mn<-min(tb)
			mod<-modularity(y,membership(icl))
			rs<-list(rs,data.table(rp=rp,ng=ng,mod=mod,vr=vr,mx=mx,mn=mn,c=c,cl=list(nicl))) %>% rbindlist(fill=T)
			inc<-bas
			rp<-sort(unlist(dg)) %>% {.[min(which(.>=rp))]} # TODO former .>rp error when largest value equal to rp, breaking change?
			icl<-nicl
			start<-T
			different<-F
			wd<-F
		}
		dg[[length(dg)+1]]<-rp
	}
	# TODO need to have two parameters, the maximum number of groups and/or the maximum size of a group
	if(!n.grp%in%rs$ng) stop(sprintf('No solution with n.grp = %s found',n.grp),call. = F)
	pck<-rs[.(n.grp),on='ng'][.N][which.min(vr)][which(mod==max(mod))][which.max(mn)][which.min(mx)][.N]
	wpck<-rbindlist(list(pck,rs))[,!'cl'] %>% duplicated %>% which
	p1<-ggplot(rs,aes(x=rp,y=mod,label=ng,color=log10(vr))) + geom_line(color='gray')+geom_text() + scale_color_viridis_c(end=.75,option='viridis') +
		geom_point(data=pck,shape=21,aes(x=rp,y=mod),color='red',size=6)

	lyt<-layout_with_stress(y %>% {E(.)$weight %<>% {1/.};.},weights=NULL,tol=1e-06,iter = 1e6)
	g<-igraph::as_data_frame(y,what = 'both')
	g$vertices$group<-membership(pck$cl[[1]]) %>% factor %>% fct_infreq(.) %>% as.integer %>% factor
	g$vertices$x<-lyt[,1]
	g$vertices$y<-lyt[,2]
	g$vertices %<>% data.table
	g$edges %<>% data.table
	g$edges[g$vertices,on='from==name',`:=`(x=x,y=y)]
	g$edges[g$vertices[,.(name,X=x,Y=y)],on='to==name',`:=`(xend=X,yend=Y)]
	alle<-rs[,.(lapply(cl,function(x) data.table(g$edges,cross=ec('dotted,solid')[igraph::crossing(x,y)+1]))),by=c][,rbindlist(V1),by=c]
	alle[rs,on='c',`:=`(rp=rp,mod=mod,var=vr,ng=ng,mx=mx,mn=mn)]

	g$edges$cross<-ec('solid,dotted')[igraph::crossing(pck$cl[[1]],y)+1]

	allv<-rs[,.(lapply(cl,function(x) data.table(name=x$names,group=x$membership))),by=c][,rbindlist(V1),by=c]
	allv %<>% .[rs[,!'cl'],on='c']
	allv[,group:=group %>% factor %>% fct_infreq %>% as.integer,by=c]
	allv[,group:=createPalette(N=max(group),RColorBrewer::brewer.pal(max(group),'Set2'),target = 'normal')[group]]
	allv[,group:={
		tg<-table(group)
		n<-names(tg[tg==1])
		group[group%in%n]<-'gray'
			group
	},by=c]
	allv[g$vertices,on='name',`:=`(x=x,y=y)]

	p2<-ggplot() +
		geom_segment(data=g$edges['dotted',on='cross'],aes(x=x,y=y,xend=xend,yend=yend,color=weight),linetype='dotted') +
		geom_segment(data=g$edges['solid',on='cross'],aes(x=x,y=y,xend=xend,yend=yend,color=weight),linetype='solid') +
		geom_point(data=g$vertices,shape=21,color=gray(1,0),aes(x=x,y=y,fill=group),size=8) +
		geom_text(data=g$vertices,color=gray(.1),aes(x=x,y=y,label=name),hjust='inward') +
		scale_fill_brewer(palette='Set2') +
		scale_color_viridis(direction=-1,option='magma',end = .9) +
		cowplot::theme_nothing() + theme(panel.background = element_rect(fill='whitesmoke',color=NA))+
		coord_fixed()
	plt(p2,hovinf = 'none')
	poly<-g$vertices[,concaveman::concaveman(cbind(x,y)) %>% data.table %>% lapply(as.numeric),by=group] %>% setnames(ec('group,x,y'))

	alle$cross %<>% factor
	levels(alle$cross) %<>% rev
	alle[allv,on=ec('from==name,c'),group:=group]
	alle['dotted',on='cross',group:='white']
	alle[,eid:=paste(from,to)]
	alle[,eid:=as.character(.I)]
	allv[,vid:=as.character(.I)]

	mtest<-alle[,!ec('rp,mod,var,ng,mx,mn,eid')] %>% melt(id.vars=ec('c,from,to,group,cross,weight'),measure.vars=list(x=ec('x,xend'),y=ec('y,yend')))
	mtest %>% setorder(from,to,c,variable)
	smt<-mtest['solid',on='cross']
	# add zero frame
	smt<-rbindlist(list(copy(smt[.(pck$c),on='c'])[,c:=0],smt))
	allv<-rbindlist(list(copy(allv[.(pck$c),on='c'])[,c:=0],allv))
	alle<-rbindlist(list(copy(alle[.(pck$c),on='c'])[,c:=0],alle))
	setorder(alle,c,-cross,-weight,group,eid)
	alle<-alle['solid',on='cross']
	alle<-split(alle,by=ec('weight,group'))
	# allv<-split(allv,by='group')
	plx<-plot_ly()
	#this is all effed
	# for(i in alle) plx %<>% add_segments(data = i,x=~x,y=~y,xend=~xend,yend=~yend,frame=~c,size=~I(weight[1]),inherit = F,color=~I(group[1]))
	# browser()
	plx %<>% add_text(data = allv %>% group_by(c,name,group),x=~x,y=~y,frame=~c,text=~name,size=I(20),color=~I(group))
	plx$x$layout<-ggplotly(p=p2)$x$layout
	plx %<>% animation_slider(active=wpck-1) %>% animation_opts(frame=1000,transition = 0,mode='immediate',redraw = T,easing = 'linear')

	# browser()
	frm<-pck$cl[[1]]$membership %>% table %>% {.-mx.grp} %>% {.[which(.>0)]} %>% abs # group that should be moved
	cnd<-pck$cl[[1]] %>% {.$names[.$membership%in%as.integer(names(frm))]}
	if(min(pck$mn)<3&!length(cnd)){
		pls<-rs[.(mx.grp+1),on='ng'][which.max(vr)]
		ra<-pls$cl[[1]]$membership %>% table %>% {.[.==min(.)]} # smallest group in next solution, candidate likely to come from here
		cnd<-communities(pls$cl[[1]])[names(ra)] %>% unlist
		frm<-pck$cl[[1]] %>% {.$membership[.$names%in%cnd]} %>% unique %>% as.character
		cnd<-communities(pck$cl[[1]])[frm] %>% unlist %>% unname
		frm<-pck$cl[[1]]$membership %>% table %>% .[frm] %>% {.-mx.grp}
	}
	to<-pck$cl[[1]]$membership %>% table %>% {.-mx.grp} %>% {.[which(.<0)]} %>% abs # potential destinations
	opt<-CJ(cnd,frm=names(to) %>% as.integer)
	tgt<-pck$cl[[1]] %>% {data.table(nm=.$names,m=.$membership)}
	prm<-try(combn(1:nrow(opt),sum(frm) %>% ifelse(.,.,1)),silent = T)
	if(!inherits(prm,'try-error')) {
		prm %<>% data.table %>% lapply(function(x) {
			copy(tgt)[opt[x],on=ec('nm==cnd'),m:=frm]
		})
		mrp<-sapply(prm,function(x) modularity(y,x$m,E(y)$weight)) %>% as.vector
	}
	fpk<-try(prm[[withr::with_seed(seed,{mrp %>% {which(.==max(.))} %>% sample(1)})]],silent = T)
	# browser()
	if(inherits(fpk,'try-error')) {
		fpk<-pck$cl[[1]] %>% {data.table(nm=.$names,m=.$membership)}
		dif<-data.table(name=NULL,from=NULL,to=NULL)
	} else {
		dif<-fpk[!!pck$cl[[1]]$membership-m]
		setnames(tgt,'m','M')
		dif[tgt,on='nm',frm:=M]
		dif %>% setnames(ec('name,to,from')) %>% setcolorder(ec('name,from,to'))
		if(nrow(dif)){cat('These members were reallocated to the indicated group\n');print(dif)}
	}

	list(fpk=fpk,g=g,ig=y,rs=rs,pck=pck,p=p1,pg=p2,pl=plx,dif)
}

#' Negative community detection, or finding groups of people who've worked together the least.
#'
#' @param el a two column data.frame where first column is sender and second receiver
#' @param out.dir output directory
#' @param type do you want to find similar or different groups?
#' @param reps increase if precision is super important, will also increase running time
#' @param cex expansion factor for vertex labels
#' @param lab.col color of vertex labels
#' @param mx.grp upper limit on the number of groups. In many situations you can use this to define the number of groups you want, unless there is an "optimal solution" that works with fewer groups.
#' @param count.last.twice discourage the most recent groups from happening again
#'
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
	V(bnet)$type<-grepl('[0-9]',V(bnet)$name)   # adds a "type" vertex attribute defining the first and second mode membership, as expected by the bipartite.projection function
	mnet<-list(proj1=bipartite.projection(bnet,which = 'false'))
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

#' Negative community detection, or finding groups of people who've worked together the least.
#' This implementation scans a resolution parameter of inverser network rather than using negative detection.
#'
#' @param el a two column data.frame where first column is sender and second receiver
#' @param out.dir output directory
#' @param type do you want to find similar or different groups?
#' @param reps increase if precision is super important, will also increase running time
#' @param cex expansion factor for vertex labels
#' @param lab.col color of vertex labels
#' @param mx.grp upper limit on the number of groups. In many situations you can use this to define the number of groups you want, unless there is an "optimal solution" that works with fewer groups.
#' @param count.last.twice discourage the most recent groups from happening again
#'
#' @return Non, but prints community results and saves plot to output directory.
#' @importFrom igraph
#' @author Brooks Ambrose, \email{brooksambrose@berkeley.edu}
#' @references \url{https://github.com/brooksambrose/MIDS}
#' @seealso \code{\link{[igraph]{spinglass.community}}}
#'
dif.groups2<-function(
		el
		,isolate
		,out.dir
		,n.grp
		,mx.grp
		,type=c('different','same')
		,reps=10
		,cex=1.25
		,lab.col='black'
			,count.last.twice=F
)
{

	require(ggraph)
	require(graphlayouts)
	require(concaveman)
	require(plotly)
	require(tilit)
	require(ggnewscale)
	require(forcats)
	require(Polychrome)
	require(igraph) # had used "statnet" but this is faster and more updated
	require(data.table) # just for concise printing of edgelists

	cat('Bipartite edgelist\n')
	print(data.table(el))

	### Parameters and data for both types ###
	bnet<-graph.edgelist(as.matrix(el),directed=F) # converts edgelist into a igraph network object
	V(bnet)$type<-grepl('[0-9]',V(bnet)$name)   # adds a "type" vertex attribute defining the first and second mode membership, as expected by the bipartite.projection function
	if(!missing(isolate)) bnet %<>% add_vertices(nv=length(isolate),name=isolate,type='false')
	mnet<-list(proj1=bipartite.projection(bnet,which = 'false'))
	for(i in which(!sapply(mnet,gsize))) E(mnet[[i]])$weight<-NA
	for(i in names(mnet)) mnet[[i]]$name<-i
	mnet %<>% lapply(simplify)

	### Inverted Edgelist Network ###
	inet<-lapply(mnet,function(x) x %>% as_adj(attr='weight') %>% {abs(.-max(.))} %>% {graph_from_adjacency_matrix(as.matrix(.),mode='undirected',weighted=T)})
	inet %<>% lapply(simplify)
	isopad<-if(missing(isolate)) 0 else length(isolate)
	withr::with_seed(seed,{
		mscn<-lapply(mnet,scan_leiden.f,mx.grp=mx.grp,n.grp=n.grp+isopad)
		iscn<-lapply(inet,scan_leiden.f,mx.grp=mx.grp,n.grp=n.grp)
	})
	list(diff=iscn,same=mscn)
}
