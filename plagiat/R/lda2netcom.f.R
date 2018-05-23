#'Community detection.
lda2netcom.f<-function(stmbow2lda,out.dir,freq.weight,reps=10,mx.grp=30){
	require(data.table)
	require(igraph)
	require(magrittr)

	if(!missing(freq.weight)) stmbow2lda$doc.top.theta<-freq.weight * stmbow2lda$doc.top.theta
	m<-stmbow2lda$doc.top.theta%*%t(stmbow2lda$doc.top.theta)
	g<-graph_from_adjacency_matrix(
		adjmatrix = m
		,mode = 'undirected'
		,weighted = 'ew'
		,diag = F
	)

	md<-list()
	bmod=-Inf
	for(i in 1:reps){
		cat('.')
		sg<-spinglass.community(g,spins=mx.grp,weights=E(g)$ew)
		md[[i]]<-modularity(sg,weights=E(g)$weight)
		if(md[[i]]>bmod) {
			bmod<-md[[i]]
			sgb<-sg
		}
	}
	save(sgb,file=paste(out.dir,'sgb.RData',sep=.Platform$file.sep))

	# diagnostics
	md<-unlist(md)
	pdf(paste(out.dir,'com.pdf',sep=.Platform$file.sep),w=8.5,h=11)
	par(mfrow=c(2,1),mar=rep(2,4))
	plot(md,type='l',main=paste('Modularity of ',reps,' trial',ifelse(reps==1,'','s'),sep=''))
	abline(h=mean(md),col='blue')
	points(x=which.max(md),y=max(md),col='red')
	plot(density(md),main='Modularity density')
	abline(v=max(md),col='red')
	abline(v=mean(md),col='blue')
	dev.off()

	# sort by topic association
	rk<-data.table(
		group=membership(sgb)
		,stmbow2lda$doc.top.theta
	)
	setnames(rk,sub('^V','T',names(rk)))
	cols<-2:ncol(rk)
	rm<-apply(rk[,-1],1,function(x) list(names(sort(-x)[1:2])))
	rm<-rk[,list(group,rm)]
	rm<-rm[,list(m=list(unique(unlist(rm)))),keyby=group]
	rm[,group:=as.character(group)]
	setkey(rm,group)

	rk<-rk[ ,lapply(.SD, mean), .SDcols = cols,keyby=group]
	rs<- rank(-apply(rk[,-1],1,max))
	rk[,group:=factor(group,levels=order(rs))]
	rk<-melt(data = rk,id.vars = 'group',variable.name = 'name',value.name = 'p')
	rk<-rk[,rank:=rank(-p),by=group]
	setkey(rk,group)

	for(i in as.character(1:nrow(rm))) rk[i,lb:=ifelse(name%in%rm[list(i),m][[1]],as.character(name),'')]

	setkey(rk,lb)
	ix<-rk[!'',do.call(seq,as.list(range(rank)))]
	setkey(rk,rank)
	xpl<-rk[,range(p)]
	xpl<-xpl+c(-.06*xpl[2],.06*xpl[2])
	tp<-ggplot(rk[list(ix)],aes(x=rank,y=p,label=lb)) +
		geom_line(color='gray') +
		geom_text(size=3,angle=90,color='red') +
		ggtitle('Scree plot for average topic probabilities within groups') +
		facet_wrap(~group) + theme(
			legend.position="bottom"
			#,panel.grid.minor.x = element_blank()
			#,panel.grid.major.x = element_blank()
			#,panel.grid.minor.y = element_blank()
			#,panel.grid.major.y = element_blank()
			#,strip.background = element_blank()
			#,strip.placement = "outside"
			#,strip.text.y = element_blank()
			#,axis.text.x = element_blank()
			#,axis.text.y = element_blank()
			#,axis.ticks.x = element_blank()
			#,axis.ticks.y = element_blank()
		) + expand_limits(y=xpl)
	ggsave(device = 'pdf',filename = 'screecom.pdf',path=od)

	com<-list(mem=membership(sgb),c=sgb,g=g,mod=md,r=rk,p=tp)
}
