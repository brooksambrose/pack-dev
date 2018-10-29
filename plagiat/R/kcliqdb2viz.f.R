#depricated
kcliqdb2viz.f<-function(
	cos2kcliqdb
	,mel2comps.dir=stop('Specify a mel2comps directory that includes cos output.')
	,out=stop('Specify output directory.')
	,type=c('crel','utel')
)
{
  library(igraph)
	mel2comps.dir
	ret<-list()
	for(i in type) if(i%in%dir(mel2comps.dir)){
		t0<-proc.time()
		p<-paste(mel2comps.dir,i,sep=.Platform$file.sep)
		olevs<-readLines(list.files(p,pattern='levs',full.names=T))
		els<-list.files(p,recursive=T,full.names=T,pattern='mel2comps.txt$')
		els<-as.matrix(do.call(rbind,lapply(els,function(x) read.delim(x,header=F,quote='',fill=F,colClasses='character'))))
		ret[[i]]$g<-graph.edgelist(els,F)
		key<-data.table(gph=1:vcount(ret[[i]]$g),src=as.integer(V(ret[[i]]$g)$name))
		setkey(key,src)

		# plot strict interpretation with terrain colors
		cos2kcliqdb[[i]]$strict<-lapply(cos2kcliqdb[[i]]$strict,function(x) key[list(x),gph])
		pdf(paste(out,paste('kcliqdb2vis',i,'strict.pdf',sep='-'),sep=.Platform$file.sep))
		cols<-as.integer(sub('^k([0-9]+).*$','\\1',names(cos2kcliqdb[[i]]$strict)))-2
		mark.col<-terrain.colors(max(cols))[cols]
		mark.border<-'white'
		plot(
			ret[[i]]$g
			# 			,mark.groups=tail(cos2kcliqdb[[i]]$strict,1)
			# 			,mark.expand=5
			#			,mark.col=mark.col
			#			,mark.border=mark.border
			,vertex.size=0
			,vertex.shape='square'
			,vertex.label=NA
			,vertex.color=gray(0,.1)
			,vertex.frame.color=NA
			#			,vertex.label.cex=0
			,edge.color=gray(0,.1)
			#,vertex.color=c('white','gray')[(1:length(V(gut4_1g))%in%unique(unlist(hulls1[samp1])))+1]
			,main="Strict"
			,layout=ret[[i]]$lout
		)
		dev.off()
		t1<-proc.time()
		t1-t0

		## Plot kcoms as nodes in hierarchical tree

		trg<-list()
		for(j in length(cos2kcliqdb[[i]]$orig):2) trg[[j]]<-graph.incidence(sapply(cos2kcliqdb[[i]]$orig[[j]],function(x) sapply(cos2kcliqdb[[i]]$orig[[j-1]],function(y) sum(x%in%y))),mode='in',weighted=T,directed=T)
		trg<-do.call(graph.union,trg)
		V(trg)$k<-as.integer(sub('^k([0-9]+).+$','\\1',V(trg)$name))
		V(trg)$layer<-max(V(trg)$k)-V(trg)$k+1
		tc<-as.integer((log(V(trg)$k-2)+.1)*10)
		V(trg)$terrain<-terrain.colors(round(max(tc)+.1*max(tc)),alpha=1)[tc]
		trg$layout<-layout.sugiyama(trg,hgap=15,layers=V(trg)$layer,maxiter=10000)$layout

		pdf(paste(out,paste('kcliqdb2vis',i,'sugiyama.pdf',sep='-'),sep=.Platform$file.sep))
		plot(trg
				 ,layout=trg$layout
				 ,vertex.label=NA
				 ,vertex.size=4
				 ,vertex.shape='square'
				 ,vertex.frame.color=NA # 'white'  #gray(0,.05)
				 ,vertex.color=V(trg)$terrain
				 ,edge.arrow.mode='-'
				 ,edge.width=.1
				 ,edge.color=gray(.1,1)
		)
		dev.off()
	}
}
