#'Network visualizations and clustering.
lda2netviz.f<-function(stmbow2lda,thresh="choose"){
  library(igraph,quietly=T)
  library(network,quietly = T)

	bam<-stmbow2lda$doc.top.theta
	b<-quantile(bam,seq(0,1,.05))
	h<-hist(bam,breaks=b,col="black")
	abline(v=b,col=gray(0,.5))
	text(
		x=rev(h$breaks)
		,y=seq(0,max(h$density),length.out=21)
		,labels=rev(paste('|',names(b),' <= ',round(b,3),sep=''))
		,pos=4
		,offset=0
		,cex=.5
	)
	if(thresh=="choose"){
		cat("\nPlease choose an edge weight threshold by clicking on the histogram near the x-axis where you would like to cut the distribution of probabilities that a document draws words from a particular topic (i.e. theta or the document-topic probability matrix). Relationships between documents and topics that fall below this threshold will be ignored.\n")
		thresh<-locator(n=1,type="p",col="red")
		abline(v=thresh$x,col="red")
		text(
			x=thresh$x
			,y=thresh$y
			,labels=rev(paste('|',round(mean(bam<thresh$x)*100,2),'% <= ',round(thresh$x,3),sep=''))
			,pos=4
			,offset=0
			,cex=1
			,col="red"
		)
	}
	bam[bam<thresh$x]<-0
	m1am<-bam%*%t(bam)
	m1net<-network(m1am,directed=F,loops=F)
	browser()
	network.vertex.names(m1net)<-sub(paste('.*','(.+)',sep=.Platform$file.sep),'\\1',rownames(m1am))
	#	pdf('doc-by-top-net.pdf')
	plot(m1net
			 ,displaylabels=T
			 ,label=paste('T',1:nrow(m1am),sep='')
			 ,label.pos=5
			 ,label.col="white"
			 ,label.cex=.25
			 ,vertex.col="black"
			 ,vertex.cex=2
	)
	#	dev.off()

	m2am<-t(bam)%*%bam
	m2net<-network(m2am,directed=F,loops=F)

	#	pdf('top-by-doc-net.pdf')
	plot(m2net
			 ,displaylabels=T
			 ,label=paste('D',1:nrow(m2am),sep='')
			 ,label.pos=5
			 ,label.col="black"
			 ,label.cex=.75
			 ,vertex.col="white"
			 ,vertex.cex=2
	)
	#	dev.off()

	# 	b<-quantile(m1am,seq(0,1,.1))
	# 	h<-hist(m1am,breaks=b)
	# 	abline(v=b,col'pink')
	# 	h<-hist(m2am,breaks=quantile(m2am,seq(0,1,.1)))

	bel<-which(bam>0,arr.ind=T)
	w<-bam[bel]
	bel<-cbind(
		sub(paste(".+","(.+)",sep=.Platform$file.sep),"\\1",rownames(bel))
		,paste("t",bel[,2],sep="")
	)
	browser()
	o<-order(bel[,1],bel[,2])
	bel<-data.frame(bel[o,])
	w<-w[o]
	colnames(bel)<-c("document","topic")
	nm1<-length(unique(bel$document))
	nm2<-length(unique(bel$topic))
	bnet<-network(bel,bipartite=nm1,matrix.type="edgelist")
	bnet%e%"w"<-w
	pdf('bimodal-net.pdf')
	plot(
		bnet
		,displaylabels=T
		,label=c(
			paste(1:nm1)
			,network.vertex.names(bnet)[-(1:nm1)]
		)
		,label.pos=5
		,label.col=c(rep("white",nm1),rep("black",nm2))
		,label.cex=.75
		,vertex.col=c(rep("black",nm1),rep("white",nm2))
		,vertex.cex=2
		#,vertex.sides=c(rep(3,nm1),rep(20,nm2))
	)
	dev.off()


}
