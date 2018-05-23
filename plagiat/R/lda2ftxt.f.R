#'Color coding original documents in topic highlighting.
lda2ftxt.f<-function(
	map,doc.top,top.word,lda2rel
	,intensify=T,intensity=.3
	,sample=10
	,out.dir=NULL
	,index=0,spacing=.4,ptsize=10,axes=F
	,pdf=F,mxdoc.word.prop=0
	,fname
	,top.ord
)
{
	# d<-dir(include.dirs = T,recursive=T,full.names = T)
	# out.dir<-grep(paste(out.dir,'$',sep=''),d,value=T)
	# if(!length(out.dir)) {warning('out.dir not found, saving to current working directory.');out.dir<-getwd()}

	require(colorspace)
	library(RColorBrewer)
	require(data.table)

	mx<-max(sapply(1:nrow(doc.top), function(x) max(doc.top[x,]*top.word)))
	s<-1:length(map)
	if(sample) s<-sort(sample(s,sample))
	if(index[1]) s<-index
	ret<-list()
	for(i in s){

		cat(c('\nRendering document',i))
		w<-order(doc.top[i,],decreasing=T)[1:2]
		tn<-doc.top[i,w]
		names(tn)<-paste('T',w,sep='')
		p<-round(sum(tn)*100,1)

		cat('\nCalculating document\'s topic by word probability matrix =\n(document by topic prob vector) * (global topic by word prob matrix)')
		m<-t(doc.top[i,w]*top.word[w,])

		cat('\nOriginal range of predicted document\'s topic by word probabilities:')
		print(range(m))

		if(intensify) {
			m<-lintran(m,c(0,mx),c(intensity,1))
			cat('Range of document\'s topic by word probabilities after linear amplification:')
			print(range(m))
		}

		colnames(m)<-names(tn)
		m<-data.table(
			t=rownames(m),m
			,r=apply(m,1,function(x) x[2]/sum(x))
			,w=apply(m,1,function(x) sum(x))
		)
		setkey(m,t)
		setkey(map[[i]],t)
		m<-merge(map[[i]],m,all.x=T,all.y=F)
		if(all(is.na(m[,4:7,with=F]))) {cat('Too short, skipping.');next}

		pal<-rev(brewer.pal(n = 7,name = 'RdYlGn'))
		col<-round(range(na.omit(m$r))*1000)-500
		col<-max(abs(col))+1
		col<-(500-col):(500+col)
		col<-cut(col,breaks = length(pal),label=pal,include.lowest = T) #hex(HSV(H=240+120*m$r,S=1,V=m$w))
		rd<-round(m$r*1000)
		rd[!is.na(rd)]<-rd[!is.na(rd)]-min(rd[!is.na(rd)])+2
		rd[is.na(rd)]<-1
		col<-c('gray80',as.character(col))[rd]
		m[,'col':=col]
		setkey(m,ord)
		mr<-.6

		par(mar=rep(0,4))
		plot.new()
		pw<-8
		ph<-1
		par(family='Times',ps=ptsize,fin=c(pw,ph))
		plot.window(xlim=c(0,pw),ylim=c(0,1))

		o<-as.vector(rbind(m$o,' '))
		t<-as.vector(rbind(m$t,' '))
		c<-as.vector(rbind(m$col,'pink'))
		shu<-strheight(LETTERS,units = 'inches')
		swu<-strwidth(o,units = 'inches')*1.25

		fl<-list()
		x<-list()
		sww<-swu
		ct<-0
		f<-cumsum(sww)%/%pw
		while(max(f)>0){
			ft<-f==0
			x[[length(x)+1]]<-cumsum(sww[ft])
			sww<-sww[!ft]
			fl[[length(fl)+1]]<-rep(ct,sum(ft))
			f<-cumsum(sww)%/%pw
			ct<-ct+1
		}
		ft<-f==0
		x[[length(x)+1]]<-cumsum(sww[f==0])
		fl[[length(fl)+1]]<-rep(ct,sum(ft))
		f<-unlist(fl)
		x<-unlist(sapply(x,function(x) c(0,x[-length(x)])))
		l<-max(f)*2+1.5

		lda2relc<-copy(lda2rel)
		setkey(lda2relc,Category)
		lda2relc<-lda2relc[names(tn)]
		lda2relc[,inc:=Term%in%t]

		leg<-sum(c(
			head=4
			,rows=nrow(lda2relc)/length(unique(lda2relc$Category))/length(unique(lda2relc$lambda))
			,foot=0
		))

		ph<-sum(c(lines=l,legend=leg,divider=1))

		par(mar=c(b=2.25,l=2.25,t=3.25,r=2.25),fin=c(pw,ph*spacing),yaxs='i')
		plot.window(xlim=c(0,pw),ylim=c(ph,1))
		box()
		if(axes){
			axis(1)
			axis(2)
		}
		abline(h=leg)

		#Topic Super Column headings
		text(x=seq(0,pw,length.out = 9)[c(3,7)],y=2,labels=names(tn),col=pal[c(1,length(pal))],font=2,pos=1,offset=0)
		#Rank listed
		text(x=seq(0,pw,length.out = 9)[-c(1,5,9)],y=3,labels=c('Anchor','~','Common'),font=3,pos=1,offset=0)

		loc<-data.table(expand.grid(Category=names(tn),lambda=unique(lda2relc$lambda)),col=c(2,6,3,7,4,8))
		setkey(lda2relc,Category,lambda)
		setkey(loc,Category,lambda)
		lda2relc<-merge(lda2relc,loc)
		lda2relc[,clr:=ifelse(inc,'black','gray')]

		# List of terms
		text(x=seq(0,pw,length.out = 9)[lda2relc$col],y=lda2relc$ord+3,labels=lda2relc$Term,col = lda2relc$clr,pos=1,offset=0)
		#Original text
		text(x=x,y=f*2+leg+1,labels=o,pos=4,offset=0,cex=1,col='black',ps=11)
		#Tokenized text
		text(x=x,y=f*2+leg+1.5,labels=t,pos=4,offset=0,cex=1,font=3,col=c)
		#Line numbers
		text(x=0,y=unique(f)*2+leg+1.25,labels=unique(f)+1,pos=4,offset=-0.5,cex=.75,col='lightgray',ps=11,srt=90)

		if(missing(top.ord)) {to<-''} else {to<-paste0('-',top.ord[as.numeric(sub('T','',names(tn)))])}

		title(main=paste0('Green ',names(tn)[1],to[1],' ~ ',ifelse(missing(fname),'Document',fname[i]),'-',sprintf('%03d',i),' ~ ','Red ',names(tn)[2],to[2],'\n',round(tn*100,2)[1],' + ',round(tn*100,2)[2],' = ',round(sum(tn)*100,2),' %',sep=''))

		if(pdf) {
			try(dev.copy2pdf(
				file=paste(out.dir,paste0(sprintf('%02d',round(tn[1]*100)),'-',ifelse(missing(fname),'doc',fname[i]),'-',sprintf('%03d',i),'.pdf'),sep=.Platform$file.sep)
				, width=pw, height=ph*spacing),silent = T)
			dev.off(dev.prev())
		}
		dev.off()
		cat('Range of ',ifelse(intensify,'amplified ',''),'probabilites for ',names(m)[[4]],sep='')
		print(range(na.omit(m[[4]])))
		cat('Range of ',ifelse(intensify,'amplified ',''),'probabilites for ',names(m)[[5]],sep='')
		print(range(na.omit(m[[5]])))

	}

	m
}
