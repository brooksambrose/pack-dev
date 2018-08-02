#' Takes stm style bag of words as input and gives LDA.
#'
#' @param stmbow stm formatted bag of words, as given by textProcessor command or stmbow importer.
#' @param k Number of topics.
#' @param alpha The alpha parameter must be greater than 0. Alpha < 1 assumes that each document is constructed from relatively few topics. Alpha > 1 assumes that each is constructed from many topics. If you aren't sure choose the convention: 50/k, which will also be the default if you specify nothing.
#' @param visualize.results Logical, if TRUE provides visualization tools to help interpret LDA output.
#' @param verbose Print iteration progress (very verbose!).
#' @param out.dir
#' @param sig.pri
#' @param it
#' @param check.for.saved.output
#' @param ... Other arguments to stm.
#' @param save.to.disk Save an .RData file of the object; helpful for lengthy estimations. May be buggy across environments.
stmbow2lda.f<-function(
	stmbow,out.dir
	,k=0,alpha=NULL,sig.pri=.5,it='Spectral'
	,visualize.results=F
	,verbose=F,save.to.disk=F,check.for.saved.output=F
	,...
)
{
	if(is.null(alpha)) alpha<-50/k
	if(check.for.saved.output) if(any(grepl(paste("stm-model-k",k,"-alpha",round(alpha,3),".RData",sep=""),dir(recursive=T,full.names=T,ignore.case=T)))) {
		warning(paste('Loading and returning first saved',paste("stm-model-k",k,"-alpha",round(alpha,3),".RData.",sep="")),call.=F)
		load(dir(pattern=paste("stm-model-k",k,"-alpha",round(alpha,3),".RData",sep=""),full.names=T,recursive=T,ignore.case=F)[1])
		return(stmbow2lda)
	}
	# Check package requirements and arguments
	library(stm,quietly = T)
	library(data.table)

	stmbow2lda<-list()
	stmbow2lda$model
	stmbow2lda<-selectModel(
		documents=stmbow$documents
		,vocab=stmbow$vocab
		,K=k
		,init.type = it
		,control=list(alpha=alpha)
		,sigma.prior=sig.pri
		,verbose=verbose
		,runs = 100
		,...
	)
	stmbow2lda$top.word.phi.beta<-sapply(data.frame(stmbow2lda$model$beta$logbeta),function(x) sapply(x,function(y) ifelse(is.infinite(y),.Machine$double.eps,exp(y)))) # called beta by stm, epsilon closest thing to zero the machine can represent, necessary to prevent error
	colnames(stmbow2lda$top.word.phi.beta)<-stmbow2lda$model$vocab
	stmbow2lda$doc.top.theta<-stmbow2lda$model$theta
	rownames(stmbow2lda$doc.top.theta)<-names(stmbow$documents)
	stmbow2lda$doc.length<-sapply(stmbow$documents,ncol)
	stmbow2lda$vocab<-stmbow2lda$model$vocab
	tn<-data.table(do.call(rbind,sapply(stmbow$documents, t)))
	setnames(tn,c('ix','freq'))
	setkey(tn,ix)
	tn<-tn[,list('freq'=sum(freq)),by=ix]
	stmbow2lda$term.frequency<-tn$freq
	names(stmbow2lda$term.frequency)<-stmbow$vocab[tn$ix]
	stmbow2lda$documents<-stmbow$documents
	if(save.to.disk){
		pfs<-.Platform$file.sep
		f<-paste(out.dir,paste("stm-model-k",k,"-alpha",round(alpha,3),".RData",sep=""),sep=pfs)
		save(stmbow2lda,file=f)
	}
	topicQuality(stmbow2lda$mod,stmbow$documents)
	stmbow2lda
}
