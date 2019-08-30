#'Converts full text data to stm style document term matrix. Redundant with stm function textProcessor()
#'
#' @param source.dir A path to a folder containing a plain text file for each document.
#' @param string A character string where each element is a full document.
#' @param lt
#' @param save.to.disk
#' @param check.for.saved.output
#' @param out.dir
#' @param sample.docs
#' @param names A vector of document names. Coerced to character. If unspecified no names will be given. If unspecified and source given, files names will be assigned instead.
#'
#' @return
#' @export
#' @import stm data.table SnowballC tm
#'
#' @examples
ftxt2stmbow.f<-function(
	source.dir=NULL
	,string=NULL
	,names=NULL
	,lt=1
	,save.to.disk=F
	,check.for.saved.output=F
	,out.dir
	,sample.docs=NULL
){
	pfs<-.Platform$file.sep
	sfn<-"ftxt2stmbow.RData"
	if(check.for.saved.output) if(any(grepl(sfn,dir(recursive=T,full.names=T,ignore.case=T)))) {
		warning(paste('Loading and returning first saved',sfn),call.=F)
		l<-ls()
		load(dir(pattern=sfn,full.names=T,recursive=T,ignore.case=F)[1])
		return(get(setdiff(ls(),c(l,'l'))))
	}

	if(all(sapply(list(source.dir,string),is.null))) stop('Specify either source.dir or string.')
	if(all(!sapply(list(source.dir,string),is.null))) stop('Specify either source.dir or string but not both.')


	# 1. Preprocessing functions in base R
	#something stupid is happening between docs and string
	if(!is.null(source.dir)) {
		docs<-list() # container for docs
		files<-list.files(source.dir,full.names=T,recursive = T)
		if(!is.null(sample.docs)) files<-sample(files,sample.docs)
		files<-files[order(sub(paste('.*','(.+)',sep=pfs),'',files))] # helps later to have files in alpha order by document name
		for(i in files) docs[[i]]<-readLines(i,warn=F)
		docs<-lapply(docs,FUN=paste,collapse=' ') # each doc is one long character string
		txt<-unlist(docs)
		string<-sapply(docs,FUN=strsplit,split='\\s') # split docs into words "\\s+" is a regex. "[[:space:]]+" also works but is R specific
		if(is.null(names)) names<-sub(paste0('^.+',pfs,'(.+)\\..+'),'\\1',files)
		names(string)<-names
		docs<-string
	} else {
		docs<-string
		txt<-unlist(docs)
		string<-sapply(docs,FUN=strsplit,split='\\s') # split docs into words "\\s+" is a regex. "[[:space:]]+" also works but is R specific
		if(is.null(names)) {names(string)<-names} else {names<-names(string)}
	}

	docs<-lapply(docs,FUN=tolower) # transform to lower case
	docs<-lapply(docs,FUN=removePunctuation) # ...
	docs<-lapply(docs,FUN=removeNumbers)
	docs<-lapply(docs,FUN=removeWords,stopwords('english'))
	docs<-lapply(docs,FUN=stemDocument,language='english')

	## here we end with a list of untabulated tokenized character vectors in original document order, with blanks preserved.

	# 2. Match tokens to originals for applying stm results for qualitative cross validation.
	ftxt2stmbow<-list()
	ftxt2stmbow$map<-mapply(function(o,t) {t<-c(t,rep('',length(o)-length(t)));data.table(o,t,ord=1:length(o))},o=string,t=docs,SIMPLIFY = F) #original and tokens
	for(i in 1:length(ftxt2stmbow$map)) ftxt2stmbow$map[[i]]$o[!sapply(ftxt2stmbow$map[[i]]$o,length)]<-'' # replace character(0) with ""
	docs<-lapply(docs,FUN=function(x) x[!!nchar(x)]) #remove blanks

	# 3. Sparse matrix format expected by stm.

	ftxt2stmbow$vocab<-sort(unique(unlist(docs))) # stm expects as input a list of matrices where each element of the list is a document and where the first row of each matrix is the index of a word and the second row is the count of the word. This is a more memory efficient form since zeros are not stored.
	for(i in 1:length(docs)){
		tb<-table(docs[[i]])
		ftxt2stmbow$documents[[i]]<-rbind(vocab.index=which(ftxt2stmbow$vocab%in%names(tb)),frequency=tb)
	}

	names(ftxt2stmbow$documents)<-names
	p<-prepDocuments(
		ftxt2stmbow$documents
		,ftxt2stmbow$vocab
		,meta = data.table(names,sapply(ftxt2stmbow$map,function(x) sum(x$t!='')/sum(x$o!='')),1:length(ftxt2stmbow$documents))
		,lower.thresh=lt
	)
	setnames(p$meta,c('names','count.prop','ord'))
	setkey(p$meta,ord)
	p$meta[,ord:=1:length(ord)]
	if(length(p$docs.removed)) {
		cat('\nRemoved:',sub(paste0('.+',pfs),'',files[p$docs.removed]))
		ftxt2stmbow$map[p$docs.removed]<-NULL
	}

	ftxt2stmbow<-do.call(list,c(txt=ifelse('docs.removed'%in%names(p),list(txt[-p$docs.removed]),list(txt)),ftxt2stmbow['map'],p))

	if(save.to.disk) try(save(ftxt2stmbow,file=gsub(paste0(pfs,'+'),pfs,paste(out.dir,sfn,sep=pfs))))
	ftxt2stmbow
}
