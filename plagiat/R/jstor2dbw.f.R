#' Title
#'
#' @param dir
#' @param out
#' @param save
#' @param import.ngrams
#' @param sample.batches
#' @param sample.size
#' @param in.parallel
#' @param drop.nchar1
#' @param drop.freq1
#' @param drop.doc1
#' @param check.for.saved.output
#'
#' @return
#' @export
#'
#' @examples
jstor2dbw.f<-function(
	dir=stop("Choose input directory containing DFR.JSTOR.ORG batches of zip archives.")
	,out=stop("Specify output directory for your project.")
	,save=T
	,import.ngrams=F
	,sample.batches=T
	,sample.size=1
	,in.parallel=F # import batches in parallel
	,drop.nchar1=T # drop ngrams of 1 character
	,drop.freq1=T # drop ngrams that appear only once
	,drop.doc1=T # drop ngrams that appear in only one document
	,check.for.saved.output=F # will scan output directory for a 'jstor2dbw.RData' file, load it, and return it instead of running a new import
)
{
	if(check.for.saved.output) if(any(grepl('jstor2dbw.RData',dir(recursive=T,full.names=T,ignore.case=T)))) {
		warning('Loading and returning saved jstor2dbw.RData.',call.=F)
		load(dir(pattern='jstor2dbw.RData',full.names=T,recursive=T,ignore.case=F)[1])
		return(jstor2dbw)
	}

	zips<-grep('\\.zip$',list.files(dir,full.names=T,recursive=T,include.dirs=T),value=T)
	n<-length(zips)
	if(sample.batches) {
		zips<-sort(sample(x=zips,size=sample.size))
		cat("\n",sample.size," or ",round(sample.size/n*100,3)," % of batches drawn at random.\n\n",sep="")
	}

	import.dfr.jstor.f<-function(zip){
	  library(tm)
	  library(SnowballC)
	  library(data.table)

		temp <- tempdir()
		unzip(zip,exdir=temp)
		f<-list.files(temp,recursive=T,include.dirs=T,full.names=T)
		dat<-try(data.table(read.table(grep('citations.tsv',f,value=T),header=F,skip=1,sep='\t',quote='',comment.char = "")))
		if(class(dat)[1]=="try-error") return(dat)
		dat[,ncol(dat):=NULL,with=F]
		setnames(dat,as.character(read.table(grep('citations.tsv',f,value=T),header=F,nrows=1,sep='\t',quote='',as.is=T)))
		dat[,id:=sub('/','_',id)]
		setkey(dat,id)

		bow.f<-function(x){
			x<-data.table(read.csv(grep(x,f,value=T),as.is=T))
			x<-x[!removeWords(x$WORDCOUNTS,stopwords('english'))=='']
			x[,WORDCOUNTS:=tolower(WORDCOUNTS)]
			x[,WORDCOUNTS:=removePunctuation(WORDCOUNTS)]
			x[,WORDCOUNTS:=removeNumbers(WORDCOUNTS)]
			x[,WORDCOUNTS:=stemDocument(WORDCOUNTS,language='english')]
			x<-x[,list(WEIGHT=sum(WEIGHT)),by=WORDCOUNTS]
			x<-x[,c(rep(WORDCOUNTS,WEIGHT))]
			x<-table(x)
			x
		}
		browser()
		if(import.ngrams) dat[,bow:=lapply(id,function(x) try(bow.f(x)))]
		unlink(temp)
		dat
	}

	if(in.parallel){
	  library(doParallel)
		cl <- makeCluster(detectCores() )
		registerDoParallel(cl, cores = detectCores() )
		jstor2dbw <- foreach(i = zips,.packages = c('data.table','tm','SnowballC'),.inorder=F) %dopar% try(import.dfr.jstor.f(zip=i))
		stopCluster(cl)
	} else {jstor2dbw<-list();for(i in zips) jstor2dbw[[i]]<-try(import.dfr.jstor.f(zip=i))}

	jstor2dbw<-rbindlist(jstor2dbw[sapply(jstor2dbw,is.data.table)])

	# condense vocab
	if(import.ngrams){
		if(drop.nchar1) jstor2dbw[,bow:=list(lapply(bow,function(x) as.table(x[nchar(names(x))>1])))]
		if(drop.freq1) jstor2dbw[,bow:=list(lapply(bow,function(x) as.table(x[x>1])))]
		vocab<-sort(unique(unlist(lapply(jstor2dbw$bow,function(x) names(x)))))
		jstor2dbw[,bow:=list(lapply(bow,function(x) {x<-matrix(c(which(vocab%in%names(x)),as.integer(x)),byrow=TRUE,nrow=2);rownames(x)<-c('vocab.index','freq');x}))]
	}
	dfr.jour<-jstor2dbw[,.N,by=journaltitle]
	print(dfr.jour)
	if(import.ngrams) attributes(jstor2dbw)$vocab<-vocab
	if(save) save(jstor2dbw,file=paste(out,'jstor2dbw.RData',sep=.Platform$file.sep))
	jstor2dbw
}
