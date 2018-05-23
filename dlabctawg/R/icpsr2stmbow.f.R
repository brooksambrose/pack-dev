#'Imports from ICPSR Congressional Record files saved as a CTAWG list, and performs conventional preprocessing to yield an stm formatted term-document "sparse matrix".
#' @param icpsr.cong A CTAWG list of 3 data frames named according to ICPSR plain text tables and containing the "GPOspeech", "SpeakerID", and "GPOdescr" substrings.
icpsr2stmbow.f<-function(icpsr,sample.size=100,wrdmin=0,wrdmax=0,lt=1,save.to.disk=F,check.for.saved.output=F,out.dir=NULL){

	sfn<-paste("icpsr2stmbow-samp",ifelse(sample.size,sample.size,'all'),".RData",sep="")
	if(check.for.saved.output) if(any(grepl(sfn,dir(recursive=T,full.names=T,ignore.case=T)))) {
		warning(paste('Loading and returning first saved',sfn),call.=F)
		l<-ls()
		load(dir(pattern=sfn,full.names=T,recursive=T,ignore.case=F)[1])
		return(get(setdiff(ls(),c(l,'l'))))
	}

	require(stm,quietly = TRUE)
	s<-1:nrow(icpsr[[grep('GPOspeech',names(icpsr))]])
	if(wrdmin) {wn<-icpsr[[grep('GPOdescr',names(icpsr))]]$word.count>=wrdmin} else {wn<-rep(T,length(s))}
	if(wrdmax) {wx<-icpsr[[grep('GPOdescr',names(icpsr))]]$word.count<=wrdmax} else {wx<-rep(T,length(s))}
	s<-s[wn&wx]
	if(sample.size) s<-sample(s,sample.size)
	GPOspeech<-grep('GPOspeech',names(icpsr))
	icpsr2stmbow<-textProcessor(
		documents=icpsr[[GPOspeech]]$speech[s]
		,lowercase=TRUE
		,removestopwords=TRUE
		,removenumbers=TRUE
		,removepunctuation=TRUE
		,stem=TRUE
		,wordLengths=c(3,Inf)
		,sparselevel=1
		,language="en"
		,verbose=TRUE
		,onlycharacter= FALSE
		,striphtml=FALSE
		,customstopwords=NULL)
	nm<-icpsr[[GPOspeech]]$speechID[s]
	if(length(icpsr2stmbow$docs.removed)) nm<-nm[-icpsr2stmbow$docs.removed]
	p<-prepDocuments(icpsr2stmbow$documents,icpsr2stmbow$vocab,meta = nm,lower.thresh = lt)
	icpsr2stmbow$documents<-p$documents
	icpsr2stmbow$vocab<-p$vocab
	names(icpsr2stmbow$documents)<-p$meta
	if(save.to.disk) try(save(icpsr2stmbow,file=paste(grep(paste(out.dir,'$',sep=''),dir(include.dirs = T,full.names=T,recursive=F,ignore.case=F),value = T)[1],sfn,sep=pfs)))
	icpsr2stmbow
}
