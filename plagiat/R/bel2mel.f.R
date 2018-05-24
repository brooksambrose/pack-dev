#' Bimodal edgelist to monomodal edgelist
#'
#' Recieves a dbl2bel and outputs one or both of two mode projections in edgelist format. May be redundant with igraph::bipartite.projection.
#'
#' @param dbl2bel
#' @param subset
#' @param type
#' @param out
#' @param check.for.saved.output
#' @param write2disk
#'
#' @return
#' @export
#'
#' @examples
bel2mel.f<-function(
	dbl2bel=NULL
	,subset=NULL # a vector of UT
	,type=c("utel","crel")
	,out=stop("Specify output directory for your project.")
	,check.for.saved.output=F
	,write2disk=F
)
{
	if(check.for.saved.output) if(any(grepl('bel2mel.RData',dir(path=out,recursive=T,full.names=T,ignore.case=T)))) {
		warning('Loading and returning saved bel2mel.RData.',call. = F)
		load(dir(path=out,pattern='bel2mel.RData',recursive=T,full.names=T,ignore.case=F)[1])
		return(bel2mel)
	}
	out
	require(data.table)
	if(ncol(dbl2bel)>2) stop('dbl2bel must be a bimodal edgelist as a two column data.table with UT in first column and CR in second. Selection on pendants, use of fuzzy replacement, etc. should be made prior to passing to bel2mel.f.')

	setnames(dbl2bel,c('ut','cr'))
	setkey(dbl2bel,ut,cr)
	dup<-duplicated(dbl2bel)
	if(sum(dup)) {
		warning(paste(sum(dup),'duplicate lines detected and removed. This may make sense after fuzzy set replacement if two items in the same bibligoraphy were treated as the same reference.'),call.=FALSE,immediate.=TRUE)
		dbl2bel<-unique(dbl2bel)
	}

	cat(c('\nBipartite edge list is now',nrow(dbl2bel),'rows long.'))

	if(write2disk){
		bel2mel<-list()
		dbl2bel[,`:=`(ut=factor(ut),cr=factor(cr))]
		if('utel'%in%type){
			lut<-gzfile(paste(out,'bel2mel-ut-levs.txt.gz',sep=.Platform$file.sep),'w')
			writeLines(dbl2bel[,levels(ut)],lut)
			close(lut)

		}
		if('crel'%in%type){
			lcr<-gzfile(paste(out,'bel2mel-cr-levs.txt.gz',sep=.Platform$file.sep),'w')
			writeLines(dbl2bel[,levels(cr)],lcr)
			close(lcr)

		}

		warn<-paste('Monopartite edgelist and levels written to',out,sep='')
		warning(warn,call.=FALSE,immediate.=TRUE)
		return(warn)
	}

	bel2mel<-list()
	dbl2bel[,ut:=as.character(ut)]
	dbl2bel[,cr:=as.character(cr)]
	if('crel'%in%type){
		cat('\nBeginning CR-UT-CR edgelist.')
		setkey(dbl2bel,ut,cr)
		utd<-dbl2bel[,.N,by=ut]
		setkey(utd,N,ut)
		if(any(utd$N>1)) {
			utiso<-utd[list(1),ut]
			bel2mel$crel<-dbl2bel[!list(utiso),data.table(do.call(rbind,lapply(1:(length(cr)-1),function(y) matrix(cr[c(rep(y,length(cr)-y),(y+1):length(cr))],ncol=2) ))),by=ut]
			setnames(bel2mel$crel,old=c('V1','V2'),new=c('cr1','cr2'))
			bel2mel$crel<-bel2mel$crel[,list(ew=.N,ut=list(ut)),keyby=c('cr1','cr2')]
		} else {bel2mel$crel<-'No connected crel.'}
		cat(' Done.')
		if('utel'%in%type) {
			save(bel2mel,file=paste(out,'bel2mel.RData',sep=.Platform$file.sep))
			cat(' Saved.')
		}
	}
	if('utel'%in%type){
		cat('\nBeginning UT-CR-UT edgelist.')
		setkey(dbl2bel,cr,ut)
		crd<-dbl2bel[,.N,by=cr]
		setkey(crd,N,cr)
		if(any(crd$N>1)) {
			criso<-crd[list(1),cr]
			bel2mel$utel<-dbl2bel[!list(criso),data.table(do.call(rbind,lapply(1:(length(ut)-1),function(y) matrix(ut[c(rep(y,length(ut)-y),(y+1):length(ut))],ncol=2) ))),by=cr]
			setnames(bel2mel$utel,old=c('V1','V2'),new=c('ut1','ut2'))
			bel2mel$utel<-bel2mel$utel[,list(ew=.N,cr=list(cr)),keyby=c('ut1','ut2')]
		} else {bel2mel$utel<-'No connected utel.'}
		cat(' Done.')
	}
	save(bel2mel,file=paste(out,'bel2mel.RData',sep=.Platform$file.sep))
	cat(' Saved.\n\n')
	bel2mel
}
