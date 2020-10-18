#' Database long 2 Bimodal Edge List
#'
#' @param wok2dbl
#' @param out
#' @param check.for.saved.output
#' @param saved_recode NULL to ignore, or list of character vectors each representing a single CR variation set
#' @param save_og4recode
#' @param trim_doi
#' @param capitalize
#' @param trim_anon
#' @param cut_samp_def
#' @param trim_pendants
#' @param trim_loops
#'
#' @return
#' @export
#' @import data.table magrittr
#'
#' @examples
dbl2bel.f<-function(
	wok2dbl
	,out=stop("Specify output directory for your project.")
	,check.for.saved.output=F
	,saved_recode=NULL
	,save_og4recode=F
	,trim_doi=T
	,capitalize=T
	,trim_anon=T
	,cut_samp_def=0 # if sample is based on a common citation set that should be removed for analysis
	,trim_pendants=T
	,trim_loops=T
)
{
	if(check.for.saved.output) if(any(grepl('dbl2bel.RData',dir(path=out,recursive=T,full.names=T,ignore.case=T)))) {
		warning('Loading and returning saved dbl2bel.RData.',call. = F)
		load(dir(path=out,pattern='dbl2bel.RData',recursive=T,full.names=T,ignore.case=F)[1])
		return(dbl2bel)
	}
	### check function requirements ###
	out

	### draw only citation edge information from wok2dbl ###
	wok2dbl<-data.table(wok2dbl)
	setkey(wok2dbl,field)
	dbl2bel<-wok2dbl["CR",list(id,val)]
	rm("wok2dbl")

	### impose formatting and nomenclature ###
	setnames(dbl2bel,c("ut","cr"))
	ogcr<-data.table(og=dbl2bel[,factor(cr) %>% levels])
	ogcr[,t:=og]
	doif<-function(x) sub(", DOI .+","",x) #remove DOI
	capf<-function(x) gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",x,perl=T) #capitalize https://stackoverflow.com/a/6365349/1639069
	anof<-function(x) sub("^\\[ANONYMOUS\\], ","",x,ignore.case = T)  # remove ANONYMOUS author
	if(trim_doi) {dbl2bel[,cr:=doif(cr)];ogcr[,t:=t %<>% doif]}
	if(capitalize) {dbl2bel[,cr:=capf(cr)];ogcr[,t:=t %<>% capf]}
	if(trim_anon) {dbl2bel[,cr:=anof(cr)];ogcr[,t:=t %<>% anof]}
	save(ogcr,file = paste(out,'dbl2bel-ogcr.RData',sep=.Platform$file.sep) %>% normalizePath)

	### recoding ###
	setkey(dbl2bel,cr)
	if(save_og4recode) {
		# save original codes to disk
		cat('\nSaving normalized original CR codes to pass to fuzzy set routine.')
		original.cr<-unique(dbl2bel$cr)
		save(original.cr,file=paste(out,.Platform$file.sep,'original-cr.RData',sep=""))
	}
	if(!is.null(saved_recode)) {
		# recode using fuzzy sets
		lfs<-length(saved_recode)
		cat('\nRecoding CR from',lfs,'sets.\n')
		dbl2bel[,zcr:=cr]
		for(i in 1:lfs) {
			cat('\r',round(i/lfs*100,4),'%\t\t',sep='')
			ix<-saved_recode[[i]]
			recode<-dbl2bel[ix,list(cr)][,.N,by=cr]
			recode<-recode$cr[recode$N==max(recode$N)]
			recode<-tolower(recode[which.min(nchar(recode))])
			dbl2bel[ix,zcr:=recode]
		}
		cat('\n')
	}
	### pendants ###
	if(trim_pendants) {
		dbl2bel[,pend:=!(duplicated(cr)|duplicated(cr,fromLast=T))]
		if(!is.null(saved_recode)) dbl2bel[,zpend:=!(duplicated(zcr)|duplicated(zcr,fromLast=T))]
	}

	### cut sample ###
	crd<-dbl2bel[,list(cr)][,.N,by=cr]
	setkey(crd,N)
	if(!is.null(saved_recode)) {
		setkey(dbl2bel,ut,zcr)
		dbl2bel[,zdup:=duplicated(dbl2bel)]
		zcrd<-dbl2bel[!dbl2bel$zdup,list(zcr)][,.N,by=zcr]
		setkey(zcrd,N)
		setkey(dbl2bel,cr)
	}

	### loops ### or pendants in the first partition
	if(trim_loops) {
		setkey(dbl2bel,ut)
		if(trim_pendants) {
			utloop<-dbl2bel[!dbl2bel$pend,.N,by=ut]
			setkey(utloop,N)
			utloop<-utloop[list(1),ut]
			dbl2bel[,loop:=FALSE]
			dbl2bel[utloop,loop:=TRUE]
		} else {dbl2bel[utloop,loop:=!(duplicated(ut)|duplicated(ut,fromLast=T))]}
		if(!is.null(saved_recode)){
			if(trim_pendants) {
				utloop<-dbl2bel[!(dbl2bel$zdup|dbl2bel$zpend),.N,by=ut]
			} else {
				utloop<-dbl2bel[!dbl2bel$zdup,.N,by=ut]
			}
			setkey(utloop,N)
			utloop<-utloop[list(1),ut]
			dbl2bel[,zloop:=FALSE]
			dbl2bel[utloop,zloop:=TRUE]
			rm(utloop)
		}
	}

	if(cut_samp_def>0){
		#cut highest degree citations, argument is the length of the list in descending order of frequency
		cat("\nEnter -indices separated by spaces- to reject high degree citations such as those defining the sample, or -enter- to reject none.\n")
		cut<-crd[nrow(crd):(nrow(crd)-cut_samp_def),list(cr=cr,index=1:cut_samp_def,N=N)]
		print(cut)
		u<-readLines(n=1)
		if(u!=""){
			u<-unlist(strsplit(u," "))
			u<-as.integer(u)
			if(any(is.na(u))) {stop("\nTry again.")} else {dbl2bel<-dbl2bel[!list(cut[u]$cr)]}
		}
	}

	### results ###
	print(crd)
	cat('\n')
	if(!is.null(saved_recode)) print(zcrd)
	cat('\n')

	setkey(crd,N)
	if(!is.null(saved_recode)) setkey(zcrd,N)

	res<-data.frame(case=c('Total acts of reference'
												 ,'Unique citations'
												 ,'Citations referenced once'
												 ,'Loop references'
												 ,'Total referenced twice or more'
												 ,'  Unique referenced twice or more'
	)
	,cr=c(crd[,sum(N)]
				,nrow(crd)
				,nrow(crd[list(1)])
				,dbl2bel[,sum(loop&!pend)]
				,crd[!list(1),sum(N)]
				,nrow(crd[!list(1)])
	)
	)
	if(!is.null(saved_recode)) {
		res$zcr<-c(zcrd[,sum(N)]
							 ,nrow(zcrd)
							 ,nrow(zcrd[list(1)])
							 ,dbl2bel[,sum(zloop&!zpend)]
							 ,zcrd[!list(1),sum(N)]
							 ,nrow(zcrd[!list(1)]))
		res$change<-res$zcr-res$cr
		res$perc.change<-round(res$zcr/res$cr*100,5)
		res$alt.change<-res$perc.change-100
	}
	print(res)
	cat('\n')
	mres<-data.frame(case=c(
		'Pendants as % of total references'
		,'Pendants as % of unique citations')
		,cr=round(c(res$cr[3]/res$cr[1],res$cr[3]/res$cr[2])*100,4)
	)

	if(!is.null(saved_recode)) mres$zcr<-round(c(res$zcr[3]/res$zcr[1],res$zcr[3]/res$zcr[2])*100,4)
	print(mres)
	attributes(dbl2bel)$results<-list(res,mres)
	save(dbl2bel,file=paste(out,.Platform$file.sep,"dbl2bel.RData",sep=""))
	dbl2bel
}
