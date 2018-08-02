#' Title
#'
#' @param wok2dbl
#' @param out
#' @param field
#' @param variations
#' @param recode
#'
#' @return
#' @export
#'
#' @examples
dbl2w.f<-function(
	wok2dbl
	,out=stop("Specify output directory")
	,field=stop("field=c(\"field1\",\"field2\",...)")
	,variations=NULL
	,recode=NULL
)
{
  library(data.table)
	wok2dbl<-data.table(wok2dbl)
	setnames(wok2dbl,c("ut","field","b"))
	setkey(wok2dbl,field)
	wok2dbl<-wok2dbl[field]
	setkey(wok2dbl,field)

	field<-tolower(field)
	field<-field[field!="ut"]

	l<-list()
	for(i in field){
		l[[i]]<-wok2dbl[i=toupper(i)][j=list(ut,b)];setkey(l[[i]],ut);setnames(l[[i]],2,i)
		if(i%in%c("cr","af")) l[[i]][,i:=factor(toupper(sub(", DOI .+","",l[[i]][[i]]))),with=F]
		if(i=="af") l[[i]]<-l[[i]][i=!grepl("ANONYMOUS",l[[i]][[i]])]
		l[[i]][,c(i):=type.convert(as.character(l[[i]][[i]]))]
	}
	rm(wok2dbl)

	if(is.null(variations)) {
		cat("\nvariations=list(field1=dbl2bel_sets1,field2=dbl2bel_sets2,...)")
	}
	else
	{
		pre<-sort(unlist(c(0:9,strsplit("! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \\ ] ^ _ ` { | } ~"," "),letters,LETTERS)))[1]
		for(i in tolower(names(variations))) for(j in 1:length(variations[[i]])) levels(l[[i]][[i]])[levels(l[[i]][[i]])%in%variations[[i]][[j]]]<-paste(pre,"z",toupper(i),formatC(j,width=nchar(as.character(length(variations[[i]]))),format="d",flag="0"),sep="")
	}

	if(is.null(recode)) {
		cat("\nrecode=list(\n\tfield1=list(\n\t\t\"recode1\"=c(\"code1\",\"code2\",...)\n\t\t,\"recode2\"=c(\"code1\",\"code2\",...)\n\t)\n\t,field2=...\n)")
	}
	else
	{
		for(i in tolower(names(recode))) for(j in names(recode[[i]]))  levels(l[[i]][[i]])[levels(l[[i]][[i]])%in%recode[[i]][[j]]]<-j
	}

	if("cr"%in%field) {
		npen<-l$cr[,.N,by="cr"]
		npen<-as.character(npen$cr[npen$N>1])
		l$nr<-l$cr[,.N,by="ut"]
		setnames(l$nr,c("ut","nr"))
		setkey(l$cr,"cr")
		l$nrtp<-l$cr[npen,.N,keyby="ut"]
		setnames(l$nrtp,c("ut","nrtp"))
		setkey(l$cr,"ut")
	}

	if("so"%in%field) {
		### improve later for custom coding of natural text
		rc<-data.frame(c=c("soci","[ck]ono","anth","poli"),r=c("Sociology","Economics","Anthropology","Political Science"))
		l$ds<-copy(l$so)
		setnames(l$ds,2,"ds")
		for(i in 1:nrow(rc)) l$ds[grep(rc$c[i],l$so$so,ignore.case=T),ds:=rc$r[i]]
		l$ds$ds<-factor(l$ds$ds)
	}

	### merge
	dbl2w<-copy(l[[1]])
	for(i in names(l)[-1]) dbl2w<-merge(dbl2w,l[[i]],all=T,allow.cartesian=TRUE)
	rm(l)

	### code selection effect
	if("cr"%in%field) {
		dbl2w[is.na(dbl2w$nr),nr:=0]
		dbl2w[is.na(dbl2w$nrtp),nrtp:=0]
		dbl2w[,sel:=as.integer(nrtp>0)]
		dbl2w[,rej1:=as.integer(nr==0)] #document rejected if no citations
		dbl2w[,rej2:=as.integer(nrtp==0&nr!=0)] #document rejected if no citations after selection
	}

	w<-unique(c("ut",field[field%in%c("af","cr")]))
	lvs<-lapply(as.list(1:length(w)),FUN=function(x) apply(combn(w,x),2,paste,sep=""))
	lvs[[1]]<-matrix(lvs[[1]],ncol=length(w))
	for(i in 1:length(lvs)) for(j in 1:ncol(lvs[[i]])) {
		levs<-lvs[[i]][,j]
		setkeyv(dbl2w,levs)

		samp<-list()
		for(k in levs) samp[[k]]<-!is.na(dbl2w[[k]])
		cat("Proportion missing:\n")
		print(round(sapply(samp,FUN=function(x) sum(!x))/nrow(dbl2w),3))
		samp<-do.call(cbind,samp)
		if(ncol(samp)>1) for(i in 2:ncol(samp)) samp[,1]<-samp[,1]&samp[,i]
		samp<-as.vector(samp[,1])
		tp<-!!dbl2w$sel

		nm<-paste(c("w",levs),collapse="")

		dbl2w<-dbl2w[i=samp,1/.N,keyby=c(levs),][dbl2w]
		dbl2w[(!samp)|is.na(dbl2w$V1),V1:=0]
		setnames(dbl2w,"V1",nm)

		setkeyv(dbl2w,levs)
		dbl2w<-dbl2w[i=tp&samp,1/.N,keyby=c(levs)][dbl2w]
		dbl2w[(!tp)|is.na(dbl2w$V1),V1:=0]
		setnames(dbl2w,"V1",paste(nm,"tp",sep=""))
		cat(nm,"\n")
		rep<-rbind(c=c(nrow(dbl2w[i=samp,NA,keyby=levs]),nrow(dbl2w[i=(!!dbl2w$sel)&samp,NA,keyby=levs])),w=c(sum(dbl2w[[nm]]),sum(dbl2w[[paste(nm,"tp",sep="")]])))
		colnames(rep)<-c("unsel","sel")
		print(rep)
	}
	w<-unlist(sapply(lvs,FUN=function(x) apply(x,2,FUN=function(y) paste(c("w",y),sep="",collapse=""))))
	w<-c(w,paste(w,"tp",sep=""))
	dbl2w<-dbl2w[j=order(names(dbl2w)%in%w),with=F]
	if(!is.null(out)) save(dbl2w,file=paste(out,"dbl2w.RData",sep=.Platform$file.sep))
	setkeyv(dbl2w,levs)
	dbl2w
}
