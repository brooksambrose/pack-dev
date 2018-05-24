#' Title
#'
#' @param bel2mel
#' @param out
#'
#' @return
#' @export
#'
#' @examples
mel2cfinder.f<-function(
	bel2mel
	,out=stop("Specify output directory for your project.")
)
{
	library(data.table)
	if('crel'%in%names(bel2mel)) {
		bel2mel$crel[,ut:=NULL]
		setkey(bel2mel$crel,cr1,cr2)
		lcr<-sort(unique(unlist(bel2mel$crel[,list(cr1,cr2)])))
		bel2mel$crel[,`:=`(cr1=as.integer(factor(cr1,levels=lcr)),cr2=as.integer(factor(cr2,levels=lcr)))]
		clcr<-gzfile(paste(out,'bel2mel-crlevs.txt.gz',sep=.Platform$file.sep),'w')
		writeLines(lcr,con=clcr)
		close(clcr)
		ccrel<-gzfile(paste(out,'bel2mel-crel.txt.gz',sep=.Platform$file.sep),'w')
		write.table(bel2mel$crel,file=ccrel,quote=FALSE,sep='\t',na='',row.names=FALSE,col.names=FALSE)
		close(ccrel)
	}
	if('utel'%in%names(bel2mel)) {
		bel2mel$utel[,cr:=NULL]
		setkey(bel2mel$utel,ut1,ut2)
		lut<-sort(unique(unlist(bel2mel$utel[,list(ut1,ut2)])))
		bel2mel$utel[,`:=`(ut1=as.integer(factor(ut1,levels=lut)),ut2=as.integer(factor(ut2,levels=lut)))]
		clut<-gzfile(paste(out,'bel2mel-utlevs.txt.gz',sep=.Platform$file.sep),'w')
		writeLines(lut,con=clut)
		close(clut)
		cutel<-gzfile(paste(out,'bel2mel-utel.txt.gz',sep=.Platform$file.sep),'w')
		write.table(bel2mel$utel,file=cutel,quote=FALSE,sep='\t',na='',row.names=FALSE,col.names=FALSE)
		close(cutel)
	}
}
