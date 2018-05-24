#' Scrape master list of journals indexed by Thompson Reuters Web of Knowledge. Subsample based on regular expression.
#'
#' @param masterurl
#' @param out
#' @param njour
#' @param npp
#' @param check.for.saved.output
#' @param save
#' @param detect.language
#' @param grep
#'
#' @return
#' @export
#'
#' @examples
master2wok.f<-function(
	masterurl='http://ip-science.thomsonreuters.com/cgi-bin/jrnlst/jlresults.cgi?PC=MASTER&mode=print'
	,out=stop("Specify output directory for your project.")
	,njour=NULL
	,npp=500
	,check.for.saved.output=F
	,save=T
	,detect.usa=T
	,grep=NULL
)
{
	if(check.for.saved.output) if(any(grepl('master2wok.RData',dir(recursive=T,full.names=T,ignore.case=T)))) {
		warning('Loading and returning saved master2wok.RData.',call.=F)
		load(dir(pattern='master2wok.RData',full.names=T,recursive=T,ignore.case=F)[1])
		return(master2wok)
	}
	require(rvest)
	require(data.table)
	if(detect.language) require(cldr)
	if(is.null(njour)) { # get total number of journals from first p
		njour<-read_html(masterurl) %>% html_node("p") %>% html_text()
		njour<- regexpr('[0-9]+',njour) %>% regmatches(x=njour) %>% as.numeric()
	}
	batches<-paste(masterurl,'&Page=',1:ceiling(njour/npp),sep = '')
	imp<-function(pageurl){
		cat(pageurl,'\n',sep='')
		dt<-read_html(pageurl)
		dt<-html_nodes(dt,xpath='//dl') %>% html_text() %>% iconv(sub='?') %>% strsplit(split='\n+') %>% .[[1]]
		b<-grep('^[0-9]+\\.',dt)
		e<-c(b[-1]-1,length(dt))
		dt<-data.table(
			ix=as.integer(sub('^([0-9]+)\\..+','\\1',dt[b]))
			,SO=sub('^[0-9]+\\. (.+)','\\1',dt[b])
			,period=factor(sub('^ ?([^0-9]*) ISSN: .+$','\\1',dt[b+1]))
			,issn=sub('^ ?[^0-9]* ISSN: ([0-9*]{4}-[0-9*]{3}[0-9xX*]).+$','\\1',dt[b+1])
			,PU=factor(sub('^ ?[^0-9]* ISSN: [0-9*]{4}-[0-9*]{3}[0-9xX*]([^,]+).+$','\\1',dt[b+1]))
			,PA=factor(sub('^ ?[^0-9]* ISSN: [0-9*]{4}-[0-9*]{3}[0-9xX*][^,]+, (.+)$','\\1',dt[b+1]))
			,db=mapply(function(beg,end) dt[(beg+2):(end)],beg=b,end=e)
		)
		dt[grep('^ISSN',dt$period),period:=NA]
	}
	master2wok<-rbindlist(lapply(batches,imp))
	if(detect.usa) {
		cat('\nFiltering on publisher location as proxy for USA origin.')
		master2wok[,usa:=as.logical(ifelse(grepl(" USA,",PA),T,F))]
	}
	if(!is.null(grep)) master2wok[,tag:=sapply(SO,tag)]
	setattr(master2wok,'date.downloaded',Sys.time())
	if(save) save(master2wok,file=paste(out,'master2wok.RData',sep=.Platform$file.sep))
	master2wok
}
