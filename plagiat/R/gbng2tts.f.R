#' google books ngram 2 token time series
#'
#' @param out hot stuff
#' @param cfso
#' @param query A character vector of terms or a long format database where the last column is the term and leading columns are factors. Factors will be returned on the result.
#' @param ys
#' @param slug
#' @param sm
#' @param ye
#'
#' @return
#' @export
#'
#' @examples
gbng2tts.f <- function(
  query=c('social','cultural')
  ,out='~/mat/dp'
  ,ys=1901
  ,ye=2000
  ,cfso=T
  ,slug='auto'
  ,sm=0
){
  require(magrittr)
  require(data.table)
  require(ngramr)
  query<-data.table(query)
  pfs<-.Platform$file.sep
  n<-dim(query) %>% as.list()
  names(n)<-c('r','c')
  if(slug=='auto') {
    if(n$c==1) {
      slug<-query[,n$c,with=F] %>% gsub(pattern='[^A-Za-z ]',replacement='') %>% strsplit(split=' +')
    } else {
      slug<-query[,!n$c,with=F]
    }
    slug<-unlist(slug) %>% substr(0,3) %>% unique() %>% paste0(collapse='-')
  }
  fso<-sub(paste0(pfs,'+'),pfs,paste(out,pfs,'gbng',ys,ye,'-',slug,'.RData',sep=''))
  if(cfso) {
    if(!file.exists(fso)) {cat('\nNo saved ngramr output.');return(NULL)}
    cat('Loading \"',fso,'\"\n',sep='')
    load(fso)
  } else {
    qu<-unique(query[[n$c]])
    lq<-length(qu)
    b<-ceiling(lq/12) # 12 term search limit
    if(b>1) {p<-split(qu,f=cut(1:lq,b,include.lowest=T))} else {p<-list(qu)}
    cat('Querying Google Ngrams in',length(p),ifelse(length(p)==1,'batch.\n','batches.\n'))
    tts<-lapply(p,function(x) data.table(ngram(phrases=x,corpus = 'eng_us_2012',year_start=ys,year_end=ye,smoothing=sm,count=T))) %>% rbindlist()
    tts<-merge(x=tts,y=query,by.x='Phrase',by.y=names(query)[n$c])
    setattr(tts,'date.queried',Sys.Date())
		cat('%Saving \"',fso,'\"\n',sep='')
		save(tts,file=fso)
	}
	tts
}
