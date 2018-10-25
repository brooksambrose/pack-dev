#' Full text to pre-processed text.
#'
#' @param ftx
#' @param pick
#' @param pl
#' @param lvl
#' @param frq
#' @param bch.sz
#' @param nchr
#'
#' @return
#' @export
#' @import data.table magrittr tilit
#'
#' @examples
ftx2pre.f<-function(ftx,pick=F,pl='d/q/pl.RData',lvl='doc',frq=2,nchr=2,bch.sz=10000){


  # split merged words ------------------------------------------------------

  # consider adding routine
  # https://stackoverflow.com/a/481773/1639069
  # https://blog.rstudio.com/2018/03/26/reticulate-r-interface-to-python/

  # lemmatize ---------------------------------------------------------------


  b<-ftx[,seq(1,.N,bch.sz)]
  m<-ftx[,.N]
  cat('lemmatizing\n')
  pb<-progress::progress_bar$new(total=length(b),format = ':elapsedfull [:bar] :percent :eta',clear=F)
  for(i in b) {
    ix<-i:min(m,i+(bch.sz-1))
    ftx[ix,`:=`(
      san=txt %>%
        qdap::replace_abbreviation(.) %>%
        qdap::replace_contraction(.,sent.cap = F) %>%
        qdap::replace_ordinal(.) %>%
        qdap::replace_number(.) %>%
        qdap::replace_symbol(.) %>% gsub('[^A-Za-z]','',.)
    )]
    ftx[ix,`:=`(
      lem=san %>% tolower %>% tm::stemDocument(language='english')
      ,sto=tolower(txt) %in% tm::stopwords('english')
      ,num=grepl('[0-9]',txt)
      ,pun=!grepl('[A-Za-z0-9]',txt)
      ,nch=nchar(txt %>% gsub('[^0-9A-Za-z]','',.))<nchr
    )]
    pb$tick()
  }

  # most common replacement -------------------------------------------------
  cat('replacing lemma with most common variant')
  ftx[,com:={
    w<-tolower(lem)
    txt %<>% gsub('[^0-9A-Za-z]','',.)
    if(!all(nchar(txt %>% unique)<nchr)) if(any(nchar(txt %>% unique)<nchr)) w<-w[!nchar(txt)<nchr]
    t<-table(w) %>% sort %>% tail(1) %>% names
    san[which(w==t)] %>% sort %>% `[`(1)
  },by=lem]
  ftx[com=='',com:=lem]
  ftx[,san:=NULL]

  # probably not a word --------------------------------------------------------------
  cat('\nfiltering nonwords')
  ms<-ftx[!(num|pun|sto|nch),.N,by=.(com=tolower(com))]
  ms<-data.table(ms,dct=tolower(ms$com)%in%c('',qdapDictionaries::GradyAugmented),dl=sapply(ms$com,function(x) length(table(strsplit(x,'')[[1]])))/nchar(ms$com))
  ms[,nch:=nchar(com)]
  gs<-ms[,.(com=list(com)),by=.(dl)]
  setorder(gs,dl)
  gs[,g:=tilit::ov2chpt.f(dl,min.period = 10,drv = 1)$g]
  ms<-ms[,.(com=list(com),N=as.double(median(N)),dct=mean(dct)),by=.(dl,nch)]
  ms<-merge(ms,gs[,.(dl,g)],by='dl')
  # ms[,x:=.I]
  # plt(myth(ggplot(ms,aes(x=x,y=dl,color=g,shape='.',size=dct,label=com))+geom_point()))
  # plt(myth(ggplot(ms,aes(x=nch,y=dl,size=dct,shape=N>1,color=g,label=com))+geom_point()))
  setkey(ms,g)
  if(length(levels(ms$g))>1) ftx[,pnw:=tolower(com)%in%ms['1',unlist(com)]] else ftx[,pnw:=F]


  # Step 3.5 Dendrogram Picker ----------------------------------------------

  if(pick){
    library(stringdist)
    m<-ftx[!(sto|pun),com %>% unique %>% sort] %>%
      grep('^[A-Z]',.,value=T) %>%
      stringdist::stringdistmatrix(useNames = T,method='jw',p=.1)
    h<-hclust(m) %>% as.dendrogram()
    library(plagiat)
    library(tilit)
    if(file.exists(pl)) {load(pl)} else {f<-pl;pl<-strdist.dend.picker(h,instruct = T);save(pl,file=f)}

    # update the lemma and common terms with new sets

    for(i in pl) {
      ftx[txt%in%i,`:=`(
        lem={j<-table(txt);names(j)[which.max(j)] %>% tolower}
        ,com={j<-table(txt);names(j)[which.max(j)]}
      )]
    }
    ftx[,pck:=txt%in%unlist(pl)]
  }

  # Step 3.6 Pre-process to Cull --------------------------------------------
  cat('\nculling')
  doc<-unique(ftx[,.(doc,com)])[,.(doc=.N),by=com] %>% setkey(com) # number of documents a word appears in
  par<-unique(ftx[,.(doc,par,com)])[,.(par=.N),by=com] %>% setkey(com)
  lin<-unique(ftx[,.(doc,par,lin,com)])[,.(lin=.N),by=com] %>% setkey(com)
  sen<-unique(ftx[,.(doc,par,lin,sen,com)])[,.(sen=.N),by=com] %>% setkey(com)
  tdd<-doc[par][lin][sen] %>% setkey(com)

  #sf<-tdd[-1,.(.N,list(com)),by=doc] %>% setorder(doc)
  #plt(myth(ggplot(sf,aes(x=doc,y=N,label=V2)) + geom_point(size=.5)) + scale_x_log10()+ scale_y_log10())

  ftx[,cul:=(com%in%tdd[get(lvl)<=frq,com])]
  ftx[!(nch|cul|com==''|pun|sto),stm:=factor(com)]

  # ~ fin ~ -----------------------------------------------------------------
  ftx
}
