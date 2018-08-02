#' Full text to pre-processed text.
#'
#' @param ftx
#'
#' @return
#' @export
#'
#' @examples
ftx2pre.f<-function(ftx,pick=F,pl='pl.RData',lvl='sen',frq=1){
  library(data.table)
  library(magrittr)
  library(tm)
  library(qdap)
  ftx[,`:=`(
    lem=txt %>%
      replace_abbreviation %>%
      replace_contraction %>%
      replace_ordinal %>%
      replace_number %>%
      replace_symbol %>%
      tolower %>% stemDocument(language='english') %>% gsub('[^a-z]','',.)
    ,sto=tolower(txt) %in% stopwords('english')
    ,num=grepl('[0-9]',txt)
    ,pun=!grepl('[A-Za-z0-9]',txt)
  )]
  ftx[,com:={
    w<-tolower(txt)
    t<-table(w) %>% sort %>% tail(1) %>% names
    txt[which(w==t)] %>% sort %>% `[`(1) %>% gsub('[^A-Za-z]','',.)
  },by=lem]
  ftx[com=='',com:=lem]

  # Step 3.5 Dendrogram Picker ----------------------------------------------

  if(pick){
    library(stringdist)
    m<-ftx[!(sto|pun),com %>% unique %>% sort] %>%
      grep('^[A-Z]',.,value=T) %>%
      stringdistmatrix(useNames = T,method='jw',p=.1)
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

  doc<-unique(ftx[,.(doc,com)])[,.(doc=.N),by=com] %>% setkey(com) # number of documents a word appears in
  hed<-unique(ftx[,.(doc,hed,com)])[,.(hed=.N),by=com] %>% setkey(com)
  lin<-unique(ftx[,.(doc,hed,lin,com)])[,.(lin=.N),by=com] %>% setkey(com)
  sen<-unique(ftx[,.(doc,hed,lin,sen,com)])[,.(sen=.N),by=com] %>% setkey(com)
  tdd<-doc[hed][lin][sen] %>% setkey(com)

  ftx[,cul:=(com%in%tdd[get(lvl)<=frq,com])]
  ftx[,stm:=factor(com,levels=com[!(cul|com==''|pun|sto)] %>% unique %>% sort)]
  # ~ fin ~ -----------------------------------------------------------------
  ftx
}
