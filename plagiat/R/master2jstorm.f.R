#' JSTOR Journal Master List
#'
#' @param downloadurl
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @import data.table igraph
#'
#' @examples
master2jstorm.f<-function(downloadurl='http://www.jstor.org/kbart/collections/all-archive-titles?contentType=journals'){
  jstorm<-fread(paste('wget -qO-',downloadurl),fill=T,quote='') # unknown error in fread https://github.com/Rdatatable/data.table/issues/2859
  jstorm[,`:=`(fc=full_coverage %>% stringr::str_extract_all('[0-9]{4}') %>% lapply(function(x) {
    x %<>% unlist %>% as.integer %>% range
    do.call(seq,as.list(x))
  }))]
  jstorm<-jstorm[,lapply(.SD,function(x) list(unique(x))),.SDcols=colnames(jstorm),by=title_id]
  jstorm[,title_id:=NULL]
  setcolorder(jstorm,c('title_id',setdiff(colnames(jstorm),'title_id')))
  for(i in colnames(jstorm)) if(all(jstorm[,sapply(get(i),length)==1])) jstorm[,(i):=unlist(get(i))]

  jstorm[,`:=`(fc=lapply(fc,function(x) unlist(x) %>% unique %>% sort)),by=title_id]

  jstorm[,discipline:=list(strsplit(discipline,' ; '))]
  jstorm[,collection:=list(strsplit(collection,' ; '))]
  jstorm[,catalog_identifier_oclc:=list(strsplit(catalog_identifier_oclc,', ') %>% lapply(lapply,FUN = as.numeric))]
  jstorm[,catalog_identifier_lccn:=list(strsplit(catalog_identifier_lccn,', '))]

  # error in jstor database
  jstorm[!preceding_publication_title_id%in%title_id,preceding_publication_title_id:='']

  g<-graph_from_edgelist(jstorm[preceding_publication_title_id!='',.(s=preceding_publication_title_id,r=title_id)] %>% as.matrix)
  iso<-setdiff(jstorm$title_id,V(g)$name)
  g<-add_vertices(g,length(iso),name=iso)
  gc<-components(g)
  gc<-data.table(title_id=V(g)$name,title_history=membership(gc),key='title_id')
  setkey(jstorm,title_id)
  jstorm<-merge(jstorm,gc)
  th<-jstorm[,.(title_current=title_id[sapply(fc,max) %>% which.max]),keyby=title_history] %>% setnames('title_history','th')
  jstorm[,title_history:=th[title_history,title_current]]
  jstorm
  }
