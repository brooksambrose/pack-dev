#' Zotero 2 Bibtex
#'
#' @param users
#' @param collections
#' @param bbt
#' @param verbose
#' @param key
#'
#' @importFrom magrittr %>%
#' @return
#' @export
#' @import data.table magrittr
#' @examples
zot2bib.f<-function(users,collections,key,bbt=T,verbose=T){
  a<-paste0(
    'https://www.zotero.org/api/users/'
    ,users
    ,'/collections/'
    ,collections
    ,'/items/top?limit=100&format=json&key='
    ,key
    ,'&v=3'
  )
  r<-list()
  for(u in a){
    n<-u
    x<-httr::GET(u)
    u<-x$headers$link %>% strsplit(',') %>% unlist %>% `[`(1:2) %>% sub('.*<([^>]+).+','\\1',.)
    s<-u %>% sub('.+start=([0-9]+).+','\\1',.) %>% as.integer
    s<-seq(s[1],s[2],s[1])
    u<-paste0(
      u[1] %>% sub('(.+start=)([0-9]+)(.+)','\\1',.)
      ,s
      ,u[1] %>% sub('(.+start=)([0-9]+)(.+)','\\3',.)
      ,'&format=json&key='
      ,key
      ,'&v=3'
    )
    if(verbose) {
      cat('Downloading',length(u)*100,'records\n')
      pb<-progress::progress_bar$new(format = "  [:bar] :elapsed :eta",total = length(s)+1, clear = FALSE, width= 60)
    }

    cols<-c('volume','issue','date')
    dcols<-c('accessDate','dateAdded','dateModified')
    be<-c('bp','ep')

    rom<-function(x) suppressWarnings(ifelse(
      x %>% as.numeric %>% is.na
      ,x %>% as.roman %>% as.numeric %>% `*`(-1) %>% `+`(1)
      ,x %>% as.numeric
    ))

    d<-list()
    i=n
    d[[i]]<-suppressWarnings(rjson::fromJSON(httr::content(x,type='text',encoding = 'UTF-8')) %>% lapply(function(y) y$data %>% as.data.table)) %>% rbindlist(fill=T)
    d[[i]][, (cols) := lapply(.SD, as.integer), .SDcols = cols]
    d[[i]][, (dcols) := lapply(.SD, lubridate::as_datetime), .SDcols = dcols]
    d[[i]][,(be):=tstrsplit(pages,split='-',type.convert = T)][,(be):=lapply(.SD,rom),.SDcols=be]
    w<-d[[i]][,(is.na(bp)|is.na(ep))&pages!='']
    if(any(w)) d[[i]][w,(be):=tstrsplit(pages %>% gsub('[^0-9-]','',.),split='-',type.convert = T)]

    if(verbose) pb$tick()
    for(i in u){
      d[[i]]<-suppressWarnings(rjson::fromJSON(file=i) %>% lapply(function(y) y$data %>% as.data.table)) %>% rbindlist(fill=T)
      d[[i]][, (cols) := lapply(.SD, as.integer), .SDcols = cols]
      d[[i]][, (dcols) := lapply(.SD, lubridate::as_datetime), .SDcols = dcols]
      d[[i]][,(be):=tstrsplit(pages,split='-',type.convert = T)][,(be):=lapply(.SD,rom),.SDcols=be]
      w<-d[[i]][,(is.na(bp)|is.na(ep))&pages!='']
      if(any(w)) d[[i]][w,(be):=tstrsplit(pages %>% gsub('[^0-9-]','',.),split='-',type.convert = T)]
      if(verbose) pb$tick()
    }
    r[[length(r)+1]]<-rbindlist(d,fill=T)
  }
  r
}
