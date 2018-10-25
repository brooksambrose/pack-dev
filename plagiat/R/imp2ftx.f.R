#' Import to Full Text database
#'
#' @param imp
#' @param bch.sz
#'
#' @return
#' @export
#' @import data.table magrittr
#'
#' @examples
imp2ftx.f<-function(imp,bch.sz=10000){
  if(!is.data.table(imp)) imp<-imp$imp
  p<-'pag'%in%names(imp)


  b<-imp[,seq(1,.N,bch.sz)]
  m<-imp[,.N]
  imp[,txt:=as.list(txt)]
  pb<-progress::progress_bar$new(total=length(b),format = ':elapsedfull [:bar] :percent :eta',clear=F) #pb<-progress::progress_bar$new(total=imp[,doc %>% unique %>% length],format = ':elapsedfull [:bar] :percent :eta',clear=F)

  cat('finding sentence boundaries\n')
  for(i in b){
    ix<-i:min(m,i+(bch.sz-1))
    imp[ix,txt:=lapply(txt,stringr::str_split,stringr::boundary('sentence'))]
    pb$tick()
  }
  imp<-imp[,.(txt=unlist(txt)),by=setdiff(names(imp), 'txt')]

  if(p) {
    imp[,sen:=c(T,grepl('[.?!]["\' )]*$',txt[-.N])) %>% cumsum,by=doc]
  } else {
    imp[,sen:=1:.N,by=doc]
  }

  cat('finding word boundaries\n')
  b<-imp[,seq(1,.N,bch.sz)]
  m<-imp[,.N]
  imp[,txt:=as.list(txt)]
  pb<-progress::progress_bar$new(total=length(b),format = ':elapsedfull [:bar] :percent :eta',clear=F) #pb<-progress::progress_bar$new(total=imp[,doc %>% unique %>% length],format = ':elapsedfull [:bar] :percent :eta',clear=F)
  for(i in b){
    ix<-i:min(m,i+(bch.sz-1))
    imp[ix,txt:=lapply(txt,stringr::str_split,stringr::boundary('word',skip_word_none = F))]
    pb$tick()
  }
  imp<-imp[,.(txt=unlist(txt)),by=setdiff(names(imp), 'txt')]

  imp[,cha:=cumsum(nchar(txt))-nchar(txt)+1,by=.(doc,lin)]
  o<-intersect(c('doc','lin','cha','pag','par','sen','txt'),names(imp))
  setcolorder(imp,c(o,setdiff(names(imp),o)))
  imp<-imp[txt!=' ']
  imp
}
