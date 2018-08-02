#' Import to Full Text database
#'
#' @param imp
#'
#' @return
#' @export
#'
#' @examples
imp2ftx.f<-function(imp){
  library(data.table)
  library(magrittr)
  library(stringr)

  if(!is.data.table(imp)) imp<-imp$imp
  p<-'pag'%in%names(imp)
  ftx<-imp[,.(txt=str_split(txt,boundary('sentence')) %>% unlist),by = setdiff(names(imp), 'txt')]
  if(p) {
    ftx[,sen:=c(T,grepl('[.?!]["\' )]*$',txt[-.N])) %>% cumsum,by=doc]
  } else {
    ftx[,sen:=1:.N,by=doc]
  }
  ftx<-ftx[,.(txt=str_split(txt,boundary('word',skip_word_none = F)) %>% unlist),by = setdiff(names(ftx), 'txt')]
  ftx[,cha:=cumsum(nchar(txt))-nchar(txt)+1,by=.(doc,lin)]
  o<-intersect(c('doc','lin','cha','pag','par','sen','txt'),names(ftx))
  setcolorder(ftx,c(o,setdiff(names(ftx),o)))
  ftx<-ftx[txt!=' ']
  ftx
}
