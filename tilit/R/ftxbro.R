#' Full text browser
#'
#' @param ftx
#'
#' @return
#' @export
#'
#' @examples
ftxbro<-function(ftx){
  if(nrow(ftx)>1) stop('Pass one location at a time.')
  if('pag'%in%names(ftx)) {
    getOption('viewer')(ftx[,paste0(doc,'#page=',pag+1)])
    x<-ftx[,pdftools::pdf_text(doc)[lin]]
  } else {
    x<-do.call(scan,ftx[1,.(what="character",file=doc,nmax=1,skip=lin-1,sep='\n')])
  }
  x<-c(
    stringr::str_sub(x,end=ftx[1,cha-1])
    ,stringr::str_sub(x,start=ftx[1,cha])
  )
  cat(x,sep='\n\t')
  invisible(x)
}
#    getOption('viewer')(jpdf %>% paste0('#page=3'))
#ftx[10] %>% ftxbro
