#' JSTOR Journal Master List
#'
#' @param pageurl
#' @param xp
#'
#' @return
#' @export
#'
#' @examples
master2jstorm.f<-function(pageurl,xp='//*[@id="content"]/div[1]/div/div[4]/div/table'){
  cat(pageurl,'\n',sep='')
  dt<-read_html(pageurl)
  t<-dt %>% html_nodes(xpath=xp) %>% html_table %>% `[[`(1) %>% data.table
  t[,c('start','stop'):=(tstrsplit(`Published Date`,'-',type.convert = T))]
  t[,`Published Date`:=NULL]
  setnames(t,old = 1,new = 'title')
  t
}
