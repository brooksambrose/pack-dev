#' Web of Knowledge Database Long (wok2dbl) 2 Merged Database Long
#'
#' @param wok2dbl
#' @param fld
#'
#' @return
#' @export
#' @import data.table magrittr
#'
#' @examples
wok2mrg.f<-function(wok2dbl,fld=c('CR','PY')){
  setkey(wok2dbl,field)
  merge(wok2dbl[fld[1],.(id,val)],wok2dbl[fld[2],.(id,val)],by='id') %>% setnames(c('id',fld) %>% tolower)
}
