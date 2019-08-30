#' Easy Grid from Character
#'
#' @param x
#' @param rs
#' @param cs
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @import data.table
#'
#' @examples
eg<-function(x,rs=' *\n *',cs=','){
  lapply(x,function(y){
    y<-strsplit(y,split=rs)[[1]]
    v<-y[[1]] %>% ec(sp=cs)
    y[-1] %>% lapply(function(z) ec(z,sp=cs)%>% t %>% data.table %>% setnames(v)) %>% rbindlist
  })
}
