#' Monopartite Edgelist to Hierarchical Community Structure
#'
#' @param bel2mel
#' @param wok2dbl
#' @param type
#'
#' @return
#' @export
#' @import data.table magrittr igraph
#'
#' @examples
mel2hcs.f<-function(bel2mel,wok2dbl,type=c('crel','utel')[1]){
  mel<-copy(bel2mel[[type]]) %>% setnames(ec('from,to,weight,id')) %>% setkey(from,to)
  gph<-graph_from_data_frame(mel,directed=F)
  rnorm(1)
  seed<-sample(.Random.seed,1)
  hcs<-cluster_louvain(gph)

  hcs<-data.table(cr=hcs$names,ml=hcs$memberships[1,],mh=hcs$memberships[nrow(hcs$memberships),]) # cr membership table
  setkey(wok2dbl,field)
  hcs<-merge(wok2dbl['CR',.N,by=val],hcs,by.x='val',by.y='cr') %>% setnames('val','cr') # membership table with citation counts!
  hcs[,ml:=factor(ml,levels = table(ml) %>% sort(decreasing = T) %>% names) %>% as.integer
      ][,mh:=factor(mh,levels = table(mh) %>% sort(decreasing = T) %>% names)%>% as.integer]
  attr(hcs,'seed')<-seed
  hcs
}
