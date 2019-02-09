#' JSTOR Clustering 2 Bimodal Unimodal Projection Illustration
#'
#' @param jclu
#' @param s
#'
#' @return
#' @export
#' @import igraph magrittr data.table
#'
#' @examples
jclu2bup.f<-function(jclu,s=300){
  par(mar=c(0,0,1,0))
  bs<-igraph::as_edgelist(jclu$b) %>% data.table %>% setnames(ec('j,l')) %>% .[sample(1:.N,s)] %>% as.matrix %>%  graph_from_edgelist
  V(bs)$type<-grepl('^[A-Z]',V(bs)$name)
  bs$name<-'journal-label'
  args<-function(x) list(x=x,main=graph_attr(x, 'name'),label.family='serif',vertex.label=NA,vertex.size=4,edge.arrow.size=0,vertex.color=V(x)$type+1,vertex.frame.color=NA,edge.color=if(E(x)$weight %>% is.null) gray(.2) else sapply(E(x)$weight,function(y) gray(level=1-(y/max(E(x)$weight)))))
  ms<-bipartite_projection(bs,remove.type = F)
  ms$proj1$name<-'journal'
  ms$proj2$name<-'label'
  docx<-'docx'%in%knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(docx) par(mfrow=c(1,3))
  do.call(plot,args(bs))
  do.call(plot,args(ms$proj1))
  do.call(plot,args(ms$proj2))
  if(docx) par(mfrow=c(1,1))
  c(list(b=bs),ms)
}
