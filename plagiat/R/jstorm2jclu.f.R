#' JSTOR Master 2 Journal Cluster
#'
#' @param jstorm
#'
#' @return
#' @export
#' @import igraph data.table ggnetwork
#' @importFrom magrittr %>%
#'
#' @examples
jstorm2jclu.f<-function(jstorm){

  jstorm<-jstorm[title_id==title_history][sapply(discipline,length)>0]
  el<-jstorm[,.(discipline=unlist(discipline) %>% unique),by=title_history] %>% as.matrix
  g<-graph_from_edgelist(el)
  V(g)$type<-V(g)$name %in% el[,2]
  g<-bipartite_projection(g)
  cw1<-cluster_walktrap(g$proj1)
  cw2<-cluster_walktrap(g$proj2)
  jclu<-list(gt=g$proj1,title_history=cw1,gd=g$proj2,discipline=cw2)


  # label membership for disciplines with super
  k<-sapply(cw2 %>% communities,function(x) {
    if(length(x)==1) return(x)
    igraph::degree(induced_subgraph(g$proj2,V(g$proj2)$name%in%x)) %>% sort(decreasing=T) %>% head(1) %>% names
  })
  f2<-factor(k[membership(jclu$discipline)],levels = k[order(sapply(cw2 %>% communities,length),decreasing=T)])
  V(jclu$gd)$memb<-as.character(f2)

  jstormr<-jstorm[,.(discipline=discipline %>% unlist %>% unique),keyby=title_history]
  jstormr<-jstormr[data.table(th=jclu$title_history$names,m=jclu$title_history %>% membership) %>% setkey(th)]
  jstormr<-jstormr[,.N,by=.(m,discipline)] %>% setorder(m,-N)
  j<-jstormr[,.(super=discipline[1],N=sum(N)),by=m][,!'m']
  f1<-factor(j$super[membership(jclu$title_history)])
  f1<-factor(f1 %>% as.character,levels = table(f1) %>% sort(decreasing = T) %>% names)
  V(jclu$gt)$memb<-as.character(f1)

  w<-j[,which(duplicated(super)|duplicated(super,fromLast = T))]
  if(length(w)) {
    s<-jstormr[,.(super=discipline[1:2] %>% paste(collapse=' (') %>% paste0(')')),by=m][w,super]
    j[w,super:=s]
    }
  jstormr<-j
  j<-data.table(title_history=jclu$title_history$names,super=jstormr[jclu$title_history %>% membership,super]) %>% setkey(title_history)

  tab <-table(f1) %>% data.frame %>% data.table %>% setnames(ec('super,N'))
  tab[,Pct:=round(N/jstorm[,.N]*100,1)]
  og<-jstorm[,sapply(tab$super,function(x) sapply(discipline,function(y) x%in%y) %>% sum)]
  tab<-rbindlist(
    list(tab,data.table('Total',jstorm[,.N],tab[,sum(Pct)]))
  )
  tab[,Labeled:=c(og,sum(og))][,LPct:=round(Labeled/jstorm[,.N]*100,1)]

  # crossings
  E(jclu$gd)$cross<-rev(c("within","between"))[(!crossing(jclu$discipline,jclu$gd)) + 1]
  E(jclu$gt)$cross<-rev(c("within","between"))[(!crossing(jclu$title_history,jclu$gt)) + 1]


  c(jclu,ft=list(f1),fd=list(f2),tab=list(tab),super=list(j))
}
