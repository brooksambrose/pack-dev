#' K-Clique Community Tree 2 Interactive Graph
#'
#' @param kcc2tree
#' @param hcs
#' @param kcc
#' @param k
#' @param c1
#' @param c2
#' @param light
#' @param dark
#' @param ew
#' @param plot
#' @param root
#' @param limit
#' @param vpal
#' @param kcol
#' @param transform
#' @param dst
#'
#' @return
#' @export
#' @import data.table magrittr igraph networkD3 tilit
#'
#' @examples
tree2igr.f<-function(kcc2tree,hcs,k,c1,c2,kcc,light=25,dark=75,ew,plot=F,root=F,limit=100,vpal='C',kcol=T,transform=F,dst=.75) {
  if(missing(kcc)) kcc<-paste0('k',k,'.k',k,'c',c1,'-',c2)
  if(root) kcc<-kcc2tree$gph %>% dfs(root=kcc,neimode='out',unreachable=F,order=T) %>% .$order %>% unclass %>% na.omit %>% names %>% grep('[^r]$',.,value=T)
  if(missing(ew)) ew<-kcc2tree$mel[,min(weight)]
  mel<-kcc2tree$mel[
    ,col:=colorRampPalette(c('lightgray','black'))(100)[round(lintran(weight,c(light,dark)))]][
      kcc,on='g'][
        ,vcol:=sub('k([0-9]+).+','\\1',g) %>% as.numeric %>% {if(transform) log(.) else .} %>% lintran(c(1,100)) %>%  round %>% {viridis::viridis(n=100,option=vpal,direction=-1)[.]} %>% desat(dst)][
          weight>=ew]
  mel[is.na(col),col:=gray(.5)][is.na(vcol),vcol:=gray(.5)]
  gph<-graph_from_data_frame(mel[,.(from,to,weight)],directed=F,vertices=hcs[mel[,c(from,to) %>% unique %>% sort],on=names(hcs)[1]])
  go<-gorder(gph)
  if(go>limit) stop(paste('Graph size of',go,'exceeds limit of',limit))
  n<-igraph_to_networkD3(gph,group = V(gph)$ml)
  n$nodes$size<-V(gph)$N
  n$links$value<-1
  cat('edge ')
  print(mel[,table(weight)])

  igr<-forceNetwork(
    Links = n$links
    ,Nodes = n$nodes
    ,Source = 'source'
    ,Target = 'target'
    ,Value = 'value'
    ,Group = 'group'
    ,NodeID = 'name'
    ,Nodesize = 'size'
    ,radiusCalculation = JS(" Math.sqrt(d.nodesize/Math.PI)+3")
    ,zoom = T
    ,legend=T
    ,opacity=1
    ,opacityNoHover = 0
    ,linkColour =  if(kcol) mel$vcol else mel$col
    ,linkWidth = JS("function(d) { return .5; }")
    # ,linkDistance = 15 # sets the length of each edge
    #,charge=-50 #the more negative the number, the more each node repels one another
    ,bounded = F
    ,fontSize = 12
    # ,height = 600
    # ,width = 800
  )
  kcc2tree$kdb[kcc,on='name',cat('\n',txt,sep='')]
  if(plot) return(igr)
}
