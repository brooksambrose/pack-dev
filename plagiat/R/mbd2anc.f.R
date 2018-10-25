#' Embedding 2 Anchors
#'
#' @param embed
#' @param bsi
#'
#' @return
#' @export
#' @import data.table magrittr
#'
#' @examples
mbd2anc.f<-function(mbd,bpcai,crs=1){
  D<-ncol(mbd)
  if(D>4) stop(paste0('D = ',D,'. Convex hull intractable beyond 4 dimensions.'))
  anch<-geometry::convhulln(mbd) %>% c %>% unique %>% sort
  names(anch)<-rownames(mbd)[anch]
  if(missing(bpcai)) return(list(anc=anch))

  Di<-dim(bpcai[[1]])
  if(!identical(dim(mbd),Di[1:2])) stop('Embedding and samples do not match.')
  Di<-Di[2]
  if(Di!=D) stop(paste0('D = ',D,' but Di = ',Di,'. Provide correct bootstrapped iteration.'))
  hull<-pbapply::pbapply(do.call(abind::abind,bpcai,3),3,function(x) {
    r<-geometry::convhulln(x) %>% c %>% unique %>% sort
    names(r)<-rownames(mbd)[r]
    r
    },cl=crs)
  canch<- hull %>% unlist %>% table %>% `/`(10) %>% data.table(keep.rownames = T) %>% setnames(c('i','pct'))
  canch<-data.table(canch[,i:=as.integer(i)],voc=rownames(mbd)[canch$i])

  ianch<-intersect(hull[[1]],hull[[2]])
  for(i in 3:length(hull)) ianch<-intersect(ianch,hull[[i]])
  names(ianch)<-rownames(mbd)[ianch]
  list(anc=anch,c=canch,i=ianch,k=sapply(hull,length),h=hull)
}

# ABORTED visualize how anchors cluster
# dend<-embed[canch$i,]
# rownames(dend)<-canch$voc
# dend<-hclust(dist(dend),'average')
# pdf('d/b/dendro.pdf',w=11,h=9)
# plot(ape::as.phylo(dend)
#      #, type = "unrooted"
#      , cex = 0.1,no.margin = TRUE,lwd=0)
# dev.off()
# as.dendrogram(dend)[[2]][[2]] %>% plot_dendro(width = 1280,height = 800) %>% highlight(dynamic = TRUE) %>% layout(autosize=T) %>% hide_legend
