#' Multi-level Corpus 2 K-Means Cluster
#'
#' @param mlc
#'
#' @return
#' @export
#' @import data.table magrittr skmeans cluster ggrepel tilit
#' @examples
mlc2kmc.f<-function(mlc){
  library(skmeans)
  library(cluster)
  library(ggrepel)

  kmc<-list()
  for(i in c('doc','par','sen')){
    sm<-stm::convertCorpus(mlc[[i]]$spm,mlc$voc,'slam')
    u<-pbapply::pbsapply(1:min(500,nrow(mlc[[i]])),function(y) {
      skmeans(x=sm,k=y,m=1.1) %>% silhouette %>% summary %>% `[[`('si.summary')
    },cl = 7 ) %>% t
    r<-data.table(u)[,i:=.I]
    r<-r[!is.infinite(Median)]
    r[Median>=quantile(Median,.98),lab:=i]
    r[,lo:=loess(Median~i) %>% predict]
    kmc[[i]]<-myth(
      ggplot(data=r,aes(x=i,y=Median)) +
        #geom_segment(aes(xend=i,y=lo,yend=Median),color='black',size=.1) +
        geom_line(aes(y=lo),color='blue',size=1.5) +
        geom_text_repel(aes(label=lab),segment.color = 'gray',nudge_y=.5,na.rm=T) +
        geom_point(size=.25) +
        geom_text_repel(data=r[which.max(lo),.(i,lo)],aes(label=i,y=lo),color='blue',nudge_y=-.01,segment.color = 'gray')
    ) + expand_limits(y=c(0,r[,max(Median)*1.05]))

  }

}
