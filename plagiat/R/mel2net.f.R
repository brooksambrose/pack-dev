mel2net.f <- function(
  bel2mel
  ,count=T
  ,rcount=T
  ,out=NULL
)
{
  cat("mel2net.f aka Plagiat!","Written by Brooks Ambrose\n",sep="\n")
  if(!count&rcount) warning("\nrcount iff count=T",call.=F)
  library(network)
  mel2net<-list()
  for(i in names(bel2mel)){
    if(is.na(bel2mel[[i]])) {mel2net[[i]]<-NA;next}
    bel2mel[[i]]<-bel2mel[[i]][order(bel2mel[[i]][,1],bel2mel[[i]][,2]),]
    mel2net[[i]]<-network(bel2mel[[i]][,1:2],matrix.type="edgelist",directed=F)
    mel2net[[i]]%e%"ew"<-bel2mel[[i]]$ew
    if(count) if("count"%in%names(attributes(bel2mel[[i]]))) {
      attributes(mel2net[[i]])$count<-attributes(bel2mel[[i]])$count
      if(rcount){

      }
    }

  }
  names(mel2net)<-sub("el$","",names(mel2net))
  if(!is.null(out)) save(mel2net,file=paste(out,"mel2net.RData",sep=.Platform$file.sep))
  mel2net
}
