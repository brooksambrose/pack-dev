thatgirlis.f <- function(
  n
  ,ew="ew"
  ,s=1000
  ,plot=F
)
{
  require(network)
  ns<-network.size(n)
  edist<-table(n%e%ew)
  z<-(ns*(ns-1)/2)-sum(edist)
  o<-names(edist)
  edist<-c(z,edist)
  names(edist)<-c("0",o)
  edist<-as.table(edist)
  print(cbind(Freq=edist,Prop=round(edist/sum(edist),digits=4)))
  cat("Tot:",sum(edist),"\n\n")

  perm<-list()
  maxcombo<-ns*(ns-1)/2
  combos<-1:maxcombo
  choices<-sum(n%e%ew)
  t1<-Sys.time()
  for(i in 1:s){
    cat("\r",i,"\t",sep="")
    rdist<-sample(combos,size=choices,replace=T)
    rdist<-table(rdist)
    z<-maxcombo-length(rdist)
    rdist<-table(rdist)
    o<-names(rdist)
    rdist<-c(z,rdist)
    names(rdist)<-c("0",o)
    rdist<-as.table(rdist)
    perm[[i]]<-rdist
  }
  t2<-Sys.time()
  cat(":",round((t2-t1)/60,2),"minutes to permute\n")
  maxcount<-max(sapply(perm,length))
  for(i in 1:length(perm)) perm[[i]]<-c(perm[[i]],rep(0,maxcount-length(perm[[i]])))
  perm<-do.call(rbind,perm)
  colnames(perm)<-0:(maxcount-1)
  if(dim(perm)[2]>4) {perm<-cbind(perm,apply(perm[,4:dim(perm)[2]],1,sum));colnames(perm)[dim(perm)[2]]<-">=3"}

  if(plot) for(i in which(edist>0)) {hist(perm[,i],breaks=(floor(min(perm[,i]))-.5):(ceiling(max(perm[,i]))+.5),freq=F,main=paste("Count:",i),xlab="",xlim=range(c(perm[,i],edist[i])));abline(v=edist[i],lty=2)}

  edist<-c(edist,rep(0,maxcount-length(edist)))
  names(edist)<-0:(maxcount-1)
  if(length(edist)>4) {edist<-c(edist,sum(edist[4:length(edist)]));names(edist)[length(edist)]<-">=3"}
  flush.console()
  edist<-cbind(observed=edist,expected=round(apply(perm,2,mean),1),sd=round(apply(perm,2,sd),3),t=round((edist-apply(perm,2,mean))/apply(perm,2,sd),3),"p o<=e"=round(apply(perm<=edist,2,mean),4),"p o>=e"=round(apply(perm>=edist,2,mean),4),t(round(apply(perm,2,quantile,prob=(c(seq(0,1,.1),.25,.75))),3)))
  edist
}
