plotpois <- function(
  pois
  ,year
  ,jour
  ,count
  ,q1="0%"
  ,q2="50%"
  ,q3="100%"
)
{
  xlim<-range(year)
  plt<-do.call(c,pois[as.character(year),jour])
  plt<-plt[!is.na(plt)]
  for(i in 1:length(plt)) plt[[i]]<-cbind(year=as.numeric(names(plt[i])),plt[[i]])
  plt<-do.call(rbind,plt)
  plt<-plt[rownames(plt)==as.character(count),]

  ylim=range(plt[,c("observed",q1,q2,q3)])
  plot.new()
  plot.window(xlim=xlim,ylim=ylim)
  axis(side=1,lab=as.character(xlim[1]:xlim[2]),at=xlim[1]:xlim[2])
  axis(side=2)
  title(main=jour,xlab="Year",ylab=paste("Count of ",count,"'s",sep=""))
  w<-which(!!diff(plt[,"year"])-1)
  b<-1
  for(i in unique(c(w,dim(plt)[1]))){
    if(!length(w)) {r<-1:dim(plt)[1]} else {r<-b:i;b<-i+1}
    lines(plt[r,"year"],y=plt[r,q2],col="red",lty=1)
    lines(plt[r,"year"],plt[r,q1],lty=3,col="red")
    lines(plt[r,"year"],plt[r,q3],lty=3,col="red")
    lines(plt[r,"year"],plt[r,"observed"],lty=1,lwd=1)
    if(!length(w)) break
  }
}
