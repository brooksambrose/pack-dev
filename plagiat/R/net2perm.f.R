#' Random Poisson Permutation of Monomodal Edgelist
#'
#' @param mel2net
#' @param nsim
#'
#' @return
#' @export
#'
#' @examples
net2perm.f <- function(
  mel2net
  ,nsim
)
{
  library(network)
  cat("\nPermuting random poisson edge distribution\nSimulating...")
  perm<-list()
  s<-network.size(mel2net)
  maxcombo<-s*(s-1)/2
  combos<-1:maxcombo
  choices<-sum(mel2net%e%"ew")
  t1<-proc.time()
  for(i in 1:nsim){
    cat("\r",i,sep="")
    rdist<-sample(combos,size=choices,replace=T)
    rdist<-table(rdist)
    z<-maxcombo-length(rdist)
    rdist<-table(rdist)
    n<-names(rdist)
    rdist<-c(z,rdist)
    names(rdist)<-c("0",n)
    rdist<-as.table(rdist)
    perm[[i]]<-rdist
  }
  cat("\nSeconds to simulate:")
  print(proc.time()-t1)

  edist<-table(mel2net%e%"ew")
  n<-length(unique(unlist(bel2mel[[i]][,1:2])))
  z<-(n*(n-1)/2)-sum(edist)
  n<-names(edist)
  edist<-c(z,edist)
  names(edist)<-c("0",n)
  edist<-as.table(edist)

  cn<-sort(unique(unlist(lapply(perm,names))))
  permdb<-data.frame(matrix(0,nrow=length(perm),ncol=length(cn)))
  permdb<-data.frame(permdb,matrix(0,nrow=length(perm),ncol=length(edist)-length(cn)))
  names(permdb)<-names(edist)
  for(i in 1:length(perm)) permdb[i,names(perm[[i]])]<-perm[[i]]

  dif<-matrix(edist,nrow=dim(permdb)[1],ncol=dim(permdb)[2],byrow=T)-permdb
  md<-apply(dif,2,mean)
  sdd<-apply(dif,2,sd)
  cid<-apply(dif,2,quantile,prob=c(.05,.95))
  tad<-apply(dif,2,table)
  num<-lapply(tad,names)
  num<-lapply(num,as.numeric)

  dens<-1-(permdb$`0`/maxcombo)
  mean((1-edist["0"]/maxcombo)-dens)*100
  sd((1-edist["0"]/maxcombo)-dens)*100
  mean((1-edist["0"]/maxcombo)-dens)/sd((1-edist["0"]/maxcombo)-dens)

  apply(t(apply(permdb,1,"*",as.numeric(colnames(permdb)))),sum)/maxcombo

  round(cbind(md,sdd,t=md/sdd,p=0,t(cid)),digits=3)

  round(cbind(md=md/maxcombo,sdd=sdd/maxcombo,t=md/sdd,p=0),digits=4)[1:7,]
  sum(edist[-(1:7)])/maxcombo

  plot(as.table((md/sdd)[1:7]),type="l",ylim=range((md/sdd)[1:7]),lwd=3)
  abline(h=0,lty="dotted",lwd=3)

  round(cbind(md,sdd,t=md/sdd,p=0),digits=1)[1:7,]
  sum(edist[-(1:7)])

  mp<-apply(dif/maxcombo,2,mean)
  sdp<-apply(dif/maxcombo,2,sd)
  cip<-apply(dif/maxcombo,2,quantile,prob=c(.05,.95))

  round(cbind(mp,sdp,p=0,t(cip)),digits=5)[1:7,]
  sum(edist[-(1:7)])

  mvd<-mean((sum(((as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2)*edist)/maxcombo)-(apply(apply(permdb,1,"*",(as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2),2,sum)/maxcombo))
  sddv<-sd((sum(((as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2)*edist)/maxcombo)-(apply(apply(permdb,1,"*",(as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2),2,sum)/maxcombo))
  cidv<-quantile((sum(((as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2)*edist)/maxcombo)-(apply(apply(permdb,1,"*",(as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2),2,sum)/maxcombo),prob=c(.05,.95))
  round(c(mvd=mvd,sddv=sddv,tvd=mvd/sddv,p=0,cidv=cidv),digits=4)

  m<-apply(permdb,2,mean)
  sd<-apply(permdb,2,sd)
  ci<-apply(permdb,2,quantile,prob=c(.05,.95))
  cbind(m,sd,ci)

  zscores<-(edist[1:7]-apply(permdb,2,mean))/apply(permdb,2,sd)

  list(
    mean=apply(matrix(edist,nrow=dim(permdb)[1],ncol=dim(permdb)[2],byrow=T)-permdb,2,mean)
    ,sd=apply(matrix(edist,nrow=dim(permdb)[1],ncol=dim(permdb)[2],byrow=T)-permdb,2,sd)
  )
}
