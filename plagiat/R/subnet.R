#' Not quite sure
#'
#' @param dbl2bel
#' @param set
#' @param source
#'
#' @import network
#'
#' @return
#' @export
#'
#' @examples
subnet <- function(
  dbl2bel=stop("Supply original dbl2bel object",call.=F)
  ,set=stop("set list(cr=incl cr,ut=incl ut,ncr=excl cr,nut=excl ut) and unused to NULL",call.=F)
  ,source=stop("Supply source",call.=F)
)
{
  source(source)
  #if(!all(c("bel2mel.f","mel2net.f")%in%ls())) stop("Load correct source")
  #w<-!!sapply(lapply(dbl2bel$bel,"%in%",set),any)
  #if(!sum(w)) stop("Not a subset of either mode of this edgelist")
  #s<-set%in%dbl2bel$bel[,w]
  #if(!all(s)) {
  #	warning(paste(sum(!s),"or",round(sum(!s)/length(s)*100,1),"% of edges from subset are not in bel. First 10 excluded:"))
  #	print(head(set[s],10))
  #}
  sub<-rep(T,dim(dbl2bel$bel)[1])
  if(length(set$cr)) sub<-sub&dbl2bel$bel$cr%in%set$cr
  if(length(set$ut)) sub<-sub&dbl2bel$bel$ut%in%set$ut
  if(length(set$ncr)) sub<-sub&!dbl2bel$bel$cr%in%set$ncr
  if(length(set$nut)) sub<-sub&!dbl2bel$bel$ut%in%set$nut
  dbl2bel$bel<-dbl2bel$bel[sub,]
  if("pend"%in%names(dbl2bel)) dbl2bel$pend<-dbl2bel$pend[sub]
  cat("\n",nrow(dbl2bel$bel)," edges, ",length(unique(dbl2bel$bel$ut))," uts, and ",length(unique(dbl2bel$bel$cr)) ," crs fed to bel2mel.\n",sep="")
  bel2mel<-bel2mel.f(dbl2bel,out=getwd())
  mel2net<-mel2net.f(bel2mel)
  mel2net
}
