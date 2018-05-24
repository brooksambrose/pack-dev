write.ergmm <- function(
  where=stop("\"where\" = folder filepath for scripts",call.=F)
  ,dat=stop("\"dat\" = name of .RData file containing (stat)nets, w. ext",call.=F)
  ,net=stop("\"net\" = named list of (stat)nets",call.=F)
  ,dimensions=2
  ,groups=1
  ,verbosity=2
  ,mcmc.size=10000
  ,mcmc.burn=30000
  ,mcmc.inter=10
)
{
  mod<-paste("d",dimensions,"G",groups,sep="")
  writeLines(c(
    "getwd()"
    ,"rm(list=ls())"
    ,"library(statnet)"
    ,"library(latentnet)"
    ,paste("dat<-\"",dat,"\"",sep="")
    ,paste("load(dat)",sep="")
    ,"hoff2fit<-list()"
    ,"time<-round(unclass(Sys.time()))"
    ,paste("for(i in rev(names(",net,"))){",sep="")
    ,"cat(\"\\n<<<<<<<<< \",i,\" >>>>>>>>>\\n\\n\",sep=\"\")"
    ,paste("hoff2fit[[paste(i,\"",mod,"\",sep=\"\")]]<-try(ergmm(",net,"[[i]]~euclidean(d=",dimensions,",G=",groups,"),verbose=",verbosity,",control=control.ergmm(\nsample.size=",mcmc.size,"\n,burnin=",mcmc.burn,"\n,interval=",mcmc.inter,"\n)))",sep="")
    ,paste("save(hoff2fit,file=paste(\"hoff2fit_\",sub(\".RData\",\"\",dat),\"_\",\"",mod,"\",\"_\",time,\".RData\",sep=\"\"))",sep="")
    ,"}"
  ),con=paste(where,.Platform$file.sep,"write.ergmm_",sub(".RData","",dat),"_",mod,".R",sep=""))
}
