unicr <- function(
  index
)
{
  yu<-list()
  cbeg<-list()
  cend<-list()
  cat("    ")
  for(i in 1:dim(index)[1]){
    d<-dimnames(index)[[1]][i]
    cat("\b\b\b\b",d)
    flush.console()
    yu[[d]]<-length(levels(droplevels(comdb[J(unlist(index[i,]),"CR")]$b)))
    cbeg[[d]]<-length(levels(droplevels(comdb[J(unlist(index[1:i,]),"CR")]$b)))
    cend[[d]]<-length(levels(droplevels(comdb[J(unlist(index[i:dim(index)[1],]),"CR")]$b)))
  }
  u<-data.frame(yu=unlist(yu),cbeg=unlist(cbeg),cend=unlist(cend))
  u
}
