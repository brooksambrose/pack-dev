#' COS parallel to K-clique Community Database
#'
#' @param mel2comps.dir
#' @param out
#' @param type
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
cos2kcc.f<-function(
  mel2comps.dir=stop('Specify a mel2comps directory that includes cos output.')
  ,out=stop('Specify output directory.')
  ,type=c('crel','utel')
)
{
  mel2comps.dir
  ret<-list()
  for(i in type) if(i%in%dir(mel2comps.dir)) {
    p<-paste(mel2comps.dir,i,sep=.Platform$file.sep)
    d<-list.dirs(p)[-1]
    olevs<-readLines(list.files(p,pattern='levs',full.names=T))
    ret[[i]]$orig<-lapply(d,function(j) {
      f<-list.files(j,pattern='[0-9]_communities\\.txt$',full.names=T)
      if(length(f)) {
        coslevs<-read.table(list.files(j,full.names=T,pattern='map$'))$V1
        k<-as.integer(sub('^.+/([0-9]+)_.+$','\\1',f))
        f<-f[order(k)]
        k<-k[order(k)]
        cos<-lapply(f,function(x) {
          x<-readLines(x)
          x<-sapply(x,function(y) lapply(strsplit(y,split='[: ]'),as.integer))
          x<-data.table(id=sapply(x,function(y) y[1]),memb=lapply(x,function(y) y[-1]))
          x<-x[,list(memb=list(memb)),by=id]
          x<-lapply(x$memb,function(y) sort(unique(unlist(y))))
          x
        })
        dup<-duplicated(cos,fromLast=T)
        cos<-cos[!dup]
        k<-k[!dup]
        f<-f[!dup]
        names(cos)<-paste('k',k,'c',sub('^.+/([0-9]+)-[0-9]+/.+','\\1',f),'-',sep='')
        cos<-do.call(c,cos)
        names(cos)<-sub('-$','',names(cos))
        cos<-lapply(cos,function(x) sort(coslevs[x+1]))
        return(cos)
      }
    })
    ret[[i]]$orig<-do.call(c,ret[[i]]$orig)
    ret[[i]]$orig<-split(ret[[i]]$orig,f=sub('^k([0-9]+).+$','\\1',names(ret[[i]]$orig)))
    ret[[i]]$orig<-ret[[i]]$orig[order(as.integer(names(ret[[i]]$orig)))]
    names(ret[[i]]$orig)<-paste('k',names(ret[[i]]$orig),sep='')
    cat('\n',i,'k-clique community distribution (original)\n')
    print(sapply(ret[[i]]$orig,length))

    ### strict membership interpretation
    x<-lapply(ret[[i]]$orig,function(x) sort(unique(unlist(x))))
    x<-rev(lapply(length(x):1, function(y) sort(unique(unlist(x[length(x):y])))))
    for(j in 1:(length(x)-1)) x[[j]] <- setdiff(x[[j]],x[[j+1]])
    ret[[i]]$strict<-mapply(function(com,reg) lapply(com,function(y) intersect(y,reg)),com=ret[[i]]$orig,reg=x) # com=communities reg=register
    ret[[i]]$strict<-lapply(ret[[i]]$strict,function(x) x[!!sapply(x,length)])
    ret[[i]]$strict<-ret[[i]]$strict[!!sapply(ret[[i]]$strict,length)]
    names(ret[[i]]$strict)<-paste(names(ret[[i]]$strict),'-',sep='')
    ret[[i]]$strict<-do.call(c,ret[[i]]$strict)
    names(ret[[i]]$strict)<-sub('^.+\\.','',sub('-$','',names(ret[[i]]$strict)))

    cat('\n',i,'k-clique community distribution (strict)\n')
    t<-table(as.integer(sub('^k([0-9]+).+$','\\1',names(ret[[i]]$strict))))
    names(t)<-paste('k',names(t),sep='')
    print(t)

    attributes(ret[[i]])$levels<-olevs
  }
  cos2kcc<-ret
  save(cos2kcc,file=paste(out,'cos2kcc.RData',sep=.Platform$file.sep))
  ret
}
