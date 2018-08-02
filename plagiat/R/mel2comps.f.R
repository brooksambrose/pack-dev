mel2comps.f <- function(
  bel2mel
  ,type=c('crel','utel')
  ,out=stop('Specify output directory')
  ,min.size=3
)
{
  library(data.table)
  library(igraph)
  out
  ret<-list()
  for(i in type) if(i%in%names(bel2mel)) {
    cat('\n',i,'\n',sep='')
    levs<-factor(bel2mel[[i]][,do.call(c,.SD),.SDcols=1:2])
    bel2mel[[i]]<-matrix(as.character(as.integer(levs)),ncol=2)
    levs<-levels(levs)
    g<-graph.edgelist(bel2mel[[i]],FALSE)
    g<-decompose.graph(g)
    comp.sizes<-sapply(g,vcount)
    print(table(comp.sizes))
    cat('Only networks of size',min.size,'and above returned.')
    cat('\nDirectories created:')
    mapply(
      function(net,id,vcount) {
        path<-paste(out,'mel2comps',i,paste(id,vcount,sep='-'),sep=.Platform$file.sep)
        dir.create(path,recursive=T)
        cat('\n',path)
        write.table(get.edgelist(net),file=paste(path,'mel2comps.txt',sep=.Platform$file.sep),sep='\t',quote=F,na='',row.names=F,col.names=F)
      }
      ,net=g[comp.sizes>=min.size]
      ,id=(1:length(g))[comp.sizes>=min.size]
      ,vcount=comp.sizes[comp.sizes>=min.size]
    )
    writeLines(levs,con=paste(out,'mel2comps',i,'mel2comps-levs.txt',sep=.Platform$file.sep))
    ret[[i]]<-g[comp.sizes>=min.size]
  }
  ret
}
