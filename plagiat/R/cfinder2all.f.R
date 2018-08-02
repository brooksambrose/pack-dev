#' Title
#'
#' @param cf.in
#' @param which
#'
#' @return
#' @export
#'
#' @examples
cfinder2all.f<-function(
	cf.in=stop('cf.out.dir = Path to CFinder output.')
	,which=c('communities_links'
					 ,'communities'
					 ,'communities_cliques'
					 ,'degree_distribution'
					 ,'graph_of_communities'
					 ,'membership_distribution'
					 ,'overlap_distribution'
					 ,'size_distribution'
	)
)
{
  library(data.table)
	fileps<-list.files(cf.in,recursive=T,full.names=T)
	ret<-list()
	for(i in which){
		if(c('communities_links')%in%i){
			cat('compiling',i,'\n')

			makerocketgonow<-function(raw){
				raw<-readLines(raw)
				raw<-strsplit(raw[grep('^[0-9][^:]+$',raw)],' ')
				raw<-data.table(do.call(rbind,lapply(raw,function(x) sort(as.integer(x)))))
				setnames(raw,c('src','tgt'))
				raw<-unique(raw)
				raw
			}

			library(doParallel)
			cl <- makeCluster(detectCores() )
			registerDoParallel(cl, cores = detectCores() )

			ret[[i]] <- foreach(j = grep(paste('.+',.Platform$file.sep,i,'$',sep=''),fileps,value=T),.packages = c("data.table"),.inorder=F) %dopar% {
				makerocketgonow(raw=j)
			}

			stopCluster(cl)

			ret[[i]]<-rbindlist(ret[[i]])[,list(ew=.N),by=c('src','tgt')]
			setkey(ret[[i]],ew)
		}
		if(c('communities')%in%i){
			cat('compiling',i,'\n')

			makerocketgonow<-function(raw){
				stub<-sub('^.+k=([0-9]+).+$','k\\1',raw)
				raw<-readLines(raw)
				raw<-strsplit(grep('^[0-9]',raw,value=T),':? ')
				nams<-sapply(raw,function(x) x[1])
				raw<-lapply(raw,function(x) as.integer(x[-1]))
				names(raw)<-paste(stub,nams,sep='-')
				raw
			}

			library(doParallel)
			cl <- makeCluster(detectCores() )
			registerDoParallel(cl, cores = detectCores() )

			ret[[i]] <- foreach(j = grep(paste('.+',.Platform$file.sep,i,'$',sep=''),fileps,value=T),.packages = c("data.table"),.inorder=F) %dopar% {
				makerocketgonow(raw=j)
			}

			stopCluster(cl)

			names(ret[[i]])<-sapply(ret[[i]],function(x) sub('^([^-]+).+$','\\1',names(x[1])))
		}
	}
	ret
}
