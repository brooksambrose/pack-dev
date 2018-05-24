#' Components to Cosparallel Fast Network Clique Percolation
#'
#' @param mel2comps.dir
#' @param cosparallel.path A path to cosparallel executable (e.g. ~/cosparallel-code/cos). Default assumes cos executable is in path.
#' @param threads
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'    # available - free memory for linux
#'    window.buffer=system('free -b | awk \'NR==2 {print $3" "$6}\'',intern = T) %>% strsplit(' ') %>% unlist %>% as.numeric %>% diff %>% log2
#' }
comps2cos.f<-function(
	mel2comps.dir=stop('Specify directory where mel2comps.txt edgelists are located.')
	,cosparallel.path='cos'
	,threads=parallel::detectCores()-1
	,window.buffer=system('free -b | awk \'NR==2 {print $6}\'',intern = T) %>% as.numeric %>% log2
)
{
	mel2comps.dir
	cosparallel.path

	mel2comps<-list.files(mel2comps.dir,pattern='mel2comps\\.txt$',recursive=T,full.names=T)

	for(i in mel2comps){
		com<-paste(
			'cd \'',sub('mel2comps.txt','\'',i)
			,' && ',sub('cos$','extras/maximal_cliques',cosparallel.path),' mel2comps.txt'
			,' && ',cosparallel.path,' -P ',threads,' mel2comps.txt.mcliques'
			,sep='')
		cat('Source data: ',i,'\n\nThreads used: ',threads,'\n\nsh: ',com,'\n\ncos stdout:\n\n',sep='')
		system(
			command=com
			#		,stdout='stdout.txt'
		)
	}
}
