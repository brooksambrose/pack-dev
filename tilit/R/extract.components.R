#' Title
#'
#' @param graph
#' @param quantile
#'
#' @import sna network
#'
#' @return
#' @export
#'
#' @examples
extract.components<-function(
	graph
	,quantile=0
)
{
	components<-sna::component.dist(graph)
	subgraphs<-list()
	j<-1
	for(i in order(components$csize[which(components$csize>=quantile(components$csize, c(quantile)))],decreasing=T)){
		subgraphs[[j]]<-graph%s%which(components$membership==i)
		j<-j+1
	}
	cat("\nComponent size distribution:")
	print(table(components$csize))
	return(subgraphs)
}
