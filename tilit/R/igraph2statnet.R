#' Title
#'
#' @param net
#'
#' @return
#' @export
#'
#' @examples
igraph2statnet<-function(
	net
)
{
	require(igraph)
	require(network)
	require(data.table)

	if(class(net)=='igraph'){
		el<-data.table(get.edgelist(net))
		dir<-!igraph::is.directed(net)
		if(dir){
			w<-el[,V1>V2]
			r<-el[w,V1]
			el[w,V1:=V2]
			el[w,V2:=r]
		}
		setkey(el,V1,V2)
		bf<-nrow(el)
		el<-unique(el)
		cat('\n',bf-nrow(el),'duplicate edges deleted\n')
		el<-as.matrix(el)
		net<-network::network(el,matrix.type="edgelist",directed=ifelse(dir,T,F))
	}

	if(class(net)=='network'){

	}
	net
}
