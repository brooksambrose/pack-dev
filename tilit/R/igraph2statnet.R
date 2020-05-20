#' Convert igraph graph to network network
#'
#' @param net
#'
#' @return
#' @export
#'
# @import igraph network data.table
#' @examples
igraph2statnet<-function(
	net
)
{
	if(class(net)=='igraph'){
		el<-data.table::data.table(igraph::as_edgelist(net))
		dir<-!igraph::is_directed(net)
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
