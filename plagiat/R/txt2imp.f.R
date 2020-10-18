#' Text files to import
#'
#' @param pth Vector of full paths to every text file you want to import.
#' @param ncores
#'
#' @import data.table magrittr
#' @importFrom pbapply pblapply
#' @return
#' @export
#'
#' @examples
txt2imp.f<-function(pth,ncores=1){
	imp<-pbapply::pblapply(pth,function(x){
		x<-data.table(doc=x,pag=1,lin=NA,par=NA
			,txt=readLines(x) %>% strsplit(x,split = '\n+') %>% unlist
		)
		x[,lin:=1:.N][,par:=lin]
		x
	},cl=ncores)
	imp %<>% rbindlist
	imp<-list(imp=imp,met=imp[,.(doc)])
	imp
}
