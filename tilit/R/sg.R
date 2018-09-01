#' Stargazer
#'
#' @param x
#' @param lab
#' @param tit
#' @param sum
#' @param col.align
#' @param rplc
#' @param ...
#'
#' @return
#' @export
#' @import stargazer
#' @importFrom magrittr %>%
#' @examples
sg <- function(x,type='html',lab='',tit,sum=F,col.align=c(old='',new=''),rplc=c(old='^X\\. ',new=''),...) {
	if(type=='html') {
	  x<-capture.output(stargazer(x,type = 'html',title = tit,label=lab,style='ajs',summary=sum,header=F,rownames=F,...))
	} else {
  if(!sum) x<-data.frame(lapply(x,gsub,pattern='&',replacement='\\&',fixed=T))
	x<-capture.output(stargazer(x,title = tit,label=lab,style='ajs',summary=sum,header=F,rownames=F,...)) %>%
		gsub(pattern='\\textbackslash ',replacement='\\',fixed=T) %>%
		gsub(pattern='\\}',replacement='}',fixed=T) %>%
		gsub(pattern='\\{',replacement='{',fixed=T) %>%
		sub(pattern=col.align[1],replacement=col.align[2]) %>%
	  gsub(pattern=rplc[1],replacement=rplc[2])
	}
	cat(x,sep='\n')
}
