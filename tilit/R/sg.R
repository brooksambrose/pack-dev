#' Custom Stargazer Tables for Bookdown
#'
#' @param x
#' @param lab
#' @param tit
#' @param sum
#' @param rplc
#' @param ...
#' @param type
#' @param old.col.align
#' @param new.col.align
#' @param dig
#' @param tabularx given preamble use new.col.align X for left Y for right Z for center
#' @param untable remove table environment wrapping for document classes that don't support, e.g. moderncv
#' @param nice
#'
#' @return
#' @export
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom lubridate is.timepoint
#' @examples
sg <- function(x,tit,dig=4,nice=T,type,lab,sum=F,old.col.align=paste0(rep('c',ncol(x)),collapse=''),new.col.align=paste0(c('l',rep('r',ncol(x)-1)),collapse=''),rplc=c(old='^X\\. ',new=''),tabularx=F,untable=F,...) {
  if(!is.data.table(x)) x<-data.table(x) else x<-copy(x)
  n<-names(x)[sapply(x,function(y) (!is.integer(y))&&is.numeric(y))]
  if(length(n)) x[,(n):=lapply(.SD,round,dig),.SDcols=n]
  n<-names(x)[sapply(x,is.factor)]
  if(length(n)) x[,(n):=lapply(.SD,as.character),.SDcols=n]
  if(missing(type)) type<-ifelse(knitr::is_latex_output(),'latex','html')
  if(missing(lab)) lab<-knitr::opts_current$get('label')
  if(is.null(lab)) lab<-''
  n<-names(x)[sapply(x,function(y) is.character(y)||is.factor(y))]
  if(!sum) {
    cn<-names(x)
    if(nice) {
      wn<-cn[sapply(x,is.numeric)]
      nd<-x[,..wn] %>% lapply(function(y) strsplit(as.character(y),split='.',fixed=T) %>%
        sapply(function(z) if(length(z)-1) nchar(z[2]) else 0) %>% max)
      tl<-x[,..wn] %>% lapply(lubridate::is.timepoint)
      for(i in names(nd)) if(tl[[i]]) x[,(i):=as.character((i))] else x[,(i):=format(get(i),digits=dig,big.mark = ',',big.interval = 3,nsmall = min(dig,nd[[i]]))]
    }
    x<-data.frame(lapply(x,as.character),stringsAsFactors = F) %>% data.table %>% setnames(cn)
  }
  if(type=='html') {
    setnames(x,htmltools::htmlEscape(names(x)))
    if(!sum) {
      if(length(n)) {
        x[,(n):=lapply(.SD,htmltools::htmlEscape),.SDcols=n]
        x[,(n):=lapply(.SD,gsub,pattern='&',replacement='\t\b&',fixed=T),.SDcols=n]
      }
    }
    x<-capture.output(stargazer::stargazer(x,type = 'html',title = tit,label=lab,style='ajs',summary=sum,header=F,rownames=F,align=ifelse(sum,T,F),...)) %>%
    {.[2]<-sub('text-align:center','text-align:right',.[2]);.} %>% {.[4] %<>% sub('tr><tr><td','tr><tbody><tr><td',.);.} %>% sub('</table>','</tbody></table>',.) %>% paste(collapse = '\n')
    } else {
    setnames(x,esc.ltx(names(x)))
    if(!sum) if(length(n)) x[,(n):=lapply(.SD,esc.ltx),.SDcols=n]
    x<-capture.output(stargazer::stargazer(x,title = tit %>% esc.ltx,label=lab,style='ajs',summary=sum,header=F,rownames=F,align=ifelse(sum,T,F),...)) %>%
      sub(pattern=old.col.align,replacement=new.col.align) %>%
      gsub('textbackslash ','',.) %>%
      gsub(rplc[1],rplc[2],.)
    if(tabularx) x<-gsub('\\{tabular\\}','\\{tabularx\\}',x) %>% gsub('begin\\{tabularx\\}','begin\\{tabularx\\}\\{\\\\textwidth\\}',.)
    if(untable) x<-x[do.call(seq,as.list(grep('\\{tabularx?\\}',x)))]
    x
  }
  structure(x,format=type,class="knitr_kable")
}
