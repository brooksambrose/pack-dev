#' Six Ages Saga to Full Text
#'
#' @return
#' @export
#' @import magrittr data.table
#'
#' @examples
sas2imp.f<-function(sas,rbl=T,h2v=T){
  library(magrittr)
  library(data.table)

  imp<-list()
  for(i in sas){
    imp[[i]]<-data.table(txt=readLines(i))
    imp[[i]][,`:=`(
      doc=i
      ,lin=1:.N
    )]
    #add title
    t<-imp[[i]][1,txt]
    imp[[i]]<-imp[[i]][-1]
    imp[[i]][,title:=t]
    #remove blanks, keeping line index
    if(rbl) imp[[i]]<-imp[[i]][txt!='']
    if(h2v) imp[[i]]<-hed2var(imp[[i]])
  }
  rbindlist(imp)
}
