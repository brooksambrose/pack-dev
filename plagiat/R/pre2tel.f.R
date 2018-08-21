#' Pre processed text to term edge list
#'
#' @param pre
#' @param stop
#'
#' @return
#' @export
#'
#' @examples
pre2tel.f<-function(pre,stop=100){
  library(progress)
  library(data.table)
  tam<-pre[!(pun|sto),.(doc,lin,cha,sen,com)]
  setkey(tam,doc,sen)
  r<-list()
  pb <- progress_bar$new(format = "  [:bar] :elapsedfull :eta",total = tam[,.(doc,sen)] %>% unique %>% nrow, clear = FALSE, width= 60)

  for(i in tam[,unique(doc)]) for(j in tam[i,unique(sen)]) {pb$tick();for(k in tam[i][sen>=j,unique(sen)]) {
    if(k-j>=stop) next
    r[[length(r)+1]]<-expand.grid(s=tam[.(i,j),com],r=tam[.(i,k),com],KEEP.OUT.ATTRS = F, stringsAsFactors = F) %>% apply(1,sort) %>% t %>% data.table(doc=i,.,ew=1/(k-j+1))
  }}
  r<-rbindlist(r)
  setnames(r,c('doc','s','r','ew'))
  r<-r[,.(ew=sum(ew)),by=.(s,r)]
  r
}
