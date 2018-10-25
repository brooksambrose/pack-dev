#' JSTOR PDF import sampler
#'
#' @param imp
#' @param size
#' @param lev
#' @param min.line
#' @param rm.bm remove back matter
#'
#' @return
#' @export
#' @import data.table
#' @importFrom magrittr %>%
#' @examples
imp2sam.f<-function(imp,lev='par',size=1000,min.line=3,rm.bm,seed){
  k<-c('doc',lev)
  nm<-c('doc','pag','par')
  setkeyv(imp,k)
  aud<-list()
  for(i in nm) aud[[i]]<-data.table(
    level=i
    ,N=imp[,.N,by=eval(nm[c(1,which(nm==i))] %>% unique)][,.N]
  )
  aud<-rbindlist(aud)
  sf<-imp[,.N,by=k][N>=min.line,!'N']

  if(missing(seed)) {rnorm(1);seed<-.Random.seed %>% sample(1)}
  set.seed(seed)
  ret<-list()
  ret[[length(ret)+1]]<-imp[sf[sample(1:.N,size)]]
  }
