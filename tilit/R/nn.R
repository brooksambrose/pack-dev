#' Nice Number Format
#'
#' @param x
#' @param ns
#'
#' @return
#' @export
#'
#' @examples
nn<-function(x,rnd=0,cap=F) {
  if(is.character(x)) return(x)
  x<-round(x,rnd)
  r<-rep(NA_character_,length(x))
  for(i in 1:length(x)){
    if(x[i]%%1==0&(x[i]+1)) {
      r[i]<-c('zero','one','two','three','four','five','six','seven','eight','nine')[x[i]+1]
      if(cap&i==1) r[i]<-paste(toupper(substring(r[i], 1,1)), substring(r[i], 2),sep="", collapse="")
    }
    if(is.na(r[i])) r[i]<-format(x[i],big.mark=',',big.interval=3)
  }
  r
}
