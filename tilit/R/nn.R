#' Nice Number Format
#'
#' @param rnd
#' @param cap
#' @param word
#' @param x
#'
#' @return
#' @export
#'
#' @examples
nn<-function(x,rnd=0,cap=F,word=T) {
  if(is.character(x)) return(x)
  x<-round(x,rnd)
  r<-rep(NA_character_,length(x))
  for(i in which(!is.na(x)&!is.infinite(x))){
    if(x[i]%%1==0&(x[i]+1)) {
      if(word) r[i]<-c('zero','one','two','three','four','five','six','seven','eight','nine')[x[i]+1]
      if(cap&i==1) r[i]<-paste(toupper(substring(r[i], 1,1)), substring(r[i], 2),sep="", collapse="")
    }
    if(is.na(r[i])) r[i]<-format(x[i],big.mark=',',big.interval=3,nsmall=rnd)
  }
  r
}
