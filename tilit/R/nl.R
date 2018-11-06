#' Nice list text concatenation with Oxford comma
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
nl<-function(x){
  r<-sub('(.+), %','\\1, and ',paste(x,collapse=', %%'))
  if(length(x)==2) gsub('%','',sub(', and %',' and ',r)) else gsub('%','',r)
}
