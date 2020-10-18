#' Number 2 Viridis palette
#'
#' @param num
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
num2vir<-function(num,...){
  viridisLite::viridis(256,...)[round(scales::rescale(num,c(1,256)))]
}
