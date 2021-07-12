#' Letter Grade Scale with even intervals
#'
#' @param points Maximum points for assignment. Use 6 for an elegant, unbiased scale.
#' @param trad Traditional 100 point scale with uneven bins.
#'
#' @return
#' @export
#' @import magrittr data.table scales
#'
#' @examples
ltrgrd<-function(points=1,trad=F) {
  ltrs<-ec('D-,D,D+,C-,C,C+,B-,B,B+,A-,A,A+,|')
  if(trad) points<-100
  ret<-seq(0,1,1/12/2) %>%
    matrix(ncol=2,byrow = T) %>%
    scales::rescale(to = c(.6,1),from=0:1) %>%
    `*`(points) %>% data.table %>%
    {data.table(ltrs,.,.[,V1[-1]])} %>%
    setnames(ec('let,bot,mid,top')) %>% .[!.N] %>% suppressWarnings
  ret %<>% {list(data.table(let='F',bot=0,mid=0,top=.6*points),.)} %>%
    rbindlist
  if(trad) ret %<>% {data.table(.[,.(let)],.[,!'let'][,.SD %>% lapply(round)])}
  ret[,let:=factor(let,levels=c('F',setdiff(ltrs,'|')))]
  ret
}
