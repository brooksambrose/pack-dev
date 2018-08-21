#' Multilevel corpus 2 Multilevel K topic distribution
#'
#' @param mlc
#' @param lev
#'
#' @return
#' @export
#' @import data.table
#' @importFrom magrittr %>%
#' @examples
mlc2mlk.f<-function(mlc,lev=c('doc','par','sen')) {
  lapply(mlc[lev], function(x) stm::stm(documents = x,vocab = mlc$voc,K = 0,max.em.its = 0,verbose = F)$settings$dim$K) %>% t
}
