#' Multilevel corpus 2 Multilevel K topic distribution
#'
#' @param mlc
#' @param lev
#' @param verb
#'
#' @return
#' @export
#' @import data.table
#' @importFrom magrittr %>%
#' @examples
mlc2mlk.f<-function(mlc,lev=c('doc','par','sen'),verb=F) {
  lapply(mlc[lev], function(x) stm::stm(documents = x$spm,vocab = mlc$voc,K = 0,max.em.its = 0,verbose = verb)$settings$dim$K) %>% t
}
#TODO add on to mlc[lev]
