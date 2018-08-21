#' Different approaches to reduce and visualize the standard topic-document and topic-word word matrices output by LDA and other topic modelling estimations.
#'
#' @param stmbow2lda
#' @param out.dir
#' @param rt
#' @param ob
#' @param launch
#'
#' @return
#' @export
#' @import LDAvis servr RJSONIO
#' @examples
lda2viz.f<-function(stmbow2lda,out.dir,rt=F,ob=F,launch=T){
  # from http://cpsievert.github.io/LDAvis/newsgroup/newsgroup.html
  # http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
  # http://glimmer.rstudio.com/cpsievert/xkcd/

  json <- createJSON(
    phi = stmbow2lda$top.word.phi.beta
    ,theta = stmbow2lda$doc.top.theta
    ,vocab = stmbow2lda$vocab
    ,doc.length = stmbow2lda$doc.length
    ,term.frequency = stmbow2lda$term.frequency
    ,reorder.topics = rt
  )
  save(json,file=paste0(out.dir,.Platform$file.sep,'viz.RData'))
  cat('Topic index maps to probability index like so:\n')
  to<-fromJSON(json)$topic.order
  print(cbind(viz=to))
  od<-paste(out.dir,'viz',sep=.Platform$file.sep)
  serVis(json)
}
