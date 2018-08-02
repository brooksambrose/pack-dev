#' Different approaches to reduce and visualize the standard topic-document and topic-word word matrices output by LDA and other topic modelling estimations.
lda2viz.f<-function(stmbow2lda,out.dir,rt=F,ob=F,launch=T){
	# from http://cpsievert.github.io/LDAvis/newsgroup/newsgroup.html
	# http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
	# http://glimmer.rstudio.com/cpsievert/xkcd/
  library(LDAvis,quietly = T)
  library(servr,quietly = T)
	library(RJSONIO,quietly = T)

	json <- createJSON(
		phi = stmbow2lda$top.word.phi.beta
		,theta = stmbow2lda$doc.top.theta
		,vocab = stmbow2lda$vocab
		,doc.length = stmbow2lda$doc.length
		,term.frequency = stmbow2lda$term.frequency
		,reorder.topics = rt
	)
	save(json,file=paste0(out.dir,.Platform$file.sep,'viz.RData'))
	cat('Topic index maps to propability index like so:\n')
	to<-fromJSON(json)$topic.order
	print(rbind(viz=to))
	od<-paste(out.dir,'viz',sep=.Platform$file.sep)
	serVis(json,open.browser = ob,out.dir=od)
	call<-paste('python -m SimpleHTTPServer && open http://localhost:8000',sep='')
	if(launch) {setwd(od);cat('You may need to manually kill the Python process');system(call,wait=F)}
	list(call=call,top.ord=to)
}
