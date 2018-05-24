#' Title
#'
#' @return
#' @export
#'
#' @examples
gng2tts.f<-function(){ # google ngram 2 token time series
	require(ngramr)
	ng<-grep('ng1870-2010.RData',dir(recursive = T),value=T)
	if(!length(ng)){
		gnq<-strsplit(
			c('social,sociology,sociological,sociologist'
				,'economic,economics,economical,economist'
				,'cultural,anthropic,anthropology,anthropological,anthropologist'
				,'political,political science,political scientist'
				,'mental,psychology,psychological,psychologist'),',')
		ng<-lapply(gnq,function(x) data.table(ngram(phrases=x,corpus = 'eng_us_2012',year_start=1870,year_end=2010,smoothing=3)))
		save(ng,file='out/ng1870-2010.RData')
	} else {
		load(ng)
	}
	ng<-rbindlist(ng)
	ng[,stem:=substr(Phrase,0,4)]
	ng[stem=='cult',stem:='anth']
	ng[stem=='ment',stem:='psyc']
	ng[,prewar:=ifelse(Year<2940,1,0)]
	setkey(ng,Phrase)
	#ng[,cat:=NULL]
	ng<-ng[!'anthropic']
	ng[c('social','economic','cultural','political','mental'),cat:='A.generic']
	ng[c('sociology','economics','anthropology','political science','psychology'),cat:='B.discipline']
	ng[c('sociological','economical','anthropological','psychological'),cat:='C.technical']
	ng[c('sociologist','economist','anthropologist','political scientist','psychologist'),cat:='D.profession']
	setkey(ng,prewar,stem)
	ng[,nminmaxF:=lintran(Frequency,range(Frequency)),by=c('prewar','Phrase')]
	ng[,nmaxF:=lintran(Frequency,c(0,max(Frequency))),by=c('prewar','Phrase')]
	ng[,nmdF:=Frequency/median(Frequency),by=c('prewar','Phrase')]
	ng[,nmnF:=Frequency/mean(Frequency),by=c('prewar','Phrase')]
	ngp<-qplot(Year,nminmaxF,data=ng[expand.grid(1:0,c('soci','econ','anth','psyc','poli'))],geom='line',color=cat,size=I(1)) + #size was 1.2
		scale_x_continuous(breaks=seq(1870,2010,10)) +
		facet_wrap(~stem,ncol=1) + scale_y_continuous(name='Scaled Frequency') +
		scale_colour_grey() + theme_bw() + theme(axis.text.x = element_text(angle = 90),legend.position="bottom")
	list(ts=ng,p=ngp)
}
