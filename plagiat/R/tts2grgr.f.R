#' Title
#'
#' @param gbng2tts
#' @param order
#'
#' @return
#' @export
#'
#' @examples
tts2grgr.f <- function(
  gbng2tts
  ,order
){
  require(data.table)
  require(forecast)
  require(lmtest)
  require(ggplot2)
  if(missing(order)) {d<-copy(gbng2tts)} else {d<-gbng2tts[,Phrase:=factor(Phrase,levels=order)]}
  setkey(d,Phrase,Year)
  lv<-c('Identity','First Difference')
  d<-rbindlist(list(
    d[,list(
      Year=Year[-1]
      ,Frequency=Frequency[-1] %>% lintran(s1=range(Frequency[-1]))
      ,Diffs=factor('Identity',levels = lv)
    ),by=Phrase]
    ,d[,list(
      Year=Year[-1]
      ,Frequency=diff(Frequency)  %>% lintran(s1=c(0,max(Frequency[-1]))) # %>% BoxCox(BoxCox.lambda(Frequency))
      ,Diffs=factor('First Difference',levels = lv)
    ),by=Phrase]
  ))

  p <- ggplot(d, aes(Year,Frequency)) + geom_line() +
    geom_text(
      aes(x, y, label=Phrase)
      ,data=data.frame(x=-Inf,y=Inf,Phrase=unique(d$Phrase),Diffs='Identity')
      , hjust=0,vjust=1,size=3) +
    facet_wrap( ~ Phrase + Diffs,scales='free_y',ncol=2,strip.position='left') +
    theme(
      axis.text.x = element_text(angle = 90,vjust=.5,debug=F)
      ,legend.position="bottom"
      ,panel.grid.minor.x = element_blank()
      ,panel.grid.minor.y = element_blank()
      ,strip.background = element_blank()
      #,strip.placement = "outside"
      ,strip.text.y = element_blank()
      ,axis.text.y = element_blank()
      ,axis.ticks = element_blank()
      ) +
    scale_x_continuous(breaks=seq(round(min(d$Year),-1),round(max(d$Year),-1),10))  +
   # scale_y_continuous(breaks=function(x) ifelse(max(x)==1,list(c(0,.5,1)),list(c(-.2,-.1,0,.1,.2)))[[1]]) +
    ylab('Scaled Frequency')
  graphics.off()
  p

}
