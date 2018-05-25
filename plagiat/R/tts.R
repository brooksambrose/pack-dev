tts <- function(
  c='A. Generic'
  ,s='soci'
  ,o=c('Frequency','Year')
  ,n=c('f','y')
  ,tts=get('gbng2tts')$ts
){
  require(data.table)
  setkey(tts,stem,cat,Year)
  tts<-copy(tts[list(s,c),rev(o),with=F])
  setkey(tts,Year)
  setnames(tts,o,n)
  tts  
}
