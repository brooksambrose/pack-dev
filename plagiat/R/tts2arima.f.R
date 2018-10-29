#' Token Time Series to ARIMA
#'
#' @param gbng2tts
#' @param by
#'
#' @return
#' @export
#' @import forecast
#'
#' @examples
tts2arima.f <- function(
  gbng2tts
  ,by=c('batch','Phrase')
)
{
  tts2arima<-gbng2tts[,list(aa=list(
    auto.arima(
      ts(Frequency,start = min(Year),frequency = 1)
      ,lambda = BoxCox.lambda(Frequency)
      ,stepwise = F
      ,seasonal = F
      ,trace=F)
    )),keyby=by]
  tts2arima[,Predicted:=list(lapply(aa,fitted))]
  tts2arima[,`Predicted t-Score`:=list(lapply(fit,tscore))]
  tts2arima
}
