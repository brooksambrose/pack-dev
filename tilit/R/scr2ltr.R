#' Score to Letter Grade
#'
#' TODO: Optimize this by only converting unique levels, then switching back
#'
#' @param scr numerical score
#' @param ... passed to ltrgrd
#' @param sigdif reduce to significant digits prior to comparison
#'
#' @return
#' @export
#'
#' @examples
scr2ltr<-function(scr,sigdif=3,...) {
  fnc<-function(x) tilit::ltrgrd(...)[signif(x,digits = sigdif)<signif(top,digits = sigdif)]$let[1]
  if(length(scr)>1e4) {
    ret<-pbapply::pbsapply(scr,fnc,cl=parallel::detectCores()-1)
  } else {
    ret<-sapply(scr,fnc)
  }
  ret %<>% factor(levels=tilit::ltrgrd(...)$let)
  ret
}
