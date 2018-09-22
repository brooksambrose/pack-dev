#' Custom kable transformations
#'
#' @param ...
#'
#' @return
#' @export
#' @import knitr kableExtra
#' @importFrom magrittr %>%
#'
#' @examples
kab<-function(...) {
  o<-knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::kable(...,format = ifelse(is.null(o),'html',o),format.args=list(big.mark = ',')) %>% kableExtra::kable_styling(full_width = F)
}
