#' Translate J9 to SO in WOK record
#'
#' @param jd
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
j9so<-function(j9) {
  wok2dbl[wok2dbl[(j9),on='val'][1,id],on='id']['SO',on='field'][,stringr::str_to_title(val)]
}
