#' Install Missing Packages
#'
#' Only installs packages that aren't already installed.
#'
#' @param x list of packages
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
install_missing_pkg<-function(x,...){
  x %>% ec %>% {setdiff(.,installed.packages() %>% rownames)} %>% install.packages
  }
