#' Add curent version string to package dependencies
#'
#' From https://stackoverflow.com/questions/38738292/how-can-i-automatically-add-update-depends-imports-suggests-versions-in-descript
#' Will \code{cat} out a cut/paste-able set of fields for a
#' \code{DESCRIPTION} file with minimum required versions for
#' each package based upon currently available package vesions
#' in CRAN.
#'
#' @param pkg package description, can be path or package name
#' @param fields fields to get & report dependencies for
#' @note R and the R version is NOT added to \code{Depends}
#' @examples
#' add_pkg_versions("qmethod")
#' add_pkg_versions("MASS")
#' \dontrun { # assumes you're in a pkg devel dir
#' add_pkg_versions()
#' }
add_pkg_versions <- function(pkg=".",
                             fields=c("Depends", "Imports", "LinkingTo", "Suggests")) {

  require(purrr)
  walk(c("dplyr", "tools", "stringi", "devtools"), require, character.only=TRUE)

  stopifnot(is_scalar_character(pkg), pkg != "")
  fields <- match.arg(fields, c("Depends", "Imports", "LinkingTo", "Suggests"),
                      several.ok=TRUE)

  avail <- as_data_frame(available.packages())

  if (pkg == ".") {
    pkg_deps <- unclass(as_data_frame(read.dcf(file.path(package_file(), "DESCRIPTION"))))
    pkg <- pkg_deps$Package
    map(fields, ~stri_split_lines(pkg_deps[[.]])) %>%
      map(function(x) {
        if (length(x) > 0) {
          unlist(x) %>%
            stri_replace_all_regex(" \\(.*$|,", "") %>%
            discard(`%in%`, c("", "R"))
        } else { x }
      }) -> pkg_deps
    names(pkg_deps) <- fields
  } else {
    pkg_deps <- map(fields, ~flatten_chr((package_dependencies(pkg,  which=.))))
    names(pkg_deps) <- fields
  }

  pkg_deps <- discard(pkg_deps, function(x) {length(x)==0})

  map(pkg_deps, function(x) {

    non_base <- filter(avail, Package %in% x)
    base <- setdiff(x, non_base$Package)

    non_base %>%
      mutate(pv=sprintf("%s (>= %s)", Package, Version)) %>%
      select(pv) %>%
      flatten_chr() -> pkg_plus_version

    sort(c(pkg_plus_version, base))

  }) -> pkg_deps

  cat("Package: ", pkg, "\n", sep="")
  walk(names(pkg_deps), function(x) {

    cat(x, ":\n", sep="")
    sprintf("    %s", pkg_deps[[x]]) %>%
      paste0(collapse=",\n") %>%
      cat()
    cat("\n")

  })

}

library(data.table)
library(magrittr)

p<-sapply(dir('R',full.names = T),NCmisc::list.functions.in.file)
u<-unlist(p) %>% {data.table(u=.,on=names(.))}
u[,n:=sub('[0-9]+$','',on)][,c('f','p'):=tstrsplit(n,split='\\.(package:)|\\.(character)')]

r<-u[!'(0)',on='p',.(r=require(unique(p),character.only = T)),by=p]

p<-sapply(dir('R',full.names = T),NCmisc::list.functions.in.file)
u<-unlist(p) %>% {data.table(u=.,on=names(.))}
u[,n:=sub('[0-9]+$','',on)][,c('f','p'):=tstrsplit(n,split='\\.(package:)|\\.(character)')]
u['(0)',on='p',.(u,f,p)]

r<-rbindlist(list(
	r,
	data.table(
		p=tilit::ec('stringdist,tilit,sna,pdftools,stringr,igraph,network,knitr,ggplot2,ecp,plotly,htmltools,stargazer,kableExtra,plagiat')
	)[,ix:=.I][,r:=require(p,character.only=T),by=ix][,!'ix']
))

p<-sapply(dir('R',full.names = T),NCmisc::list.functions.in.file)
u<-unlist(p) %>% {data.table(u=.,on=names(.))}
u[,n:=sub('[0-9]+$','',on)][,c('f','p'):=tstrsplit(n,split='\\.(package:)|\\.(character)')]
u['(0)',on='p',.(u,f,p)]

t<-r[,.(t=try(is.null(usethis::use_package(unique(p),min_version = T)))),by=p]
