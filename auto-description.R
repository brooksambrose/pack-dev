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
library(tilit)

# first try to load all functions declared by import, library, require, or ::
pck<-pbapply::pblapply(dir('R',full.names = T),function(x) {
	l<-readLines(x) %>% paste(collapse=' ')
	r<-stringr::str_extract_all(l,'(require\\(|library\\()([A-Za-z][A-Za-z0-9.]*)',simplify = F) %>% unlist %>% sub('.*\\(([A-Za-z][A-Za-z0-9.]*)','\\1',.)
	i<-stringr::str_extract_all(l,'(@import +)([A-Za-z][A-Za-z0-9. ]*)',simplify = F) %>% unlist %>% sub('^[^ ]+ +','',.) %>% strsplit(' ') %>% unlist
	n<-stringr::str_extract_all(l,'([A-Za-z][A-Za-z0-9.]*)::',simplify = F) %>% unlist %>% sub('::','',.)
	f<-stringr::str_extract_all(l,'@importFrom +([A-Za-z][A-Za-z0-9.]*)',simplify = F) %>% unlist %>% sub('@importFrom +','',.)
	c(r,i,n,f) %>% unique %>% sort
}) %>% unlist %>% unique %>% sort
x<-pck %>% sapply(function(x) if(!require(x,character.only = T)) try({install.packages(x);library(x,character.only = T)}))
r<-x %>% sapply(inherits,'try-error') %>% {names(.)[.]} # these may need to be installed from github
cat(r,sep='\n')
# inspect errors
x[r]
#random https://github.com/colearendt/xlsx/issues/128
# devtools::install_github("colearendt/xlsx@develop")

#try CRAN github first
y<-r %>% sapply(function(x) if(!require(x,character.only = T)) try({devtools::install_github(sprintf('CRAN/%s',x));library(x,character.only = T)}))
log<-s<-y %>% sapply(inherits,'try-error') %>% {names(.)[.]} # these may need to be installed from github
cat(s,sep='\n')
# inspect errors
y[s]
# manual fixes
z<-'RGoogleDrive' %>% sapply(function(x) if(!require(x,character.only = T)) try({devtools::install_github(sprintf('1beb/%s',x));library(x,character.only = T)}))
# cldr appears to be mothballed and not able to install

# repeat this section until all packages in scripts are loaded
# use multi file search to find functions and manually load their packages
p<-pbapply::pblapply(dir('R',full.names = T),NCmisc::list.functions.in.file,cl=parallel::detectCores())
u<-unlist(p) %>% {data.table(u=.,on=names(.))}
u[,n:=sub('[0-9]+$','',on)][,c('f','p'):=tstrsplit(n,split='\\.(package:)|\\.(character)')]
u['(0)',on='p',.(u,f,p)] %>% {split(.,.$f)} %>% {lapply(.,function(x) {print(x);y<-readline('enter nothing to continue, q to quit');if(y=='q') stop('All done!',call. = F)})}
pkn<-ec('')
r<-u[!'(0)',on='p',.(r=require(unique(p),character.only = T)),by=p]

# perform final check that remaining functions are not from loadable packages
p<-pbapply::pblapply(dir('R',full.names = T),function(x) NCmisc::list.functions.in.file(x) %>% unlist %>% {names(.)<-paste(x,names(.),sep='.');.},cl=parallel::detectCores())
u<-unlist(p) %>% {data.table(u=.,on=names(.))}
u[,n:=sub('[0-9]+$','',on)][,c('f','p'):=tstrsplit(n,split='\\.(package:)|\\.(character)')]
u['(0)',on='p',.(u,f,p)]

# add to DESCRIPTION
t<-u[!'(0)',on='p'
		 ,.(p=unique(p) %>% na.omit %>%
		 	 	sub('ggplot','ggplot2',.) %>%
		 	 	sub('xml','xml2',.) %>%
		 	 	sub('networkD','networkD3',.) %>%
		 	 	{base::Filter(f=function(x) !x%in%'cld',x=.)})][
		 	 		,.(t=try(is.null(usethis::use_package(p,min_version = F)))),by=p]
# manual fixes
# xml

# once DESCRIPTION is good, detect SystemRequirements using sysreqs
# sysreqs still in BETA so may be undetected dependencies
D<-readLines('DESCRIPTION')
wp<-grep('^ ',D)
P<-D[wp] %>% sub('^[ ]+([^ ]+).+','\\1',.)
S<-list()
for(i in P) {cat(i,'');S[[i]]<-try(jsonlite::read_json(sprintf('https://sysreqs.r-hub.io/pkg/%s',i)) %>% unlist %>% {.[grep('DEB$',names(.))]})}

c('USER root\nRUN apt-get update \\\n  && apt-get install -y --no-install-recommends \\'
	,sprintf('   #%s\n    %s \\',names(S),S)
	,'  && apt-get purge \\\n  && apt-get clean \\\n  && rm -rf /var/lib/apt/lists/*\nUSER ${NB_USER}'
) %>% paste(collapse='\n') %>% cat(sep='\n')
