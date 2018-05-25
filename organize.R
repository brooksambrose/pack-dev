cat('\014')
rm(list=ls())

# dirty script files (functions and nonfunctions)
writeLines(con = 'script/dissertation.R',readLines('https://raw.githubusercontent.com/brooksambrose/knowledge-survival/71e30380158be5790696ab61eef868dd7f71f867/dissertation.R'))
writeLines(con = 'script/function_map.R',readLines('https://raw.githubusercontent.com/brooksambrose/knowledge-survival/71e30380158be5790696ab61eef868dd7f71f867/function_map.R'))
writeLines(con = 'script/cfindertest.R',readLines('https://raw.githubusercontent.com/brooksambrose/knowledge-survival/71e30380158be5790696ab61eef868dd7f71f867/cfindertest.R'))
writeLines(con = 'script/string_comparison_metrics_knowledge-survival.R',readLines('https://raw.githubusercontent.com/brooksambrose/knowledge-survival/71e30380158be5790696ab61eef868dd7f71f867/string_comparison_metrics.R'))
writeLines(con = 'script/fuzzy-sets-source.R',readLines('https://raw.githubusercontent.com/brooksambrose/knowledge-survival/71e30380158be5790696ab61eef868dd7f71f867/scrap/fuzzy-sets-source.R'))

# clean source files (only functions)
writeLines(con = 'source/dissertation_source_knowledge-survival.R',readLines('https://raw.githubusercontent.com/brooksambrose/knowledge-survival/71e30380158be5790696ab61eef868dd7f71f867/dissertation_source.R'))
writeLines(con = 'source/glossary.R',readLines('https://raw.githubusercontent.com/brooksambrose/plagiat/master/%7E/wrk/flw/glossary.R'))
writeLines(con = 'source/src.R',readLines('https://raw.githubusercontent.com/brooksambrose/plagiat/master/%7E/tls/src.R'))

for(i in list.files('source',recursive = T,full.names = T)) source(i)
l<-ls() %>% sapply(function(x) get(x) %>% class)
l<-names(l)[l=='function']
tosort<-setdiff(
	l
	,sapply(c(list.dirs() %>% grep('/R$',.,value = T)
						,'tosort'
						),list.files,USE.NAMES = F) %>%
		unlist %>% sub('\\.R','',.)
)
tosort
for(i in tosort) {
	i %>% get %>% View
	sink(file = paste0('tosort/',i,'.R'));cat(i,'<- ');print(get(i));sink()
	readline('Press any key to continue.')
	}
