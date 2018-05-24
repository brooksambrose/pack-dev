cat('\014')
rm(list=ls())
source('source/dissertation_source.R')
l<-ls() %>% sapply(function(x) get(x) %>% class)
l<-names(l)[l=='function']
tosort<-setdiff(
	l
	,sapply(c('dlabctawg/R','oikos/R','plagiat/R','tilit/R'
						#,'tosort'
						),list.files,USE.NAMES = F) %>%
		unlist %>% sub('\\.R','',.)
)
tosort
tosort[1] %>% get %>% View
for(i in tosort[1]) sink(file = paste0('tosort/',i,'.R'));cat(i,'<- ');print(get(i));sink()
