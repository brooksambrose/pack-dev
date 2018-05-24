tag <- function(so=SO,srch=c('ANTH', 'SOCI', 'ECON', 'POLI','PSYC')) {
  y<-sapply(srch,function(x) if(grepl('SOCIETY',so)) {all(grepl(x,so),!grepl('OF THE',so),!grepl('SOCIETY (OF)|(FOR)',so))} else {grepl(x,so)})
  if(any(y)) {paste(tolower(names(y)[y]),collapse=', ')} else {'Out'}
}
