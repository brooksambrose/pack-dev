#' JSTOR PDF to imp
#'
#' @param jpdf
#' @param ocr
#' @param depth
#'
#' @return
#' @export
#'
#' @examples
jpdf2imp.f<-function(jpdf,ocr=F,depth=5){
  library(data.table)
  library(stringr)
  library(pdftools)
  library(stringdist)
  library(igraph)
  library(progress)
  # library(parallel)
  # ncores<-c(1,detectCores()/2 %>% ceiling) %>% max

  # import
  imp<-jpdf %>% lapply(pdf_text)
  imp %<>% lapply(str_split,'\n')
  # separate cover page
  cp<-lapply(imp,function(x) x[[1]])
  for(i in 1:length(imp)) imp[[i]][1]<-NULL
  # ocr
  oc<-sapply(imp,sapply,length) %>% sapply(function(x) 3%in%x) %>% which
  if(length(oc)){
    cat('\n',length(oc),' undigitized documents detected.',sep='')
    if(!ocr) {
      cat(' Removing from corpus:',basename(jpdf[oc]),sep='\n')
      dropped<-jpdf[oc]
      jpdf<-jpdf[-oc]
      cp<-cp[-oc]
      imp<-imp[-oc]
    } else {
      cat(' Performing optical character recognition:')
      ol<-sapply(jpdf[oc],function(x) suppressMessages(pdftools::pdf_info(x))$pages)
      pb <- progress_bar$new(format = "  [:bar] :elapsed :eta",total = sum(ol)-length(ol), clear = FALSE, width= 60)
      ol<-sapply(ol,seq,from=2)
      pb$tick(0)
      for(i in 1:length(ol)) for(j in 1:length(ol[[i]])){
        x<-pdftools::pdf_convert(
          pdf = names(ol)[i]
          ,dpi=300
          ,pages=ol[[i]][j]
          ,filenames = sprintf('%s/%s%03d.png',tempdir(),sub('.pdf','',basename(names(ol[i]))),ol[[i]][j])
          ,verbose = F
        )
        imp[[oc[i]]][[j]]<-tesseract::ocr(x) %>% gsub('\n\n+','\n     ',.) %>% stringr::str_split('\n') %>% `[[`(1)
        pb$tick()
      }
    }
  }
  # metadata key
  met<-data.table(
    title=sapply(cp,function(x) {
      b<-grep('^Author',x)
      if(!length(b)) b<-grep('^Source',x)
      x[1:(b-1)] %>% paste(collapse=' ') %>% sub(' +',' ',.)
    })
    ,author=lapply(cp,function(x) {
      b<-grep('^Author',x)
      e<-grep('^Source',x)
      if((!length(b))|(!length(e))) return('')
      x[b:(e-1)] %>% paste(collapse=' ') %>% sub('^[^:]+: ','',.) %>% str_split(',|( and )') %>% unlist %>%
        sub('^ *','',.) %>% sub(' *$','',.) %>% sub(' +',' ',.) %>% `[`(.!='')
    })
    ,source=lapply(cp,function(x) {
      b<-grep('^Source',x)
      e<-grep('^Published',x)
      x[b:(e-1)] %>% paste(collapse=' ') %>% sub('^[^:]+: ','',.) %>% str_split('[,)(]') %>% unlist %>%
        sub('^ *','',.) %>% sub(' *$','',.) %>% sub(' +',' ',.) %>% `[`(.!='')
    })
    ,url=sapply(cp,function(x) {
      b<-grep('^Stable',x)
      x[b] %>% sub('^[^:]+: ','',.) %>% sub('^ *','',.) %>% sub(' *$','',.)
    })
    ,accessed=sapply(cp,function(x) {
      b<-grep('^Accessed',x)
      x[b] %>% sub('^[^:]+: ','',.) %>% sub('^ *','',.) %>% sub(' *$','',.)
    })
  )
  met[,doc:=jpdf]
  met[,`:=`(
    journal=sapply(source,`[`,1)
    ,year=sapply(source,function(x) grep('^[0-9]{4}$',x,value = T) %>% tail(1))
  )]
  setcolorder(met,c('doc','journal','year','title','author','source','url','accessed'))
  # header detection, accurate solution is too slow
  cat('\nHeader detection\n')
  clusth<-function(y) {
    y<-gsub(' +',' ',y)
    m<-1-stringdistmatrix(y,y,method = 'jw',p = .1)
    n<-graph_from_adjacency_matrix(m,'undirected',weighted=T)
    if(is_connected(n)) {c<-cluster_spinglass(n) %>% communities}else{c<-cluster_walktrap(n) %>% communities}
    r<-data.table(c=c)
    r[,s:=sapply(c,function(x) E(induced_subgraph(n,x))$weight%>% mean)]
    r<-r[sapply(c,length)>1]
    pb$tick()
    r
  }

  h<-lapply(imp,function(x) {
    d<-c(depth,sapply(x,length)) %>% min
    sapply(x,function(y) {head(y,d)}) %>% t %>% data.table %>% setnames(paste0('h',1:ncol(.)))})
  f<-lapply(imp,function(x) {
    d<-c(depth,sapply(x,length)) %>% min
    sapply(x,function(y) {tail(y,d)}) %>% t %>% data.table %>% setnames(paste0('f',1:ncol(.)))})

  il<-sapply(imp,length)==1
  pb <- progress_bar$new(format = "  [:bar] :elapsed :eta",total = sum(sapply(h,dim)[2,],sapply(f,dim)[2,]), clear = FALSE, width= 60)
  pb$tick(0)
  hc<-lapply(h[!il],function(x) lapply(x,clusth))
  fc<-lapply(f[!il],function(x) lapply(x,clusth))
  for(i in (1:length(imp))[!il]){
    l<-sapply(imp[[i]],length)
    drop<-sapply(hc[[i]],function(x) x[,mean(s)>.9&.N<5]) %>% which
    for(j in 1:length(l)) imp[[i]][[j]]<-imp[[i]][[j]][-c(drop,tail(1:l[j],depth)[sapply(fc[[i]],function(x) x[,mean(s)>.9&.N<5]) %>% which])]
  }
  # paragraph detection
  cat('\nParagraph detection\n')
  imp<-lapply(imp,function(x) {
    data.table(
      pag=1:length(x) %>% rep(sapply(x,length))
      ,lin=sapply(x,length) %>% sapply(seq) %>% unlist
      ,txt=x %>% unlist
    )})
  pb <- progress_bar$new(format = "  [:bar] :elapsed :eta",total = length(imp), clear = FALSE, width= 60)
  cn<-c('doc','pag','lin','par','txt')
  pb$tick(0)
  for(i in 1:length(imp)) {
    imp[[i]][,es:=grepl('[.!?\'\",]$',txt)]
    imp[[i]][,bp:=grepl(' {3,6}',txt)]
    imp[[i]][,par:=c(T,es[-.N]&bp[-1]) %>% cumsum]
    imp[[i]][,doc:=jpdf[i]]
    imp[[i]][,setdiff(names(imp[[i]]),cn):=NULL]
    setcolorder(imp[[i]],cn)
    pb$tick()
  }
  imp<-rbindlist(imp) %>% setkey(doc)
  setkey(met,doc)
  list(imp=imp,met=met)
}
