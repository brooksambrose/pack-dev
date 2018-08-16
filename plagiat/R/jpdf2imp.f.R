#' JSTOR PDF to imp
#'
#' @param jpdf
#' @param ocr
#' @param depth
#' @param ncores
#' @param logdir
#'
#' @return
#' @export
#'
#' @examples
jpdf2imp.f<-function(jpdf,ocr=F,depth=5,ncores,logdir){
  library(data.table)
  library(stringr)
  library(pdftools)
  library(stringdist)
  library(igraph)
  library(progress)

  # parallel always seems slower
  if(missing(ncores)) ncores<-c(1,parallel::detectCores()/2 %>% ceiling) %>% max
  ncores<-1

  # import
  jimp<-function(x) {
    x<-pdf_text(x) %>% as.list
    # find last cover page, usually only one except in long titles
    w<-grep('collaborating with JSTOR to digitize preserve and extend access' %>% strsplit(' ') %>% unlist %>% paste(collapse='[\n, ]+'),x)[1]
    if(w>1){
      x[w]<-x[1:w] %>% unlist %>% paste(collapse='\n')
      x[0:(w-1)]<-NULL
      x[[1]] %<>% sub('This content downloaded from[^\n]+','',.) %>% sub('All use subject to[^\n]+','',.)
    }
    x<-strsplit(unlist(x),'\n')
    attr(x,'ncp')<-w
    x
  }
  cat('Importing text from',length(jpdf),'pdfs:\n')
  imp<-pbapply::pblapply(jpdf,cl=ncores,jimp)

  # separate cover page
  cp<-lapply(imp,function(x) x[[1]])
  for(i in 1:length(imp)) imp[[i]][1]<-NULL
  ncp<-sapply(imp,attr,'ncp')

  # ocr
  oc<-lapply(imp,sapply,length) %>% sapply(function(x) any((1:3)%in%x)) %>% which
  if(length(oc)){
    cat('\n',length(oc),' undigitized documents detected.',sep='')
    if(!missing(logdir)) writeLines(jpdf[oc],paste(logdir,'log_badocr.txt',sep=.Platform$file.sep))
    if(!ocr) {
      cat(' Removing from corpus:',basename(jpdf[oc]),sep='\n')
      dropped<-jpdf[oc]
      jpdf<-jpdf[-oc]
      cp<-cp[-oc]
      imp<-imp[-oc]
    } else {
      ol<-sapply(jpdf[oc],function(x) suppressMessages(pdftools::pdf_info(x))$pages,simplify = F,USE.NAMES = T)
      ol<-mapply(seq,to=ol,from=ncp[oc]+1,USE.NAMES = T,SIMPLIFY = F)
      ol<-mapply(function(d,p) list(d=d,p=p)
                 ,d=rep(names(ol),sapply(ol,length))
                 ,p=unlist(ol)
                 ,SIMPLIFY = F,USE.NAMES = F)
      cat(' Performing optical character recognition on',length(ol),'pages:\n')

      tocr<-function(y) {
        x<-pdftools::pdf_convert(
          pdf = y$d
          ,dpi=300
          ,pages=y$p
          ,filenames = tempfile(pattern = sub('.pdf',sprintf('_%03d_',y$p),basename(y$d)),fileext = '.png')
          ,verbose = F
        )
        invisible(tesseract::ocr(x)) %>% gsub('\n\n+','\n     ',.) %>% strsplit('\n') %>% `[[`(1)
      }

      imp[oc]<-pbapply::pblapply(ol,cl=ncores,tocr) %>% split(sapply(ol,`[[`,'d'))

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
  met[,ncp:=ncp]
  setcolorder(met,c('doc','journal','year','title','author','source','url','accessed','ncp'))
  # header detection, accurate solution is too slow
  clusth<-function(y) {
    y<-gsub(' +',' ',y)
    m<-1-stringdistmatrix(y,y,method = 'jw',p = .1)
    n<-graph_from_adjacency_matrix(m,'undirected',weighted=T)
    if(is_connected(n)) {c<-cluster_spinglass(n) %>% communities}else{c<-cluster_walktrap(n) %>% communities}
    r<-data.table(c=c)
    r[,s:=sapply(c,function(x) E(induced_subgraph(n,x))$weight%>% mean)]
    r<-r[sapply(c,length)>1]
    r
  }

  h<-lapply(imp,function(x) {
    d<-c(depth,sapply(x,length)) %>% min
    sapply(x,function(y) {head(y,d)}) %>% t %>% data.table %>% setnames(paste0('h',1:ncol(.)))})
  f<-lapply(imp,function(x) {
    d<-c(depth,sapply(x,length)) %>% min
    sapply(x,function(y) {tail(y,d)}) %>% t %>% data.table %>% setnames(paste0('f',1:ncol(.)))})

  # headers not removed from single page articles, need to revisit at volume level rather than paper level
  il<-sapply(imp,length)==1
  cat('\nHeader detection:\n')
  hc<-pbapply::pblapply(h[!il],cl=ncores,function(x) lapply(x,clusth))
  cat('\nFooter detection:\n')
  fc<-pbapply::pblapply(f[!il],cl=ncores,function(x) lapply(x,clusth))
  ix<-(1:length(imp))[!il]
  for(i in ix){
    l<-sapply(imp[[i]],length)
    hdrop<-sapply(hc[[which(ix==i)]],function(x) x[,mean(s)>.9&.N<5]) %>% which
    for(j in 1:length(l)) {
      fdrop<-tail(1:l[j],depth)[sapply(fc[[which(ix==i)]],function(x) x[,mean(s)>.9&.N<5]) %>% which]
      imp[[i]][[j]]<-imp[[i]][[j]][-c(hdrop,fdrop) %>% na.omit]
    }
  }

  # paragraph detection
  cat('\nParagraph detection:\n')
  par<-function(i,cn=c('doc','pag','lin','par','txt')) {
    x<-imp[[i]]
    pag<-1:length(x) %>% rep(sapply(x,length))
    lin<-sapply(x,length) %>% `[`(!!.) %>% lapply(seq) %>% unlist
    txt<-x %>% unlist
    if(length(pag)!=length(lin)|length(lin)!=length(txt)|length(pag)!=length(txt)) browser()
    x<-data.table(
      pag
      ,lin
      ,txt
    )
    x[,es:=grepl('[.!?\'\",]$',txt)]
    x[,bp:=grepl(' {3,6}',txt)]
    x[,par:=c(T,es[-.N]&bp[-1]) %>% cumsum]
    x[,doc:=jpdf[i]]
    x[,setdiff(names(x),cn):=NULL]
    setcolorder(x,cn)
    x
  }
  imp<-pbapply::pblapply(1:length(imp),cl=ncores,par)
  imp<-rbindlist(imp) %>% setkey(doc)
  setkey(met,doc)
  list(imp=imp,met=met)
}
