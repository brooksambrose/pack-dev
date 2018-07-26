rm(list = ls())
cat('\014')

# Step 1 Import ------------------------------------------------------------------
#' Heading to Variable
#'
#' @return
#' @export
#'
#' @examples
hed2var<-function(x,p='[A-Za-z0-9]$'){
  require(data.table)
  hed<-grep(p,x$txt)
  n<-diff(c(hed,length(x$txt)+1))-1
  data.table(x[-hed],hed=rep(x$txt[hed],n))
}

#' Six Ages Saga to Full Text
#'
#' @return
#' @export
#'
#' @examples
sas2imp.f<-function(sas,rbl=T,h2v=T){
  require(magrittr)
  require(data.table)

  imp<-list()
  for(i in sas){
    imp[[i]]<-data.table(txt=readLines(i))
    imp[[i]][,`:=`(
      doc=i
      ,lin=1:.N
    )]
    #add title
    t<-imp[[i]][1,txt]
    imp[[i]]<-imp[[i]][-1]
    imp[[i]][,title:=t]
    #remove blanks, keeping line index
    if(rbl) imp[[i]]<-imp[[i]][txt!='']
    if(h2v) imp[[i]]<-hed2var(imp[[i]])
  }
  rbindlist(imp)
}

sas<-dir(pattern = 'scratch[0-9]')
imp<-sas2imp.f(sas)


# Step 2 Full Text ------------------------------------------------------------------
#' Import to Full Text database
#'
#' @param imp
#'
#' @return
#' @export
#'
#' @examples
imp2ftx.f<-function(imp){
  require(data.table)
  require(magrittr)
  require(stringr)

  if(!is.data.table(imp)) imp<-imp$imp
  p<-'pag'%in%names(imp)
  ftx<-imp[,.(txt=str_split(txt,boundary('sentence')) %>% unlist),by = setdiff(names(imp), 'txt')]
  if(p) {
    ftx[,sen:=c(T,grepl('[.?!]["\' )]*$',txt[-.N])) %>% cumsum,by=doc]
  } else {
    ftx[,sen:=1:.N,by=doc]
  }
  ftx<-ftx[,.(txt=str_split(txt,boundary('word',skip_word_none = F)) %>% unlist),by = setdiff(names(ftx), 'txt')]
  ftx[,cha:=cumsum(nchar(txt))-nchar(txt)+1,by=.(doc,lin)]
  o<-intersect(c('doc','lin','cha','pag','par','sen','txt'),names(ftx))
  setcolorder(ftx,c(o,setdiff(names(ftx),o)))
  ftx<-ftx[txt!=' ']
  ftx
}

#' Full text browser
#'
#' @param ftx
#'
#' @return
#' @export
#'
#' @examples
ftxbro<-function(ftx){
  if(nrow(ftx)>1) stop('Pass one location at a time.')
  if('pag'%in%names(ftx)) {
    getOption('viewer')(ftx[,paste0(doc,'#page=',pag+1)])
    x<-ftx[,pdftools::pdf_text(doc)[lin]]
  } else {
    x<-do.call(scan,ftx[1,.(what="character",file=doc,nmax=1,skip=lin-1,sep='\n')])
  }
  x<-c(
    stringr::str_sub(x,end=ftx[1,cha-1])
    ,stringr::str_sub(x,start=ftx[1,cha])
  )
  cat(x,sep='\n\t')
  invisible(x)
}

ftx<-imp2ftx.f(imp)
print(ftx)
ftx[230] %>% ftxbro

# Step 3 Pre-process ------------------------------------------------------------------

#' Full text to pre-processed text.
#'
#' @param ftx
#'
#' @return
#' @export
#'
#' @examples
ftx2pre.f<-function(ftx,pick=F,pl='pl.RData',lvl='sen',frq=1){
  require(data.table)
  require(magrittr)
  require(tm)
  require(qdap)
  ftx[,`:=`(
    lem=txt %>%
      replace_abbreviation %>%
      replace_contraction %>%
      replace_ordinal %>%
      replace_number %>%
      replace_symbol %>%
      tolower %>% stemDocument(language='english') %>% gsub('[^a-z]','',.)
    ,sto=tolower(txt) %in% stopwords('english')
    ,num=grepl('[0-9]',txt)
    ,pun=!grepl('[A-Za-z0-9]',txt)
  )]
  ftx[,com:={
    w<-tolower(txt)
    t<-table(w) %>% sort %>% tail(1) %>% names
    txt[which(w==t)] %>% sort %>% `[`(1) %>% gsub('[^A-Za-z]','',.)
  },by=lem]
  ftx[com=='',com:=lem]

  # Step 3.5 Dendrogram Picker ----------------------------------------------

  if(pick){
    require(stringdist)
    m<-ftx[!(sto|pun),com %>% unique %>% sort] %>%
      grep('^[A-Z]',.,value=T) %>%
      stringdistmatrix(useNames = T,method='jw',p=.1)
    h<-hclust(m) %>% as.dendrogram()
    library(plagiat)
    library(tilit)
    if(file.exists(pl)) {load(pl)} else {f<-pl;pl<-strdist.dend.picker(h,instruct = T);save(pl,file=f)}

    # update the lemma and common terms with new sets

    for(i in pl) {
      ftx[txt%in%i,`:=`(
        lem={j<-table(txt);names(j)[which.max(j)] %>% tolower}
        ,com={j<-table(txt);names(j)[which.max(j)]}
      )]
    }
    ftx[,pck:=txt%in%unlist(pl)]
  }

  # Step 3.6 Pre-process to Cull --------------------------------------------

  doc<-unique(ftx[,.(doc,com)])[,.(doc=.N),by=com] %>% setkey(com) # number of documents a word appears in
  hed<-unique(ftx[,.(doc,hed,com)])[,.(hed=.N),by=com] %>% setkey(com)
  lin<-unique(ftx[,.(doc,hed,lin,com)])[,.(lin=.N),by=com] %>% setkey(com)
  sen<-unique(ftx[,.(doc,hed,lin,sen,com)])[,.(sen=.N),by=com] %>% setkey(com)
  tdd<-doc[hed][lin][sen] %>% setkey(com)

  ftx[,cul:=(com%in%tdd[get(lvl)<=frq,com])]
  ftx[,stm:=factor(com,levels=com[!(cul|com==''|pun|sto)] %>% unique %>% sort)]
  # ~ fin ~ -----------------------------------------------------------------
  ftx
}

pre<-ftx2pre.f(ftx)
rm(ftx)
pre

# Step 3.7 Pre-process to Multi Level Corpus ------------------------------

#' Pre-process to Multi Level Corpus
#'
#' @param pre
#' @param tdd
#'
#' @return
#' @export
#'
#' @examples
pre2mlc.f<-function(pre){
  require(data.table)
  setkey(pre,doc,lin,cha)
  f<-function(sd) pre[,.(list({x<-stm %>% as.numeric %>% table;x<-rbind(names(x) %>% as.numeric,x);attr(x,'dimnames')<-NULL;x})),by=sd]$V1
  mlc<-list(
    doc=f('doc')
    ,hed=f(c('doc','hed'))
    ,lin=f(c('doc','hed','lin'))
    ,sen=f(c('doc','hed','lin','sen'))
    ,voc=pre[,levels(stm)]
  )
  mlc
}

mlc<-pre2mlc.f(pre)

# Step 4 Term Adjacency Matrix ---------------------------------------------------

pre2tam.f<-function(pre,stop=100){
  require(progress)
  require(data.table)
  tam<-pre[!(pun|sto),.(doc,lin,cha,sen,com)]
  setkey(tam,doc,sen)
  r<-list()
  pb <- progress_bar$new(format = "  [:bar] :elapsedfull :eta",total = tam[,.(doc,sen)] %>% unique %>% nrow, clear = FALSE, width= 60)

  for(i in tam[,unique(doc)]) for(j in tam[i,unique(sen)]) {pb$tick();for(k in tam[i][sen>=j,unique(sen)]) {
    if(k-j>=stop) next
    r[[length(r)+1]]<-expand.grid(s=tam[.(i,j),com],r=tam[.(i,k),com],KEEP.OUT.ATTRS = F, stringsAsFactors = F) %>% apply(1,sort) %>% t %>% data.table(doc=i,.,ew=1/(k-j+1))
  }}
  r<-rbindlist(r)
  setnames(r,c('doc','s','r','ew'))
  r<-r[,.(ew=sum(ew)),by=.(doc,s,r)]
  r
}


# Step 5 Topic Model ------------------------------------------------------

tam<-pre2tam.f(pre[!is.na(stm)],stop = 3)
fam<-tam[,.(ew=sum(ew)),by=.(s,r)]

library(igraph)
g<-graph_from_edgelist(fam[,.(s,r)] %>%  as.matrix,directed = F)
E(g)$weight<-fam[,ew]
c<-cluster_louvain(g) %>% communities
str(c)

mod<-list()

mod$`0`<-stm(
  documents = mlc$sen
  ,vocab = mlc$voc
  ,K = length(c)
  ,max.em.its = 0
  ,init.type = 'Random'
)
mod$`0`$beta$logbeta %>% str

setkey(pre,com)
logbeta<-sapply(c,function(x) pre[x,table(stm)] %>% prop.table) %>% t %>% log
dimnames(logbeta)<-NULL
logbeta %>% str


mod$`1_1`<-stm(
  documents = mlc$sen
  ,vocab = mlc$voc
  ,K = length(c)
  ,init.type = 'Spectral'
)

mod$`2_1`<-stm(
  documents = mlc$sen
  ,vocab = mlc$voc
  ,K = length(c)
  ,init.type = 'Custom'
  ,control=list(custom.beta=list(logbeta))
)

mod$`1_2`<-stm(
  documents = mlc$lin
  ,vocab = mlc$voc
  ,K = length(c)
)

mod$`2_2`<-stm(
  documents = mlc$lin
  ,vocab = mlc$voc
  ,K = length(c)
  ,init.type = 'Custom'
  ,control=list(custom.beta=list(logbeta))
)

mod$`3_2`<-stm(
  documents = mlc$lin
  ,vocab = mlc$voc
  ,K = length(c)
  ,init.type = 'Custom'
  ,control=list(custom.beta=mod$`1_1`$beta$logbeta)
)
mod$`3_2`<-stm(
  documents = mlc$lin
  ,vocab = mlc$voc
  ,K = length(c)
  ,init.type = 'Custom'
  ,control=list(custom.beta=mod$`1_1`$beta$logbeta)
  ,model = mod$`3_2`
  ,max.em.its = 1000
)

plotModels(
  list(
   runout=mod
  ,semcoh=lapply(mod,semanticCoherence,documents=mlc$lin)
  ,exclusivity=lapply(mod,exclusivity)
  )
  ,legend.position="bottomleft"
)



mod$`3`<-stm(
  documents = tp$documents
  ,vocab = tp$vocab
  ,K = 0 #length(c)
  ,prevalence = ~doc
  ,data = tp$meta
  # ,init.type = 'Custom'
  # ,control=list(custom.beta=list(logbeta))
  ,verbose = T
)

plot(mod$`0`$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence")

setkey(pre,doc,lin,cha)
fxt<-pre[!is.na(stm),.(doc,lin)] %>% unique
setkey(pre,doc,lin)
fxt<-pre[fxt,.(doc,lin,cha,txt)]
fxt[,e:=cha+nchar(txt)]
fxt[,s:=c(1,cha[-1]-e[-.N])]
fxt[s<0,s:=1]
fxt<-fxt[,.(txt=paste0(c('',' ')[s+1],txt,collapse='')),by=.(doc,lin)]


stmCorrViz(
  mod$1_2
  ,"corrviz1_2.html"
  ,title = "1_2 Spectral Line"
  ,documents_raw=fxt[,txt]
  ,documents_matrix=prepDocuments(mlc$lin,mlc$voc,lower.thresh = 0)$documents
  ,verbose=T
  ,clustering_threshold = F
)

stmCorrViz(
  mod$3_2
  ,"corrviz3_2.html"
  ,title = "3_2 Custom Nest Line"
  ,documents_raw=fxt[,txt]
  ,documents_matrix=prepDocuments(mlc$lin,mlc$voc,lower.thresh = 0)$documents
  ,verbose=T
  ,clustering_threshold = F
)

# source('R/stmbow2lda.f.R')
# mod$0<-stmbow2lda.f(stmbow = tp,out.dir = '.')

logbeta %>% str
mod$beta$logbeta %>% str

sm<-selectModel(  documents = tp$documents
                  ,vocab = tp$vocab
                  ,K = length(c)
                  ,prevalence = ~doc
                  ,data = tp$meta
                  ,max.em.its = 200
)



# Model Commensuration ----------------------------------------------------

stm1<-mod$`1_2`
stm2<-mod$`3_2`

stm2cmn<-function(stm1,stm2,pre){
  # convert doc.top to term frequencies/word counts
  dtc<-pre[,.(stm %>% is.na %>% `!` %>% sum),by=.(doc,lin)]$V1 # document term count
  dtc<-sweep(stm1$theta,1,dtc,'*')
  # convert top.ter to term frequencies/word counts
  ttc<-apply(dtc,2,sum)
  ttp<-prop.table(ttc)
  tso<-data.table(`%`=(ttp*100) %>% round(1))[,T:=.I] %>% setcolorder(2:1) %>% setorder(-`%`)
  ttc<-sweep(stm1$beta$logbeta %>% `[[`(1) %>% exp,1,ttc,'*')

}

stmCorrViz(
  stm1
  ,"corrviz1_2.html"
  ,title = "1_2 Spectral Line"
  ,documents_raw=fxt[,txt]
  ,documents_matrix=prepDocuments(mlc$lin,mlc$voc,lower.thresh = 0)$documents
  ,verbose=T
  ,clustering_threshold = F
)


library(igraph)
dtn<-dtc
dimnames(dtn)<-list(doc=paste0('D',1:nrow(dtn)),top=paste0('T',1:ncol(dtn),'-',tso$T))

dn<-dtn%*%t(dtn)
tn<-t(dtn)%*%dtn

g<-graph_from_adjacency_matrix(tn,weighted = T,mode = 'upper')

c<-g %>% cluster_spinglass %>% communities
str(c)
c

ttn<-ttc
dimnames(ttn)<-list(top=paste0('T',1:nrow(ttn),'-',tso$T),ter=mlc$voc)
tn<-ttn%*%t(ttn)
dn<-t(ttn)%*%ttn

g<-graph_from_adjacency_matrix(tn,weighted = T,mode = 'upper')

c<-g %>% cluster_spinglass %>% communities
str(c)
c

# JSTOR -------------------------------------------------------------------

u<-paste0('https://www.jstor.org/action/showJournals?contentType=journals&letter=',c('0-9',LETTERS))

master2jstorm.f<-function(pageurl,xp='//*[@id="content"]/div[1]/div/div[4]/div/table'){
  cat(pageurl,'\n',sep='')
  dt<-read_html(pageurl)
  t<-dt %>% html_nodes(xpath=xp) %>% html_table %>% `[[`(1) %>% data.table
  t[,c('start','stop'):=(tstrsplit(`Published Date`,'-',type.convert = T))]
  t[,`Published Date`:=NULL]
  setnames(t,old = 1,new = 'title')
  t
}
if(file.exists('jstorm.RData')){
  load('jstorm.RData')
} else {
  jstorm<-list()
  for(i in u[which(u%in%i):length(u)]){
    jstorm[[i]]<-try(master2jstorm.f(i))
    Sys.sleep(rnorm(1) %>% abs %>% `+`(.5))
  }
  jstorm<-jstorm[!sapply(jstorm,inherits,'try-error')] %>% rbindlist
  save(jstorm,file='jstorm.RData')
}
long<-jstorm[start<1900&stop>2000,]
long[,lang:=list(lapply(title,cld2::detect_language_mixed))]
long[,`:=`(
  bg=sapply(lang,function(x) x[[1]][1,1])
  ,pg=sapply(lang,function(x) x[[1]][1,4])
)]
setorder(long,bg,-pg,start)
View(long)

# JSTOR full text import --------------------------------------------------

#txt doc lin
#' JSTOR PDF to imp
#'
#' @param jpdf
#'
#' @return
#' @export
#'
#' @examples
jpdf2imp<-function(jpdf,ocr=F,depth=5){
  require(data.table)
  require(stringr)
  require(pdftools)
  require(stringdist)
  require(igraph)
  require(progress)
  # require(parallel)
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

jpdf<-jpdf2imp(dir('1895',full.names = T
                   #,pattern = '(Hirsch)|(Jenks)'
))
save(jpdf,file='jpdf.RData')
#    getOption('viewer')(jpdf %>% paste0('#page=3'))
load('jpdf.RData')
ftx<-imp2ftx.f(jpdf)
ftx[1001] %>% ftxbro

