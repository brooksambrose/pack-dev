w2tab.f <- function(
  dbl2w
  ,out=NULL
  ,hg #higher (smaller) group classification
  ,lg #lower (larger) group classification
  ,sort #level by which all tables should be sorted in descending order
  ,s.e=F
  ,reps=10
  ,decreasing=T
  ,addlev=NULL
)
{
  library(data.table)
  w<-grep("w",names(dbl2w),value=T) #automatically processes any "weight" variables with a w- prefix
  nw<-nchar(w)
  lvar<-w[which.max(nw)]
  lvar<-gsub("(^w)|(tp$)","",lvar)
  nw<-nchar(lvar)
  lvar<-mapply(substr,lvar,seq(1,nw,2),seq(2,nw,2),USE.NAMES=F)
  lvar<-unique(c(lvar,addlev))
  dbl2w<-dbl2w[j=c(lvar,hg,lg,w),with=F]
  setkeyv(x=dbl2w,cols=lvar)

  t2prop<-function(x) if(!(is.character(x)|is.factor(x))) {prop.table(x)*100} else {x}

  tl.f<-function(dbl2w,wvar,levs,hg,lg,sort=c("key","order","given"),decreasing=T,subset=NULL){
    if(!is.null(subset)) {
      setkeyv(dbl2w,levs)
      dbl2w<-dbl2w[i=subset]
    }
    tl<-dbl2w[j=list(hc=sum(get(wvar))),keyby=hg]
    tl<-tl[dbl2w[j=list(lc=sum(get(wvar))),keyby=c(hg,lg)]]
    setkeyv(tl,c(hg,lg))
    if(sort[1]=="order") {o<-order(tl[[2]],tl[[4]],decreasing=decreasing);tl<-list(tl=tl,o=o)}
    if(sort[1]=="given") tl<-tl[i=sort,]
    tl
  }

  init<-tl.f(dbl2w,wvar=sort,sort="order",hg=hg,lg=lg)
  tl<-list(init$tl)
  names(tl)<-sort

  wo<-w[w!=sort]

  for(i in wo) tl[[i]]<-tl.f(dbl2w,wvar=i,hg=hg,lg=lg)
  tl<-tl[w]
  dup<-!duplicated(tl[[sort]][init$o,get(hg)])
  lh<-cumsum(dup)+1:nrow(tl[[sort]])
  h<-which(!(1:max(lh))%in%lh)
  hlo<-order(c(h,lh))
  ow<-grep("tp$",w,value=T,invert=T)
  if(s.e){
    se<-list()
    for(i in ow){
      wvar<-sub("tp$","",i)
      samplev<-unlist(sapply(as.list(addlev),FUN=function(x) sub(x,"",grep(x,wvar,value=T))))
      if(length(samplev)) {wvartp<-paste(samplev,"tp",sep="")} else {wvartp<-paste(wvar,"tp",sep="")}
      lvar<-gsub("(^w)|(tp$)","",wvartp) # level varnams cannot have w or tp in them, and must be exactly two characters, e.g. ut, af, cr
      levs<-mapply(substr,lvar,seq(1,nchar(lvar),2),seq(2,nchar(lvar),2),USE.NAMES=F)
      samp<-list()
      for(j in levs) samp[[j]]<-dbl2w[j=!is.na(get(j))]
      samp<-do.call(cbind,samp)
      if(ncol(samp)>1) for(j in 2:ncol(samp)) samp[,1]<-samp[,1]&samp[,j]
      samp<-as.vector(samp[,1])
      samp<-dbl2w[i=samp,NA,keyby=levs][,levs,with=F]
      cat(i,"\n")
      se[[i]]<-replicate(reps
                         ,tl.f(dbl2w,wvar=wvar
                               ,levs=levs
                               ,subset=samp[i=sample(1:nrow(samp),sum(dbl2w[[wvartp]]))]
                               ,hg=hg
                               ,lg=lg
                         )
                         ,simplify=F)

      J<-se[[i]][[1]][j=c(1,3,2,4),with=F][tl[[i]][j=c(1,3),with=F]]
      for(j in 2:length(se[[i]])) J[,paste(c("hc","lc"),j,sep=""):=as.list(se[[i]][[j]][j=c(1,3,2,4),with=F][tl[[i]][j=c(1,3,2,4),with=F]][,c("hc","lc"),with=F])]
      J[is.na(J)]<-0
      J<-J[init$o,]
      se[[i]]<-cbind(
        unlist(c(J[dup,j=hg,with=F],J[j=lg,with=F]))
        ,rbindlist(list(
          J[i=dup,j=grep("hc",names(J),value=),with=F]
          ,J[j=grep("lc",names(J),value=),with=F]))
      )[hlo]


      se[[i]]<-list(f=se[[i]],p=copy(se[[i]]))
      col<-colnames(se[[i]]$p)[-1]
      se[[i]]$p[h,col:=lapply(se[[i]]$p[h,col,with=F],t2prop),with=F]
      se[[i]]$p[!h,col:=lapply(se[[i]]$p[!h,col,with=F],t2prop),with=F]
      ssr<-function(x) sum(round(x,4)^2)
      sr<-function(x) sum(round(x,4))
      if(length(h)==nrow(se[[i]]$f)) {
        se[[i]]$p<-data.frame(level=c(as.character(se[[i]]$p[[1]]),"High Total","High H Index"),rbind(
          se[[i]]$p[h,!1,with=F]
          ,se[[i]]$p[h,lapply(.SD, sr),.SDcols=-1]
          ,se[[i]]$p[h,lapply(.SD, ssr),.SDcols=-1]
        ))
        se[[i]]$f<-data.frame(level=c(as.character(se[[i]]$f[[1]]),"High Total"),rbind(
          se[[i]]$f[h,!1,with=F]
          ,se[[i]]$f[h,lapply(.SD, sr),.SDcols=-1]
        ))
      } else {
        se[[i]]$p<-data.frame(level=c(as.character(se[[i]]$p[[1]]),"High Total","Low Total","High H Index","Low H Index"),rbind(
          se[[i]]$p[,!1,with=F]
          ,se[[i]]$p[h,lapply(.SD, sr),.SDcols=-1]
          ,se[[i]]$p[!h,lapply(.SD, sr),.SDcols=-1]
          ,se[[i]]$p[h,lapply(.SD, ssr),.SDcols=-1]
          ,se[[i]]$p[!h,lapply(.SD, ssr),.SDcols=-1]
        ))
        se[[i]]$f<-data.frame(level=c(as.character(se[[i]]$f[[1]]),"High Total","Low Total"),rbind(
          se[[i]]$f[,!1,with=F]
          ,se[[i]]$f[h,lapply(.SD, sr),.SDcols=-1]
          ,se[[i]]$f[!h,lapply(.SD, sr),.SDcols=-1]
        ))
      }
      se[[i]]$sht.p<-try(do.call(rbind,apply(se[[i]]$p[,-1],1,FUN=function(x) shapiro.test(x)[1:2]))) #shapiro test of normality of p table
      for(j in c("f","p")) {se[[i]][[j]]<-data.frame(level=se[[i]][[j]][[1]],s.e.=apply(se[[i]][[j]][,-1],1,sd));se[[i]][[j]][[2]]<-round(se[[i]][[j]][[2]],4)}
    }
  }

  t2prop<-function(x) if(!(is.character(x)|is.factor(x))) {round(prop.table(x)*100,3)} else {x}
  for(j in names(tl)) {
    tl[[j]]<-tl[[j]][init$o]
    tl[[j]]<-cbind(
      unlist(c(tl[[j]][dup,j=hg,with=F],tl[[j]][j=lg,with=F]))
      ,rbindlist(list(
        tl[[j]][i=dup,j=grep("hc",names(tl[[j]]),value=),with=F]
        ,tl[[j]][j=grep("lc",names(tl[[j]]),value=),with=F]))
    )[hlo]

    tl[[j]]<-list(f=tl[[j]],p=copy(tl[[j]]))
    col<-colnames(tl[[j]]$p)[-1]
    tl[[j]]$p[h,col:=lapply(tl[[j]]$p[h,col,with=F],t2prop),with=F]
    tl[[j]]$p[!h,col:=lapply(tl[[j]]$p[!h,col,with=F],t2prop),with=F]

    ssr<-function(x) sum(round(x,3)^2)
    sr<-function(x) sum(round(x,3))
    if(length(h)==nrow(tl[[j]]$f)) {
      tl[[j]]$p<-data.frame(level=c(as.character(tl[[j]]$p[[1]]),"High Total","High H Index"),rbind(
        tl[[j]]$p[h,!1,with=F]
        ,tl[[j]]$p[h,lapply(.SD, sr),.SDcols=-1]
        ,tl[[j]]$p[h,lapply(.SD, ssr),.SDcols=-1]
      ))
      tl[[j]]$f<-data.frame(level=c(as.character(tl[[j]]$f[[1]]),"High Total"),rbind(
        tl[[j]]$f[h,!1,with=F]
        ,tl[[j]]$f[h,lapply(.SD, sr),.SDcols=-1]
      ))
    } else {
      tl[[j]]$p<-data.frame(level=c(as.character(tl[[j]]$p[[1]]),"High Total","Low Total","High H Index","Low H Index"),rbind(
        tl[[j]]$p[,!1,with=F]
        ,tl[[j]]$p[h,lapply(.SD, sr),.SDcols=-1]
        ,tl[[j]]$p[!h,lapply(.SD, sr),.SDcols=-1]
        ,tl[[j]]$p[h,lapply(.SD, ssr),.SDcols=-1]
        ,tl[[j]]$p[!h,lapply(.SD, ssr),.SDcols=-1]
      ))
      tl[[j]]$f<-data.frame(level=c(as.character(tl[[j]]$f[[1]]),"High Total","Low Total"),rbind(
        tl[[j]]$f[,!1,with=F]
        ,tl[[j]]$f[h,lapply(.SD, sr),.SDcols=-1]
        ,tl[[j]]$f[!h,lapply(.SD, sr),.SDcols=-1]
      ))
    }

  }

  tab<-list()
  ow<-names(sort(dbl2w[,lapply(.SD,sum),.SDcols=ow]))
  for(j in ow){
    tab[[j]]<-data.frame(tl[[j]]$p,tl[[paste(j,"tp",sep="")]]$p)[,-3]
    tab[[j]]<-data.frame(tab[[j]],apply(tab[[j]][,-1],1,diff))
    if(s.e) tab[[j]]<-data.frame(tab[[j]],se[[j]]$p[[2]],tab[[j]][[4]]/se[[j]]$p[[2]])
    tab[[j]][,-1]<-round(tab[[j]][,-1],3)
    if(s.e) {colnames(tab[[j]])<-c("l","o","s","Δ","se","t")} else {colnames(tab[[j]])<-c("l","o","s","Δ")}
    wt<-grep("Total",tl[[j]]$f$level)
    tot<-tl[[paste(j,"tp",sep="")]]$f[wt,]
    tot[[1]]<-sub("Total","N",tot[[1]])
    if(s.e) {tot<-data.frame(tot[[1]],tl[[j]]$f[wt,2],tot[[2]],round((tot[[2]]-tl[[j]]$f[wt,2])/tl[[j]]$f[wt,2]*100,3),rep(NA,length(wt)),rep(NA,length(wt)))
    } else{tot<-data.frame(tot[[1]],tl[[j]]$f[wt,2],tot[[2]],round((tot[[2]]-tl[[j]]$f[wt,2])/tl[[j]]$f[wt,2]*100,3))}
    if(s.e) {colnames(tab[[j]])<-c("l","o","s","Δ","se","t")} else {colnames(tab[[j]])<-c("l","o","s","Δ")}
    if(s.e) {colnames(tot)<-c("l","o","s","Δ","se","t")} else {colnames(tot)<-c("l","o","s","Δ")}
    tab[[j]]<-rbind(tab[[j]],tot)
  }
  tab<-tab[ow]
  tab<-data.frame(tab[[1]],lapply(tab[-1],FUN=function(x) {x[[1]]<-NULL;x}))
  cn<-grep("\\.",names(tab),value=T,invert=T)
  colnames(tab)[colnames(tab)%in%cn]<-paste(ow[1],cn,sep=".")
  colnames(tab)<-sub("^w","",colnames(tab))
  colnames(tab)[1]<-"level"
  ro<-grepl("\\.o$",colnames(tab))
  tab<-data.frame(tab,replicate(sum(ro),rep(NA,nrow(tab)),simplify=F))[,order(c(1:ncol(tab),which(ro)-.5))]
  ro<-grepl("\\.o$",colnames(tab))
  colnames(tab)[which(ro)-1]<-""
  tab[grep("Total",tab[,1]),grep("(\\.se$)|(\\.t$)",colnames(tab))]<-NA
  if(!is.null(out)) write.table(tab,file=paste(out,"selection_table.tab",sep=""),sep="\t",quote=F,row.names=F,na="")
  w2tab
}
