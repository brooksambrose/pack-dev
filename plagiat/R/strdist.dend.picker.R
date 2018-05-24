strdist.dend.picker <- function(
  hd # must be dendrogram
  ,s1=c(0,.075)
  ,s2=c(0,1)
  ,lightest.val=c(">"=.5,"="=.8)
  ,maxlist=50
  ,must.be.this.short.to.ride=.075
  ,out=NULL
  ,instruct=F
)
{

  require(dendextend)
  require(data.table)

  hs<-get_nodes_attr(hd,"height",simplify=T)
  l<-which(hs==0)
  n<-which(hs!=0)
  oc<-sapply(as.list(l),function(x) hs[n[which.max(which(n<x))]])
  names(oc)<-labels(hd)
  c<-lintran(oc,s1=s1,s2=s2)
  c[c>lightest.val[">"]]<-lightest.val["="]
  labels_colors(hd)<-gray(c)

  hk<-heights_per_k.dendrogram(hd)

  for(i in 1:length(hk)) if(max(table(cutree_1k.dendrogram(hd,k=as.integer(names(hk[i])),use_labels_not_values=FALSE,dend_heights_per_k=hk)))<=maxlist) break
  #if(i==1) maxlist<-as.integer(names(hk[i]))
  hdc<-cutree_1k.dendrogram(hd,k=as.integer(names(hk[i])),use_labels_not_values=T,dend_heights_per_k=hk)
  if(length(hdc)==2) hdc[2]<-1
  j<-i-1
  hkh<-hk[ifelse(j!=0,j,1)]

  df<-data.frame(get_nodes_xy(hd),get_nodes_attr(hd,"label"))
  names(df)<-c("x","y","lab")
  df<-df[!is.na(df$lab),]
  rownames(df)<-df$lab
  df[names(oc),"lh"]<-oc
  df[names(hdc),"hdc"]<-hdc
  rownames(df)<-NULL

  df<-data.table(df,key="lab")

  hdc<-split(hdc,hdc)
  hd<-color_branches(hd,k=length(hdc))
  lhdc<-length(hdc)

  graphics.off()
  sets<-list()
  if(instruct) cat("\rLeft-click to left of name to choose members of a set.\nWhen set is finished, right-click once to start a new set, or twice to advance the page.\nIf you want to accept the entire page, left-click five times, then right click twice.\nIf you make a mistake before starting a new set, select the same name three times, right-click to start a new set, and begin again.")
  c<-0

  for(i in 1:length(hdc)){
    sub<-names(hdc[[i]])
    if(min(df[sub,y])>must.be.this.short.to.ride) next
    yl<-range(df[sub,x],max(df[sub,x])-maxlist)
    #cx<-strheight(sub[1])*1.5*length(sub)/diff(yl)*10
    cx<-.7
    par(mai=c(.75,0,0,strwidth(sub[which.max(nchar(sub))],units="inches")*cx+1),cex=cx)
    plot(hd,"triangle",horiz=T,ylim=yl)
    te<-try(rect.dendrogram(hd,k=lhdc,which=lhdc-i+1,horiz=T,border="red",lty="dotted",hpk=hk),silent=T)
    if(!inherits(te,"try-error")) rect.dendrogram(hd,k=lhdc,which=lhdc-i+1,horiz=T,border="red",lty="dotted",hpk=hk)
    loc<-NA
    e<-1
    while(!is.null(loc)) {
      ind<-length(sets)+1
      nind<-rev(unlist(strsplit(as.character(ind),"")))[1]
      loc<-locator(type="p",pch=nind,col="red",cex=.5)$y
      if(is.null(loc)) break
      sets[[ind]]<-labels(hd)[round(loc)]
      if(any(table(sets[[ind]])%in%3:4)) sets[[ind]]<-sub
      segments(x0=0,y0=which(labels(hd)%in%sets[[ind]])
               ,x1=0 #max(strwidth(sets[[ind]]))
               ,lwd=10,col=hsv(.1*e,1,1,.5))
      e<-e+1
    }
    if(!is.null(out)) {
      c<-c+1
      dev.print(pdf,paste(out,c,file="picker_log.pdf",sep=""))
    }
  }
  sets[sapply(sets,function(x) any(table(x)>=5))]<-NULL
  sets<-lapply(sets,unique)
  sets[sapply(sets,function(x) any(sapply(setdiff(sets,list(x)),function(y) all(x%in%y))))]<-NULL
  if(!is.null(out)) save(sets,file=paste(out,file="picker_sets.RData",sep=""))
  sets
}
