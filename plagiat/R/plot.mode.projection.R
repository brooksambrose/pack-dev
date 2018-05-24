plot.mode.projection <- function(
  dbl2bel
  ,m1.stub="^m1"
  ,out="/Users/bambrose/Dropbox/2013-2014/winter2014_isi_data/1941out/descriptive/" #may add a stub at the end of this line
  ,vcx=1
  ,vlw=1
  ,vlt="solid"
  ,elw=1
  ,elt="solid"
  ,loopcx=1
  ,m1vsides=3
  ,m2vsides=4
  ,m1col="black"
  ,m2col="white"
  ,ecol="gray"
  ,trim.pendants=c("don't","m1","m2")
  ,bmain="Bimodal"
  ,m1main="Mode 1"
  ,m2main="Mode 2"
  ,mar=1
  ,omi=.25
  ,cex=1
  ,pnt.v=list() #named list of character vectors where name is color and contents are vertices to paint
  ,pnt.e=list() #named list of directed character edgelists where name is color and contents are edges to paint
  ,pnt.vlty=list() #named list of character vectors where name is lty and contents are vertices to paint
  ,pnt.lty=list() #named list of directed character edgelists where name is lty and contents are edges to paint
  ,pnt.v2e=T
  ,layout=list()
)
{
  require(network)

  el<-data.frame(s=as.character(el[,1]),r=as.character(el[,2]),stringsAsFactors=F)
  el<-el[order(el$s,el$r),]
  suppressWarnings(if(trim.pendants=="m1") {
    t<-table(el[[1]])
    elo<-el
    el<-el[el[[1]]%in%names(t)[t>1],]
  })
  suppressWarnings(if(trim.pendants=="m2") {
    t<-table(el[[2]])
    elo<-el
    el<-el[el[[2]]%in%names(t)[t>1],]
  })
  trim<-grepl("^m",trim.pendants[1])
  if(trim){
    om1n<-sort(unique(elo[[1]]))
    om2n<-sort(unique(elo[[2]]))
    om1l<-length(om1n)
    om2l<-length(om2n)
    (pbmnet<-network(elo,bipartite=om1l,directed=T,matrix.type="edgelist"))
  }

  ### paint logic
  if(length(pnt.v)&pnt.v2e) for(i in 1:length(pnt.v)) {
    pnt.e[[length(pnt.e)+1]]<-t(apply(combn(
      setdiff(unlist(el[apply(apply(el,2,is.element,pnt.v[[i]]),1,any),]),pnt.v[[i]])
      ,2),2,sort))
    names(pnt.e)[length(pnt.e)]<-names(pnt.v[i])
  }

  if(length(pnt.e)) for(i in 1:length(pnt.e)) colnames(pnt.e[[i]])<-c("s","r")

  if(length(pnt.vlty)&pnt.v2e) for(i in 1:length(pnt.vlty)) {
    pnt.lty[[length(pnt.lty)+1]]<-t(apply(combn(
      setdiff(unlist(el[apply(apply(el,2,is.element,pnt.vlty[[i]]),1,any),]),pnt.vlty[[i]])
      ,2),2,sort))
    names(pnt.lty)[length(pnt.lty)]<-names(pnt.vlty[i])
  }
  if(length(pnt.lty)) for(i in 1:length(pnt.lty)) colnames(pnt.lty[[i]])<-c("s","r")

  print(pnt.v)
  print(pnt.e)
  print(pnt.vlty)
  print(pnt.lty)

  m1n<-sort(unique(el[[1]]))
  m2n<-sort(unique(el[[2]]))
  m1l<-length(m1n)
  m2l<-length(m2n)
  (bmnet<-network(el,bipartite=m1l,directed=T,matrix.type="edgelist"))

  m<-bmnet[,]
  w<-grepl(m1.stub,colnames(m))
  m1<-m[w,!w]%*%t(m[w,!w])
  m2<-t(m[w,!w])%*%m[w,!w]
  m1[lower.tri(m1)]<-0
  m2[lower.tri(m2)]<-0

  m1<-data.frame(s=rownames(m1)[which(!!m1,arr.ind=T)[,1]],r=rownames(m1)[which(!!m1,arr.ind=T)[,2]],ew=m1[which(!!m1)],stringsAsFactors=F)
  m2<-data.frame(s=rownames(m2)[which(!!m2,arr.ind=T)[,1]],r=rownames(m2)[which(!!m2,arr.ind=T)[,2]],ew=m2[which(!!m2)],stringsAsFactors=F)

  m1<-m1[order(m1$s,m1$r),]
  m2<-m2[order(m2$s,m2$r),]

  (m1net<-network(m1[,1:2],loops=T,directed=F,matrix.type="edgelist"))
  (m2net<-network(m2[,1:2],loops=T,directed=F,matrix.type="edgelist"))

  network.vertex.names(m1net)<-m1n
  network.vertex.names(m2net)<-m2n

  set.edge.attribute(m1net, "ew", m1$ew)
  set.edge.attribute(m2net, "ew", m2$ew)

  #mats<-matrix(nrow=m1l,ncol=m2l,dimnames=list(m1n,m2n))
  #system.time(for(i in 1:nrow(el)) mats[el[[1]],el[[2]]]<-1)
  #matr<-matrix(sample(c(0,0,0,0,1),replace=T,size=24),ncol=6,nrow=4)
  #mat<-matr
  #mat<-mats

  pdf(paste(out,"mode_projection.pdf",sep=""),h=(8.5-2)/3*ifelse(trim,2,1),w=(8.5-2)/3*ifelse(trim,2,3))
  if(trim)  par(mfrow=c(2,2),mar=rep(mar,4),omi=rep(omi,4)) else par(mfrow=c(1,3),mar=rep(mar,4),omi=rep(omi,4))
  if(trim){
    vcol<-c(rep(m1col,om1l),rep(m2col,om2l))
    el<-data.frame(as.edgelist(pbmnet))
    if(!is.directed(pbmnet)) el<-data.frame(t(apply(el,1,sort)),stringsAsFactors=F)
    ecolp<-rep(ecol,nrow(el))
    eltp<-rep(elt,nrow(el))
    vltp<-rep(vlt,network.size(pbmnet))

    if(length(pnt.v)) for(i in 1:length(pnt.v)) vcol[network.vertex.names(pbmnet)%in%pnt.v[[i]]]<-names(pnt.v[i])
    if(length(pnt.e)) for(i in 1:length(pnt.e)) ecolp[which(duplicated(rbind(do.call(cbind,el),pnt.e[[i]]),fromLast=T))]<-names(pnt.e[i])
    if(length(pnt.vlty)) for(i in 1:length(pnt.vlty)) vltp[network.vertex.names(pbmnet)%in%pnt.vlty[[i]]]<-names(pnt.vlty[i])
    if(length(pnt.lty)) for(i in 1:length(pnt.lty)) eltp[which(duplicated(data.frame(rbind(do.call(cbind,el),pnt.lty[[i]])),fromLast=T))]<-names(pnt.lty[i])

    lay<-list()
    for(i in names(layout)) lay[[i]]<-layout[[i]](x=network.size(pbmnet))

    plot(pbmnet
         ,vertex.sides=c(rep(m1vsides,om1l),rep(m2vsides,om2l))
         ,vertex.col=vcol
         ,edge.col=ecolp
         ,vertex.cex=vcx
         ,vertex.lwd=vlw
         ,edge.lwd=elw
         ,edge.lty=eltp
         ,vertex.lty=vltp
         ,layout.par=lay
    )
    box()
    mtext(paste("Original",bmain),cex=cex)
  }

  vcol<-c(rep(m1col,m1l),rep(m2col,m2l))
  el<-data.frame(as.edgelist(bmnet))
  if(!is.directed(bmnet)) el<-data.frame(t(apply(el,1,sort)),stringsAsFactors=F)
  ecolp<-rep(ecol,nrow(el))
  eltp<-rep(elt,nrow(el))
  vltp<-rep(vlt,network.size(bmnet))

  if(length(pnt.v)) for(i in 1:length(pnt.v)) vcol[network.vertex.names(bmnet)%in%pnt.v[[i]]]<-names(pnt.v[i])
  if(length(pnt.e)) for(i in 1:length(pnt.e)) ecolp[which(duplicated(rbind(do.call(cbind,el),pnt.e[[i]]),fromLast=T))]<-names(pnt.e[i])
  if(length(pnt.vlty)) for(i in 1:length(pnt.vlty)) vltp[network.vertex.names(bmnet)%in%pnt.vlty[[i]]]<-names(pnt.vlty[i])
  if(length(pnt.lty)) for(i in 1:length(pnt.lty)) eltp[which(duplicated(data.frame(rbind(do.call(cbind,el),pnt.lty[[i]])),fromLast=T))]<-names(pnt.lty[i])

  lay<-list()
  for(i in names(layout)) lay[[i]]<-layout[[i]](x=network.size(bmnet))

  plot(bmnet
       ,vertex.sides=c(rep(m1vsides,m1l),rep(m2vsides,m2l))
       ,vertex.col=vcol
       ,edge.col=ecolp
       ,vertex.cex=vcx
       ,vertex.lwd=vlw
       ,edge.lwd=elw
       ,edge.lty=eltp
       ,vertex.lty=vltp
       ,layout.par=lay
  )
  box()
  mtext(ifelse(trim,paste(bmain,"w/o Pendants"),bmain),cex=cex)

  vcol<-rep(m1col,m1l)
  el<-data.frame(as.edgelist(m1net))
  if(!is.directed(m1net)) el<-data.frame(t(apply(el,1,sort)),stringsAsFactors=F)
  ecolp<-rep(ecol,nrow(el))
  eltp<-rep(elt,nrow(el))
  vltp<-rep(vlt,network.size(m1net))

  if(length(pnt.v)) for(i in 1:length(pnt.v)) vcol[network.vertex.names(m1net)%in%pnt.v[[i]]]<-names(pnt.v[i])
  if(length(pnt.e)) for(i in 1:length(pnt.e)) ecolp[which(duplicated(rbind(do.call(cbind,el),pnt.e[[i]]),fromLast=T))]<-names(pnt.e[i])
  if(length(pnt.vlty)) for(i in 1:length(pnt.vlty)) vltp[network.vertex.names(m1net)%in%pnt.vlty[[i]]]<-names(pnt.vlty[i])
  if(length(pnt.lty)) for(i in 1:length(pnt.lty)) eltp[which(duplicated(data.frame(rbind(do.call(cbind,el),pnt.lty[[i]])),fromLast=T))]<-names(pnt.lty[i])

  lay<-list()
  for(i in names(layout)) lay[[i]]<-layout[[i]](x=network.size(m1net))

  plot(m1net
       ,attrname="ew"
       ,loop.cex=loopcx
       ,vertex.sides=rep(m1vsides,m1l)
       ,vertex.col=vcol
       ,edge.col=ecolp
       ,vertex.cex=vcx
       ,vertex.lwd=vlw
       ,edge.lwd=elw
       ,edge.lty=eltp
       ,vertex.lty=vltp
       ,layout.par=lay
  )
  box()
  mtext(m1main,cex=cex)

  vcol<-rep(m2col,m2l)
  el<-data.frame(as.edgelist(m2net))
  if(!is.directed(m2net)) el<-data.frame(t(apply(el,1,sort)),stringsAsFactors=F)
  ecolp<-rep(ecol,nrow(el))
  eltp<-rep(elt,nrow(el))
  vltp<-rep(vlt,network.size(m2net))

  if(length(pnt.v)) for(i in 1:length(pnt.v)) vcol[network.vertex.names(m2net)%in%pnt.v[[i]]]<-names(pnt.v[i])
  if(length(pnt.e)) for(i in 1:length(pnt.e)) ecolp[which(duplicated(rbind(do.call(cbind,el),pnt.e[[i]]),fromLast=T))]<-names(pnt.e[i])
  if(length(pnt.vlty)) for(i in 1:length(pnt.vlty)) vltp[network.vertex.names(m2net)%in%pnt.vlty[[i]]]<-names(pnt.vlty[i])
  if(length(pnt.lty)) for(i in 1:length(pnt.lty)) eltp[which(duplicated(data.frame(rbind(do.call(cbind,el),pnt.lty[[i]])),fromLast=T))]<-names(pnt.lty[i])

  lay<-list()
  for(i in names(layout)) lay[[i]]<-layout[[i]](x=network.size(m2net))

  plot(
    m2net
    ,attrname="ew"
    ,loop.cex=loopcx
    ,vertex.sides=rep(m2vsides,m2l)
    ,edge.col=ecolp
    ,vertex.col=vcol
    ,vertex.cex=vcx
    ,vertex.lwd=vlw
    ,edge.lwd=elw
    ,edge.lty=eltp
    ,vertex.lty=vltp
    ,layout.par=lay
  )
  box()
  mtext(m2main,cex=cex)
  dev.off()
  cat("\nPlot saved to",out)
  ret<-list(bmnet=bmnet,m1net=m1net,m2net=m2net)
  if(trim) ret<-c(pbmnet=list(pbmnet),ret)
  ret
}
