#' Interactive group picker for short strings clustered by string distance
#'
#' @param hd
#' @param s1
#' @param s2
#' @param lightest.val
#' @param maxlist
#' @param must.be.this.short.to.ride
#' @param out
#' @param instruct
#'
#' @import data.table dendextend
#' @return
#' @export
#'
#' @examples
strdist.dend.picker<-function(
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

  library(dendextend)
  library(data.table)
  rect.dendrogram<-function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2,
                             cluster = NULL, horiz = FALSE, density = NULL, angle = 45,
                             text = NULL, text_cex = 1, text_col = 1, xpd = TRUE, lower_rect, hpk=NULL,
                             ...
  )
  {
    if (!is.dendrogram(tree))
      stop("x is not a dendrogram object.")
    if (length(h) > 1L | length(k) > 1L)
      stop("'k' and 'h' must be a scalar(i.e.: of length 1)")
    if(is.null(hpk)) {tree_heights <- heights_per_k.dendrogram(tree)[-1]} else {tree_heights<-hpk[-1]}
    tree_order <- order.dendrogram(tree)
    if (!is.null(h)) {
      if (!is.null(k))
        stop("specify exactly one of 'k' and 'h'")
      ss_ks <- tree_heights < h
      k <- min(as.numeric(names(ss_ks))[ss_ks])
      k <- max(k, 2)
    }
    else if (is.null(k))
      stop("specify exactly one of 'k' and 'h'")
    if (k < 2 | k > length(tree_heights))
      stop(gettextf("k must be between 2 and %d", length(tree_heights)),
           domain = NA)
    if (is.null(cluster))
      cluster <- cutree(tree, k = k)
    clustab <- table(cluster)[unique(cluster[tree_order])]
    m <- c(0, cumsum(clustab))
    if (!is.null(x)) {
      if (!is.null(which))
        stop("specify exactly one of 'which' and 'x'")
      which <- x
      for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
    }
    else if (is.null(which))
      which <- 1L:k
    if (any(which > k))
      stop(gettextf("all elements of 'which' must be between 1 and %d",
                    k), domain = NA)
    border <- rep_len(border, length(which))
    retval <- list()
    old_xpd <- par()["xpd"]
    par(xpd = xpd)
    for (n in seq_along(which)) {
      if (!horiz) {
        xleft = m[which[n]] + 0.66
        if (missing(lower_rect))
          lower_rect <- par("usr")[3L] - strheight("W") *
            (max(nchar(labels(tree))) + 1)
        ybottom = lower_rect
        xright = m[which[n] + 1] + 0.33
        ytop = tree_heights[names(tree_heights) == k]
      }
      else {
        ybottom = m[which[n]] + 0.66
        if (missing(lower_rect))
          lower_rect <- par("usr")[2L] + strwidth("X") *
            (max(nchar(labels(tree))) + 1)
        xright = lower_rect
        ytop = m[which[n] + 1] + 0.33
        xleft = tree_heights[names(tree_heights) == k]
      }
      rect(xleft, ybottom, xright, ytop, border = border[n],
           density = density, angle = angle, ...)
      if (!is.null(text))
        text((m[which[n]] + m[which[n] + 1] + 1)/2, grconvertY(grconvertY(par("usr")[3L],
                                                                          "user", "ndc") + 0.02, "ndc", "user"), text[n],
             cex = text_cex, col = text_col)
      retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
    }
    par(xpd = old_xpd)
    invisible(retval)
  }

  lintran<-function(x,s1=c(0,1),s2=c(0,1)) {a=diff(s2)/diff(s1);b=s2[1]-a*s1[1];return(a*x+b)}
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
