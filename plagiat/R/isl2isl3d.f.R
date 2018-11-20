#' Island Plot 2 3D Island Plot
#'
#' @param kcc2isl
#' @param res
#' @param rad
#' @param ln
#' @param ex
#' @param floor
#' @param close
#' @param tight
#'
#' @return
#' @export
#' @import data.table magrittr
#' @importFrom plotly highlight
#' @importFrom plotly highlight_key
#' @importFrom plotly plot_ly
#' @importFrom plotly add_mesh
#' @importFrom plotly add_markers
#' @importFrom plotly layout
#'
#' @examples
isl2isl3d.f<-function(kcc2isl,res=100,rad=1/2,ln=T,ex=F,floor=F,close=F,tight=F){
  kcc2isl<-copy(kcc2isl)
  bgc<-attr(kcc2isl,'colors')[1]
  bck<-list(backgroundcolor=bgc,color=bgc,showbackground=T,showspikes=F,visible=T,showticklabels=F,showaxeslabels=F,title=F,showgrid=F)
  cm<-kcc2isl %>% names %>% setdiff(ec('x,y,names,v,k,g,text,gn'))
  names(cm)<-cm
  csc<-colorRamps::blue2red(101) %>% replace(51,'white')
  for(i in cm) kcc2isl[,paste0(i,'-clr'):=csc[lintran(get(i),c(1,101)) %>% round]]
  ki<-highlight_key(kcc2isl,~g,'K-clique Community')
  cs<-cbind(seq(0,1,.1) %>% lintran,col=colorRamps::blue2red(11) %>% replace(6,'white'))
  k<-kcc2isl$k %>% unique %>% sort
  scs<-attr(kcc2isl,'colors') %>% cbind(seq(length(.)) %>% lintran,.)
  adm<-lapply(cm,function(x) function(p,vis,y=x) {yc<-ki$origData()[[paste0(y,'-clr')]];add_markers(p,data=ki,visible=vis,text=~text,hoverinfo='text',hoverlabel=list(bgcolor=I(yc)),showlegend=F,marker=list(symbol='circle',size=4,opacity=1,line=list(width=0),color=I(yc),cmin=paste0('~min(`',y,'`)') %>% as.formula,cmax=paste0('~max(`',y,'`)') %>% as.formula,colorscale=cs,colorbar=list(yanchor='top',len=.5,xpad=5)))})

  sfc<-with(kcc2isl,akima::interp(x =x, y = y, z = k,nx=res,ny=res,linear=ln,extrap = ex))

  ix<-sfc$z %>% melt(na.rm = T) %>% data.table %>% setnames(ec('xi,yi,z'))
  ix[,`:=`(x=sfc$x[xi],y=sfc$y[yi])]
  nn<-RANN::nn2(
    ix[,.(x,y)]
    ,kcc2isl[,.(x,y)]
    ,k=1
  )

  rd<-kcc2isl[,mean(range(x) %>% diff,range(y) %>% diff)]/res*rad
  ix[nn$nn.idx[nn$nn.dists<=rd],K:=kcc2isl$k[nn$nn.dists<=rd]]
  ix[nn$nn.idx[nn$nn.dists>rd],K:=1]
  if(tight) ix[is.na(K),K:=1] else ix<-ix[!is.na(K)]
  for(i in ix[,seq(.N)]) sfc$z[ix$xi[i],ix$yi[i]]<-ix$K[i]

  if(floor) sfc$z<-floor(sfc$z)
  sfc$z[sfc$z<1]<-1
  if(close) for(h in 1:2){
    w<-sfc$z %>% apply(1,function(x) {
      x<-c(0,is.na(x) %>% diff)
      w<-which(x<0)-1
      w<-c(w,which(x>0))
      w
    })
    for(i in 1:length(w)) sfc$z[i,w[[i]]]<-0
    w<-sfc$z %>% apply(2,function(x) {
      x[x==0]<-NA
      x<-c(0,is.na(x) %>% diff)
      w<-which(x<0)-1
      w<-c(w,which(x>0))
      w
    })
    for(i in 1:length(w)) sfc$z[w[[i]],i]<-0
  }
  sfc$z[sfc$z==0]<-1
  nsew<-list(showarrow = F,z = quantile(kcc2isl$k,.25) %>% round,text = "W",xanchor = "center",opacity = 1)

  isl3d<-plot_ly(x=~x,y=~y,z=~k+.1,showlegend=F) %>%
    add_surface(data=sfc,x=~x,y=~y,z=~t(z),autocolorscale=F,colorscale=scs
                ,hoverinfo='skip'
                ,colorbar=list(yanchor='bottom',len=.5,title='K',xpad=5),contours = list(
                  x = list(highlight=F),y = list(highlight=F),showlabels=T
                  ,z = list(show=TRUE,usecolormap=TRUE,highlight=F,highlightcolor="#ff0000",project=list(z=TRUE)
                  ))) %>%
    #add_mesh(inherit=F,data=kcc2isl,x=~x,y=~y,z=~k,intensity=~k,colors=attr(kcc2isl,'colors')[-1],hoverinfo='skip',colorbar=list(yanchor='bottom',len=.5,title='K',xpad=5)) %>%
    adm$cc(p=.,vis=F) %>% adm$mut(p=.,vis=F) %>% adm$m2ut(p=.,vis=F) %>% adm$gini(p=.,vis=F) %>% adm$`AM ANTHROPOL`(p=.,vis=F) %>% adm$`AM J SOCIOL`(p=.,vis=F) %>% adm$py(p=.,vis=F) %>% adm$ca(p=.,vis=F) %>%
    layout(paper_bgcolor=bgc,plot_bgcolor=bgc,dragmode='turntable'
           ,scene=list(
             aspectratio = list(x = 1, y = 1, z = 0.25),xaxis=bck,yaxis=bck,zaxis=bck,camera=list(eye=list(x=0,y=-.001,z=1.1),center=list(x=0,y=0,z=0))
             ,annotations = list(
               c(list(y =  sfc$y %>% max,x = sfc$x %>% range %>% mean,text = "N"),nsew)
               ,c(list(y =  sfc$y %>% min,x = sfc$x %>% range %>% mean,text = "S"),nsew)
               ,c(list(x =  sfc$x %>% max,y = sfc$y %>% range %>% mean,text = "E"),nsew)
               ,c(list(x =  sfc$x %>% min,y = sfc$y %>% range %>% mean,text = "W"),nsew)
             )
           )
           ,margin=list(l=0,r=0,t=0,b=0)
           ,updatemenus = list(list(
             type = "button",direction = "down",xanchor = 'left',yanchor = "top",x=0,y=1,
             buttons = list(
               list(method = "restyle", args = list("visible", list(T,F,F,F,F,F,F,F,F)),label = "Landscape")
               ,list(method = "restyle",args = list("visible", list(T,T,F,F,F,F,F,F,F)),label = "Citation Count")
               ,list(method = "restyle",args = list("visible", list(T,F,T,F,F,F,F,F,F)),label = "Mean Ref. Count")
               ,list(method = "restyle",args = list("visible", list(T,F,F,T,F,F,F,F,F)),label = "2nd High Ref.")
               ,list(method = "restyle",args = list("visible", list(T,F,F,F,T,F,F,F,F)),label = "Gini Ref. Count")
               ,list(method = "restyle",args = list("visible", list(T,F,F,F,F,T,F,F,F)),label = "Anthropology")
               ,list(method = "restyle",args = list("visible", list(T,F,F,F,F,F,T,F,F)),label = "Sociology")
               ,list(method = "restyle",args = list("visible", list(T,F,F,F,F,F,F,T,F)),label = "Avg Publication Year")
               ,list(method = "restyle",args = list("visible", list(T,F,F,F,F,F,F,F,T)),label = "Avg Citation Age")
             )))
    )  %>%
    highlight(on='plotly_click',opacityDim = 1, selected = attrs_selected(marker = list(
      showscale = FALSE
      ,line=list(width=1,color='magenta'))
    )
    )
  attr(isl3d,'kcc2isl')<-kcc2isl
  isl3d
}
