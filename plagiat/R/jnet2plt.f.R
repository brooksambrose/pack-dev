# jnet2plt.f<-function(jnet,zref){
#   tl<-'lnet'%in%names(attributes(jnet))
#   net<-attr(jnet,'net')
#   lnet<-attr(jnet,'lnet')
#
#   v<-data.table(
#     net %v% 'vertex.names'
#     ,net %v% 'memb'
#     ,if(tl) lnet$mkl$sociality %>% exp else sna::degree(net)
#     ,scale(if(tl) {latentnet:::plot.ergmm(lnet,Z.ref = zref) %>% apply(2,jitter)} else attr(jnet,'zref'))
#   ) %>% setnames(ec('vn,memb,rs,x,y'))
#
#
#   e<-net %>% as.edgelist
#   vn<-attributes(e)$vnames
#   e<-data.table(e) %>% setnames(ec('s,r'))
#   e[,`:=`(s=vn[s],r=vn[r])]
#   e[,weight:=net%e%'weight']
#   e[,cross:=net%e%'cross']
#   ct<-RColorBrewer::brewer.pal(unique(v$memb) %>% length,'Dark2')
#   names(ct)<-table(v$memb) %>% sort(T) %>% names
#
#   setkey(v,vn)
#   v[,clr:=ct[memb]]
#   e[,`:=`(
#     x=v[s,x]
#     ,xend=v[r,x]
#     ,y=v[s,y]
#     ,yend=v[r,y]
#     ,xm=v[s,memb]
#     ,ym=v[r,memb]
#   )]
#   e[,clr:=mapply(function(s,r) colorRampPalette(c(s,r))(3)[2],s=ct[xm],r=ct[ym])]
#
#   setorder(v,-rs)
#   list(v=v,e=e)
# }
#
# ve2ftf<-function(ve){
#   cp<-rbindlist(ve,fill=T)
#   cs<-cp[,.(memb,xm,ym)] %>% apply(1,function(x) x %>% sort %>% na.omit %>% paste(collapse=' - '))
#   cp[,cs:=cs]
#   cp[cs!=memb,clr:=desat(clr,.5)]
#   cs<-cp[,clr]
#   names(cs)<-cp$cs
#   cs<-cs[!duplicated(names(cs))]
#   cs<-cs[order(names(cs))]
#   cp[is.na(vn),vn:='']
#   cp[,sk:=lintran(rs,s2=c(1,5))]
#   cp[,sh:=c(5,1)[(rs>=1)+1] %>% factor]
#   attr(cp,'colorscale')<-cs
#   cp
# }
# cp<-jnet2plt.f(jnet) %>% ve2ftf %>% .[,f:='FR']
# cs<-attr(cp,'colorscale')
# cp<-rbindlist(list(
#   cp
#   ,jnet2plt.f(lnetg$`1`,attributes(jnet)$sref) %>% ve2ftf%>% .[,f:='LS']
# ))
#
# sr<-cp[,.(s,r)] %>% apply(1,function(x) x %>% sort %>% na.omit %>% paste(collapse=' - '))
# cp[,ids:=sr]
# cp[ids=='',ids:=vn]
# cp[,memb:=factor(memb,levels=levels(jclu$fd))]
# setkey(cp,f,vn)
# cp[.('FR'),sh:=cp[.('LS'),sh]]
# cp[.('FR'),rs:=cp[.('LS'),rs]]
# #cp[,rs:=lintran(rs,s2=c(2,6)),by=f]
# cp[,a:={a<-lintran(weight,s2=c(.5,1));a[is.na(a)]<-0;a}]
# css<-cs
# rm(cs)
#
# cp[,node:=is.na(weight)]
# setkey(cp,f,node)
# g<-rbindlist(list(
#   copy(cp[.('FR',T)])[,f:='FRn']
#   ,cp
#   ,copy(cp[.('LS',T)])[,f:='LSn']
# ))[,f:=factor(f,levels=ec('FR,FRn,LSn,LS'))]
# setorder(g,f,node,-rs)
# setkey(g,f)
# g <- highlight_key(g[ec('FRn,LSn')],~memb,'Superdiscipline')
# pp<-myth(ggplot(data=g,mapping=aes(x=x,y=y,frame=f,ids=ids)) +
#   geom_point(mapping=aes(text=vn,size=rs,color=cs),shape=g$origData()$sh,show.legend = F)) +
#   theme_blank() + scale_size_continuous(range=c(1,5))
# pp
# pp<-plt(pp,tooltip='text') %>%
#   plotly::animation_opts(frame=1*1e3,redraw = T) %>%
#   layout(showlegend = F) %>%
#   animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
#   animation_slider(currentvalue = list(prefix = "", font = list(color="red")),fromcurrent=T) %>%
#   highlight(selectize = T)
# pp
# save(pp,file='d/b/pp.RData')
# #
# # clr<-c("#7570B3","#AD8D5A","#AE4C9E","#6D8B68","#8D7368","#A7675A","#488794","#E6AB02","#E66946","#A6A810","#C6900F","#DF8402","#80A43C","#E7298A","#A66754","#C64F53","#E04445","#816380","#66A61E","#868E1D","#9F8210","#40A24A","#A6761D","#BF6A0F","#608A49","#D95F02","#7A7E3C","#1B9E77")
# # # t<-list()
# # # for(i in sample(1:length(bclr))) {
# # #   clr<-bclr[i]
# #   d<-data.frame(x=rnorm(length(clr)),y=rnorm(length(clr)),clr=clr,a=runif(length(clr)))
# #   # t[[i]]<-try(
# #     ggplot(d,aes(x,y)) + geom_point(color=d$clr,alpha=d$a)
# #     # )
# # # }
# #
# # color
# # d<-data.frame(x=rnorm(3),y=rnorm(3),clr=c("#7570B3","#AD8D5A","#AE4C9E"),a=runif(3))
# # d<-data.frame(x=rnorm(3),y=rnorm(3),clr=c("red","green","blue"),a=runif(3))
# # d<-rbind(d,d)
# # p<-ggplot(d,aes(x,y)) + geom_point(aes(color=clr,group=clr),alpha=d$a)
# # p
# # ggplotly(p)
