pws2bch.f<-function(id){
id=letters[1:14]
l<-length(id)
omega<-!is.integer(l/2) # omega is instructor
if(omega) id<-c(id,'Ω');l<-length(id)
y<-list(id[c(l,1:(l-1))])
for(i in 2:(l-1)) y[[i]]<-y[[i-1]][c(l,1:(l-1))]
x<-cbind(s=id,r=unlist(y)) %>% data.table
x<-apply(x,1,sort,simplify = F) %>% do.call(rbind,.) %>% data.table %>% unique
x %<>% .[!'Ω',on='s']
x %<>% .[!'Ω',on='r']
e<-split(x,1:nrow(x))[-1] %>% unname
set<-list(x[1])
while(length(e)) {
  ls<-length(set)
  w<-which(!sapply(set,function(z) any(unlist(e[1])%in%unlist(z))))[1]
  print(w)
  if(is.na(w)) set[ls+1]<-e[1] else set[[w]] %<>% {rbindlist(list(.,e[[1]]))}
  e[1]<-NULL
}
# x[,w:=rep(1:l,.N/l)]
# split(x,by='w')
#
#
# b<-1:{(l-1)/2}
# x[,i:=rep(1:l,length(b)) %>% sort]
# x[,w:=id[i]]
# x[w==s|w==r,i:=i-1]
# x[,w:=id[i]] %>% setorder(w)
# x[w==s|w==r]
# x[,.N,keyby=w]
# x[,unique(.SD),by=rep(id,l-1) %>% sort]
set
}
