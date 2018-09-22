# jsub0<-jstorm[jclu$super][super=='Social Sciences',.(discipline)] %>% unlist %>% table %>%  sort(decreasing = T)
# jsub0<-jsub0[!names(jsub0)%in%jclu$tab$super]
# jsub0<-data.table(jsub0,round(prop.table(jsub0 %>% as.numeric)*100,1)) %>% setnames(c('Subdiscipline','N','Pct'))
# jsub0[,w:=
#         # Pct>1
#         cumsum(Pct) %>% `%/%`(10) %>% `<=`(6)
#       ]
# jsub0<-rbindlist(list(jsub0[(w),!'w']
#                       ,data.table('Other',jsub0[(!w),cbind(N,Pct) %>% margin.table(2) %>% rbind])))
# sg(jsub0,tit='JSTOR Social Sciences Journal Counts')
