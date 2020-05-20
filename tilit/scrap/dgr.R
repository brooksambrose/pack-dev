dgr<-function(){
  library(DiagrammeR)
  g<-fread('from to
concept useful
concept true
true thought
true communication
true artifact
')
  f<-g %>% unlist %>% unique %>% factor
  l<-levels(f)
  create_graph(
    data.table(id=f %>% as.numeric,type=NA,label=f %>% as.character)  ,lapply(g,match,l) %>% do.call(create_edge_df,.)
  ) %>% render_graph('tree')
}
