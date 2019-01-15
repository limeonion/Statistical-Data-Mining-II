library(igraph)
library(Rgraphviz)
node<-data.frame(names=c("A","B","C","D","E","F"))
relations=data.frame(from=c("B","B","D","D","E","F","C"),to=c("C","E","B","E","D","C","A"))
g<-graph.data.frame(relations,directed = TRUE, vertices = node)
x11()
plot(g)

pg1<-page.rank(g,damping = 0.05)
pg1$vector

pg2<-page.rank(g,damping = 0.25)
pg2$vector

pg3<-page.rank(g,damping = 0.5)
pg3$vector

pg4<-page.rank(g,damping = 0.75)
pg4$vector

pg5<-page.rank(g,damping = 0.95)
pg5$vector

node2<-data.frame(names=c("A","B","C","D","E","F","G","H"))
relations2=data.frame(from=c("B","D","E","F","G","H","C"),to=c("A","B","B","C","C","C","A"))
g2<-graph.data.frame(relations2,directed = TRUE, vertices = node2)
x11()
plot(g2)

pg.b<-page.rank(g2,damping = 0.15)
pg.b$vector