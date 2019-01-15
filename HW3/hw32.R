load("/home/shreyas/primate.scpulae.rdata" )
ls(primate.scapulae)
primate.scapulae<-na.omit(primate.scapulae)
names(primate.scapulae)
head(primate.scapulae)

library(cluster)
hc1<-agnes(primate.scapulae,method="complete")
hc1$ac
x11()
plot(hc1)

hc2<-agnes(primate.scapulae,method="single")
hc2$ac
x11()
plot(hc2)

hc3<-agnes(primate.scapulae,method="average")
hc3$ac
x11()
plot(hc3)

library(NbClust)
ps<- primate.scapulae
ps$class<-NULL

fviz_nbclust(ps, kmeans) +
  geom_vline(xintercept = 3, linetype = 2)

true.labels<-primate.scapulae$class
km.out5 <- kmeans(ps, 5)
t5<-table(true.labels, km.out5$cluster)
randIndex(t5)

km.out3 <- kmeans(ps, 3)
t<-table(true.labels, km.out3$cluster)
library(flexclust)
randIndex(t)
