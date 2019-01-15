rm(list = ls())

#Part A 
genes <- read.csv("Ch10Ex11.csv", header = FALSE)
dist <- hclust(as.dist(1 - cor(genes)), method = "complete")
x11()
plot(dist)

#Part B(Different linkage methods)
hc.single <- hclust(as.dist(1 - cor(genes)), method = "single")
x11()
plot(hc.single)
hc.average <- hclust(as.dist(1 - cor(genes)), method = "average")
x11()
plot(hc.average)

#Part C
pr.out <- prcomp(t(genes))
head(pr.out$rotation)

total.load <- apply(pr.out$rotation, 1, sum)
index <- order(abs(total.load), decreasing = TRUE)
index[1:15]
