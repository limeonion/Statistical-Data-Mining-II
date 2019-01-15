rm(list = ls())
set.seed(1000)

#Ecludian Distance
eclDist <- hclust(dist(USArrests), method = "complete")
x11()
plot(eclDist)

#Part B
cutree(eclDist, 3)

#Part C
sd.data <- scale(USArrests)
scaledEclDist <- hclust(dist(sd.data), method = "complete")
x11()
plot(scaledEclDist)
cutree(scaledEclDist, 3)
table(cutree(eclDist, 3), cutree(scaledEclDist, 3))
