rm(list = ls())
set.seed(10000)

#Part A
x <- matrix(rnorm(60 * 50, mean = 0, sd = 0.01), ncol = 50)
x[1:20, 2] <- 1
x[21:40, 1] <- 2
x[21:40, 2] <- 2
x[41:60, 1] <- 1
true.labels <- c(rep(1, 20), rep(2, 20), rep(3, 20))

#Part B(PCA)
pr.out <- prcomp(x)
x11()
plot(pr.out$x[, 1:2], col = 1:3, xlab = "Z1", ylab = "Z2", pch = 19)

#Part C
km.out3 <- kmeans(x, 3, nstart = 20)
table(true.labels, km.out3$cluster)

#Part D
km.out2 <- kmeans(x, 2, nstart = 20)
table(true.labels, km.out2$cluster)

#Part E
km.out4 <- kmeans(x, 4, nstart = 20)
table(true.labels, km.out4$cluster)

#Part F
km.out.pc <- kmeans(pr.out$x[, 1:2], 3, nstart = 20)
table(true.labels, km.out.pc$cluster)

#Part G
km.out.scaled <- kmeans(scale(x), 3, nstart = 20)
table(true.labels, km.out.scaled$cluster)