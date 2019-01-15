library(ElemStatLearn)
data("marketing")
#attach(marketing)
head(marketing)

install.packages("rpart")
library("rpart")
#library("MMST")
mark=marketing
attach(mark)

model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit.income <- rpart(Income~., data = mark, method = "class", control = model.control)

x11()
plot(fit.income, uniform = T, compress = T)
text(fit.income, cex = 0.5)

x11()
plot(fit.income, uniform = T, compress = T)
text(fit.income, use.n = T, all = T, cex = 0.5)

x11()
plot(fit.income, branch = .4, uniform = T, compress = T)
text(fit.income, use.n = T, all = T, cex = 0.5)

#load("digging_data.RData")
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit.dig <- rpart(Income~., data = mark, method = "class", control = model.control)

x11()
plot(fit.dig$cptable[,4], main = "Cp for model selection", ylab = "cv error")

min_cp = which.min(fit.dig$cptable[,4])
pruned_fit_dig <- prune(fit.dig, cp = fit.dig$cptable[min_cp,1])

x11()
plot(pruned_fit_dig, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_dig, cex = 1)

x11()
plot(fit.dig, branch = .3, compress=T, main = "Full Tree")
text(fit.dig, cex = 0.3)


