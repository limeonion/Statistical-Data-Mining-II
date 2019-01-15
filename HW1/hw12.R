
rm(list = ls())
install.packages("arules")
library(arules)
library(MASS)
data("Boston")
bost=Boston
head(Boston)
names(Boston)
attach(bost)
library(ggplot2)

hist(crim)
bost$crim <- ordered(cut(crim, c(0,10,90), labels = c("low",  "high")))

hist(zn)
bost$zn <- ordered(cut(zn, c(-1,20,400), labels = c("low",  "high")))

hist(indus)
bost$indus <- ordered(cut(indus, c(0,15,110), labels = c("low",  "high")))

hist(chas)
bost$chas <- ordered(cut(chas, c(-1,.5,2), labels = c("low",  "high")))

hist(nox)
bost$nox <- ordered(cut(nox, c(0,0.6,2), labels = c("low",  "high")))

hist(rm)
bost$rm <- ordered(cut(rm, c(0,6,110), labels = c("low",  "high")))

hist(age)
bost$age <- ordered(cut(age, c(-1,60,150), labels = c("low",  "high")))

hist(dis)
bost$dis <- ordered(cut(dis, c(0,4,20), labels = c("low",  "high")))

hist(rad)
bost$rad <- ordered(cut(rad, c(-1,6,40), labels = c("low",  "high")))

hist(tax)
bost$tax <- ordered(cut(tax, c(0,400,1110), labels = c("low",  "high")))

hist(ptratio)
bost$ptratio <- ordered(cut(ptratio, c(0,19,40), labels = c("low",  "high")))

hist(black)
bost$black <- ordered(cut(black, c(0,350,660), labels = c("low",  "high")))

hist(lstat)
bost$lstat <- ordered(cut(lstat, c(0,15,60), labels = c("low",  "high")))

hist(medv)
bost$medv <- ordered(cut(medv, c(0,25,70), labels = c("low",  "high")))

bostTrans <- as(bost, "transactions")

itemFrequencyPlot(bostTrans, support = 0.02, cex.names = 0.8)
rules <- apriori(bostTrans, parameter = list(support = 0.02, confidence = 0.7))

summary(rules)
partC_rules <- subset(rules, subset = lhs %in% "crim=low" &rhs %in% "dis=low" & lift>1)
summary(partC_rules)
inspect(head(sort(partC_rules, by = "lift",decreasing = TRUE), n = 10))

partD_rules <- subset(rules, subset = lhs %in% "ptratio=low" & lift>1.5)
summary(partD_rules) 
inspect(head(sort(partD_rules, by = "lift",decreasing = TRUE), n = 10))
