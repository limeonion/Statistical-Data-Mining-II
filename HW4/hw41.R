load("cad1.RData")
install.packages("bnlearn")
source("http://bioconductor.org/biocLite.R")
biocLite(c("graph", "Rgraphviz", "RBGL"))
install.packages("gRain")

library(gRain)
#library(RHugin)
library(Rgraphviz)
library(gRbase)
library(ggm)
library(gRim)
library(bnlearn)
library(igraph)

cad.bn <- hc(cad1)
net <- as(amat(cad.bn), "graphNEL")
x11()
plot(net)
cad.tab <- xtabs( ~ ., data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])

##########
g<-list(~AngPec, ~STcode|AngPec, ~SuffHeartF|STcode, ~Hypertrophi|SuffHeartF,~QWavecode|STcode:Hypertrophi, ~Heartfail|Hypertrophi:STcode, ~CAD|Heartfail:AngPec,
     ~AMI|CAD, ~Hyperchol|CAD, ~Smoker|CAD, ~Inherit|CAD, ~Sex|CAD, ~STchange|CAD:STcode:Hypertrophi, ~QWave|AMI:CAD)
dagL <- dagList(g)
##########

g1<-list(~Sex,~CAD|Inherit:Hyperchol,~SuffHeartF,~Hyperchol|SuffHeartF:Smoker,~Smoker|Sex,~Inherit|Smoker)
dagL1<-dagList(g1)
#######
yn <- c("yes", "no")

sex<-cptable(~Sex, values = c(4,96), levels =yn)
s<-cptable(~SuffHeartF, values = c(2,8), levels =yn)
h.s<-cptable(~Hyperchol|SuffHeartF:Smoker, values = c(8,92,6,4,81,19,5,95), levels =yn)
s.s<-cptable(~Smoker|Sex, values = c(5,5,93,7), levels =yn)
i.s<-cptable(~Inherit|Smoker, values = c(9,91,1,9), levels =yn)
c.ih<-cptable(~CAD|Inherit:Hyperchol, values = c(9,91,12,88,87,13,21,71), levels =yn)

plist <- compileCPT(list(sex,s,h.s,s.s,i.s,c.ih))
grn1 <- grain(plist)
summary(grn1)

x11()
plot(grn1)

grn1c <- compile(grn1)
summary(grn1c)

grn1c <- propagate(grn1c)
grn1c.ev <- setFinding(grn1c, nodes = c("Sex", "Hyperchol"), states = c("Female", "Yes"))
abs <- querygrain(grn1c.ev, nodes = c("CAD", "SuffHeartF"), type = "marginal")
not_abs <- querygrain(grn1c, nodes = c("CAD", "SuffHeartF"), type = "marginal")

cadtrain<-cad1[c("Sex","SuffHeartF","Hyperchol","Smoker","Inherit","CAD")]

cadtrain$Sex<-as.character(cadtrain$Sex)
cadtrain$Sex[cadtrain$Sex == "Male"] <- "1"
cadtrain$Sex[cadtrain$Sex == "Female"] <- "0"
cadtrain$Sex<-as.integer(cadtrain$Sex)

cadtrain$Smoker<-as.character(cadtrain$Smoker)
cadtrain$Smoker[cadtrain$Smoker == "Yes"] <- "1"
cadtrain$Smoker[cadtrain$Smoker == "No"] <- "0"
cadtrain$Smoker<-as.integer(cadtrain$Smoker)

cadtrain$SuffHeartF<-as.character(cadtrain$SuffHeartF)
cadtrain$SuffHeartF[cadtrain$SuffHeartF == "Yes"] <- "1"
cadtrain$SuffHeartF[cadtrain$SuffHeartF == "No"] <- "0"
cadtrain$SuffHeartF<-as.integer(cadtrain$SuffHeartF)

cadtrain$Hyperchol<-as.character(cadtrain$Hyperchol)
cadtrain$Hyperchol[cadtrain$Hyperchol == "Yes"] <- "1"
cadtrain$Hyperchol[cadtrain$Hyperchol == "No"] <- "0"
cadtrain$Hyperchol<-as.integer(cadtrain$Hyperchol)

cadtrain$Inherit<-as.character(cadtrain$Inherit)
cadtrain$Inherit[cadtrain$Inherit == "Yes"] <- "1"
cadtrain$Inherit[cadtrain$Inherit == "No"] <- "0"
cadtrain$Inherit<-as.integer(cadtrain$Inherit)

cadtrain$CAD<-as.character(cadtrain$CAD)
cadtrain$CAD[cadtrain$CAD == "Yes"] <- "1"
cadtrain$CAD[cadtrain$CAD == "No"] <- "0"
cadtrain$CAD<-as.integer(cadtrain$CAD)

x1 <- sample(0:1,5,replace=T)
x2 <- sample(0:1,5,replace=T)
x3 <- sample(0:1,5,replace=T)
x4 <- sample(0:1,5,replace=T)
x5 <- sample(0:1,5,replace=T)
x6 <- sample(0:1,5,replace=T)
dftest<-data.frame(cbind(x1,x2,x3,x4,x5,x6))

colnames(dftest)<-(c("Sex","SuffHeartF","Hyperchol","Smoker","Inherit","CAD"))
for (i in 1:5){
  if (x1[i]==0){
    df5$Sex[i]<-"Male"
  }
  else{
    df5$Sex[i]<-"Female"
  }
  if (x2[i]==0){
    df5$SuffHeartF[i]="Yes"
  }
  else{
    df5$SuffHeartF[i]="No"
  }
  
  if (x3[i]==0){
    df5$Hyperchol[i]="Yes"
  }
  else{
    df5$Hyperchol[i]="No"
  }
  if (x4[i]==0){
    df5$Smoker[i]="Yes"
  }
  else{
    df5$Smoker[i]="No"
  }
  if (x5[i]==0){
    df5$Inherit[i]="Yes"
  }
  else{
    df5$Inherit[i]="No"
  }
  if (x6[i]==0){
    df5$CAD[i]="Yes"
  }
  else{
    df5$CAD[i]="No"
  }
}
df5<-as.data.frame(df5)

SmokeTrain<-lm(Smoker~.,cadtrain)
smokepred<-predict.lm(SmokeTrain,dftest)

cadTrain<-lm(CAD~.,cadtrain)
cadpred<-predict.lm(cadTrain,dftest)


################################

x51 <- sample(0:1,500,replace=T)
x52 <- sample(0:1,500,replace=T)
x53 <- sample(0:1,500,replace=T)
x54 <- sample(0:1,500,replace=T)
x55 <- sample(0:1,500,replace=T)
x56 <- sample(0:1,500,replace=T)

dftest5<-data.frame(cbind(x51,x52,x53,x54,x55,x56))

colnames(dftest5)<-(c("Sex","SuffHeartF","Hyperchol","Smoker","Inherit","CAD"))
df500<-colnames(c("Sex","SuffHeartF","Hyperchol","Smoker","Inherit","CAD"))
for (i in 1:500){
  if (x51[i]==0){
    df500$Sex[i]<-"Male"
  }
  else{
    df500$Sex[i]<-"Female"
  }
  if (x52[i]==0){
    df500$SuffHeartF[i]="Yes"
  }
  else{
    df500$SuffHeartF[i]="No"
  }
  
  if (x53[i]==0){
    df500$Hyperchol[i]="Yes"
  }
  else{
    df500$Hyperchol[i]="No"
  }
  if (x54[i]==0){
    df500$Smoker[i]="Yes"
  }
  else{
    df500$Smoker[i]="No"
  }
  if (x55[i]==0){
    df500$Inherit[i]="Yes"
  }
  else{
    df500$Inherit[i]="No"
  }
  if (x56[i]==0){
    df500$CAD[i]="Yes"
  }
  else{
    df500$CAD[i]="No"
  }
}
df500<-as.data.frame(df500)

smokepred5<-predict.lm(SmokeTrain,dftest5)
cadpred5<-predict.lm(cadTrain,dftest5)
