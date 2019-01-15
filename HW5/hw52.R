rm(list=ls())

library(tidyverse)
library(magrittr)
set.seed(1)

titanic <- read_csv("/home/shreyas/Downloads/train.csv")


# Use any clustering method. Mostly an analysis
titanic = subset(titanic, select =-c(PassengerId, Name, Ticket, Cabin, SibSp, Parch))

#titanic <- titanic %>% select(-`PassengerId`, -`Name`, -`Ticket`, -`Cabin`, -`SibSp`, -`Parch`)
#titanic<-titanic[,-c('PassengerId', 'Name', 'Ticket', 'Cabin', 'SibSp', 'Parch')]
#titanic <- titanic %>% na.omit() # replace with data cleaning
titanic<-na.omit(titanic)
clust <- hclust(dist(titanic %>% as.matrix))
#clust<-hclust(dist(as.matrix(titanic)))
#clust %>% plot(labels = FALSE, hang = 0, main = 'Cluster')
plot(clust)
clustcut <- cutree(clust, k = 5)

titanic %>% filter(clustcut == 1) %$% Survived %>% sum %>% divide_by(564)
titanic %>% filter(clustcut == 2) %$% Survived %>% sum %>% divide_by(100)
titanic %>% filter(clustcut == 3) %$% Survived %>% sum %>% divide_by(15)
titanic %>% filter(clustcut == 4) %$% Survived %>% sum %>% divide_by(30)
titanic %>% filter(clustcut == 5) %$% Survived %>% sum %>% divide_by(3)

combine <- rbind(titanic %>% filter(clustcut == 2),titanic %>% filter(clustcut == 3),titanic %>% filter(clustcut == 4),titanic %>% filter(clustcut == 5))

titanic %>% filter(clustcut == 1) %$% Sex %>% table()
combine %$% Sex %>% table()

titanic %>% filter(clustcut == 1) %$% Age %>% hist()
combine %$% Age %>% hist()

titanic %>% filter(clustcut == 1) %$% Pclass %>% table()
combine %$% Pclass %>% table()



titanic$Sex[titanic$Sex == 'male'] <- 0
titanic$Sex[titanic$Sex == 'female'] <- 1

titanic$Embarked[titanic$Embarked == 'C'] <- 0
titanic$Embarked[titanic$Embarked == 'Q'] <- 1
titanic$Embarked[titanic$Embarked == 'S'] <- 2


kmean3 <- kmeans(titanic,3)

table(kmean3$cluster, titanic$Survived)

#titanic %>% filter(kmean3$cluster == 3)
combine2 <- rbind(titanic %>% filter(kmean3$cluster == 1),titanic %>% filter(kmean3$cluster == 2))

titanic %>% filter(kmean3$cluster == 3) %$% Sex %>% table()
combine2 %$% Sex %>% table()

titanic %>% filter(kmean3$cluster == 3) %$% Age %>% hist()
combine2 %$% Age %>% hist()

titanic %>% filter(kmean3$cluster == 3) %$% Pclass %>% table()
combine2 %$% Pclass %>% table()