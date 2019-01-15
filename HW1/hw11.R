# install.packages("qlcMatrix")
# library(qlcMatrix)

#Part A
m<-matrix(NA, nrow = 3, ncol = 8)
colnames(m)<-c("a","b","c","d","e","f","g","h")
rownames(m)<-c("A","B","C")
vecA<-c(4,5,NA,5,1,NA,3,2)
vecB<-c(NA,3,4,3,1,2,1,NA)
vecC<-c(2,NA,1,3,NA,4,5,3)
m[1,]<-vecA
m[2,]<-vecB
m[3,]<-vecC
m
m1<-m
for (i in 1:nrow(m1)){
  for (j in 1:ncol(m1)){
    if (is.na(m1[i,j])){
      m1[i,j]<-0
    }else{
      m1[i,j]<-1
    }
    
    
  }
}
#install.packages("proxy")

JacOrig<-proxy::dist(m1, by_rows = TRUE, method = "Jaccard")
cosOrig<-proxy::dist(m1, by_rows = TRUE, method = "cosine")

#################
#Part B
mnew<-m
for (i in 1:nrow(mnew)){
  for (j in 1:ncol(mnew)){
    ifelse(mnew[i,j]>2, mnew[i,j]<-1, mnew[i,j]<-0)
    if (is.na(mnew[i,j])){
      mnew[i,j]<-0
    }
    
    
    }
}
mnew
JacDisc<-proxy::dist(mnew, by_rows = TRUE, method = "Jaccard")
cosDisc<-proxy::dist(mnew, by_rows = TRUE, method = "cosine")
JacDisc
JacOrig

cosOrig
cosDisc

################
#Part C
mean<-rowSums(m, na.rm = TRUE, dims = 1)/ncol(m)
normMat<-m-mean
cosNorm<-proxy::dist(normMat, by_rows = TRUE, method = "cosine")
cosNorm
#############
