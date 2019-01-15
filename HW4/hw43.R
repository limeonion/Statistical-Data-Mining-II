#PROBLEM 3

library(gRain)
#library(RHugin)
library(Rgraphviz)
library(gRbase)
library(ggm)
library(gRim)
library(bnlearn)
library(igraph)

g <- list(~a, ~b,~c|a, ~d|a:b, ~e|b, ~f|c:e, ~g|d:e, 
          ~h|f:g)
chestdag <- dagList(g)

dSep(as(chestdag, "matrix"), "c", "g", c())
dSep(as(chestdag, "matrix"), "c", "e", c())
dSep(as(chestdag, "matrix"), "c", "e", c("g"))
dSep(as(chestdag, "matrix"), "a", "g", c("d", "e"))
dSep(as(chestdag, "matrix"), "a", "g", c("d"))