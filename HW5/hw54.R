data(state)
?state
library(kohonen)
state.scaled <- scale(state.x77)
state.som2 <- som(state.scaled, grid=somgrid(2,1,"hexagonal"))
x11()
plot(state.som2, main = "State Data")

names(state.som2)
state.som2$unit.classif
state.som2$codes

state.som2$changes
x11()
plot(state.som2$changes)

state.som3 <- som(state.scaled, grid=somgrid(3,1,"hexagonal"))
x11()
plot(state.som3, main = "State Data")

names(state.som3)
state.som3$unit.classif
state.som3$codes

state.som3$changes
x11()
plot(state.som3$changes)

state.som4 <- som(state.scaled, grid=somgrid(2,2,"hexagonal"))
x11()
plot(state.som4, main = "State Data")

names(state.som4)
state.som4$unit.classif
state.som4$codes

state.som4$changes
x11()
plot(state.som4$changes)

state.som20 <- som(state.scaled, grid=somgrid(5,4,"hexagonal"))
x11()
plot(state.som20, main = "State Data")

names(state.som20)
state.som20$unit.classif
state.som20$codes

state.som20$changes
x11()
plot(state.som20$changes)

library(gRbase)
library(gRim)
library(gRain)
library(glasso)
dats<-as.data.frame(state.x77)
x11()
pairs(dats)
S.body <- cov.wt(dats, method = "ML")
PC.body <- cov2pcor(S.body$cov)
heatmap(PC.body)

S <- S.body$cov 
library(graph)names(m0.lasso)

# Estimate over a range of rho's
rhos <- c(2, 4, 6, 10, 15)
m0.lasso <- glassopath(S, rho = rhos)
graphics.off()
for (i in 1:length(rhos)){
  my.edges <- m0.lasso$wi[, , i] != 0
  diag(my.edges) <- FALSE
  g.lasso <- as(my.edges, "graphNEL") # convert for plotting
  nodes(g.lasso) <- names(dats)
  glasso.net <- cmod(g.lasso, data = dats)
  
  x11()
  plot(glasso.net)
}
