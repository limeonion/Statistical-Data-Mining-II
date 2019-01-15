
g<-list(~Burglary, ~Earthquake, ~TV, ~Nap,~Johncall|Burglary:Earthquake:TV, ~Marycall|Burglary:Earthquake:Nap:TV)
dagL <- dagList(g)
plot(dagL)
