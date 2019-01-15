rm(list = ls())
load("SwissBankNotes.rdata")

genuine <- SwissBankNotes[1:100,]
fake <- SwissBankNotes[101:200,]
allNotes <- SwissBankNotes

pc_gen <- princomp(genuine)
pc_fake <- princomp(fake)
pc_all <- princomp(allNotes)

x11()
plot(pc_gen)

x11()
plot(pc_fake)

x11()
plot(pc_all)