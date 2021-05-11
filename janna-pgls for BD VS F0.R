## ----------------------------------------------------------------------------------------
library(ape)
library(phytools)
library(tidyverse)

#tree and data
treeparrot<-read.nexus("../data/ptree2")



## ----------------------------------------------------------------------------------------
Body_mass <- read.csv(("../../humm-swift/data/dunning_part1_measures.csv"))

Body_mass <- plyr::ddply(
  Body_mass,
  plyr::.(Species.name),
  function(i) {
    data.frame(
      mass_mean= mean(i$mean,na.rm = T)
    )
  })
library(tidyverse)

Body_mass <- rename(Body_mass,Species= Species.name) 
Body_mass <- mutate(Body_mass, Species= str_replace(Species, " ", "_"))
extraparrot <- read.csv("../data/unique parrots.csv")
Body_mass <- bind_rows(Body_mass,extraparrot) %>%
  dplyr::select(-c(notes, reference))


## ----------------------------------------------------------------------------------------
parrotdata <- read.table("../data/formatparrotforphy.txt",header = T)
parrotdata <- left_join(parrotdata, Body_mass)


## ----------------------------------------------------------------------------------------
newparrottree<-drop.tip(treeparrot, setdiff(treeparrot$tip.label, parrotdata$Species))
plotTree(newparrottree)

setdiff (parrotdata$Species,newparrottree$tip.label)

compdata<-comparative.data(newparrottree, parrotdata, names.col="Species")


## ----------------------------------------------------------------------------------------
PGLS<-pgls(log(f0_mean) ~ log(mass_mean), data=compdata)
summary(PGLS)


## ----------------------------------------------------------------------------------------
library(ggplot2)

# The plot:
ggplot(parrotdata, aes(log(mass_mean), log(f0_mean))) +
  geom_point(size=5) +
  # geom_text(aes(label=Species),hjust=-0.1, vjust=0.4) +
  xlab("ln Body mass (g)") +
  ylab("Fundamental frequency (Hz)") +
  geom_abline(
    intercept=PGLS$model$coef[1], 
    slope=PGLS$model$coef[2],
    colour="red", 
    size=1.3
  )

