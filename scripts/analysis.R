source("scripts/functions.R")
source("scripts/tga1.R")
options(stringsAsFactors = FALSE)
library(plyr)
library(ggplot2)

#############################################################################################
############################# GET RELATIVE ABUNDANCES FOR ALL TRAITS ########################
#############################################################################################

# load in data

alltraits <- read.csv("data/alltraits.csv", header=TRUE)
percentcover <- read.csv("data/percentcover.csv", header=TRUE)

# subset out individual traits and remove null observations

maxheight <-   na.omit(
               cbind("species" = alltraits[1], 
                     "maxheight" = alltraits[2]))

seedmass <- na.omit(
              cbind("species" = alltraits[1], 
                    "seedmass" = alltraits[3]))

SLA <- na.omit(
         cbind("species" = alltraits[1], 
               "SLA" = alltraits[4]))

WD <- na.omit(
        cbind("species" = alltraits[1], 
              "WD" = alltraits[5]))

# remove observations that average less than 1% cover
percentcover <- subset(percentcover, avgcover > 1)

# generate df with total % cover (across all strata) of species at each plot
plotsums <- ddply(percentcover, .(plotID, species), summarise, speciescover = sum(avgcover))

# find species in % cover list associated with trait values 
maxheight.cover <- findtraitvals(plotsums, maxheight)
seedmass.cover <- findtraitvals(plotsums, seedmass)
SLA.cover <- findtraitvals(plotsums, SLA)
WD.cover <- findtraitvals(plotsums, WD)

# find total cover at each plot for traits

maxheight.totalcover <- ddply(maxheight.cover, .(plotID), summarise, totalcover = sum(speciescover))
seedmass.totalcover <- ddply(seedmass.cover, .(plotID), summarise, totalcover = sum(speciescover))
SLA.totalcover <- ddply(SLA.cover, .(plotID), summarise, totalcover = sum(speciescover))
WD.totalcover <- ddply(WD.cover, .(plotID), summarise, totalcover = sum(speciescover))

# add total cover into cover dfs
maxheight.cover <- merge(maxheight.cover, maxheight.totalcover, by.x = "plotID", by.y = "plotID")
seedmass.cover <- merge(seedmass.cover, seedmass.totalcover, by.x = "plotID", by.y = "plotID")
SLA.cover <- merge(SLA.cover, SLA.totalcover, by.x = "plotID", by.y = "plotID")
WD.cover <- merge(WD.cover, WD.totalcover, by.x = "plotID", by.y = "plotID")


# calculate and add relative abundance

maxheight.cover <- relabund(maxheight.cover)
seedmass.cover <- relabund(seedmass.cover)
SLA.cover <- relabund(SLA.cover)
WD.cover <- relabund(WD.cover)

test <- ddply(maxheight.cover, .(plotID), summarise, summedRelCover = (sum(relcover)))
print(test)
rm(test)

# rejig for insertion into TGA script

output(maxheight.cover, maxheight)
output(seedmass.cover, seedmass)
output(SLA.cover, SLA)
output(WD.cover, WD)

# what are the most common species? (not necessary for this analysis but I wanted to know and the data was loaded...)
#commonspp <- ddply(percentcover, .(plotID), summarise, uniquespp. = unique(species))
#commonspp <- ddply(commonspp, .(uniquespp.), transform, idcount=length(uniquespp.)) # don't know what transform does here but it works..
#commonspp <- arrange(commonspp, desc(idcount))
#write.csv(commonspp, file="C:/Users/JLawson/Desktop/stuff/glasshouse/commonspp.csv")
#View(commonspp)



## now run tga_alltraits.R script ##

setwd("C:/Users/JLawson/Desktop/stuff/data/analysis/R/TGAall2")
par(mfrow=c(1,1))
source('scripts/tga_alltraits.R')

# now load up the various outputs

D_maxheight <- read.csv("output/maxheight.logt.W-TRUE/D.csv", header=TRUE)
Sna_maxheight <-  read.csv("output/maxheight.logt.W-TRUE/Sna.csv", header=TRUE)
T_maxheight <- read.csv("output/maxheight.logt.W-TRUE/T.csv", header=TRUE)

D_seedmass <- read.csv("output/seedmass.logt.W-TRUE/D.csv", header=TRUE)
Sna_seedmass <-  read.csv("output/seedmass.logt.W-TRUE/Sna.csv", header=TRUE)
T_seedmass <- read.csv("output/seedmass.logt.W-TRUE/T.csv", header=TRUE)

D_SLA <- read.csv("output/SLA.logt.W-TRUE/D.csv", header=TRUE)
Sna_SLA <-  read.csv("output/SLA.logt.W-TRUE/Sna.csv", header=TRUE)
T_SLA <- read.csv("output/SLA.logt.W-TRUE/T.csv", header=TRUE)

D_WD <- read.csv("output/WD.sqrt.W-TRUE/D.csv", header=TRUE)
Sna_WD <-  read.csv("output/WD.sqrt.W-TRUE/Sna.csv", header=TRUE)
T_WD <- read.csv("output/WD.sqrt.W-TRUE/T.csv", header=TRUE)

# merge Sna data with hydro data

hydro <- read.csv("data/hydronorm.csv", header=TRUE)
hydro$plot <- hydro$plotID
hydro$plotID <- NULL

  D_Sna_maxheight <- merge(D_maxheight, Sna_maxheight)
  D_Sna_hydro_maxheight <- merge(D_Sna_maxheight, hydro)
  
  D_Sna_seedmass <- merge(D_seedmass, Sna_seedmass)
  D_Sna_hydro_seedmass <- merge(D_Sna_seedmass, hydro)
  
  D_Sna_SLA <- merge(D_SLA, Sna_SLA)
  D_Sna_hydro_SLA <- merge(D_Sna_SLA, hydro)
  
  D_Sna_WD <- merge(D_WD, Sna_WD)
  D_Sna_hydro_WD <- merge(D_Sna_WD, hydro)

# make lists with all the betaT and alphaT values (for the sheer hell of it)

betaT.list <- list()
betaT.list$betaT_maxheight <- D_Sna_hydro_maxheight$betaT
betaT.list$betaT_seedmass <- D_Sna_hydro_seedmass$betaT
betaT.list$betaT_SLA <- D_Sna_hydro_SLA$betaT
betaT.list$betaT_WD <- D_Sna_hydro_WD$betaT

alphaT.list <- list()
alphaT.list$alphaT_maxheight <- D_Sna_hydro_maxheight$alphaT
alphaT.list$alphaT_seedmass <- D_Sna_hydro_seedmass$alphaT
alphaT.list$alphaT_SLA <- D_Sna_hydro_SLA$alphaT
alphaT.list$alphaT_WD <- D_Sna_hydro_WD$alphaT
  
# get tp's (site means) and betaT.ranges, put them in hydroplots

hydroplots <- hydro

hydroplots$Tp_maxheight <- unique(T_maxheight$tp)
hydroplots$Tp_seedmass <- unique(T_seedmass$tp)
hydroplots$Tp_SLA <- unique(T_SLA$tp)
hydroplots$Tp_WD <- unique(T_WD$tp)

tp <- data.frame(hydroplots$Tp_maxheight, 
                 hydroplots$Tp_seedmass, 
                 hydroplots$Tp_SLA, 
                 hydroplots$Tp_WD)
tp.cor <- cor(tp, method="pearson")

hydroplots$betaTrange_maxheight <- tapply(D_Sna_hydro_maxheight$betaT, D_Sna_hydro_maxheight$plot, spread)
hydroplots$betaTrange_seedmass <- tapply(D_Sna_hydro_seedmass$betaT, D_Sna_hydro_seedmass$plot, spread)
hydroplots$betaTrange_SLA <- tapply(D_Sna_hydro_SLA$betaT, D_Sna_hydro_SLA$plot, spread)
hydroplots$betaTrange_WD <- tapply(D_Sna_hydro_WD$betaT, D_Sna_hydro_WD$plot, spread)

betaTrange <- data.frame(hydroplots$betaTrange_maxheight, 
                         hydroplots$betaTrange_seedmass, 
                         hydroplots$betaTrange_SLA,
                         hydroplots$Tp_WD)

betaTrange.cor <- cor(betaTrange, method="pearson")
plot(betaTrange)

hydroplots$alphaTrange_maxheight <- tapply(D_Sna_hydro_maxheight$alphaT, D_Sna_hydro_maxheight$plot, spread)
hydroplots$alphaTrange_seedmass <- tapply(D_Sna_hydro_seedmass$alphaT, D_Sna_hydro_seedmass$plot, spread)
hydroplots$alphaTrange_SLA <- tapply(D_Sna_hydro_SLA$alphaT, D_Sna_hydro_SLA$plot, spread)
hydroplots$alphaTrange_WD <- tapply(D_Sna_hydro_WD$alphaT, D_Sna_hydro_WD$plot, spread)

# what even is alphaT range?!

alphaTrange <- data.frame(hydroplots$alphaTrange_maxheight, 
                          hydroplots$alphaTrange_seedmass, 
                          hydroplots$alphaTrange_SLA,
                          hydroplots$Tp_WD)

alphaTrange.cor <- cor(alphaTrange, method="pearson")
plot(alphaTrange)

# add Rs to hydroplots values for comparison with betaTrange

hydroplots$Rs_maxheight <- tapply(D_Sna_hydro_maxheight$Rs, D_Sna_hydro_maxheight$plot, mean)
hydroplots$Rs_seedmass <- tapply(D_Sna_hydro_seedmass$Rs, D_Sna_hydro_seedmass$plot, mean)
hydroplots$Rs_SLA <- tapply(D_Sna_hydro_SLA$Rs, D_Sna_hydro_SLA$plot, mean)
hydroplots$Rs_WD <- tapply(D_Sna_hydro_WD$Rs, D_Sna_hydro_WD$plot, mean)


########### LETS OUTPUT SOME GRAPHS - stuff, along hydro gradients #############


# plot.linear requires: df, var, trait, labels. 
# var is alphaT/betaT/ts/Rs, etc.



plot.linear(D_Sna_hydro_maxheight, D_Sna_hydro_maxheight$ts, maxheight)
plot.linear(D_Sna_hydro_seedmass, D_Sna_hydro_seedmass$ts, seedmass)
plot.linear(D_Sna_hydro_SLA, D_Sna_hydro_SLA$ts, SLA)
plot.linear(D_Sna_hydro_WD, D_Sna_hydro_WD$ts, WD)

plot.quad(D_Sna_hydro_maxheight, D_Sna_hydro_maxheight$ts, maxheight)
plot.quad(D_Sna_hydro_seedmass, D_Sna_hydro_seedmass$ts, seedmass)
plot.quad(D_Sna_hydro_SLA, D_Sna_hydro_SLA$ts, SLA)
plot.quad(D_Sna_hydro_WD, D_Sna_hydro_WD$ts, WD)

plot.linear(hydroplots, hydroplots$betaTrange_WD, WD)
plot.linear(hydroplots, hydroplots$betaTrange_SLA, SLA)
plot.linear(hydroplots, hydroplots$betaTrange_seedmass, seedmass)
plot.linear(hydroplots, hydroplots$betaTrange_maxheight, maxheight)

plot.quad(hydroplots, hydroplots$betaTrange_WD, WD)
plot.quad(hydroplots, hydroplots$betaTrange_SLA, SLA)
plot.quad(hydroplots, hydroplots$betaTrange_seedmass, seedmass)
plot.quad(hydroplots, hydroplots$betaTrange_maxheight, maxheight)

plot.linear(hydroplots, hydroplots$alphaTrange_WD, WD)
plot.linear(hydroplots, hydroplots$alphaTrange_SLA, SLA)
plot.linear(hydroplots, hydroplots$alphaTrange_seedmass, seedmass)
plot.linear(hydroplots, hydroplots$alphaTrange_maxheight, maxheight)

plot.quad(hydroplots, hydroplots$alphaTrange_WD, WD)
plot.quad(hydroplots, hydroplots$alphaTrange_SLA, SLA)
plot.quad(hydroplots, hydroplots$alphaTrange_seedmass, seedmass)
plot.quad(hydroplots, hydroplots$alphaTrange_maxheight, maxheight)

plot.linear(hydroplots, hydroplots$Tp_WD, WD)
plot.linear(hydroplots, hydroplots$Tp_SLA, SLA)
plot.linear(hydroplots, hydroplots$Tp_seedmass, seedmass)
plot.linear(hydroplots, hydroplots$Tp_maxheight, maxheight)

plot.quad(hydroplots, hydroplots$Tp_WD, WD)
plot.quad(hydroplots, hydroplots$Tp_SLA, SLA)
plot.quad(hydroplots, hydroplots$Tp_seedmass, seedmass)
plot.quad(hydroplots, hydroplots$Tp_maxheight, maxheight)

## stuff to do:
# fix plot.linear and plot.quadratic to output to the right directories using sprintf
# have an option somewhere to only use data for which entries exist for all traits
#   that way I can compare alphaT/betaT for traits directly, 
#   and can then compare alphaT/betaT with alphaTrange/betaT range relationships between traits
# have a think about whether range is the appropriate metric for dispersion 
#   (because we're comparing different traits with different units)
#
# something to do with null models...
#
# something seems fishy with the WD Tp values. Why are the results not the same as the WDmeans values?