source("scripts/functions.R")
source("scripts/tga1.R")
options(stringsAsFactors = FALSE)
library(plyr)

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
source('scripts/tga_alltraits.R')

# now load up the various outputs

D_maxheight <- read.csv("output/maxheight.logt.W-TRUE/D.csv", header=TRUE)
Sna_maxheight <-  read.csv("output/maxheight.logt.W-TRUE/Sna.csv", header=TRUE)
