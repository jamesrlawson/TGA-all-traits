source("scripts/functions.R")
options(stringsAsFactors = FALSE)
library(plyr)

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

output(maxheight.cover)
output(seedmass.cover)
output(SLA.cover)
output(WD.cover)

# what are the most common species? (not necessary for this analysis but I wanted to know and the data was loaded...)
commonspp <- ddply(percentcover, .(plotID), summarise, uniquespp. = unique(species))
commonspp <- ddply(commonspp, .(uniquespp.), transform, idcount=length(uniquespp.)) # don't know what transform does here but it works..
commonspp <- arrange(commonspp, desc(idcount))
write.csv(commonspp, file="C:/Users/JLawson/Desktop/stuff/glasshouse/commonspp.csv")
View(commonspp)
