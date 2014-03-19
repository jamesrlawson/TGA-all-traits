source("scripts/functions.R")
options(stringsAsFactors = FALSE)

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

# add total cover into plotsums df
maxheight.cover <- merge(maxheight.cover, maxheight.totalcover, by.x = "plotID", by.y = "plotID")

# find relative abundance

