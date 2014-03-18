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

# subset by stratum

ground <- percentcover [percentcover$stratum == "groundcover",]
shrub <- percentcover [percentcover$stratum == "shrub",]
subcanopy <- percentcover [percentcover$stratum == "subcanopy",]
canopy <- percentcover [percentcover$stratum == "canopy",]

# find trait values at each layer

maxheight.ground <- traitsbylayer(maxheight, ground)
maxheight.shrub <- traitsbylayer(maxheight, shrub)
maxheight.subcanopy <- traitsbylayer(maxheight, subcanopy)
maxheight.canopy <- traitsbylayer(maxheight, canopy)

seedmass.ground <- traitsbylayer(seedmass, ground)
seedmass.shrub <- traitsbylayer(seedmass, shrub)
seedmass.subcanopy <- traitsbylayer(seedmass, subcanopy)
seedmass.canopy <- traitsbylayer(seedmass, canopy)

SLA.ground <- traitsbylayer(SLA, ground)
SLA.shrub <- traitsbylayer(SLA, shrub)
SLA.subcanopy <- traitsbylayer(SLA, subcanopy)
SLA.canopy <- traitsbylayer(SLA, canopy)

WD.ground <- traitsbylayer(WD, ground)
WD.shrub <- traitsbylayer(WD, shrub)
WD.subcanopy <- traitsbylayer(WD, subcanopy)
WD.canopy <- traitsbylayer(WD, canopy)

totalabundances =  as.data.frame(cbind("maxheight.ground" = c(tapply(maxheight.ground$avgcover, maxheight.ground$plotID, sum)),
                                       "maxheight.shrub" = c(tapply(maxheight.shrub$avgcover, maxheight.shrub$plotID, sum)),
                                       "maxheight.subcanopy" = c(tapply(maxheight.subcanopy$avgcover, maxheight.subcanopy$plotID, sum)),
                                       "maxheight.canopy" = c(tapply(maxheight.canopy$avgcover, maxheight.canopy$plotID, sum)),
                                       "seedmass.ground" = c(tapply(seedmass.ground$avgcover, seedmass.ground$plotID, sum)),
                                       "seedmass.shrub" = c(tapply(seedmass.shrub$avgcover, seedmass.shrub$plotID, sum)),
                                       "seedmass.subcanopy" = c(tapply(seedmass.subcanopy$avgcover, seedmass.subcanopy$plotID, sum)),
                                       "seedmass.canopy" = c(tapply(seedmass.canopy$avgcover, seedmass.canopy$plotID, sum)),
                                       "SLA.ground" = c(tapply(SLA.ground$avgcover, SLA.ground$plotID, sum)),
                                       "SLA.shrub" = c(tapply(SLA.shrub$avgcover, SLA.shrub$plotID, sum)),
                                       "SLA.subcanopy" = c(tapply(SLA.subcanopy$avgcover, SLA.subcanopy$plotID, sum)),
                                       "SLA.canopy" = c(tapply(SLA.canopy$avgcover, SLA.canopy$plotID, sum)),
                                       "WD.ground" = c(tapply(WD.ground$avgcover, WD.ground$plotID, sum)),
                                       "WD.shrub" = c(tapply(WD.shrub$avgcover, WD.shrub$plotID, sum)),
                                       "WD.subcanopy" = c(tapply(WD.subcanopy$avgcover, WD.subcanopy$plotID, sum)),
                                       "WD.canopy" = c(tapply(WD.canopy$avgcover, WD.canopy$plotID, sum))))

# get total abundances over all layers (sum by row for specified columns)

totalabundances$maxheight <- rowSums(totalabundances[, c(2, 3, 4, 5)])
totalabundances$seedmass <- rowSums(totalabundances[, c(6, 7, 8, 9)])
totalabundances$SLA <- rowSums(totalabundances[, c(10, 11, 12, 13)])
totalabundances$WD <- rowSums(totalabundances[, c(6, 7, 8, 9)])

                             
                                      




traitslayers <- as.data.frame(cbind("traits" = c("maxheight","seedmass","SLA","WD"),
                                    "layers" = c("ground", "shrub", "subcanopy", "canopy")))

for (i in 1:4(stuff) {

  a <- traitsbylayer(traitslayers$traits[i], traitslayers$layers[i])

  write.table(traitsbylayer.df, file="output/fitmodels.csv", append=TRUE, sep=",", col.names=FALSE, row.names=FALSE)

  
  
  
  
  plotsavgs <- cbind("plotID" = unique(canopy$plotID),
                     "avg" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))