#spread <- function(x) (diff(range(x)))

#seedmass_betaT.range <- tapply(seedmass$betaT, seedmass$plotID, spread)

# hydro_ranges <- hydro

#hydro_ranges <- cbind(hydro_ranges, seedmass_betaT.range)

##

alltraits_betaT <- cbind(
  WDavg$betaT, 
  seedmass$betaT, 
  maxheight$betaT,
  SLA$betaT)


alltraits_ts <- cbind(
  WDavg$ts,
  seedmass$ts,
  maxheight$ts,
  SLA$ts)

alltraits_alphaT <- cbind(
  WDavg$alphaT, 
   seedmass$alphaT, 
   maxheight$alphaT, 
   SLA$alphaT)


colnames(alltraits_betaT) <- c("WDavg.betaT", 
                               "seedmass.betaT",
                               "maxheight.betaT",
                               "SLA.betaT")

colnames(alltraits_alphaT) <- c("WDavg.alphaT", 
                               "seedmass.alphaT",
                               "maxheight.alphaT",
                               "SLA.alphaT")

colnames(alltraits_ts)    <- c("WDavg.ts",
                               "seedmass.ts",
                               "maxheight.ts",
                               "SLA.ts")

alltraits_betaT <- as.data.frame(alltraits_betaT)
alltraits_alphaT <- as.data.frame(alltraits_alphaT)
alltraits_ts <- as.data.frame(alltraits_ts)

  
WDavg_T <- read.csv("TGA all traits/output_wood density avg/T_hydro.csv", header=TRUE)
seedmass_T <- read.csv("TGA all traits/output_seedmass/T_hydro.csv", header=TRUE)
maxheight_T <- read.csv("TGA all traits/output_maxheight/T_hydro.csv", header=TRUE)
SLA_T <- read.csv("TGA all traits/output_SLA/T_hydro.csv", header=TRUE)

alltraitmeans_tp <- cbind(
                      WDavg_T$tp, 
                      seedmass_T$tp, 
                      maxheight_T$tp, 
                      SLA_T$tp)

alltraitmeans_betaT.range <- cbind(
                                  hydro_ranges$WDavg_betaT.range,
                                  hydro_ranges$seedmass_betaT.range,
                                  hydro_ranges$maxheight_betaT.range,
                                  hydro_ranges$SLA_betaT.range)

colnames(alltraitmeans_tp) <- c("WDavg_T.tp",
                                 "seedmass.tp",
                                 "maxheight.tp", 
                                 "SLA_T.tp")

colnames(alltraitmeans_betaT.range) <- c("hydro_ranges.WDavg_betaT.range",
                                         "hydro_ranges.seedmass_betaT.range",
                                         "hydro_ranges.maxheight_betaT.range",
                                         "hydro_ranges.SLA_betaT.range")

alltraitmeans_tp <- as.data.frame(alltraitmeans_tp)
alltraitmeans_betaT.range <- as.data.frame(alltraitmeans_betaT.range)

alltraits_betaT.cor <- cor(alltraits_betaT)
alltraits_alphaT.cor <- cor(alltraits_alphaT)
alltraits_ts.cor <- cor(alltraits_ts)

alltraitmeans_tp.cor <- cor(alltraitmeans_tp)
alltraitmeans_betaT.range.cor <- cor(alltraitmeans_betaT.range)



pairs(alltraitmeans_tp)
pairs(alltraitmeans_betaT.range)

pairs(alltraits_ts)
pairs(alltraits_betaT)
pairs(alltraits_alphaT)

# run some PCAs

library(ggbiplot)
library(ggplot2)

## these first princomps don't seem to work

alltraits_ts.PCA <- princomp(alltraits_ts, cor=TRUE, scale = TRUE)
summary(alltraits_ts.PCA)

#g <- ggbiplot(alltraits_ts.PCA, obs.scale = 1, var.scale = 1, 
#              groups = D_Sna_hydro$category, ellipse = TRUE, circle = TRUE)
#g <- g + scale_color_continous(name = '')
#g <- g + theme(legend.direction = 'horizontal', 
#               legend.position = 'top')
#g <- g + ggtitle("PCA of hydro metrics related to ts, plots circled by hydro category")
#print(g)

alltraits_betaT.PCA <- princomp(alltraits_betaT, cor=TRUE, scale = TRUE)
summary(alltraits_betaT.PCA)

#g <- ggbiplot(alltraits_betaT.PCA, obs.scale = 1, var.scale = 1, 
#              groups = D_Sna_hydro$category, ellipse = TRUE, circle = TRUE)
#g <- g + scale_color_discrete(name = '')
#g <- g + theme(legend.direction = 'horizontal', 
#               legend.position = 'top')
#g <- g + ggtitle("PCA of hydro metrics related to betaT, plots circled by hydro category")
#print(g)

alltraits_alphaT.PCA <- princomp(alltraits_alphaT, cor=TRUE, scale = TRUE)
summary(alltraits_alphaT.PCA)

#g <- ggbiplot(alltraits_alphaT.PCA, obs.scale = 1, var.scale = 1, 
              groups = D_Sna_hydro$category, ellipse = TRUE, circle = TRUE)
#g <- g + scale_color_discrete(name = '')
#g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
# <- g + ggtitle("PCA of hydro metrics related to alphaT, plots circled by hydro category")
#print(g)

alltraitmeans_betaT.range.PCA <- princomp(alltraitmeans_betaT.range, cor=TRUE, scale = TRUE)

g <- ggbiplot(alltraitmeans_betaT.range.PCA, obs.scale = 1, var.scale = 1, 
              groups = hydro$category, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
g <- g + ggtitle("PCA of hydro metrics related to betaT.range, plots circled by hydro category")
print(g)

summary(alltraitmeans_betaT.range.PCA)

alltraitmeans_tp.PCA <- princomp(alltraitmeans_tp, cor=TRUE, scale = TRUE)

g <- ggbiplot(alltraitmeans_tp.PCA, obs.scale = 1, var.scale = 1, 
              groups = hydro$category, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
g <- g + ggtitle("PCA of hydro metrics related to tp, plots circled by hydro category")
print(g)

summary(alltraitmeans_tp.PCA)
