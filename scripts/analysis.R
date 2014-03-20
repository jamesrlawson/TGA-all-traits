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


##############################################################################################
#############################  BEGIN TGA ANALYSIS ############################################
##############################################################################################

# now run TGA script for all traits (nullmodel appears to be broken when using sppmeans - check later)
# output D, Sna, T and N1[2] for each trait. if in situ = FALSE...

# using in situ data = TRUE because I can't get it working with FALSE (except first time)

source("scripts/tga.R")

## for maxheight ##

      InSituTraitData = TRUE
      
      if (InSituTraitData) {
        # ** working directory should be set to location of data files **
        # Enter name of datafile below
        datafile = 'output/maxheight.txt' # USER: enter datafile
        trt.col = 4 #USER: column of data file with desired trait
        D = load.data(datafile,trt.col)
      } else {
        #if trt.col = NA then no insitu trait data in available - expand trait data from species means:
        #  meansfile is name of file with species means, species names (exactly matching data file) should be in first column. Trait means should be in desired units and transformation
        # 	colnum is column number with means for appropriate trait
        
        meansfile = 'output/maxheight_sppmeans.txt'
        colnum = 2
        D=expand.spp.means(D,meansfile,colnum)
        # END expand species means
      }
      
      
      print(names(D))
      print(dim(D))
      
      if (sum(is.na(D[,4]))>0) {
        print('Missing values present in file') 
        missval = TRUE
      } else {
        print('trait data is complete in D')
        missval = FALSE
      }
      
      #Examine distribution of trait data, and decide whether to transform
      par(mfrow=c(2,2))
      hist(D[,4])
      hist(log10(D[,4]))
      hist(sqrt(D[,4]))
      par(mfrow=c(1,1))
      
      # set log and weighted options
      trtname = names(D)[ncol(D)]
      logt = TRUE #USER: log transform trait data?
      sqrt.t = FALSE #USER: square root transform trait data?
      weighted=TRUE #USER: use abundance weighted analysis?
      dname = paste(trtname,'.W-',weighted,sep='')
      
      
      #Log transform data - any transformation may be conducted at this point and carried through the rest of the analysis
      if (logt) {
        D[,4] = log10(D[,4])
        names(D)[4] = paste('log_',names(D)[4],sep='')
        if (ncol(D) == 5) {
          D[,5] = log10(D[,5])
          names(D)[5] = paste('log_',names(D)[5],sep='')
        }
        dname = paste('output','/',trtname,'.logt','.W-',weighted,sep='')
      }
      
      if (sqrt.t) {
        D[,4] = sqrt(D[,4])
        names(D)[4] = paste('sqrt_',names(D)[4],sep='')
        if (ncol(D) == 5) {
          D[,5] = sqrt(D[,5])
          names(D)[5] = paste('sqrt_',names(D)[5],sep='')
        }
        
        dname = paste('output','/',trtname,'.sqrt','.W-',weighted,sep='')
      }
      print(dname)
      
      ## End enter data ##
      
      
      ## Expand missing data ##
      #IF missing values present, they can be expanded with species means calculated from the available data, or read in from an independent species means file; expanded data will be placed in column 4 and original data with NAs moved to col 5
      
      D=expand.spp.means.internal(D,weighted)
      
      # check that expansion of means worked
      if (sum(is.na(D[,4]))>0) print('Missing values present in file') else print('trait data is complete in D')
      ## End expand missing data ##
      
      ## Trait gradient analysis ##
      # set options and traitname:
      
      ## CORE ANALYSIS
      T = calc.means(D,weighted)
      S = calc.spec.attr(T,calc.slopes=TRUE,use.spmeans=TRUE)
      if (missval) {
        Sna = calc.spec.attr(T,calc.slopes=TRUE,use.spmeans=FALSE) 
      } else {
        Sna = S
      }
      P = calc.plot.attr(Sna,T)
      plotTS(S,T,weighted)

print(dname)
write.tg.files(dname,D,T,S,Sna,P)

## for seedmass ##

      source("scripts/tga.R")

      InSituTraitData = TRUE
      
      if (InSituTraitData) {
        # ** working directory should be set to location of data files **
        # Enter name of datafile below
        datafile = 'output/seedmass.txt' # USER: enter datafile
        trt.col = 4 #USER: column of data file with desired trait
        D = load.data(datafile,trt.col)
      } else {
        #if trt.col = NA then no insitu trait data in available - expand trait data from species means:
        #  meansfile is name of file with species means, species names (exactly matching data file) should be in first column. Trait means should be in desired units and transformation
        # 	colnum is column number with means for appropriate trait    
        
        meansfile = 'output/seedmass_sppmeans.txt'
        colnum = 2
        D=expand.spp.means(D,meansfile,colnum)
        # END expand species means
      }
      
      
      print(names(D))
      print(dim(D))
      
      if (sum(is.na(D[,4]))>0) {
        print('Missing values present in file') 
        missval = TRUE
      } else {
        print('trait data is complete in D')
        missval = FALSE
      }
      
      #Examine distribution of trait data, and decide whether to transform
      par(mfrow=c(2,2))
      hist(D[,4])
      hist(log10(D[,4]))
      hist(sqrt(D[,4]))
      par(mfrow=c(1,1))
      
      # set log and weighted options
      trtname = names(D)[ncol(D)]
      logt = TRUE #USER: log transform trait data?
      sqrt.t = FALSE #USER: square root transform trait data?
      weighted=TRUE #USER: use abundance weighted analysis?
      dname = paste(trtname,'.W-',weighted,sep='')
      
      
      #Log transform data - any transformation may be conducted at this point and carried through the rest of the analysis
      if (logt) {
        D[,4] = log10(D[,4])
        names(D)[4] = paste('log_',names(D)[4],sep='')
        if (ncol(D) == 5) {
          D[,5] = log10(D[,5])
          names(D)[5] = paste('log_',names(D)[5],sep='')
        }
        dname = paste('output','/',trtname,'.sqrt','.W-',weighted,sep='')
      }
      
      if (sqrt.t) {
        D[,4] = sqrt(D[,4])
        names(D)[4] = paste('sqrt_',names(D)[4],sep='')
        if (ncol(D) == 5) {
          D[,5] = sqrt(D[,5])
          names(D)[5] = paste('sqrt_',names(D)[5],sep='')
        }
        
        dname = paste('output','/',trtname,'.sqrt','.W-',weighted,sep='')
      }
      print(dname)
      
      ## End enter data ##
      
      
      ## Expand missing data ##
      #IF missing values present, they can be expanded with species means calculated from the available data, or read in from an independent species means file; expanded data will be placed in column 4 and original data with NAs moved to col 5
      
      D=expand.spp.means.internal(D,weighted)
      
      # check that expansion of means worked
      if (sum(is.na(D[,4]))>0) print('Missing values present in file') else print('trait data is complete in D')
      ## End expand missing data ##
      
      ## Trait gradient analysis ##
      # set options and traitname:
      
      ## CORE ANALYSIS
      T = calc.means(D,weighted)
      S = calc.spec.attr(T,calc.slopes=TRUE,use.spmeans=TRUE)
      if (missval) {
        Sna = calc.spec.attr(T,calc.slopes=TRUE,use.spmeans=FALSE) 
      } else {
        Sna = S
      }
      P = calc.plot.attr(Sna,T)
      plotTS(S,T,weighted)