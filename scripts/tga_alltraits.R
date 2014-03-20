#############################  BEGIN TGA ANALYSIS ############################################
##############################################################################################

# now run TGA script for all traits (nullmodel appears to be broken when using sppmeans - check later)
# output D, Sna, T and N1[2] for each trait. if in situ = FALSE...

# using in situ data = TRUE because I can't get it working with FALSE (except first time)

setwd("C:/Users/JLawson/Desktop/stuff/data/analysis/R/TGAall2")
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
  #   colnum is column number with means for appropriate trait
  
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
setwd("C:/Users/JLawson/Desktop/stuff/data/analysis/R/TGAall2")
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

## for SLA ##
setwd("C:/Users/JLawson/Desktop/stuff/data/analysis/R/TGAall2")
source("scripts/tga.R")

InSituTraitData = TRUE

if (InSituTraitData) {
  # ** working directory should be set to location of data files **
  # Enter name of datafile below
  datafile = 'output/SLA.txt' # USER: enter datafile
  trt.col = 4 #USER: column of data file with desired trait
  D = load.data(datafile,trt.col)
} else {
  #if trt.col = NA then no insitu trait data in available - expand trait data from species means:
  #  meansfile is name of file with species means, species names (exactly matching data file) should be in first column. Trait means should be in desired units and transformation
  #   colnum is column number with means for appropriate trait    
  
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

## for WD ##
setwd("C:/Users/JLawson/Desktop/stuff/data/analysis/R/TGAall2")
source("scripts/tga.R")

InSituTraitData = TRUE

if (InSituTraitData) {
  # ** working directory should be set to location of data files **
  # Enter name of datafile below
  datafile = 'output/WD.txt' # USER: enter datafile
  trt.col = 4 #USER: column of data file with desired trait
  D = load.data(datafile,trt.col)
} else {
  #if trt.col = NA then no insitu trait data in available - expand trait data from species means:
  #  meansfile is name of file with species means, species names (exactly matching data file) should be in first column. Trait means should be in desired units and transformation
  #   colnum is column number with means for appropriate trait    
  
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
logt = FALSE #USER: log transform trait data?
sqrt.t = TRUE #USER: square root transform trait data?
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

setwd("C:/Users/JLawson/Desktop/stuff/data/analysis/R/TGAall2")
