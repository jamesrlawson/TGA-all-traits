#This file contains useful series of commands to run interactively

#load 'tg-functions.R'; check wdir is set to location of file
# ** set path to match file location on your computer **
source("scripts/tga1.R")

## Enter data ##
# Set this to true if species_plot data file also has in situ trait data, false if species means will be read in separately.

InSituTraitData = TRUE

if (InSituTraitData) {
  # ** working directory should be set to location of data files **
  # Enter name of datafile below
  datafile = 'output/maxheight.txt' # USER: enter datafile
  trt.col = 4 #USER: column of data file with desired trait
  D = load.data(datafile,trt.col)
} else {
  #if trt.col = NA then no insitu trait data in available - expand trait data from species means:
  #	meansfile is name of file with species means, species names (exactly matching data file) should be in first column. Trait means should be in desired units and transformation
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
## End trait gradient analysis

## Plot results highlighting selected species
## enter list of species rownumbers (from S) or names in next line
spp = c(20,27)
plot.species(S,T,spp)

#write results files to 'tg' directory after running analysis
# CHECK WORKING DIRECTORY IS CORRECT!
print(dname)
write.tg.files(dname,D,T,S,Sna,P)

## ADVANCED ANALYSIS - read comments for each function in tga.R
# remove next 2 lines??
# xxx Placeholder - how to make it easy for users to decide here!
# if (missval) missval = TRUE

# Remove each species in succession and recalculate parameters
# if use.spmeans = TRUE then pass S, else pass Sna
use.spmeans = TRUE
if (use.spmeans) Stmp=S else Stmp=Sna
Sj = calc.spec.attr.jackknife(D,T,Stmp,weighted,compare=TRUE,	calc.slopes=TRUE,use.spmeans=use.spmeans)

# Bootstrap T matrix to obtain confidence intervals on S variables
B1 = bootstrapD(D,T,reps=5,stratified=TRUE,showplot=FALSE,	showreps=TRUE,calc.slopes=TRUE,use.spmeans=use.spmeans)

# Null models of plot-species-trait associations
library(moments)
N1 = nullD(D,S,T,reps=100,
           randomize='plot',showplot=FALSE,calc.slopes=TRUE,
           use.spmeans=use.spmeans,showreps=TRUE)


#If you have conducted the analyses above previously,
#this allows you to read matrices back in from results directory.
#be sure to set correct options to set dname correctly, 
#and check that current working directory is correct
trtname = 'enter_trait_name_here'
logt = TRUE
sqrt.t = FALSE
weighted=TRUE

if (logt) {
  dname = paste(trtname,'.log','.W-',weighted,sep='')
} else if (sqrt.t) {
  dname = paste(trtname,'.sqrt','.W-',weighted,sep='')
} else {
  dname = paste(trtname,'.W-',weighted,sep='')
}
print(dname)

TG=read.tg.files(dname)
D=TG[[1]]
T=TG[[2]]
S=TG[[3]]
Sna=TG[[4]]
P=TG[[5]]