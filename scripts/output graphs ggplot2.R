library(ggplot2)

tp.lm <- function(k, data=T_hydro) {
  
  # set x as the head of each column within data  
  
  x  <- data[,k]
  
  # pull out p-value and R2 from linear model
  
  pvalue <- pf(summary(lm(tp ~ x, data=data))$fstatistic[1L], summary(lm(tp ~ x, data=data))$fstatistic[2L], summary(lm(tp ~ x, data=data))$fstatistic
               [3L],lower.tail = F)
  
  pvaluerounded <- signif(pvalue, digits = 3)
  
  Rsquared <- summary(lm(tp ~ x, data=data))[8]
  
  # create output dir
  
  mainDir <- "C:/Users/JLawson/Desktop/stuff/data/analysis/R"
  subDir <- "TGA all traits/output/tp"
  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  
  
  # use sprintf within the jpeg graphics device to add pvals and R2's to our file names  
  
  jpeg(sprintf("TGA all traits/output/tp/tp_%s_p-%s_r2-%s.jpg", k, pvaluerounded, Rsquared), quality = 100, width = 600, height = 500)
  
  # ggplot2 plot  
  
  p <- qplot(x, tp, data = T_hydro)
#  p <- p + geom_smooth(method="lm", se=TRUE) # plot a lm line with 95% confidences - set se=FALSE to remove
  p <- p + geom_point(aes(colour = factor(T_hydro$category), size = 4)) # give the points some colours according to their hydrological class
  p <- p + guides(size=FALSE) # removes size legend because it's been set to a constant
#  p <- p + xlab(k) + ylab("betaT range")
  
  cols <- c("stable winter baseflow"="orange","unpredictable baseflow"="blue","unpredictable intermittent"="green") # make a vector describing names and colours
  p <- p + scale_colour_manual(name="Hydrological class", values=cols) # give the legend a name and set it to describe the categories according to the colours and names set in cols
  p <- p + theme(legend.position="top") # position legend at bottom of graph
  
  # p <- p + ggtitle("Relationship of site mean niche breadths (Rs.mean) and betaT ranges") 
  
  print(p)
  
  
  dev.off()
  
}

tpmodels = sapply(names(T_hydro), function(z)tp.lm(k=z, data=T_hydro)) # I still don't know how this actually works 

