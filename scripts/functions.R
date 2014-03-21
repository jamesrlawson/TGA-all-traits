
# find species in % cover list associated with trait values

findtraitvals <- function(plotsums, trait) {
  
  
  
  y <- data.frame()
  
  for(i in 1:nrow(plotsums)) {
    
    species <- as.data.frame(plotsums$species)
    colnames(species) <- c("species")
    x <- species[i,]
    
    # grep returns index of matches (similar to match but returns all indexes, not just the first)
    
    x <- grep(x, plotsums$species, fixed=TRUE) 
    x <- plotsums[x,]
    x <- merge(x, trait, by.x = "species", by.y = "species")
    
    # iteratively add things to dataframe y!
    y <- rbind(y,x)
    
  }
  # subset by matches 
  #x <- plotsums[na.omit(x), ] 
    return(y)
}


# relabund finds relative % cover for each species at each site

relabund <- function(df) {
                              
                relcover <- df$speciescover / df$totalcover 
                cbind(df, "relcover" = relcover)
}
  
# output data to txt file, in format that is usable by TGA script

output <- function(df, trait) {
  
  dfname <- names(df[4])  
  
  x <- cbind(df[1],df[2],df[6],df[4])  
  
  colnames(x)[1] <- "plot"
  colnames(x)[3] <- "abund"
  
  y <- findtraitvals(plotsums, trait)
  y$plotID <- NULL
  y$speciescover <- NULL
  
  write.table(x, file=sprintf("output/%s.txt", dfname), sep ="\t", row.names=FALSE)
  write.table(y, file=sprintf("output/%s_sppmeans.txt", dfname), sep ="\t", row.names=FALSE)
  

}

spread <- function(x) (diff(range(x)))



plot.linear <- function(df, var, trait, labels) { # var is alphaT/betaT/ts/Rs, etc.
  
  mainDir <- "C:/Users/JLawson/Desktop/stuff/data/analysis/R/TGAall2"
  subDir <- deparse(substitute(trait))
  dir.create(file.path(mainDir,subDir))  
  
  for(i in 1:ncol(df)) {
    hydro <- df[[i]]  
    hydroname <- as.expression(colnames(df[i]))   
    fit.linear <- lm(var ~ hydro, data = df)
    
  #  padj <- labels$p.adj[i]
    r2 <- signif(summary(fit.quad)$r.squared, 5)
    
    png(sprintf("output/figures/%s/%s_p-%s_r2-%s.png", trait, hydroname, padj, r2), width = 600, height = 500)
    #on.exit(dev.off())
    
    p <- qplot(hydro, var, data = df) 
    p <- p + geom_point(aes(shape = labels$catname), size =3)
    p <- p + scale_shape_discrete(name = "Hydrological \n class", labels = c("stable winter baseflow", "unpredictable baseflow", "unpredictable intermittent"))
    p <- p + stat_smooth(method = "lm", formula = y ~ x, se=TRUE, col="black") 
    p <- p + xlab(hydroname)
  #  p <- p + ylim(0.45, 0.75)
    p <- p + ylab(labels$ylab)
   # p <- p + annotate("text",                    
   #                   x=max(hydro)/1.5, y=0.5,
   #                   label=paste("R^2 = ",signif(summary(fit.quad)$r.squared, 5),
   #                               "\np.adj =",labels$p.adj[i]),
   #                   size = 4)
    #    p <- p + ggtitle(labels$title)    
    p <- p + theme_minimal() # if you want to use a preset theme and then modify it, call it first
    p <- p + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(size=.7, color = "black"),
                   legend.position = "bottom",
                   panel.background = element_blank(),      
                   plot.title = element_text(size=12),
                   axis.text = element_text(size=12),
                   text = element_text(size=12))   
    
    print(p)
    dev.off()
  }
}

# keeping this for future reference. returns the dataframe D_Sna_hydro_*trait*

gettrait<- function(trait) {
  
  traitname <- deparse(substitute(trait))
  get(sprintf("D_Sna_hydro_%s", traitname))
  
}
