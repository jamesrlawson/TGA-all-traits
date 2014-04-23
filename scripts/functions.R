
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



plot.linear <- function(df, var, trait) { # var is alphaT/betaT/ts/Rs, etc.
  
  figureDir <- "C:/Users/JLawson/Desktop/stuff/data/analysis/R/TGAall2/output/figures"
  traitDir <- deparse(substitute(trait))
  varDir <- deparse(substitute(var))
  
  outDir <- sprintf("%s/%s/%s/%s", figureDir, traitDir, varDir, "linear")
  
  dir.create(outDir, recursive=TRUE)
    
  for(i in 1:ncol(df)) {
    hydro <- df[[i]]  
    hydroname <- as.expression(colnames(df[i]))   
    fit.linear <- lm(var ~ hydro, data = df)
    
  #  padj <- labels$p.adj[i]
    r2 <- signif(summary(fit.linear)$r.squared, 5)
    pval <- anova(fit.linear)[1,"Pr(>F)"]
  
  labels <- list("ylab" <- c(deparse(substitute(trait))),
                 "catname" <- as.factor(df$category)
  )
    
    png(sprintf("%s/%s_pval-%s_r2-%s.png", outDir, hydroname, pval, r2), width = 600, height = 500)
    
    p <- qplot(hydro, var, data = df) 
    p <- p + geom_point()
    p <- p + geom_point(aes(shape = labels[2]), size =3)
    p <- p + scale_shape_discrete(name = "Hydrological \n class", labels = c("stable winter baseflow", "unpredictable baseflow", "unpredictable intermittent"))
    p <- p + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x, se=TRUE, col="black") 
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


plot.quad <- function(df, var, trait, labels) { # var is alphaT/betaT/ts/Rs, etc.
  
  
  figureDir <- "C:/Users/JLawson/Desktop/stuff/data/analysis/R/TGAall2/output/figures"
  traitDir <- deparse(substitute(trait))
  varDir <- deparse(substitute(var))
  
  outDir <- sprintf("%s/%s/%s/quad", figureDir, traitDir, varDir)
  
  dir.create(outDir, recursive=TRUE)
  
  labels <- list("ylab" <- c(deparse(substitute(trait))),
                 "catname" <- as.factor(df$category)
  )
  
  for(i in 1:ncol(df)) {
    hydro <- df[[i]]  
    hydroname <- as.expression(colnames(df[i]))   
    fit.quad <- lm(var ~ hydro, data = df)
    
    #  padj <- labels$p.adj[i]
    r2 <- signif(summary(fit.quad)$r.squared, 5)
    pval <- anova(fit.quad)[1,"Pr(>F)"]
    
    png(sprintf("%s/%s_pval-%s_r2-%s.png", outDir, hydroname, pval, r2), width = 600, height = 500)
    
    p <- qplot(hydro, var, data = df) 
    p <- p + geom_point()
    p <- p + geom_point(aes(shape = labels$catname), size =3)
    p <- p + scale_shape_discrete(name = "Hydrological \n class", labels = c("stable winter baseflow", "unpredictable baseflow", "unpredictable intermittent"))
    p <- p + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x + I(x^2), se=TRUE, col="black") 
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
