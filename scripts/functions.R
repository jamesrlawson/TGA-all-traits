
# find species in % cover list associated with trait values

findtraitvals <- function(plotsums, trait) {
  
  # get indexes for matches which return TRUE
  
  y <- data.frame()
  
  for(i in 1:nrow(plotsums)) {
    
    species <- as.data.frame(plotsums$species)
    colnames(species) <- c("species")
    x <- species[i,]
    x <- grep(x, plotsums$species, fixed=TRUE)
    x <- plotsums[x,]
    x <- merge(x, trait, by.x = "species", by.y = "species")
    
    y <- rbind(y,x)
    
  }
  # subset by matches 
  #x <- plotsums[na.omit(x), ] 
    return(y)
}


relabund <- function(df) {
                              
                relcover <- df$speciescover / df$totalcover 
                cbind(df, "relcover" = relcover)
}
  


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






