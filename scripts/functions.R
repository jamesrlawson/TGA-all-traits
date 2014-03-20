
# find species in % cover list associated with trait values

findtraitvals <- function(plotsums, trait) {
  
  # get indexes for matches which return TRUE
  x <- match(trait$species, plotsums$species) 
  
  # subset by matches 
  x <- plotsums[na.omit(x), ] 
  
  x <- merge(x, trait, by.x = "species", by.y = "species")
  
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






