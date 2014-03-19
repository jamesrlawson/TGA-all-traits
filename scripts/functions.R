
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
  
output <- function(df, filename) {
  
  x <- cbind(df[1],df[2],df[6],df[4])
  
  write(x, file="blah.txt", "", sep ="\t")

}



#relabund <- function(traitlayer, trait) {
  
#  total <- as.data.frame(cbind(totalabundances$plotID, totalabundances$trait))
#  colnames(total) <- c("plotID", "sumAbundance")
#  df <- merge(traitlayer, total)
  
#  blah <- ddply(traitlayer, .(plotID, species), summarise, total = sum(avgcover))
  
#  for(i in 1:nrow(df)) {
    
#    relabundance <- df$avgcover[i] / df$sumAbundance[i]
    
#    df$relabuncance <- relabundance
    
#  }
  
#  return(df)
#}




