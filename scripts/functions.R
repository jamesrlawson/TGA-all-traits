# find trait values

traitsbylayer <- function(trait, layer) {
  
  # get indexes for matches which return TRUE
  traitbylayer <- match(trait$species, layer$species) 
  
  # subset by matches 
  traitbylayer <- layer[na.omit(traitbylayer), ] 
  
  traitbylayer <- merge(traitbylayer, trait, by.x = "species", by.y = "species")

}

relabund <- function(traitlayer, trait) {
  
  total <- as.data.frame(cbind(totalabundances$plotID, totalabundances$trait))
  colnames(total) <- c("plotID", "sumAbundance")
  df <- merge(traitlayer, total)
  
  for(i in 1:nrow(df)) {
    
    relabundance <- df$avgcover[i] / df$sumAbundance[i]
    
    df$relabuncance <- relabundance
    
  }
  
  return(df)
}




