getpreydf <- function(taxa, Taxonomy = readRDS("Taxonomy.rda")){
  disambiguateTaxa <- function(taxa, Taxonomy){
    # taxa is a vector of taxon names
    # taxonomy is a dataframe of the iNaturalist taxonomy 
    sapply(1:length(taxa), function(x){
      #handle taxa marked as duplicates or to be removed
      if(taxa[x] %in% c("DUPLICATE","TWO OR MORE PREY","REMOVE")){
        return(NA)
      }
      
      d <- filter(Taxonomy, scientificName == taxa[x])
      if(dim(d)[1]== 1){
        return(d)
      }
      if(all(c("complex","species") %in% d$taxonRank) && length(unique(d$taxonRank)) ==2){
        #a taxon name is occupied only by a species and a complex
        # in this case, assume all observations are of the complex
        # the nominate species should be a part of that complex
        solution <- d$id[which(d$taxonRank == "complex")]
        return(solution)
      }
      if(all(c("genus","subgenus") %in% d$taxonRank) && length(unique(d$taxonRank)) ==2){
        #a taxon name is occupied only by a genus and a subgenus
        # in this case, assume all observations are of the genus
        # the subgenus should be a part of that genus
        solution <- d$id[which(d$taxonRank == "genus")]
        return(solution)
      }
      
      if(dim(d)[1]==0){
        print(paste(taxa[x],"is not a valid taxon."))
        return(taxa[x])
      }

      else{
        print(x)
        print(taxa[x])
        same <- sapply(c(5:12), function(y){
          if((length(unique(d[,y])) == 1) && #there is only 1 name listed at this rank
             d[1,y] != "" && # that name is not blank
             d[1,y] != d[1,14]) { #that name is not identical to the ambiguous name in question
            d[1,y]
          }
          else{
            NA
          }
        })
        
        same <- same[!is.na(same)]
        parentagreement <- same[length(same)]
        parentrank <- colnames(d)[5:12][length(same)] 
        solution <- filter(Taxonomy, 
                           scientificName == parentagreement,
                           taxonRank == parentrank)$id
        
        print(paste("The taxon",d$scientificName[1],
                    "is ambiguous: setting to most recent common parent node,",
                    parentagreement))
        return(solution)
      }
    })
  }
  
  matchtaxa <- function(taxa, Taxonomy){
    ambiguoustaxa <- taxa %in% Taxonomy[which(Taxonomy$duplicated),"scientificName"]
    #get IDs for ambiguous taxa
    if(length(which(ambiguoustaxa) > 0)){
      ambiguousIDs <- disambiguateTaxa(taxa[ambiguoustaxa],Taxonomy)
    #get ids for unambiguous taxa
    }
    
    
    unambiguousIDs <- sapply(taxa[!ambiguoustaxa], function(x){
      id <- filter(Taxonomy, scientificName == x)$id
      if(length(id)==0){id <- 0}
      return(id)
    })
    
    #return taxon ids 
    
    taxIDs <- rep(0, length(taxa))
    taxIDs[which(ambiguoustaxa)] <- ambiguousIDs
    taxIDs[which(!ambiguoustaxa)] <- unambiguousIDs
    
    return(taxIDs)
  }
  
  
  
  
  ids <- matchtaxa(taxa,Taxonomy)
  
  return(Taxonomy[match(ids,Taxonomy$id),])
  

}



 



