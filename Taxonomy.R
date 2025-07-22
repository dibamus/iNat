setwd("C:/Users/Isaac/Syncthing-Docs/Dissertation/Ch 2/iNat")

Animalia <- read.csv("taxa.csv")

#filter out non-animals (snakes eat animals)
Animalia <- Animalia[which(Animalia$kingdom == "Animalia"),]

# set up hierarchical states for the Ranks
hierarchy <- c("kingdom","phylum","subphylum","superclass","class","subclass","infraclass","subterclass","superorder","order","suborder","infraorder","parvorder","section","zoosection","subsection","zoosubsection","superfamily","epifamily","family","subfamily","supertribe","tribe","subtribe","genushybrid","genus","subgenus","complex","species",
               "subspecies",
               "hybrid","infrahybrid","variety","form"
)
Animalia$taxonRank <- factor(Animalia$taxonRank, levels = hierarchy)

#indicate if the taxon is a duplicate
dups <- Animalia$scientificName[Animalia$scientificName %>% duplicated() %>% which()]

Animalia$duplicated <- Animalia$scientificName %in% dups

saveRDS(Animalia, "Taxonomy.rda")
