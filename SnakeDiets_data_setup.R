setwd("C:/Users/User1/Syncthing-Docs/Dissertation/Ch 2/iNat")
library('tidyverse')
library('ggplot2')

# you will need these URLs to access data in our spreadsheets
# the URLs file is intentionally NOT on github. It is in our shared drive folder
# download it from there and put it in your working directory
# if you can't find it, contact Isaac Krone
URLs <- readRDS("URLs.rda")

#### data cleaning - initial prey taxonomy resolution ####
dataset <- "observations-02-02-25.csv"

iNat <- read.csv(paste0("iNat Datasets/",dataset))

#1 - find any prey ID inconsistencies ####
fields <- grep("field",colnames(iNat))
url <- which(colnames(iNat) == "field.eating...interaction.")

fields <- fields[-which(fields == url)]

rf <- function(x){
  nms <- as.character(unique(unlist(iNat[x,fields])))
  #nms <- nms[-which(NM)]
  nms <- nms[-which(nms == "")]
  return(nms)
}

num <- c(1:dim(iNat)[1])

preyIDs <- sapply(num, rf)

iNat$preyID<- preyIDs

#### FIX OBSERVATIONS WITH CONFLICTING PREY IDS ####
#where are there conflicting or duplicate Prey Item IDs?

df <- data.frame(num = num,len = sapply(preyIDs, length))

df$problem <- FALSE
df$problem[which(df$len > 1)] <- TRUE

iNat$problem <- df$problem

problems <- iNat[iNat$problem,]

library("googlesheets4")
sh <- URLs$problems

previous_problems <- read_sheet(URLs$problems, sheet = 1)

old_problem_url <- problems$url[which(problems$url %in% previous_problems$url)]

problems$Solution <- previous_problems$Solution[match(problems$url, previous_problems$url)]

#Add in known solutions
for(i in problems$url){
  iNat_index <- which(iNat$url == i)
  problems_index <- which(problems$url == i)
  
  if(!is.na(problems[problems_index,]$Solution)){ #is there a solution?
    iNat[iNat_index,]$preyID <- problems[problems_index,]$Solution
    iNat[iNat_index,]$problem <- FALSE
  }
}

#append the new problems onto the old sheet
sheet_append(filter(iNat, problem) %>% 
              select(url,field.eating...interaction.,
                     field.eating,field.preying.on,
                     field.hunting,field.hunting.feeding,
                     field.interaction..preyed.on,field.prey.taxon,
                     field.prey.id,field.feeding.on,field.depredando) %>%
              mutate(Solution = NA, `Why not solved?`=NA ), 
            ss = sh,
            sheet = 1)
#
iNat[which(iNat$preyID == "TWO OR MORE PREY"),] #which observations involve more than one prey item?

#Just two observations with two prey items each
iNat <- rbind(iNat, iNat[which(iNat$preyID == "TWO OR MORE PREY"),]) #so duplicate these observations

#now let's re-add these 

#this one actually has 3, but 2 are Carpophis vermis
iNat[which(iNat$url == 
             "https://www.inaturalist.org/observations/82423240"),]$preyID <- 
  c("Carphophis vermis","Scincella lateralis")

iNat[which(iNat$url == 
             "https://www.inaturalist.org/observations/144799030"),]$preyID <- 
  c("Lithobates","Anolis")

iNat[which(iNat$url == 
             "https://www.inaturalist.org/observations/164503269"),]$preyID <- 
  c("Macrotermes","Microcerotermes")


#### FIND THE OBSERVATIONS THAT LACK A PREY IDENTIFICATION ####
#which observations are missing prey identifications?
previously_missing_prey_sheet <- URLs$preyIDmissing
previously_missing <- read_sheet(previously_missing_prey_sheet)


no_prey_names_or_Life <- c(which(sapply(iNat$preyID, length)==0),
                           which(iNat$preyID=="Life"))
                           

no_prey <- iNat[no_prey_names_or_Life,]


old_missing_url <- no_prey$url[which(problems$url %in% previously_missing$url)]

no_prey$Solution <- previously_missing$Solution[match(no_prey$url, previously_missing$url)]

#Add in known solutions
for(i in no_prey$url){
  iNat_index <- which(iNat$url == i)
  noprey_index <- which(no_prey$url == i)
  
  if(!is.na(no_prey[noprey_index,]$Solution)){ #is there a solution?
    iNat[iNat_index,]$preyID <- no_prey[noprey_index,]$Solution
  }
}

no_prey_names_or_Life <- c(which(sapply(iNat$preyID, length)==0),
                           which(iNat$preyID=="Life"))

sheet_append(iNat[no_prey_names_or_Life,] %>% 
              select(url) %>%
              mutate(Solution = NA, `Why not solved?`=NA ), 
            ss = previously_missing_prey_sheet)

#### GET TAXONOMIC INFO FOR PREY ITEMS ####

source("getpreydf.R")

preydf <- getpreydf(unique(unlist(iNat$preyID)))

#There are some problematic taxon names in there! Fix those ####
ambiguousNames <- c("Acontiinae","Anisoptera","Micropterus","Helicina")

ambiguousTaxa <- iNat[which(iNat$preyID %in% 
                              ambiguousNames),]
ambig_prey_taxon_sheet <- URLs$ambiguous

ambiguous<- read_sheet(ambig_prey_taxon_sheet)

old_ambig_url <- ambiguousTaxa$url[which(ambiguousTaxa$url %in% ambiguous$url)]

ambiguousTaxa$Solution <- ambiguous$Solution[match(ambiguousTaxa$url, ambiguous$url)]

#Add in known solutions
for(i in ambiguousTaxa$url){
  iNat_index <- which(iNat$url == i)
  ambig_index <- which(ambiguousTaxa$url == i)
  
  if(!is.na(ambiguousTaxa[ambig_index,]$Solution)){ #is there a solution?
    iNat[iNat_index,]$preyID <- ambiguousTaxa[ ambig_index,]$Solution
  }
}

sheet_append(iNat[which(iNat$preyID %in% 
                          ambiguousNames),] %>% 
               select(url, preyID) %>%
               mutate(Solution = NA, `Why not solved?`=NA ), 
             ss = ambig_prey_taxon_sheet)

#Add Prey ID Data to iNat sheet ####
iNat$preyTaxon <- NA
iNat$preyID_count <- sapply(iNat$preyID, FUN = length)
table(iNat$preyID_count)
iNat$preyTaxon[which(iNat$preyID_count == 1)] <- unlist(iNat$preyID[which(iNat$preyID_count == 1)])


preydf <- getpreydf(unique(unlist(iNat$preyID)))
preydf <- select(preydf, id,phylum,class,order,family,genus,specificEpithet,scientificName,taxonRank)

prey_row <- match(iNat$preyTaxon[which(iNat$preyID_count == 1)], preydf$scientificName)

prey_taxonomy_to_append <- preydf[prey_row,]
colnames(prey_taxonomy_to_append) <- paste0("prey_",colnames(prey_taxonomy_to_append))
colnames(prey_taxonomy_to_append)[1] <- "prey_taxon_id_number"

iNat <- cbind(iNat,prey_taxonomy_to_append)

# Once again, find the problematic IDs ####

previously_missing_prey_sheet <- URLs$preyIDmissing
previously_missing <- read_sheet(previously_missing_prey_sheet)

bad_annotations <- which(is.na(iNat$prey_taxon_id_number))
skip <- which(iNat[bad_annotations,]$preyID %in% c("DUPLICATE","TWO OR MORE PREY","REMOVE"))

bad_annotations <- iNat[bad_annotations[-skip],]



bad_annotations$Solution <- previously_missing$Solution[match(bad_annotations$url, previously_missing$url)]


#Add in known solutions
for(i in bad_annotations$url){
  iNat_index <- which(iNat$url == i)
  noprey_index <- which(bad_annotations$url == i)
  
  if(!is.na(bad_annotations[noprey_index,]$Solution)){ #is there a solution?
    iNat[iNat_index,]$preyID <- bad_annotations[noprey_index,]$Solution
  }
}

bad_annotation_ids <- bad_annotations[which(is.na(bad_annotations$Solution))]$id

sheet_append(iNat[which(iNat$id == bad_annotation_ids),] %>% 
               select(url) %>%
               mutate(Solution = NA, `Why not solved?`=NA ), 
             ss = previously_missing_prey_sheet)


#### ASSIGN OBSERVATIONS TO CONTINENTS ####

library(countrycode)

iNat$continent <- countrycode(sourcevar = as.character(iNat$place_country_name),
                              origin = "country.name",
                              destination = "continent")
iNat$continent[which(iNat$place_country_name %in% 
                       c( "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
                          "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
                          'Uruguay', "Venezuela","French Guiana", 
                          "Falkland Islands","South Georgia", 
                          "South Sandwich Islands"))] <- "South America"
iNat$continent[which(iNat$place_country_name %in% 
                       c("Canada","United States","Mexico"))] <- "North America"
iNat$continent[which(iNat$continent == "Americas")] <- "Central America"
iNat$continent[which(iNat$place_country_name == "Australia")] <- "Australia"


# Indicate which records are in the Maritz & Maritz (2020) paper
iNat$Maritz <- iNat$id %in% readRDS("Maritz_obs_ids.RDS")

#### Search for Duplicates ####

suggestDuplicates <- function(x){
  x <- iNat[x,] 
  samedate <- filter(iNat, observed_on == x$observed_on)
  samesnake <- filter(samedate, taxon_genus_name == x$taxon_genus_name)
  sameprey <- filter(samesnake, preyClass == x$preyClass)
  
  return(sameprey$id)
}

suggestedDuplicates <- sapply(1:dim(iNat)[1],suggestDuplicates)

d <- suggestedDuplicates[!sapply(suggestedDuplicates, function(x){length(x)==1})]
iNat$maybeduplicated <- sapply(suggestedDuplicates, function(x){length(x)!=1})
dsor <- sapply(d, sort)

dsorU <- unique(dsor)


#### More Taxonomy info for predator snakes ####
source("getpreydf.R")

predatordf <- getpreydf(unique(iNat$scientific_name))

predator_row <- match(iNat$scientific_name, predatordf$scientificName)

predator_taxonomy_add <- (predatordf %>% 
  select(family, genus, specificEpithet))[predator_row,]

colnames(predator_taxonomy_add) <- c("predator_family","predator_genus","predator_specific_ep")

iNat <- cbind(iNat,predator_taxonomy_add)

#### get dates in shape ####
library('lubridate')

iNat$observed_on <- ymd(iNat$observed_on)
iNat$created_at <- parse_date_time(iNat$created_at, orders = "%Y-%m-%d %H:%M:%S %z")
iNat$time_observed_at <- parse_date_time(iNat$time_observed_at, 
                                         orders = "%Y-%m-%d %H:%M:%S %z")

#### Save the dataset ####
date <- readline(prompt = "What is today's date? ")

if(dim(iNat)[1] == unlist(iNat$preyID) %>% length()){
  iNat$preyID <- unlist(iNat$preyID)
  saveRDS(iNat, file = paste0("iNatDataset",date,".RDS"))
  
  write.csv(iNat, file = paste0("iNatDataset",date,".csv"))
  
  sheet_write(iNat, ss= URLs$clean_dataset, sheet = date)
  
}
