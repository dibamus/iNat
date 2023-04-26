setwd("C:/Users/Isaac/Syncthing-Docs/Dissertation/Ch 2/iNat")

library('squamatabase')
library('tidyverse')

#### initial prey taxonomy resolution ####
iNat <- read.csv("Snake_Diet_project.csv")

preytax <- readRDS("preytaxonomy.RDS")

gettax <- function(name, rank){
  x <- as.data.frame(preytax[[name]])
  if(rank %in% x$rank){
    return(x[which(x$rank == rank),'name'])
  }
  else(
    return(NA)
  )
}
names <- names(preytax)

preytaxonomy <- data.frame(preyID = names,
       preyPhylum = sapply(names, FUN = gettax, "phylum"),
       preyClass = sapply(names, FUN = gettax, "class"),
       preyOrder = sapply(names, FUN = gettax, "order"),
       preyFamily = sapply(names, FUN = gettax, "family"),
       preyGenus = sapply(names, FUN = gettax, "genus"),
       preyspecies = sapply(names, FUN = gettax, "species"))

iNatprey <- preytaxonomy[iNat$field.eating,]

iNat <- cbind(iNat,iNatprey)
iNat$continent <- countrycode(sourcevar = iNat$place_country_name,
                              origin = "country.name",
                              destination = "continent")
iNat$continent[which(iNat$place_country_name %in% 
                       c( "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
                          "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
                          'Uruguay', "Venezuela","French Guiana", 
                          "Falkland Islands","South Georgia", 
                          "South Sandwich Islands"))] <- "South America"
iNat$continent[which(iNat$continent == "Americas")] <- "North & Central America"
iNat$continent[which(iNat$place_country_name == "Australia")] <- "Australia"

iNat$continent[which(iNat$place_country_name %in% 
                       c( "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
                          "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
                          'Uruguay', "Venezuela","French Guiana", 
                          "Falkland Islands","South Georgia", 
                          "South Sandwich Islands"))] <- "South America"
iNat$continent[which(iNat$continent == "Americas")] <- "North & Central America"
saveRDS(iNat, file = "iNatDataset.RDS")

#### analysis ####
iNat <- readRDS("iNatDataset.RDS")
library(countrycode)
library(ggrepel)

preytable <- table(iNat$preyspecies)
preytable <- preytable[order(preytable, decreasing = TRUE)]

ggplot(data = iNat, aes(x = "", y = preyClass, fill = preyClass)) +
geom_col() +
  coord_polar(theta = "y")+
  theme_void()

ggplot(data = iNat, aes(x = "", y = continent, fill = continent)) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_void()

