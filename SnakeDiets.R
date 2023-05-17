setwd("C:/Users/Isaac/Syncthing-Docs/Dissertation/Ch 2/iNat")
library('tidyverse')
library('ggplot2')

print('skip the data cleaning steps')

#### data cleaning - initial prey taxonomy resolution ####
iNat <- read.csv("Snake_Diet_project.csv")
iNat <- subset(iNat,select = -c(field.eating...interaction.)) #don't need this column

#resolve any prey ID inconsistencies
fields <- grep("field",colnames(iNat))

rf <- function(x){
  nms <- unique(unlist(iNat[x,fields]))
  nms <- nms[-which(is.na(nms))]
  nms <- nms[-which(nms == "")]
  return(nms)
}

num <- c(1:dim(iNat)[1])

preyIDs <- sapply(num, rf)

df <- data.frame(num = num,len = sapply(preyIDs, length))

df$problem <- FALSE
df$problem[which(df$len > 1)] <- TRUE
df$ready
df$ready[which(df$len == 1)] <- TRUE
df$solution <- NA
df$solution[which(df$ready)] <- unlist(preyIDs[which(df$ready)])

problems <- preyIDs[df$problem]

#when prey IDs disagree, take either
# 1 - the most specific ID (lowest taxon level)
#2 - when taxon levels match, the agreed upon level between them

#if the dataset changes, these will need to be reassessed
solutions <- c("Hemidactylus",
               "Cyprinodon variegatus",
               "Hemidactylus mabouia",
               "Aepyceros melampus melampus",
               "Passeriformes",
               "Cordylus niger",
               "Rhinella marina",
               "Thraupis palmarum",
               "Smilisca baudinii",
               "Catostomus commersonii",
               "Anaxyrus americanus",
               "Phoxinus",
               "Scincidae",
               "Falco sparverius",
               "Rhacophorus malabaricus",
               "Sceloporus occidentalis",
               "Columbidae",
               "Ameiurus natalis",
               "Cypriniformes",
               "Myomorpha",
               "Scincella lateralis",
               "Aepyceros melampus melampus",
               "Pteropus conspicillatus",
               "Anura",
               "Leptodactylus labyrinthicus",
               "Rhinella horribilis",
               "Notocitellus adocetus",
               "Accipitridae",
               "Placentalia",
               "Lithobates clamitans",
               "Lithobates")

df$solution[which(df$problem)] <- solutions

iNat$preyID <- df$solution

library(taxize)



#resolve 
preytax <- readRDS("preytaxonomy.RDS")

existing <- names(preytax)
missingprey <- which(is.na(iNat$preyID))
preyitems <- unique(iNat$preyID)

add <- preyitems[which(!(preyitems %in% existing))]
# data cleaning - initial preytax run#### 
#for first run, all the preytax 
a <- classification(preyitems[1:200], db = "itis")
b <- classification(preyitems[201:400], db = "itis")
c <- classification(preyitems[401:600], db = "itis")
d <- classification(preyitems[601:800], db = "itis")
e <- classification(preyitems[801:977], db = "itis")
preytax <-c(a,b,c,d,e)


nf <- preyitems[which(is.na(preytax))]

missing <- classification(nf, db = "wiki")

preytax[names(which(!is.na(missing)))] <- missing[!is.na(missing)]

stillmissing <- missing[which(is.na(missing))][-1]

stillmissing <- classification(names(stillmissing), db = "eol")

preytax[names(stillmissing)]<- stillmissing
preytax["Leptodactylus luctator"] <- classification("Leptodactylus luctator", db = "gbif")

saveRDS(preytax, "preytaxonomy.RDS")

# data cleaning - subsequent runs####

# #17('Python natalensis') gives a 404 error, so remove it
newprey <- classification(add[which(!is.na(add))][-17], db = "itis")

newprey_NA <- which(is.na(newprey))

newprey_wiki <- classification(c(names(newprey[newprey_NA]),"Python natalensis"), db = "wiki")

newprey[newprey_NA]<- newprey_wiki[1:5]

newprey$`Python natalensis` <- newprey_wiki[6]

preytax <- c(preytax,newprey)

saveRDS(preytax, "preytaxonomy.RDS")


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

row.names(preytaxonomy)<- preytaxonomy$preyID

write.csv(preytaxonomy, "PreyTaxonomy.csv")

preytaxonomy <- read.csv("PreyTaxonomy.csv")[1:978,]
row.names(preytaxonomy) <- preytaxonomy[,1]

which(duplicated(preytaxonomy[,1]))


iNatprey <- preytaxonomy[iNat$preyID,]
iNatprey <- iNatprey[,-1]

iNat <- cbind(iNat[,1:50],iNatprey)


iNat <- as.data.frame(iNat)

library(countrycode)
iNat$place_country_name <- read.csv("observations-318441.csv")$place_country_name

iNat$continent <- countrycode(sourcevar = iNat$place_country_name,
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

iNat$continent[which(iNat$place_country_name %in% 
                       c( "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
                          "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
                          'Uruguay', "Venezuela","French Guiana", 
                          "Falkland Islands","South Georgia", 
                          "South Sandwich Islands"))] <- "South America"
iNat$continent[which(iNat$continent == "Americas")] <- "North & Central America"
saveRDS(iNat, file = "iNatDataset.RDS")

write.csv(iNat, file = "iNatDataset.csv")


#### analysis ####

iNat <- as.data.frame(readRDS("iNatDataset.RDS"))

preytable <- table(iNat$preyspecies)
preytable <- preytable[order(preytable, decreasing = TRUE)]

ggplot(data = iNat, aes(x = "", y = preyClass, fill = preyClass)) +
geom_col() +
  coord_polar(theta = "y")+
  scale_fill_brewer(palette = "Paired") +
  theme_void()

ggplot(data = iNat, aes(x = "", y = continent, fill = continent)) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_void()


anax <-table(anax)
anax[order(anax, decreasing = T)]
