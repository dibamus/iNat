setwd("C:/Users/User1/Syncthing-Docs/Dissertation/Ch 2/iNat")
library('tidyverse')
library('ggplot2')

#### data cleaning - initial prey taxonomy resolution ####
iNat <- read.csv("observations-01-05-24.csv")

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

df <- data.frame(num = num,len = sapply(preyIDs, length))

df$problem <- FALSE
df$problem[which(df$len > 1)] <- TRUE

iNat <- cbind(iNat, df)

#End cleaning####
library(taxize)

#resolve 
preytax <- readRDS("preytaxonomy.RDS")

existing <- names(preytax)
missingprey <- which(sapply(iNat$preyID, length)==0)
preyitems <- unique(iNat$preyID)

add <- preyitems[which(!(preyitems %in% existing))]
add <- add[which(sapply(add, length)==1)]
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
newprey <- classification(unlist(add[which(sapply(add, length)!=0)]), db = "itis")

newprey_NA <- which(is.na(newprey))

newprey_wiki <- classification(c(names(newprey[newprey_NA]),"Python natalensis"), db = "wiki")

newprey[newprey_NA]<- newprey_wiki[1:5]

newprey$`Python natalensis` <- newprey_wiki[6]

#for quick & dirty (don't make corrections to the NAs)
newprey <- newprey[-newprey_NA]

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

preytaxonomy <- read.csv("PreyTaxonomy.csv")
row.names(preytaxonomy) <- preytaxonomy[,1]

which(duplicated(preytaxonomy[,1]))

solvedprey <-rep(NA, times = dim(iNat)[1])# unlist(iNat$preyID[!iNat$problem])
solvedprey[which(!iNat$problem)] <- iNat$preyID[!iNat$problem]
solvedprey[which(sapply(solvedprey, length)==0)] <- NA
solvedprey <- unlist(solvedprey)

preydf <- preytaxonomy[solvedprey,][,-c(1:2)]

iNat <- cbind(iNat,preydf)

library(countrycode)

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

saveRDS(iNat, file = "iNatDataset.RDS")

write.csv(iNat[,-which(colnames(iNat)=="preyID")], file = "iNatDataset.csv")

#PROBLEMS
problemdf <- read.csv("observations-01-05-24.csv") 
fixthese <- which(!(problemdf$id %in% iNat$id))

write.csv(problemdf[fixthese,], file = "problems.csv")


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
