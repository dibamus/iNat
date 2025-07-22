#Comparison w/ Maritz & Maritz Findings
setwd("C:/Users/User1/Syncthing-Docs/Dissertation/Ch 2/iNat")
library('tidyverse')
library('ggplot2')
library('vegan')
library('lubridate')
library('readxl')
library('reshape2')
sn <- readRDS(file = "iNatDataset.RDS")
mf <- read.csv(file = "Maritz_FacebookDatabase.csv")
mi <- read.csv(file = "Maritz_INaturalistDatabase.csv")

#fix up the iNat dates
mi$observed_on <- as.POSIXct(mi$observed_on, tz = "UTC", format ="%d/%m/%Y")
sn$observed_on <- as.POSIXct(sn$observed_on, tz = "UTC", format = "%Y-%m-%d")

max(mi$observed_on, na.rm = TRUE) #last report is Dec 11th, 2019

#Separate out the african interacitons from the iNat dataset
sn.af <- filter(sn, continent == "Africa")

#remove the observations already counted by Maritz & Maritz
sn.af <- sn.af[!(sn.af$id %in% mi$id),]

#remove observations from after the Mritz & Maritz data collection
#sn.af <- filter(sn.af, observed_on > max(mi$observed_on, na.rm = TRUE))
#dim(sn.af)[1]
#ok, so (naively), our dataset contains 211 observations 
#not reported as being on iNat by Maritz & Maritz

#but do any match observations from their facebook dataset?
mf.dated <- filter(mf, Obs.Date!="")
mf.dated$Obs.Date <- as.POSIXct(mf.dated$Obs.Date, tz = "UTC", format ="%d/%m/%Y")
max(mf.dated$Obs.Date, na.rm = T)

mf.datematch <- mf.dated[sapply(mf.dated$Obs.Date, 
                                function(x){x%in%sn.af$observed_on}),]

mf.datematch$matches <- lapply(1:dim(mf.datematch)[1], 
                       function(x){
                         date <- mf.datematch$Obs.Date[x]
                         t.sn.af <- sn.af[which(sn.af$observed_on == date),]
                           
                         matches <- c(mf.datematch$Predator[x], mf.datematch$Prey[x]) %in%
                           c(t.sn.af$scientific_name, t.sn.af$preyID)
                         if(any(matches)){
                           return(t.sn.af$id)
                         }
                         else{
                           return(FALSE)
                         }
                       })

#Of these, the following observations may be duplicates:
#118828515 - geography, predator & prey match
#146429588 - geography, predator & prey
#187877883 - is not a match
#19707063 - comments indicate that it is in the Maritz & Maritz group
#19707089 - geography, predator & prey
#116586231 - geography, predator & prey

Maritzobs <- c(mi$id, 118828515, 146429588, 187877883, 19707063,116586231)
saveRDS(Maritzobs, file = "Maritz_obs_ids.RDS")
