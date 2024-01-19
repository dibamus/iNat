
setwd("C:/Users/User1/Syncthing-Docs/Dissertation/Ch 2/iNat")
library('tidyverse')
library('ggplot2')
library('vegan')
library('lubridate')
library('reshape2')
sn <- readRDS(file = "iNatDataset.RDS")
sn <- sn[!sn$problem,]
sn$preyID[which(sapply(sn$preyID, length)==0)] <- NA
sn$preyID <- unlist(sn$preyID)

sn$created_at <- as.POSIXct(sn$created_at, tz = "UTC")

sn$month <- round_date(sn$created_at, unit = "month")

#### 1: Accumulation Curves ####

monthlyacc <- table(sn$month)

newnames <- function(df, months = names(monthlyacc)){
  #make a list of all the species observed
  m_df <- data.frame(month = as.POSIXct(months, tz = "UTC"))
  
  m_df$n_Obs <- sapply(m_df$month, function(x){length(which(df$month == x))})
  
  spp <- unique(df$scientific_name)
  
  #cumulative observations
  m_df$n_Cum <- sapply(1:dim(m_df)[1], function(x){sum(m_df$n_Obs[1:x])})
  
  #number of and cumulative species
  for(i in 1:dim(m_df)[1]){
    wdf <- df[which(df$month == m_df$month[i]),]
    m_df$n_sp[i]<- length(unique(wdf$scientific_name))
    m_df$new_sp[i] <- length(which(unique(wdf$scientific_name) %in% spp))
    spp[which(spp %in% unique(wdf$scientific_name))]<- NA
  }
  
  m_df$cum_sp <- sapply(1:dim(m_df)[1], function(x){sum(m_df$new_sp[1:x])})
  return(m_df)
}
  
globalSpcurve <- rbind(newnames(filter(sn, continent == "Africa")),
                    newnames(filter(sn, continent == "Asia")),
                    newnames(filter(sn, continent == "Australia")),
                    newnames(filter(sn, continent == "Central America")),
                    newnames(filter(sn, continent == "Europe")),
                    newnames(filter(sn, continent == "North America")),
                    newnames(filter(sn, continent == "Oceania")),
                    newnames(filter(sn, continent == "South America")),
                    newnames(sn)
                    )


globalSpcurve$Continent <- c(rep("Africa", 149),
                             rep("Asia", 149),
                             rep("Australia", 149),
                             rep("Central America", 149),
                             rep("Europe", 149),
                             rep("North America", 149),
                             rep("Oceania", 149),
                             rep("South America", 149),
                             rep("Global", 149))

continentcolors <- list(Africa = "#dd4433",
                        Asia = "#dd9933",
                        Europe = "#ddcc33",
                        
                        Australia = "#448844",
                        Oceania = "#44cc44",
                        
                        `North America` = "#66aaff",
                        `Central America` = "#aa77dd",
                        `South America` = "#ee77ee",
                        
                        Global = "#444444")


obsTaxa <- melt(globalSpcurve, id.vars = c("month","Continent"), 
                measure.vars = c("n_Obs","n_Cum","n_sp","cum_sp"))

contSp <- ggplot(filter(obsTaxa, variable %in% c("cum_sp"))) +
  geom_line(aes(x = month, y = value, 
                group = Continent, 
                color = Continent, linetype = variable))
  
contObs <- ggplot(filter(obsTaxa, variable %in% c("n_Cum","cum_sp"))) +
  geom_line(aes(x = month, y = value, 
                group = Continent, 
                color = Continent), linewidth = 2)+
  facet_wrap(~variable, ncol = 1, scales = "free") +
  scale_color_manual(values = unlist(continentcolors), breaks = names(continentcolors)) +
  theme_IWK()




#### 2: Food Web ####
library('igraph')

diet_graph <- graph_from_edgelist(as.matrix(sn[which(!is.na(sn$preyID)),
                                               c("scientific_name","preyID")]), directed = TRUE)

thamnophis <- sn[(sn$scientific_name == "Thamnophis sirtalis")&(!is.na(sn$preyID)),
                 c("scientific_name","preyID")]

thamnophis_diet_graph <- graph_from_edgelist(as.matrix(thamnophis), directed = TRUE)

