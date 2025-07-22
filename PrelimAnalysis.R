#### analysis ####
library('tidyverse')
library('ggplot2')
library('ggstance')

iNat <- as.data.frame(readRDS("iNatDataset02-02-2025.RDS"))

iNat$predatorID <- sapply(iNat$scientific_name, FUN = function(x){
  str_match(x,"^\\S+\\s\\S+")
})
iNat$predatorGenus <- sapply(iNat$scientific_name, FUN = function(x){
  str_match(x, "^\\S+")
})

preytable <- table(iNat$preyID)
preytable <- preytable[order(preytable, decreasing = TRUE)]

iNat$prey_class <- factor(iNat$prey_class)
iNat$prey_order <- factor(iNat$prey_order)
iNat$prey_phylum <- factor(iNat$prey_phylum)
iNat$prey_genus_sp <- paste(iNat$prey_genus, iNat$prey_specificEpithet)
iNat$prey_genus_sp[which(iNat$prey_specificEpithet == "")] <- NA

classes <- levels(iNat$prey_class)

class_color <- setNames(
  c("#888888","#336677","#668721","#222222","#dd4411","#222222",
    "#cc5588","#996633","#225511","#663333","#664477","#336677","#339999"),
  levels(iNat$prey_class)
)

#Vertebrata
ggplot(data = data.frame(n = table(iNat$prey_class),
                         Class = levels(iNat$prey_class)), aes(x = "", y = n.Freq, fill = Class)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y")+
  scale_fill_manual(values = class_color)

ggplot(data = data.frame(n = table(iNat$preyPhylum),
                         Phylum = levels(iNat$preyPhylum)), aes(x = "", y = n.Freq, fill = Phylum)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y")+
  scale_fill_manual(values = setNames(c(
    "#cc5588","#888888","#664477","#339999"
  ),
  levels(iNat$preyPhylum)
  ))



iNat$observed_on <- ymd(iNat$observed_on)

continent_sums <- data.frame(continent = unique(iNat$continent)[-7],
                             sum = sapply(unique(iNat$continent)[-7],
                                          FUN = function(x){
                                            dim(filter(iNat, continent ==x))[1]
                                          }))

#Prey class accumulation
ggplot(iNat %>%
  group_by(prey_class) %>%
  arrange(observed_on) %>%
  mutate(cumulative_count = row_number()),
  aes(x = observed_on, 
    y = cumulative_count, 
    color =prey_class, 
    group = prey_class))+
  geom_line(linewidth = 2) +
  theme_IWK() +
  xlab("Date Observed") +
  ylab("Number of observations") +
  coord_cartesian(xlim = mdy(c("01-01-2010","02-10-2025")))

#Continent accumulation
continent_sums <- data.frame(continent = unique(iNat$continent)[-7],
                             sum = sapply(unique(iNat$continent)[-7],
                                          FUN = function(x){
                                            dim(filter(iNat, continent ==x))[1]
                                          }))




allcontinents <- ggplot(iNat %>%
                            filter(!is.na(continent)) %>%
                            group_by(continent) %>%
                            arrange(observed_on) %>%
                            mutate(cumulative_count = row_number()),
                          aes(x = observed_on, 
                              y = cumulative_count, 
                              color =continent, 
                              group = continent))+
  geom_line(linewidth = 2) +
  theme_IWK() +
  theme(legend.position = "none")+
  xlab("Date Observed") +
  ylab("Number of observations")

othercontinents <- allcontinents +
  geom_label(data = continent_sums,
             aes(x = mdy("02-10-2025"), y = sum,
                 label = continent, group = continent),
             hjust = 0) +
  coord_cartesian(xlim = mdy(c("01-01-2010","01-10-2035")),
                  ylim = c(0,1500))

library('patchwork')

timechart <- allcontinents + othercontinents

timestats <- iNat %>% select(observed_on) %>% arrange(observed_on) %>% 
  mutate(count = 1) %>% mutate(cum = cumsum(count))
ym_stamp<-stamp( "2019-12", orders = "ym")
timestats$month <- ym_stamp(timestats$observed_on) %>% ym()

event <- expression(paste("June 1949: ", italic("Agkistrodon laticinctus"),
                          " eats 20 ", italic("Datana"), " caterpillars in Terrell County, Texas"))

txt <- data.frame(x = ym("1949-06"), y = 200, label = "*testing* test")
library(scales)
library('ggtext')
ggplot(timestats, aes(x = as.POSIXct(month))) +
  geom_bar( fill = "#74ac00") +
  # geom_label(data = txt, aes(x = x, y = y, label = label), color = "#333333",
  #            fill = "#74ac00") + 
  theme_IWK() +
  scale_x_datetime(labels = date_format("%Y")) +
  coord_cartesian(xlim = as.POSIXct(ym(c("1948-01", "2025-02"))),
                  expand = 0)+
  xlab("Date (by month)") +
  ylab("Observations")

ggplot(timestats,aes(x = observed_on, y = cum)) +
  geom_point() +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
  
snakefamilies <- c("Acrochordidae","Aniliidae","Anomalepididae","Atractaspididae",
                   "Boidae","Boyleriidae","Colubridae","Cyclocoridae","Cylindrophiidae",
                   "Elapidae","Gerrhopilidae","Homalopsidae","Lamprophiidae",
                   "Leptotyphlopidae","Loxocemidae","Micrelapidae","Pareidae",
                   "Prosymnidae","Psammodynastidae","Psamophiidae","Pseudaspididae",
                   "Pseudoxyrhophiidae","Pythonidae","Tropidophiidae","Typhlopidae",
                   "Uropeltidae","Viperidae","Xenodermidae","Xenopeltidae","Xenophidiidae",
                   "Xenoptyphlopidae")

ophiophagy <- filter(iNat, prey_family %in% snakefamilies)


library(igraph)

ophiophagy <- filter(iNat, prey_family %in% snakefamilies) %>%
  select(predator_family,prey_family)

ophiograph <- graph_from_edgelist(ophiophagy %>% as.matrix)
ophiophagy$interaction <- paste(ophiophagy$predator_family, "ate",
                                ophiophagy$prey_family)
ophiophagy$incidence <- count_multiple(ophiograph)

ophio.summary <- ophiophagy[match(unique(ophiophagy$interaction), ophiophagy$interaction),]

ophio.summary.graph <- graph_from_edgelist(ophio.summary[,1:2] %>% as.matrix)

E(ophio.summary.graph)$incidence <- ophio.summary$incidence

V(ophio.summary.graph)$predator_family <- V(ophio.summary.graph)%>% names()
V(ophio.summary.graph)$angle <- 90 - (((1:length(V(ophio.summary.graph))))/
                                   (length(V(ophio.summary.graph))) * 360) +
  180/length(V(ophio.summary.graph))

library('ggraph')
ophiophagy_visual <- ggraph(ophio.summary.graph, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(arrow = arrow(type = "closed", length = unit(4, 'mm')),
                check_overlap = TRUE,
                linewidth = 2,
                lineend = "round",
                aes(color = log(incidence))) + 
  geom_edge_loop(arrow = arrow(type = "closed", length = unit(4, 'mm')),
                 check_overlap = TRUE,
                 linewidth = 2,
                 lineend = "round",
                 linejoin = "mitre",
                 aes(strength = 0.6,
                     span = 110,
                   color = log(incidence),
                     direction = 270- ((from - 1) * 360 / length(ophio.summary.graph))
                     ))+
  geom_node_label(aes(label = predator_family,
                      angle = angle,
                      hjust = 0),
                  label.size = 0,
                  fill = "#aaa0",
                  nudge_y = ((V(ophio.summary.graph)$angle -90)/180 *pi) %>% cos() *0.05,
                  nudge_x = ((V(ophio.summary.graph)$angle)/180 *pi) %>% cos() *0.05,
                  hjust = 0)+
  scale_edge_color_gradient(low = "#bbb",high = "#74ac00")+
  coord_fixed() +
  expand_limits(y = c(2, -2), x = c(2, -2)) +
  theme(legend.position = "none",
    panel.background  = element_rect(fill = "#eeeeee", color = NA))

ophio.summary <- ophio.summary %>% arrange(incidence)
ophio.summary$interaction <- factor(ophio.summary$interaction,
                                       levels = ophio.summary$interaction)


ggplot(data = ophio.summary %>% filter(incidence>7) ) +
  geom_col(aes(x = interaction, y = incidence), fill ="#74ac00") + 
  theme_IWK() +
  coord_cartesian(ylim = c(28,34)) +
  coord_flip()


#### Prey Flow ####
library('networkD3')
preyflow <- dplyr::select(iNat, predator_family, prey_class) %>% 
  filter(predator_family != "", prey_class != "", !is.na(prey_class)) %>%
  mutate(interaction = paste(predator_family, "ate", prey_class))
preyflow_table <- table(preyflow$interaction)

occ <- match(names(preyflow_table), preyflow$interaction)

preyflow_df <- data.frame(source = preyflow$predator_family[occ],
                             target = preyflow$prey_class[occ],
                             value = preyflow_table)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(preyflow_df$source), 
         as.character(preyflow_df$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
preyflow_df$IDsource <- match(preyflow_df$source, nodes$name)-1 
preyflow_df$IDtarget <- match(preyflow_df$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = preyflow_df, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value.Freq", NodeID = "name", 
                   sinksRight=FALSE,
                   fontFamily = "Arial",
                   fontSize = 14)
p

# with ggplot
library('ggalluvial')
preyflow2 <- dplyr::select(iNat, predator_family, prey_class) %>% 
  filter(predator_family != "", prey_class != "", !is.na(prey_class)) %>%
  count(predator_family, prey_class)


preyflow_alluvium <- ggplot(data = preyflow2,
                            aes(axis1 = predator_family,   # First variable on the X-axis
                                axis2 = prey_class, # Second variable on the X-axis
                                y = n)) +
  geom_alluvium(aes(fill = prey_class),decreasing = FALSE) +
  geom_stratum(decreasing = FALSE, aes(fill = prey_class), color = "#cccccc") +
  scale_fill_manual(values = class_color) +
  theme_IWK()+
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_line(color = "#00000000"),
        legend.position = "none") 

preyflow_alluvium

preyflow_alluvium +
  coord_cartesian(ylim = c(0,1000))

preyflow_alluvium +
  coord_cartesian(ylim = c(0,100))


preyflow_sp <- ggplot(data = dplyr::select(westernsnakes, predatorID, prey_genus_sp) %>% 
                        filter(predatorID != "", prey_genus_sp != "", !is.na(prey_genus_sp)) %>%
                        count(predatorID, prey_genus_sp) %>% 
                        mutate(prey = paste0(prey_genus_sp,"-")) %>%
                        arrange(n, decreasing = TRUE),
       aes(axis1 = predatorID,   # First variable on the X-axis
           axis2 = prey, # Second variable on the X-axis
           y = n)) +
  geom_alluvium(decreasing = FALSE, fill = "#74ac00") +
  geom_stratum(decreasing = FALSE, fill = "#74ac00", color = "#cccccc") +
  theme_IWK()+
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_line(color = "#00000000"),
        legend.position = "none")
preyflow_sp 


#### Make a Map ####
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
earth <- ne_countries(type = "countries", scale = "medium")

mapdf <- st_as_sf(iNat%>%filter(!is.na(latitude)), 
                  coords = c(x = "longitude", y = "latitude"))
st_crs(mapdf) <- 4326

worldmap <- ggplot(data = earth)+
  geom_sf() +
  geom_sf(data = mapdf, color = "#74ac00") +
  theme_IWK()

west <- st_crop(earth, xmin = -135, xmax = -95,
                ymin = 15, ymax = 55)

westernsnakes <- st_crop(mapdf, xmin = -135, xmax = -95,
                         ymin = 15, ymax = 55)

westernmap <- ggplot(data = west)+
  geom_sf() +
  geom_sf(data = westernsnakes, aes(color = prey_class)) +
  coord_sf()+

  theme_IWK()