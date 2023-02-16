###Script Mikael
###############################
## Analyses des donnees RMQS 
## 2022 phase test
## 24/08/2022
## Analyse data barbers: abondances
###############################

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
install.packages("formattable")
library(formattable)

#Chargement donnees
ORCP22 <- read.csv("data/raw-data/Donnees Orchamp 22.csv", h=T , sep = "," )
#carac <- read.xlsx(xlsxFile = "data/Carac_stations_221121.xlsx")

str(ORCP22)
ORCP22c <- ORCP22 %>%
  filter(method == "barber")  %>%
  mutate(rank = ordered(rank, c("Class", "Order", "Family", "Genus", "Species"))) %>%
  filter(!class %in% c("Collembola"), !order %in% c("Diptera"))


#Donut chart of taxonomic distribution
donut <- mydataBA %>%
  select(order, class, ab) %>%
  mutate(order = ifelse(order == "", "NI", order)) %>%
  mutate(order = ifelse(is.na(order), "NI", order)) %>% 
  mutate(class = ifelse(is.na(class), "NI", class)) %>%
  mutate(class = as.factor(class), order = as.factor(order))%>%  
  group_by(order, class) %>%
  summarise(n = sum(ab)) 


jpeg("figs/BA_piedonut.jpeg", width = 700, height = 700)
PieDonut(donut, aes(class, order, count=n), explode = 1:6, donutLabelSize = 2.5,
         pieLabelSize = 3, ratioByGroup = FALSE, 
         title = "Répartition taxonomique des effectifs échantillonnés par pièges barber")
dev.off()

# ---------------------------------------------------
#Niveau d'identification
NivId <- mydataBA %>%
  mutate(kingdom = "animalia", .after = class) %>%
  select(species, genus, family, order, class, kingdom, rank, stade, ab) %>%
  mutate_all(na_if,"") %>%
  mutate_if(is.character, ~replace_na(.,"NI")) %>%
  mutate(stade = ifelse(stade %in% c("cocon", "pupe", "larve"), "juvenile", stade)) %>%
  group_by(species, genus, family, order, class, kingdom, rank, stade) %>%   
  summarise(n = sum(ab)) 

NivId_Class <- NivId %>%
  select(class, n, rank)%>%
  group_by(class, rank) %>%
  summarise(n = sum(n)) %>% 
  mutate(freq = formattable::percent(n / sum(n))) %>%
  filter(class %in% c("Arachnida", "Chilopoda", "Diplopoda", "Gastropoda", "Insecta", "Malacostraca")) 

jpeg("figs/BA_nivId.jpeg", width = 1400, height = 700)
ggplot(NivId_Class, aes(x = reorder(class, desc(freq)), y = freq, fill = rank)) +
  geom_bar(position="fill", stat="identity")+
  labs(x = "Classes principales", y = "Pourcentage d'individus identifiés")+
  theme_bw()
dev.off()


NivId_Insecta <- NivId %>%
  filter(class == "Insecta") %>%
  select(order, stade, n, rank) %>%
  mutate(stade = ifelse(stade == "adulte", "adulte", "immature")) %>%
  group_by(order, stade, rank) %>%
  summarise(n = sum(n)) %>%
  filter(order != "NI")%>% 
  mutate(freq = formattable::percent(n / sum(n))) %>%
  filter(order != "Diptera"| stade != "adulte")

jpeg("figs/BA_nivId_Insecta.jpeg", width = 1400, height = 700)
ggplot(NivId_Insecta, aes(x = reorder(order, desc(freq)), y = freq, fill = rank)) +
  geom_bar(position="fill", stat="identity")+
  labs(x = "Classes principales", y = "Pourcentage d'individus identifiés")+
  theme_bw()+
  facet_grid(stade~.)
dev.off()