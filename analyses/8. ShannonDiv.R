#relation altitude-diversity 

library (dplyr)
library(vegan)
library(tidyr)
library(tidyverse)
library(ggplot2)

read.csv("data/derived-data/Esp/clean_data_2023-05-30.csv",header=T, sep=",")->ESP
ESP%>%
  filter(grepl("ARG|ARM|RIS|VCHA|VAL|PEC|MOU|VTN|TAN|MSB|VER", gradient))->ESP

Herbi<-ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",]
Predat<-ESP[ESP$familyName=="Carabidae",]
Detri<-ESP[ESP$orderName %in% "Isopoda"|
             ESP$className %in% c("Diplopoda","Clitellata"),]
Para<-ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",]


Para%>%
  filter(rankName=="Genre")%>%
  filter(method%in%"barber")%>%
  group_by(alti, name)%>%
  summarise(tot=sum(abundance))->tp
 
pivot_wider(tp,
              id_cols = "alti",
              names_from = "name", 
              values_from = "tot",
              values_fill = 0)->matrice
matrice%>%
  na.omit()%>%
  select(alti)->ALT

matrice%>%
  na.omit()%>%
  remove_rownames()%>%
  column_to_rownames("alti")->matrice


vegdist(matrice, method="bray", na.rm=T)->dist
as.matrix(dist)->dist
rowMeans(dist)->paradist

k1<-data.frame(Guild="Herbivore", Diss=herbidist)
k2<-data.frame(Guild="Predators", Diss=predatdist)
k3<-data.frame(Guild="Detritivores", Diss=detridist)
k4<-data.frame(Guild="Parasitoids", Diss=paradist)

Beta<-bind_rows(k1, k2, k3, k4)

ggplot(Beta, aes(x=Guild, y=Diss))+
  geom_boxplot()+
  geom_point()+
  theme_minimal()





diversity(matrice, "shannon")->H
as.data.frame(ALT)->ALT
ALT%>%
  add_column(H)->final

final$alti <- as.numeric(final$alti)
final$H <- as.numeric(final$H)

ggplot(final, aes(x = alti, y = H)) +
  geom_point() +
  geom_smooth(method = "loess", color="orange", fill = "yellow", se = TRUE) +
  labs(x = "Altitude (m)", y = "Shannon Diversity", title = "Parasitoids Shannon Diversity for every gradients", subtitle = "LOESS fitting") +
  theme_minimal()
   

