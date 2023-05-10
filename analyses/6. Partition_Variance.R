########################
#Analyse de variance----
########################

#librairies
library(vegan)
library(dplyr)
library(tibble)
library(tidyverse)
library(esquisse)

#Espèces-----


##Utilisation de la fonction adipart()
#besoin d'un tableau communautés, et d'un tableau avec les échelles en hierarchie
#decroissante


#tableau communautes-----
read.csv("data/derived-data/Esp/clean_data_2023-05-10.csv", h=T, sep=',')->ESP


#Les decomposeurs-----
ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(orderName %in% "Isopoda" | className %in% c("Diplopoda","Clitellata"))%>% 
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice0

#passage de la colone id en index
ESP_matrice0%>%
  remove_rownames()%>%
  column_to_rownames('id_sample')->ESP_matrice

#tableau des echelles----


#echelle de l echantillon
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample

#echelle du plot (gradient et altitude)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle du gradient (localité)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude

#echelle du massif (alpe ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = ifelse(gradient %in% c("MSB","VER","CAU"),"Pyr", "Alp"))%>%
  select(c(id_sample, massif))->massif


#Concaténation 

ESP_matrice0%>%
  left_join(plot, by="id_sample")%>%
  left_join(altitude, by="id_sample")%>%
  left_join(localite, by="id_sample")%>%
  left_join(massif, by="id_sample")%>%
  select(c(id_plot, gradient, massif))->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


#Partitition de variance addititve-----
adipart(ESP_matrice, echelle,
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
        )->addipartESP

#hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->hier

as.data.frame(hier$statistic)->hierM
hierM%>%
  add_column(echelle=row.names(hierM))%>%
  relocate(echelle, .before = 'hier$statistic')%>%
  rename(variance='hier$statistic')->hierM


addipartESP$statistic
sum(addipartESP$statistic[5:7])
sum(addipartESP$statistic[c(1,5:7)])

esquisser(hierM)


#Les orthopteres----

ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(orderName %in% "Orthoptera")%>% 
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice0

#passage de la colone id en index
ESP_matrice0%>%
  remove_rownames()%>%
  column_to_rownames('id_sample')->ESP_matrice

#tableau des echelles----


#echelle de l echantillon
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample

#echelle du plot (gradient et altitude)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle du gradient (localité)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude

#echelle du massif (alpe ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = ifelse(gradient %in% c("MSB","VER","CAU"),"Pyr", "Alp"))%>%
  select(c(id_sample, massif))->massif


#Concaténation 

ESP_matrice0%>%
  left_join(plot, by="id_sample")%>%
  left_join(altitude, by="id_sample")%>%
  left_join(localite, by="id_sample")%>%
  left_join(massif, by="id_sample")%>%
  select(c(id_plot, gradient, massif))->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


#Partitition de variance addititve-----
adipart(ESP_matrice, echelle,
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP

#hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->hier

as.data.frame(hier$statistic)->hierM
hierM%>%
  add_column(echelle=row.names(hierM))%>%
  relocate(echelle, .before = 'hier$statistic')%>%
  rename(variance='hier$statistic')->hierM


addipartESP$statistic
sum(addipartESP$statistic[5:7])
sum(addipartESP$statistic[c(1,5:7)])

esquisser(hierM)

#Partition de variance multiplicative----

