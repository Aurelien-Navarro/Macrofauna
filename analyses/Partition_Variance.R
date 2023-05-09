########################
#Analyse de variance----
########################

#librairies
library(vegan)


##Utilisation de la fonction adipart()
#besoin d'un tableau communautés, et d'un tableau avec les échelles en hierarchie
#decroissante


#tableau communautes-----
read.csv("data/derived-data/Esp/clean_data_2023-04-25.csv", h=T, sep=',')->ESP

ESP%>%
  filter(!grepl("0", abundance))%>%
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice

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

#echelle du massif (alpe ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = ifelse(gradient==c("MSB","VER","CAU"),"Pyr", "Alp"))%>%
  select(c(id_sample, massif))->massif

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude

#Concaténation 

sample%>%
  left_join(plot, by="id_sample")%>%
  left_join(localite, by="id_sample")%>%
  left_join(massif, by="id_sample")%>%
  left_join(altitude, by="id_sample")->echelle


#Partitition de variance-----

