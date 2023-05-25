#Script pour avoir un tableau avec les échelles 
#comme pour la partition de variance sauf que 
#celui de la partition de variance est enfermé dans 
#la fonctin my_addipart_function

#library
library(dplyr)
library(tibble)
library(tidyverse)


#importation tableau
read.csv("data/derived-data/Esp/clean_data_2023-05-24.csv", h=T, sep=',')->echelle0
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0

#creation echelle
echelle0%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample

#echelle du plot (gradient et altitude)
echelle0%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle du gradient (localité)
echelle0%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'habitat 
#importation du tableau qui possède les habitats

habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(plot, by='id_plot')%>%
  left_join(localite, by='id_sample')%>%
  unite(habitat, Milieu, gradient)%>%
  select(c(id_sample, habitat))->milieu


#echelle de l'altitude
echelle0%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (Alpes S ou AlpesN ou pyr)
echelle0%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = case_when(gradient %in% c("MSB","VER","CAU")~"Pyr",
                            gradient %in% c("VCHA", "VTN", "MOU","RIS")~"AlpS",
                            gradient %in% c("VAL", "BOU", "TAN","PEC","ARG","ARM")~"AlpN"))%>%
  select(c(id_sample, massif))->massif



#Echelle totale(gamma)
echelle0%>%
  distinct(id_sample, .keep_all=T)%>%
  add_column(Dispositif = "Orchamp")%>%
  select(c("id_sample","Dispositif"))->gamma


#Concaténation 

plot%>%
  inner_join(altitude, by="id_sample")%>%
  inner_join(milieu, by="id_sample")%>%
  inner_join(localite, by="id_sample")%>%
  inner_join(massif, by="id_sample")%>%
  inner_join(gamma, by="id_sample")%>%
  select(c(id_sample, id_plot, habitat, gradient, massif, Dispositif))%>%
  relocate(habitat, .before=gradient)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample

#SOrtie de l echelle 
write.csv(echelle, file = paste0("data/derived-data/ECHELLE" , as.character(Sys.Date()) , ".csv"))

