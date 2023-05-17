#GDM affiné 

#Script pour insérer les différentes conditions au GDM et le produire 

#librairies
librarian::shelf(dplyr,vegan, ggplot2, betapart, gdm, tibble, tidyverse)

#Importation des données
read.csv("data/derived-data/Envir/ENV_2023-05-17.csv", row.names = 1)->ENV
ENV%>%
  filter(!codeplot %in% c("BOU","CAU"))->ENV
read.csv("data/derived-data/Traits/traits_homo_2023-04-27.csv",header=T, sep=",")->TRAITS
read.csv("data/raw-data/envir/phyto.data3.csv",header=T, sep=",")->Phyto
Phyto%>%
  filter(!codeplot %in% c("BOU","CAU"))->Phyto
read.csv("data/derived-data/Esp/clean_data_2023-05-10.csv",header=T, sep=",")->ESP
ESP%>%
  filter(!gradient %in% c("BOU","CAU"))->ESP


#Fonctions 
source("analyses/functions/my_gdm_function.R")

#GDM ESPECE------

  ##HERBIVORES----
  HERBI_ESP<-my_gdm_function(ENV=ENV,
                            COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                            PHYTO=Phyto,
                            Methode= "barber")
  ##PREDATEURS----
  ##DECOMPOSEURS----
  ##PARASITES----

#GDM TRAITS------

  ##HERBIVORES----
  ##PREDATEURS----
  ##DECOMPOSEURS----
  ##PARASITES----


