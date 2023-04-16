####SORTIE TRAITS DE BESTI####
#Script pour sortir les traits disponibles et potentiellement
#interessants de la BDD betsi en fonction des especes ou genre
#---------------

#packages----
library(dplyr)
library(tidyverse)
#------
#Importation de la BDD----
read.csv("data/raw-data/BETSI_220221.csv",header = T, sep=";")->BETSI


#Sélection des traits dispos pour les genres et ce pour chaque grand taxa-----
    #Carabidae----
Carabidae_traits_dispo<-inner_join(
  filter(BETSI, grepl("Abax",taxon_name))%>%
  distinct(trait_name),
  filter(BETSI, grepl("Pterostichus",taxon_name))%>%
  distinct(trait_name),
  filter(BETSI, grepl("Carabus",taxon_name))%>%
  distinct(trait_name),
  by="trait_name")

Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Cychrus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Calathus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")

Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Cicindela",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Nebria",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")

Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Microlestes",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")


    #Orthoptera-----

Orthopera_traits_dispo<-inner_join(
  filter(BETSI, grepl("Arcyptera",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Stauroderus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Chorthippus",taxon_name))%>%
    distinct(trait_name),
    by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Gomphoceridius",taxon_name))%>%
    distinct(trait_name),
    by="trait_name")

