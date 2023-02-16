###################################
#BETA DIVERSITE 2022##############
#################################

library(ggplot2)
library(tidyverse)
library(vegan)
library(dplyr)
library(tibble)
library(ggplot2)
library(rcompendium)
#add_compendium(compendium=".")
library(esquisse)
install.packages("fossil")
library(fossil)
#-----------------------------------

#---------------------
#Importation donnes#####
#---------------------

all_orchamp22 <-read.csv("data/raw-data/Donnees ORCHAMP 22.csv", sep=",", dec=".",header=TRUE)
str(all_orchamp22)

#----------------------------
#Organisation des donnees####
#----------------------------

tp<-all_orchamp22 %>%
  mutate(Order2 = ifelse(Order == "", "unid", Order)) %>% ##supression des vides 
  group_by(id_plot, Order2, method)%>%
  summarise(tot = sum(abundance)) 
#---------------------------------------

#####transformation en matrice#### 
pivot_wider(tp,
            id_cols = c('id_plot','method'),
            names_from = 'Order2', 
            values_from = 'tot',
            values_fill = 0)->matrice
#infos sur la matrice
str(matrice)
head(matrice)
summary(matrice)

#-------------------------------------
#Passage des variables en numerique####
#--------------------------------------

as.numeric(matrice$Araneae)->matrice$Araneae
as.numeric(matrice$Coleoptera)->matrice$Coleoptera
as.numeric(matrice$Collembola)->matrice$Collembola
as.numeric(matrice$Glomerida)->matrice$Glomerida
as.numeric(matrice$Hemiptera)->matrice$Hemiptera
as.numeric(matrice$Homoptera)->matrice$Homoptera
as.numeric(matrice$Hymenoptera)->matrice$Hymenoptera
as.numeric(matrice$Isopoda)->matrice$Isopoda
as.numeric(matrice$Opiliones)->matrice$Opiliones
as.numeric(matrice$Opistophora)->matrice$Opistophora
as.numeric(matrice$Orthoptera)->matrice$Orthoptera
as.numeric(matrice$Pulmonata)->matrice$Pulmonata
as.numeric(matrice$Thysanoptera)->matrice$Thysanoptera
as.numeric(matrice$unid)->matrice$unid
as.numeric(matrice$Geophilomorpha)->matrice$Geophilomorpha
as.numeric(matrice$Lithobiomorpha)->matrice$Lithobiomorpha
as.numeric(matrice$Julida)->matrice$Julida
as.numeric(matrice$Polydesmida)->matrice$Polydesmida
as.numeric(matrice$Diptera)->matrice$Diptera
as.numeric(matrice$Lepidoptera)->matrice$Lepidoptera
as.numeric(matrice$Dermaptera)->matrice$Dermaptera
as.numeric(matrice$Blattoptera)->matrice$Blattoptera

#------------------------------------------------

#-------------------------------------------
#Creation de la matrice pour analyses
#-------------------------------------------

matrice2<-matrice[,c(3,4,6:15,17:20, 22:24)]#conservation uniquement des variables ordre
str(matrice2)

matricebin<-ifelse(matrice>0,1,0)
