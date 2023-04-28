##############
#----GDM-----
#############

##@Objectif : Essais de GDM sur donnees ORCHAMP macrofaune

#Packages 
library(dplyr)
library(vegan)
library(ggplot2)
library(betapart)
library(cowplot)
library(gdm)
library(FactoMineR)
library(factoextra)
library(pander)
library(tibble)

#injection des DF 
read.csv("data/derived-data/ENV_2023-04-26.csv", row.names = 1)->ENV
read.csv("data/derived-data/traits_homo_2023-04-27.csv",header=T, sep=",")->TRAITS

#DECOMPOSEURS-------------
  ##VDT (on n'a que ça)---
source("analyses/functions/IndCom.R")
lumbricid_tr <- TRAITS %>% 
  filter(trait_name %in% c("Body_length", "Habitat", "ecological_strategy"))
lumbricid_ind <- myIndices(DF = df[df$orderName == "Crassiclitellata",], 
                           IDresol = "Espèce", TR = lumbricid_tr)
    
lumbricid_ind$alpha->TRAITSVDT
na.omit(TRAITSVDT)->TRAITSVDT

#Creation de la matrice de dissimularite 
  ##On va prendre Bray-Curtis comme metrique

      ###Passage de ID sample en lignes_index
TRAITSVDT %>%
  filter(!c(is.na(id_sample)))%>%
  remove_rownames()%>%
  column_to_rownames(var='id_sample')->TRAITSVDT
  #matrice
Dissvdt <- vegdist(TRAITSVDT, method="bray", na.rm=T)
as.matrix(Dissvdt)->Dissvdt

