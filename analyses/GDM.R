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
read.csv("data/derived-data/envir/ENV_2023-04-26.csv", row.names = 1)->ENV
read.csv("data/derived-data/traits_homo_2023-04-27.csv",header=T, sep=",")->TRAITS

#DECOMPOSEURS-------------
  
#Creation de la matrice de dissimularite 
  ##On va prendre Bray-Curtis comme metrique

#adequation entre ENV et Dissvdt
ENV%>%
  rename(id_plot='codeplot')->ENV
detrialphaplot%>%
inner_join(ENV, by="id_plot")%>%
  select(c('id_plot','ab','mass','q0','q1','q2','Body_length'))->detrialphaplot
ENV%>%
  inner_join(detrialphaplot, by="id_plot")%>%
  select(!c('id_plot','ab','mass','q0','q1','q2','Body_length'))->ENV

###Passage de ID sample en lignes_index
as.tibble(detrialphaplot)->detrialphaplot
detrialphaplot %>%
  filter(!c(is.na(id_plot)))%>%
  remove_rownames() %>%
  column_to_rownames()->TRAITSVDT
#matrice de dissimalite 
Dissvdt <- vegdist(TRAITSVDT, method="bray", na.rm=T)
as.matrix(Dissvdt)->Dissvdt
