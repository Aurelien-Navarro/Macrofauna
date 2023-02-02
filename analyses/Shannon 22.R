#############################
#####Diversite de Shannon####
############################

library(ggplot2)
library(tidyverse)
library(vegan)
library(dplyr)
library(tibble)
library(ggplot2)
library(rcompendium)
#add_compendium(compendium=".")
library(esquisse)
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
#Creation de la matrice pour analyse Shannon
#-------------------------------------------

matrice2<-matrice[,c(3:15,17:24)]#conservation uniquement des variables ordre
str(matrice2)

#-------------------------------------------

#------------------------------------------
#Creation de l'indice de shannon-----------
#------------------------------------------

vegan::diversity(matrice2, index="shannon")->H;H #shannon index pour chaque site et chaque methode     
Htabl<-matrice[,c(1,2)]
Htabl%>%
  add_column(H=H)->Htabl
  
  
  #Bar plot exploratoire-----
ggplot(data=Htabl, aes(x=id_plot, y=H), ylab="localite",xlab="Diversite de Shannon")+
  geom_bar(stat="identity")+
  theme_minimal()->bpHtabl
bpHtabl+coord_flip()
#--------------------------

#Armenaz------
Htabl[c(1:4),]->Harm

#Plot----------------

ggplot(Harm) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Armenaz : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHarm
bpHarm+coord_flip()
#-------------------

#Massif saint Barthelemy---------
Htabl[c(5:15),]->Hmsb

#Plot----------------

ggplot(Hmsb) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Massif Saint Barthelemy : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHmsb
bpHmsb+coord_flip()

#------------------

#Pecloz--------
Htabl[c(16:20),]->Hpec

#Plot----------------

ggplot(Hpec) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Pecloz : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHpec
bpHpec+coord_flip()
#-------------------

#Pecloz-------
Htabl[c(21:27),]->Hval

#Plot----------------

ggplot(Hval) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Valoire : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHval
bpHval+coord_flip()
#-------------------

#Tania------
Htabl[c(28:38),]->Hver

#Plot----------------

ggplot(Hver) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Lac Vert : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHver
bpHver+coord_flip()
#-------------------

#Ventoux------
Htabl[c(39:51),]->Hvtn

#Plot----------------

ggplot(Hvtn) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Ventoux : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHvtn
bpvtn+coord_flip()
#-------------------

