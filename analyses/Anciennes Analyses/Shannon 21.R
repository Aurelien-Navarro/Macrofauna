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
install.packages("esquisse")
library(esquisse)
#-----------------------------------

#---------------------
#Importation donnes#####
#---------------------

all_orchamp21 <-read.csv("data/derived-data/Donnees_ORCHAMP_esp_propres.csv", sep=",", dec=".",header=TRUE)
str(all_orchamp21)

#----------------------------
#Organisation des donnees####
#----------------------------

tp<-all_orchamp21 %>%
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
as.numeric(matrice$Opistophora)->matrice$Opistophora
as.numeric(matrice$Pulmonata)->matrice$Pulmonata
as.numeric(matrice$unid)->matrice$unid
as.numeric(matrice$Diptera)->matrice$Diptera
as.numeric(matrice$Homoptera)->matrice$Homoptera
as.numeric(matrice$Hymenoptera)->matrice$Hymenoptera
as.numeric(matrice$Orthoptera)->matrice$Orthoptera
as.numeric(matrice$Dermaptera)->matrice$Dermaptera
as.numeric(matrice$Hemiptera)->matrice$Hemiptera
as.numeric(matrice$Glomerida)->matrice$Glomerida
as.numeric(matrice$Geophilomorpha)->matrice$Geophilomorpha
as.numeric(matrice$Julida)->matrice$Julida
as.numeric(matrice$Lithobiomorpha)->matrice$Lithobiomorpha
as.numeric(matrice$Polydesmida)->matrice$Polydesmida
as.numeric(matrice$Lepidoptera)->matrice$Lepidoptera
as.numeric(matrice$Isopoda)->matrice$Isopoda
as.numeric(matrice$Blattoptera)->matrice$Blattoptera

#------------------------------------------------

#-------------------------------------------
#Creation de la matrice pour analyse Shannon
#-------------------------------------------

matrice2<-matrice[,c(3:22)]#conservation uniquement des variables ordre
str(matrice)

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

#Argentiere------
Htabl[c(1:15),]->Harg

#Plot----------------

ggplot(Harg) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Argentiere : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHarg
bpHarg+coord_flip()
#-------------------

#Armenaz---------
Htabl[c(16,17),]->Harm

#Plot----------------

ggplot(Harm) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Armenaz : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHarm
bpHarm+coord_flip()

#------------------

#Massif Saint Barthelemy--------
Htabl[c(18:23),]->Hmsb

#Plot----------------

ggplot(Hmsb) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Massif Saint Barthelemy : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHmsb
bpHmsb+coord_flip()
#-------------------

#Pecloz-------
Htabl[c(24:26),]->Hpec

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

#Tania------
Htabl[c(27:38),]->Htan

#Plot----------------

ggplot(Htan) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Tania : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpHtan
bpHtan+coord_flip()
#-------------------

#Valoire------
Htabl[c(39:42),]->Hval

#Plot----------------

ggplot(Hval) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Valoire : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpval
bpval+coord_flip()
#-------------------

#Lac vert------
Htabl[c(43:49),]->Hver

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
Htabl[c(51:57),]->Hvtn

#Plot----------------

ggplot(Hvtn) +
  aes(x = id_plot, y = H, fill = id_plot) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Sites", y = "H'", title = "Ventoux : Shannon Diversity") +
  theme_minimal() +
  facet_wrap(vars(method))->bpvtn
bpvtn+coord_flip()
#-------------------
