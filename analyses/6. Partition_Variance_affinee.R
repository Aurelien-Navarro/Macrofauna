#Partition de variance version affinee 
#
#objectif : encapsuler la fonction adipart() dans une fonction afin de pouvoir 
#facilement injecter les conditions 

#/!\ il y a une confusion dans valloire 2710 ou 2715 car altitude de Orchamp c'est 2710 
#mais celui des echantillons c'est 2715, il faut parfois changer ça dans les talbeurs

#librairies
library(vegan)
library(dplyr)
library(tibble)
library(tidyverse)
library(esquisse)
library(ggplot2)

#Importation des donnees 
#tableau especes
read.csv("data/derived-data/Esp/clean_data_2023-05-30.csv", h=T, sep=',')->ESP
ESP%>%
  filter(!gradient %in% c("BOU","CAU"))->ESP #Forêts du bout et Cauteret non terminés
#tableau des echelles 
read.csv("data/derived-data/Esp/clean_data_2023-05-24.csv", h=T, sep=',')->echelle0
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0


#les fonctions

source("analyses/functions/my_adipart_function.R")
source("analyses/functions/my_null_model_function.R")
#utilisation des fonctions

##Orthopteres----
my_null_model_function(ESP = ESP[ESP$orderName == "Orthoptera",], 
                                      ECHELLE = echelle0,
                                      Methode = "barber",
                                      habit0=habit0)
ORTHO_addipart<-my_adipart_function(ESP = ESP[ESP$orderName == "Orthoptera",], 
                 ECHELLE = echelle0,
                 Methode = "barber",
                 habit0=habit0)

  ###Sauvergarde des differents outputs 
write.csv(ORTHO_addipart$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Herbivores/Orthopteres_alphabeta" , as.character(Sys.Date()) , ".csv"))
write.csv(ORTHO_nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Herbivores/Orthopteres_nullmodel" , as.character(Sys.Date()) , ".csv"))


##Carabidae----
my_null_model_function(ESP = ESP[ESP$familyName == "Carabidae",], 
                                      ECHELLE = echelle0,
                                      Methode = "barber",
                                      habit0=habit0)
CARAB_addipart<-my_adipart_function(ESP = ESP[ESP$familyName == "Carabidae",], 
                                    ECHELLE = echelle0,
                                    Methode = "barber",
                                    habit0=habit0)

###Sauvergarde des differents outputs 
write.csv(CARAB_addipart$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Carabidae/Carab_alphabeta" , as.character(Sys.Date()) , ".csv"))
write.csv(CARAB_nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Carabidae/Carab_nullmodel" , as.character(Sys.Date()) , ".csv"))


##Decomposeurs----
my_null_model_function(ESP = ESP[ESP$orderName %in% "Isopoda"|
                                   ESP$className %in% c("Diplopoda","Clitellata"),], 
                                      ECHELLE = echelle0,
                                      Methode = c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                      habit0=habit0)
DECOMPO_addipart<-my_adipart_function(ESP = ESP[ESP$orderName %in% "Isopoda"|
                                                  ESP$className %in% c("Diplopoda","Clitellata")|
                                                  ESP$familyName %in% "Geotrupidae",], 
                                    ECHELLE = echelle0,
                                    Methode = c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                    habit0=habit0)

###Sauvergarde des differents outputs 
write.csv(DECOMPO_addipart$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/Decompo_alphabeta" , as.character(Sys.Date()) , ".csv"))
write.csv(DECOMPO_nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/Decompo_nullmodel" , as.character(Sys.Date()) , ".csv"))


##Hymenoptera----
my_null_model_function(ESP = ESP[ESP$orderName %in% "Hymenoptera"
                                       |!ESP$familyName %in%"Formicidae",], 
                                        ECHELLE = echelle0,
                                        Methode = "barber",
                                        habit0=habit0)
HYMENO_addipart<-my_adipart_function(ESP = ESP[ESP$orderName %in% "Hymenoptera",], 
                                      ECHELLE = echelle0,
                                      Methode = "barber",
                                      habit0=habit0)

###Sauvergarde des differents outputs 
write.csv(HYMENO_addipart$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Hymeno/Hymeno_alphabeta" , as.character(Sys.Date()) , ".csv"))
write.csv(HYMENO_nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Hymeno/Hymeno_nullmodel" , as.character(Sys.Date()) , ".csv"))

##Comparaison entre groupes ------

resum <- tibble(
  echelle = c("1.Alpha", "β1_Sample","β2_Plots","β3_Gradients", "β4_Mountains"), 
  Parasitoids = HYMENO_addipart$Variance_expliquee,
  Detritivores = DECOMPO_addipart$Variance_explique,
  Predators = CARAB_addipart$Variance_expliquee,
  Herbivores = ORTHO_addipart$Variance_expliquee)

resum <- resum %>%
  pivot_longer(cols = 2:5, names_to = "taxo")
resum<-resum[-c(17,18,19,20),]
ggplot(resum) +
  aes(x = taxo, y = value, fill = echelle) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_grey(end=1)+
  facet_grid(echelle~., scales="free_y")+
  labs(
    x = "",
    y = "Diversity explained (%) "
  ) +
  theme_minimal()+
  theme(legend.position = "none")


