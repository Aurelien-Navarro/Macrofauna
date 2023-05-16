#Partition de variance version affinee 
#
#objectif : encapsuler la fonction adipart() dans une fonction afin de pouvoir 
#facilement injecter les conditions 


#librairies
library(vegan)
library(dplyr)
library(tibble)
library(tidyverse)
library(esquisse)
library(ggplot2)

#Importation des donnees 
#tableau especes
read.csv("data/derived-data/Esp/clean_data_2023-05-10.csv", h=T, sep=',')->ESP
ESP%>%
  filter(!gradient %in% c("BOU","CAU"))->ESP #Forêts du bout et Cauteret non terminés
#tableau des echelles 
read.csv("data/derived-data/Esp/clean_data_2023-05-10.csv", h=T, sep=',')->echelle0
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0


#les fonctions

source("analyses/functions/my_adipart_function.R")
source("analyses/functions/my_null_model_function.R")
#utilisation de la fonction

##Orthopteres----
ORTHO_nullmod<-my_null_model_function(ESP = ESP[ESP$orderName == "Orthoptera",], 
                                      ECHELLE = echelle0,
                                      Methode = "barber",
                                      habit0=habit0
)
ORTHO_addipart<-my_adipart_function(ESP = ESP[ESP$orderName == "Orthoptera",], 
                 ECHELLE = echelle0,
                 Methode = "barber",
                 habit0=habit0
                 )

  ###Sauvergarde des differents outputs 
write.csv(ORTHO_addipart$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_alphabeta" , as.character(Sys.Date()) , ".csv"))
write.csv(ORTHO_nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_nullmodel" , as.character(Sys.Date()) , ".csv"))


##Carabidae----
CARAB_nullmod<-my_null_model_function(ESP = ESP[ESP$familyName == "Carabidae",], 
                                      ECHELLE = echelle0,
                                      Methode = "barber",
                                      habit0=habit0
)
CARAB_addipart<-my_adipart_function(ESP = ESP[ESP$familyName == "Carabidae",], 
                                    ECHELLE = echelle0,
                                    Methode = "barber",
                                    habit0=habit0
)

###Sauvergarde des differents outputs 
write.csv(CARAB_addipart$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Carabidae/Carab_alphabeta" , as.character(Sys.Date()) , ".csv"))
write.csv(CARAB_nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Carabidae/Carab_nullmodel" , as.character(Sys.Date()) , ".csv"))


##Decomposeurs----
DECOMPO_nullmod<-my_null_model_function(ESP = ESP[ESP$orderName %in% "Isopoda"|
                                                    ESP$className %in% c("Diplopoda","Clitellata")|
                                                    ESP$familyName %in% "Geotrupidae",], 
                                      ECHELLE = echelle0,
                                      Methode = c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                      habit0=habit0
)
DECOMPO_addipart<-my_adipart_function(ESP = ESP[ESP$orderName %in% "Isopoda"|
                                                  ESP$className %in% c("Diplopoda","Clitellata")|
                                                  ESP$familyName %in% "Geotrupidae",], 
                                    ECHELLE = echelle0,
                                    Methode = c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                    habit0=habit0
)

###Sauvergarde des differents outputs 
write.csv(DECOMPO_addipart$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/Decompo_alphabeta" , as.character(Sys.Date()) , ".csv"))
write.csv(DECOMPO_nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/Decompo_nullmodel" , as.character(Sys.Date()) , ".csv"))



##Hymenoptera----
HYMENO_nullmod<-my_null_model_function(ESP = ESP[ESP$orderName %in% "Hymenoptera",], 
                                        ECHELLE = echelle0,
                                        Methode = "barber",
                                        habit0=habit0
)
HYMENO_addipart<-my_adipart_function(ESP = ESP[ESP$orderName %in% "Hymenoptera",], 
                                      ECHELLE = echelle0,
                                      Methode = "barber",
                                      habit0=habit0
)

###Sauvergarde des differents outputs 
write.csv(HYMENO_addipart$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Hymeno/Hymeno_alphabeta" , as.character(Sys.Date()) , ".csv"))
write.csv(HYMENO_nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Hymeno/Hymeno_nullmodel" , as.character(Sys.Date()) , ".csv"))

##Comparaison entre groupes ------

resum <- tibble(
  echelle = c("1.Alpha", "2.Beta-échantillons","3.Beta-stations","4.Beta-gradients", "5.Beta-massifs"), 
  G4hymeno = HYMENO_addipart$Variance_expliquee,
  G3decompo = DECOMPO_addipart$Variance_explique,
  G2carab = CARAB_addipart$Variance_expliquee,
  G1ortho = ORTHO_addipart$Variance_expliquee)

resum <- resum %>%
  pivot_longer(cols = 2:5, names_to = "taxo")

ggplot(resum) +
  aes(x = taxo, y = value, fill = echelle) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_grey(end=1)+
  facet_grid(echelle~., scales="free_y")+
  labs(
    x = "",
    y = "% de variance expliquée",
    title = "Partition de la variance taxonomique",
    subtitle = "Dispositif Orchamp, 2021-2022, 11 gradients"
  ) +
  theme_minimal()+
  theme(legend.position = "none")


