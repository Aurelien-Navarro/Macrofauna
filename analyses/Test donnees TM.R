#####Tests avec donnees 2021####
#-----------------------------
  #Working directory####
#----------------------------
setwd("C:/AUREL/STAGE/M2/Work/Macrofauna/analysis")
#-------------------------------------------------
#---------------
#Librairies#####
#---------------

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

data<-read.csv("C:/AUREL/STAGE/M2/work/Macrofauna/data/derived-data/Donnees_ORCHAMP_esp_propres.csv", sep=",", dec=".",header=TRUE)
str(data)

#----------------------------
#Mise en forme des donness####
#-----------------------------

na.omit(data$Valid_Name)->data$Valid_Name
data%>%
  distinct(Valid_Name)%>%
  count(Valid_Name)

#-------------------------
#Richesse specifique#####
#------------------------
   
#####Creation de la RS#####

RSalt<-data%>%
  group_by(id_plot)%>%
  distinct(Valid_Name)%>%
  dplyr::summarize(nb=n());RSalt

#####Transformation des donnees pour RS#####
str(RSalt)
as.numeric(RSalt$nb)->RSalt$nb
as.vector(RSalt$id_plot)->RSalt$id_plot
str(RSalt)

#####Barplot exploratoire####
ggplot(data=RSalt, aes(x=id_plot, y=nb), ylab="localite",xlab="Richesse specifique")+
  geom_bar(stat="identity")+
  theme_minimal()->bpRSalt
bpRSalt+coord_flip()
##############################

########Creation des plots pour chaque site####
  ####Argentiere####
data%>%
  group_by(id_plot)%>%
  distinct(Valid_Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(1:6)->ARG
      ####ggplot####
PlotARG<-ggplot(ARG) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(ARG_1420 = "#F8766D", 
                               ARG_1620 = "#93AA00", ARG_1790 = "#00C19F", ARG_2000 = "#619CFF", ARG_2230 = "#FF61C3", ARG_2400="#CC0033")) +
  scale_color_manual(values = c(ARG_1420 = "#F8766D", 
                                ARG_1620 = "#93AA00", ARG_1790 = "#00C19F", ARG_2000 = "#619CFF", ARG_2230 = "#FF61C3", ARG_2400="#CC0033")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Argentiere", caption = "Altitudes") +
  theme_minimal();PlotARG


  ####Armenaz####  
data%>%
  group_by(id_plot)%>%
  distinct(Valid_Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(7,8)->ARM
      ####plot####
PlotARM<-ggplot(ARM) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(ARM_1520 = "#F8766D", 
                               ARM_1750 = "#93AA00")) +
  scale_color_manual(values = c(ARM_1520 = "#F8766D", 
                                ARM_1750 = "#93AA00")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Armenaz", caption = "Altitudes") +
  theme_minimal();PlotARM

  ####Massif Saint Barthelemy####
data%>%
  group_by(id_plot)%>%
  distinct(Valid_Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(9:13)->MSB
      ####ggplot####
PlotMSB<-ggplot(MSB) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(MSB_1370 = "#F8766D", 
                              MSB_1420 = "#93AA00", MSB_1890 = "#00C19F", MSB_2020 = "#619CFF", MSB_2260 = "#FF61C3")) +
  scale_color_manual(values = c(MSB_1370 = "#F8766D", 
                              MSB_1420 = "#93AA00", MSB_1890 = "#00C19F", MSB_2020 = "#619CFF", MSB_2260 = "#FF61C3")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Massif Saint Barthelemy", caption = "Altitudes") +
  theme_minimal();PlotMSB

  ####Pecloz####

data%>%
  group_by(id_plot)%>%
  distinct(Valid_Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(14,15)->PEC
      ####ggplot####
PlotPEC<-ggplot(PEC) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(PEC_1152 = "#F8766D", 
                               PEC_1375 = "#93AA00")) +
  scale_color_manual(values = c(PEC_1152 = "#F8766D", 
                                PEC_1375 = "#93AA00")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Pecloz", caption = "Altitudes") +
  theme_minimal();PlotPEC

  #####Tania####

data%>%
  group_by(id_plot)%>%
  distinct(Valid_Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(16:20)->TAN
    ####ggplot####
PlotTAN<-ggplot(TAN) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(TAN_1420 = "#F8766D", 
                               TAN_1700 = "#93AA00", TAN_1890 = "#00C19F", TAN_2100 = "#619CFF", TAN_2300 = "#FF61C3")) +
  scale_color_manual(values = c(TAN_1420 = "#F8766D", 
                                TAN_1700 = "#93AA00", TAN_1890 = "#00C19F", TAN_2100 = "#619CFF", TAN_2300 = "#FF61C3")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Tania", caption = "Altitudes") +
  theme_minimal();PlotTAN

  #####Valoire####

data%>%
  group_by(id_plot)%>%
  distinct(Valid_Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(21:23)->VAL
      ####ggplot####
PlotVAL<-ggplot(VAL) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(VAL_1860 = "#F8766D", 
                               VAL_2050 = "#93AA00", VAL_2250 = "#00C19F")) +
  scale_color_manual(values = c(VAL_1860 = "#F8766D", 
                                VAL_2050 = "#93AA00", VAL_2250 = "#00C19F")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Valoire", caption = "Altitudes") +
  theme_minimal();PlotVAL

  #####Lac vert####

data%>%
  group_by(id_plot)%>%
  distinct(Valid_Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(24:28)->VER
      ####ggplot####
PlotVER<-ggplot(VER) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(VER_1200 = "#F8766D", 
                               VER_1400F = "#93AA00", VER_1400P = "#00C19F", VER_1800 = "#619CFF", VER_2000 = "#FF61C3")) +
  scale_color_manual(values = c(VER_1200 = "#F8766D", 
                                VER_1400F = "#93AA00", VER_1400P = "#00C19F", VER_1800 = "#619CFF", VER_2000 = "#FF61C3")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Lac Vert", caption = "Altitudes") +
  theme_minimal();PlotVER

  #####Ventoux####

data%>%
  group_by(id_plot)%>%
  distinct(Valid_Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(29:33)->VTN
        ####ggplot####
PlotVTN<-ggplot(VTN) +
 aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
 geom_bar() +
 scale_fill_manual(values = c(VTN_1130 = "#F8766D", 
VTN_1340 = "#93AA00", VTN_1510 = "#00C19F", VTN_1675 = "#619CFF", VTN_1860 = "#FF61C3")) +
 scale_color_manual(values = c(VTN_1130 = "#F8766D", 
VTN_1340 = "#93AA00", VTN_1510 = "#00C19F", VTN_1675 = "#619CFF", VTN_1860 = "#FF61C3")) +
 labs(x = "Altitudes Croissantes", 
 y = "Richesse spécifique", subtitle = "Ventoux", caption = "Altitudes") +
 theme_minimal();PlotVTN



#############################
#####Diversite de Shannon####
############################

all_orchamp21 <-read.csv("data/derived-data/Donnees_ORCHAMP_esp_propres.csv", sep=",", dec=".",header=TRUE)
str(all_orchamp21)


tp<-all_orchamp21 %>%
  mutate(Order2 = ifelse(Order == "", "unid", Order)) %>% ##supression des vides 
  group_by(id_plot, Order2, method)%>%
  summarise(tot = sum(abundance)) 

  
pivot_wider(tp,
            id_cols = c('id_plot','method'),
            names_from = 'Order2', 
            values_from = 'tot',
            values_fill = 0)->matrice
str(matrice)
head(matrice)
summary(matrice)


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





matrice2<-matrice[,c(3:22)]
str(matrice)

vegan::diversity(matrice2, index="shannon")->H;H     
