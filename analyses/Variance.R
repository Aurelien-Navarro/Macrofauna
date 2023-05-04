#Partition de variance 

#@Objectif : déterminer à quelle échelle se joue la plus forte variabilité 
#@Cible : les donnees taxo et les donnes traits

#Librairies 
library(vegan)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tibble)
library(betapart)
library(cowplot)
library(gdm)
library(FactoMineR)
library(factoextra)
library(pander)
library(esquisse)

#Importation des BDD 
  ##BDD especes
read.csv("data/derived-data/Esp/matrice_esp_2023-04-25.csv", header=T, sep=",")->ESP_plot
read.csv("data/derived-data/Esp/clean_data_2023-04-25.csv", header=T, sep=",")->ESP_samplenomatr

  ##BDD traits
read.csv("data/derived-data/Traits/traits_homo_2023-04-27.csv", header=T, sep=",")->TRAITS

  ##BDD environnement
read.csv("data/derived-data/Envir/ENV_2023-04-28.csv", header=T, sep=",")->ENV

#Pour préparer l'analyse, il faut un tableau de donnée type matrice pour chaque échelle 
#On a donc 
#1. Esp_sample 
#2. Esp_plot
#3. Esp_gradient
#4. Esp_massif
#5. Esp_milieu

#Prepartion des donnees-----
  ##preparation de Esp_sample-----
ESP_samplenomatr%>%
  filter(!grepl("0", abundance))%>%
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
    ###transfo en matrice------
        pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_sample

  ##preparation de esp_plot----
ESP_samplenomatr%>%
  filter(!grepl("0", abundance))%>%
      unite(id_plot, gradient, alti)%>%
      mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
      group_by(id_plot, name2)%>%
      summarise(tot = sum(abundance))->tp2
      
    ###transfo en matrice-----
pivot_wider(tp2,
            id_cols = 'id_plot',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_plot
      
  ##Preparation de esp_gradient-----
    #reprendre une matrice qui n'est pas une matrice esp deja prete 
          ESP_samplenomatr%>%
  filter(!grepl("0", abundance))%>%
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(gradient, name2)%>%
  summarise(tot = sum(abundance))->tp3
      ###transformation en matrice-----
        pivot_wider(tp3,
            id_cols = 'gradient',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_grad

  ##Preparation de esp_massif-----
    #reprendre une matrice qui n'est pas une matrice esp deja prete

 ESP_samplenomatr%>%
  filter(!grepl("0", abundance))%>%
  mutate(massif = ifelse(gradient==c("MSB","VER","CAU"),"Pyr", "Alp"))%>%#creation d'une colone massif
  mutate(name2 = ifelse(name == "", "unid", name)) %>%
  group_by(massif, name2)%>%
  summarise(tot = sum(abundance))->tp4
    ###transformation en matrice----        
pivot_wider(tp4,
            id_cols = 'massif',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_massif

  ##Preparation de esp_milieu-----
ENV%>%
  rename(id_plot=codeplot)%>%
  mutate(Milieu2 = ifelse(Milieu == "0", "Ouvert", 'Foret'))%>%
  select(c(id_plot, Milieu2))->tp5.0#recuperation de l'adequation site/milieu ouvert ou ferme
 
ESP_samplenomatr%>%
  filter(!grepl("0", abundance))%>%
  unite(id_plot, gradient, alti)%>%
  mutate(name2 = ifelse(name == "", "unid", name))->tp5.1
inner_join(tp5.0, tp5.1, by="id_plot")->tp5.2
tp5.2%>%
group_by(Milieu2, name2)%>%
  summarise(tot = sum(abundance))->tp5.3

  #transformation en matrice

pivot_wider(tp5.3,
            id_cols = 'Milieu2',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_milieu


#creation de matrices de dissimilarite par couples de site
ESP_sample%>%
  remove_rownames()%>%
  column_to_rownames(var='id_sample')->ESP_sample_ind #passage samples en index

ESP_plot%>%
  remove_rownames()%>%
  column_to_rownames(var='id_plot')->ESP_plot_ind

ESP_grad%>%
  remove_rownames()%>%
  column_to_rownames(var='gradient')->ESP_grad_ind

ESP_massif%>%
  remove_rownames()%>%
  column_to_rownames(var='massif')->ESP_massif_ind

ESP_milieu%>%
  remove_rownames()%>%
  column_to_rownames(var='Milieu2')->ESP_milieu_ind


  ##dissimilarite sur esp_sample----
ESP_sample_ind->ESP_sample_bin
ESP_sample_bin[ESP_sample_bin!=0]<-1
C1 <- beta.pair(ESP_sample_bin, index.family = "jaccard")
jac_plots_C1 <- as.matrix(C1$beta.jac)

  ##dissimilarite sur esp_plot----
ESP_plot_ind->ESP_plot_bin
ESP_plot_bin[ESP_plot_bin!=0]<-1
C2 <- beta.pair(ESP_plot_bin, index.family = "jaccard")
jac_plots_C2 <- as.matrix(C2$beta.jac)

  ##dissimilarite sur esp_gradient----
ESP_grad_ind->ESP_grad_bin
ESP_grad_bin[ESP_grad_bin!=0]<-1
C3 <- beta.pair(ESP_grad_bin, index.family = "jaccard")
jac_plots_C3 <- as.matrix(C3$beta.jac)

  ##dissimilarite sur esp_massif
ESP_massif_ind->ESP_massif_bin
ESP_massif_bin[ESP_massif_bin!=0]<-1
C4 <- beta.pair(ESP_massif_bin, index.family = "jaccard")
jac_plots_C4 <- as.matrix(C4$beta.jac)

  ##Dissimilarite sur sp_milieu
ESP_milieu_ind->ESP_milieu_bin
ESP_milieu_bin[ESP_milieu_bin!=0]<-1
C5 <- beta.pair(ESP_milieu_bin, index.family = "jaccard")
jac_plots_C5 <- as.matrix(C5$beta.jac)



  #Dissimilarité Moyenne

mean(jac_plots_C1)->BetaC1
mean(jac_plots_C2)->BetaC2
mean(jac_plots_C3)->BetaC3
mean(jac_plots_C4)->BetaC4
mean(jac_plots_C5)->BetaC5


#faible beta moyenne : elements semblables 
#forte beta moyenne : element dissemblables 

Echelle <- c("Echantillons","plot","gradient","massif","habitat")
Dissimilarite <-c(BetaC1, BetaC2, BetaC3, BetaC4, BetaC5)

beta_echelle<- data.frame(Echelle, Dissimilarite)


library(ggplot2)

ggplot(beta_echelle) +
 aes(x = Echelle, fill = Echelle, colour = Echelle, weight = Dissimilarite ) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 scale_color_hue(direction = 1) +
 theme_minimal()

#Plus l'echelle est haute, plus la dissimilarite moyenne entre element est faible
#A large echelle, les elements se ressemblent plus qu'à faible echelle 

#####################
#ZONE DE TRAVAUX-----
####################

#recup du tableau ech-esp

ESP_samplenomatr%>%
  distinct(id_sample, .keep_all=T)%>%
  inner_join(ESP_sample, by='id_sample')%>%
  select(id_sample) ->echelle0

ESP_samplenomatr%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot))%>%
  inner_join(ESP_sample, by='id_sample')%>%
  select(id_plot) ->echelle1

ESP_samplenomatr%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  inner_join(ESP_sample, by='id_sample')%>%
  select(gradient)->echelle2

ESP_samplenomatr%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  inner_join(ESP_sample, by='id_sample')%>%
  mutate(massif = ifelse(gradient==c("MSB","VER","CAU"),"Pyr", "Alp"))%>%
  select(massif)->echelle3

echelle1%>%
  inner_join(ENV1, by='id_plot')->test
  

  





varpart(ESP_sample_ind, echelle1, echelle2, echelle3)->spe.part.all
spe.part.all$part

plot(spe.part.all,
     Xnames = c("echelle 1","echelle2",'echelle3'), # noms des matrices explicatives
     bg = c("seagreen3", "mediumpurple", "purple"), alpha = 80,
     digits = 2,
     cex = 1)
