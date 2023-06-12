#my_adipart_function
#
#fonction qui recoit les conditions et rejette les tableaux/figures 
#
#librairies
library(vegan)
library(dplyr)
library(tibble)
library(tidyverse)

my_adipart_function <- function(ESP, ECHELLE, habit0, Methode){
  ESP%>%
    filter(!grepl("0", abundance))%>%
    filter(method == Methode)%>%
    filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
    mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
    group_by(id_sample, name2)%>%
    summarise(tot = sum(abundance))->tp1
  ###transfo en matrice
  pivot_wider(tp1,
              id_cols = 'id_sample',
              names_from = 'name2', 
              values_from = 'tot',
              values_fill = 0)->ESP_matrice0
  
  #passage de la colone id en index
  ESP_matrice0%>%
    remove_rownames()%>%
    column_to_rownames('id_sample')->ESP_matrice
    
  ECHELLE%>%
    filter(!grepl("0", abundance))%>%
    distinct(id_sample, .keep_all=T)%>%
    select(id_sample) ->sample
  
  #echelle du plot (gradient et altitude)
  ECHELLE%>%
    filter(!grepl("0", abundance))%>%
    distinct(id_sample, .keep_all=T)%>%
    unite(id_plot, gradient, alti)%>%
    select(c(id_sample, id_plot)) ->plot
  
  #echelle du gradient (localité)
  ECHELLE%>%
    filter(!grepl("0", abundance))%>%
    distinct(id_sample, .keep_all=T)%>%
    select(c(id_sample, gradient))->localite
  
  #echelle de l'habitat 
  #importation du tableau qui possède les habitats
  
  habit0%>%
    select(c(Milieu, id_plot))%>%
    left_join(plot, by='id_plot')%>%
    left_join(localite, by='id_sample')%>%
    unite(habitat, Milieu, gradient)%>%
    select(c(id_sample, habitat))->milieu
  
  
  #echelle de l'altitude
  ECHELLE%>%
    filter(!grepl("0", abundance))%>%
    distinct(id_sample, .keep_all=T)%>%
    select(c(id_sample, alti)) ->altitude
  
  
  #echelle du massif (Alpes S ou AlpesN ou pyr)
  ECHELLE%>%
    filter(!grepl("0", abundance))%>%
    distinct(id_sample, .keep_all=T)%>%
    select(c(id_sample, gradient))%>%
    mutate(massif = case_when(gradient %in% c("MSB","VER","CAU")~"Pyr",
                              gradient %in% c("VCHA", "VTN", "MOU","RIS")~"AlpS",
                              gradient %in% c("VAL", "BOU", "TAN","PEC","ARG","ARM")~"AlpN"))%>%
    select(c(id_sample, massif))->massif
  
  
  
  #Echelle totale(gamma)
  ECHELLE%>%
    distinct(id_sample, .keep_all=T)%>%
    add_column(Dispositif = "Orchamp")%>%
    select(c("id_sample","Dispositif"))->gamma
  
  
  #Concaténation 
  
  ESP_matrice0%>%
    inner_join(plot, by="id_sample")%>%
    inner_join(altitude, by="id_sample")%>%
    inner_join(milieu, by="id_sample")%>%
    inner_join(localite, by="id_sample")%>%
    inner_join(massif, by="id_sample")%>%
    inner_join(gamma, by="id_sample")%>%
    select(c(id_sample, id_plot, habitat, gradient, massif, Dispositif))%>%
    relocate(habitat, .before=gradient)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample
  
   
   
   #Variance additive 
    adipart(ESP_matrice, echelle[,-3],
          index="shannon", 
          weights = "prop",
          relative = T,
          nsimul = 150,
           )->addipart

    #creation d'un tableau pour les plots 

    niveaux<-addipart$statistic[c(1,6:9)]
    factor1<-(c("1.Alpha", "2.B-Echan","3.B-Station","4.B-Gradient", "5.B-Massif"))
    
    data.frame(x=factor1, y=niveaux)->plotvariance
    plotvariance%>%
      rename(Echelle = x)%>%
      rename(Variance_expliquee = y)->plotvariance
    
return(plotvariance)
    
}
