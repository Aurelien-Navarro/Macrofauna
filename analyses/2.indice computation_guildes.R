# @title
# IndComm sur données Orchamp
# 
# @description
# Calcul d'indices de communautés sur les données Orchamp
# 
# @objectif
# Calculer des indices de communautés, globaux ou par taxon
#
# @details
# 0. Import des données d'identification (fusion ID Eco&Sols et ID INat)
#         - définir des groupes (Grp) d'intérêt
##             - détritivores : vdt, diplo, iso 
##             - prédateurs : carabiques, araignées, ...(?)
##             - herbivores : criquets, ... (?)
# 1. Calcul de variables de communautés


# Libraries
librarian::shelf(dplyr, forcats, stringr, ggplot2)



# Data load
## Community data load 
df <- read.csv("data/derived-data/Esp/clean_data_2023-05-24.csv", 
               h = T, sep = ",") 
df$rankName <-  fct_recode(df$rankName, "Famille" = "Sous-Famille",
                           "Famille" = "Super-Famille",
                           "Classe" = "Infra-Classe",
                           "Genre" = "Sous-Genre",
                           "Ordre" = "Sous-Ordre", 
                           "Phylum" = "Sous-Phylum")


  

# /!\Passer directement a la ligne 53, car correction des taxas de BETSI deja réalisee

####A faire une fois######
## Species trait data load 
traits <- read.csv("data/raw-data/BETSI_220221.csv", h = T, sep = ";")
# if taxonomic homogenization needed (/|\ take hours !!)
traits <- traits %>%
  filter(Taxa %in% c("Arachnida", "Coleoptera", "Dermaptera", "Diplopoda", "Gastropoda",
                     "Isopoda", "Oligochaeta", "Orthoptera","Hymenoptera"))

source("analyses/functions/my_taxonChecker function code.R")
trait_taxa_correct0 <- my_taxonChecker(traits$taxon_name)
trait_taxa_correct <- trait_taxa_correct0 %>%
  mutate(canonic = ifelse(is.na(canonic) == T, scientificName, canonic))
trait_taxa_correct%>%
  rename(taxon_name=fullName)->trait_taxa_correct
traits <- left_join(traits, trait_taxa_correct, by='taxon_name', relationship = "many-to-many") 

#sauvegarde des traits homogeneises 
write.csv(traits, file = paste0("data/derived-data/traits_homo_" , as.character(Sys.Date()) , ".csv"))
#sauvegardé au 10-05


#------------------------------------------------------
## Selection of trait(s) of interest
source("analyses/functions/IndCom.R")


#Importation de traits homogénéisés : 
read.csv("data/derived-data/Traits/traits_homo_2023-04-27.csv", header=T, sep=",")->traits
# Indice computation

## By Guild 
      ###(e.g. detritivores)-----
      detritivore_tr <- traits %>% 
      filter(trait_name %in% c("Body_length", "Habitat"))  
      detritivore_ind  <- myIndices(DF = df[df$orderName %in% "Isopoda"|df$className %in% c("Diplopoda","Clitellata")|df$familyName %in% "Geotrupidae",], 
                              IDresol = "Espèce", TR = detritivore_tr)
      
      (detritivore_ind$alpha)->detrialphaM
      

      ####Echelle : sample----
     write.csv(detrialphaM, file = paste0("data/derived-data/Traits/detriti/detrialphaM_" , as.character(Sys.Date()) , ".csv"))
      
     
      ####Echelle : plot----
      

      #creation colone id_plot sur df pour l'exemple
      df%>%
        unite(id_plot, gradient, alti)%>%
        select(c(id_sample, id_plot)) -> colones_types

      #Fusion entre colone_types et le tableur de sortie 
      left_join(detrialphaM, colones_types, by="id_sample")%>%
        relocate(id_plot, .after = id_sample)->detrialphaM

      #Moyennisage selon non pas id_sample mais selon id_plot
      detrialphaM%>%
        group_by(id_plot)%>%
        summarise(ab = mean(ab),
            mass= mean(mass),
            q0=mean(q0),
            q1=mean(q1),
            q2=mean(q2),
            Body_length=mean(Body_length))->detrialphaplot
      
      #Sortie du df pour les plots 
      write.csv(detrialphaplot, file = paste0("data/derived-data/Traits/detriti/detrialphaplot_" , as.character(Sys.Date()) , ".csv"))
      
      
      ###By predators----
      predat_tr <- traits %>% 
        filter(trait_name %in% "Body_length")  
      predat_ind  <- myIndices(DF = df[df$familyName %in% "Carabidae",], 
                                    IDresol = "Espèce", TR = predat_tr)
    
     
      ####Echelle : sample----
      (predat_ind$alpha)->predatalphaM
      
      write.csv(predatalphaM, file = paste0("data/derived-data/Traits/predat/predatalphaM_" , as.character(Sys.Date()) , ".csv"))
        ####Echelle plot----
      
      #creation colone id_plot sur df pour l'exemple
      df%>%
        unite(id_plot, gradient, alti)%>%
        select(c(id_sample, id_plot)) -> colones_types
      
      #Fusion entre colone_types et le tableur de sortie 
      left_join(predatalphaM, colones_types, by="id_sample")%>%
        relocate(id_plot, .after = id_sample)->predataphaM
      
      #Moyennisage selon non pas id_sample mais selon id_plot
      predataphaM%>%
        group_by(id_plot)%>%
        summarise(ab = mean(ab),
                  mass= mean(mass),
                  q0=mean(q0),
                  q1=mean(q1),
                  q2=mean(q2),
                  Body_length=mean(Body_length))->predataphaplot
      
      #Sortie du df pour les plots 
      write.csv(predataphaplot, file = paste0("data/derived-data/Traits/predat/predatalphaplot_" , as.character(Sys.Date()) , ".csv"))


      ###By herbivores----
      herbi_tr <- traits %>% 
        filter(trait_name %in% c("Body_length","Habitat"))  
      herbi_ind  <- myIndices(DF = df[df$orderName %in% c("Orthoptera")|df$familyName=="Chrysomelidae",], 
                              IDresol = "Espèce", TR = herbi_tr)
      
        ####Echelle : sample---- 
      (herbi_ind$alpha)->herbialphaM
      
      write.csv(herbialphaM, file = paste0("data/derived-data/Traits/herbi/herbialphaM_" , as.character(Sys.Date()) , ".csv"))
      ####Echelle plot----
      
      #creation colone id_plot sur df pour l'exemple
      df%>%
        unite(id_plot, gradient, alti)%>%
        select(c(id_sample, id_plot)) -> colones_types
      
      #Fusion entre colone_types et le tableur de sortie 
      left_join(herbialphaM, colones_types, by="id_sample")%>%
        relocate(id_plot, .after = id_sample)->herbialphaM
      
      #Moyennisage selon non pas id_sample mais selon id_plot
      herbialphaM%>%
        group_by(id_plot)%>%
        summarise(ab = mean(ab),
                  mass= mean(mass),
                  q0=mean(q0),
                  q1=mean(q1),
                  q2=mean(q2),
                  Body_length=mean(Body_length))->herbialphaplot
      
      #Sortie du df pour les analyses
      write.csv(herbialphaplot, file = paste0("data/derived-data/Traits/herbi/herbialphaplot_" , as.character(Sys.Date()) , ".csv"))
      
      
      
     
      ###by Parasits
      #Je n ai pas trouvé de donnees traits sur des hymeno parasites


      

################
#Zone de travaux
################
PredatT%>%
        rename(Y=X)->test
      