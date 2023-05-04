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
    df <- read.csv("data/derived-data/Esp/clean_data_2023-04-25.csv", 
                   h = T, sep = ",") 
    df$rankName <-  fct_recode(df$rankName, "Famille" = "Sous-Famille",
                               "Famille" = "Super-Famille",
                               "Classe" = "Infra-Classe",
                               "Genre" = "Sous-Genre",
                               "Ordre" = "Sous-Ordre", 
                               "Phylum" = "Sous-Phylum")
  
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
  
    
## Selection of trait(s) of interest
    source("analyses/functions/IndCom.R")

    
#Importation de traits homogénéisés : 
    read.csv("data/derived-data/Traits/traits_homo_2023-04-27.csv", header=T, sep=",")->traits
# Indice computation
    ## By taxonomic group
    lumbricid_tr <- traits %>% 
                       filter(trait_name %in% c("Body_length", "Habitat", "ecological_strategy"))
    lumbricid_ind <- myIndices(DF = df[df$orderName == "Crassiclitellata",], 
                     IDresol = "Espèce", TR = lumbricid_tr)
    lumbricid_ind$alpha
      ###Carabidae
    carab_tr<- traits%>%
                    filter(trait_name %in% c("Body_length", "Habitat", "ecological_strategy"))
    carab_ind <- myIndices(DF = df[df$familyName == "Carabidae",], 
                               IDresol = "Espèce", TR = carab_tr)
    carab_ind$alpha
      
    
    ## By Guild 
      ###(e.g. detritivores)
    detritivore_tr <- traits %>% 
                     filter(trait_name %in% c("Body_length", "Habitat"))  
    detritivore_ind  <- myIndices(DF = df[df$className %in% c("Diplopoda", "Isopoda", "Clitellata"),], 
                     IDresol = "Espèce", TR = detritivore_tr)
    view(detritivore_ind$alpha)
    detritivore_ind$indmass
    detritivore_ind$rankID

    ## By trait
    BL <- filter(traits, trait_name %in% c("Body_length", "Habitat"))  
    BL_ind  <- myIndices(DF = df, IDresol = "Espèce", TR = BL) 
    BL_ind$alpha

    
    ##representation abondance detritivores   
wrkdataset <- df[, c(2, 4:6)] %>%
  left_join(detritivore_ind$alpha) %>%
  filter(method %in% c("barber")) %>%
  replace(is.na(.), 0) %>%
  group_by(gradient, alti) %>%
  summarise(meanAb = mean(ab))


ggplot(wrkdataset, aes(x=as.numeric(alti), y=meanAb, color= gradient))+
  geom_line()+
  facet_wrap(.~gradient, scales = "free_y")+
  theme_bw()

#######################
#ZONE DE TRAVAUX------
######################
(detritivore_ind$alpha)->detrialphaM

write.csv(detrialphaM, file = paste0("data/derived-data/Traits/detriti/detrialphaM_" , as.character(Sys.Date()) , ".csv"))
#Maintenant on va essayer de passer du code ANI (detritvore_ind$alpha) au code des plots en moyennisant les traits. 
#Nécessaire pour les prochaines analyses 

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
 write.csv(detrialphaplot, file = paste0("data/derived-data/Traits/detriti/detrialphaplot_" , as.character(Sys.Date()) , ".csv"))
 