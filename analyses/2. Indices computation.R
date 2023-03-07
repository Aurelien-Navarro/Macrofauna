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
librarian::shelf(dplyr, forcats, stringr)

# Data load
  ## Community data load 
    df <- read.csv("data/derived-data/clean_data_2023-03-07.csv", 
                   h = T, sep = ",") %>%
            select(!c(1))
    df$rankName <-  fct_recode(df$rankName, "Famille" = "Sous-Famille",
                               "Famille" = "Super-Famille",
                               "Classe" = "Infra-Classe",
                               "Genre" = "Sous-Genre",
                               "Ordre" = "Sous-Ordre", 
                               "Phylum" = "Sous-Phylum")
  ## Species trait data load 
    traits <- read.csv("data/raw-data/BETSI_220221.csv", h = T, sep = ";")
    # if taxonomic homogenization needed (/|\ take hours !!)
    #traits <- traits %>%
    #          filter(Taxa %in% c("Arachnida", "Coleoptera", "Dermaptera", "Diplopoda", "Gastropoda",
    #                             "Isopoda", "Oligochaeta", "Orthoptera"))
    #trait_taxa_correct0 <- my_taxonChecker(traits$taxon_name)
    #trait_taxa_correct <- trait_taxa_correct0 %>%
    #                      mutate(canonic = ifelse(is.na(canonic) == T, scientificName, canonic))
    #traits <- left_join(traits, trait_taxa_correct0) 
  
  ## Selection of trait(s) of interest
              
# Indice computation
    lumbricid_traits <- traits %>% 
                       filter(trait_name %in% c("Body_length", "Habitat", "ecological_strategy"))
    lumbricid_ind <- myIndices(DF = df[df$orderName == "Crassiclitellata",], 
                     IDresol = "Espèce", TR = lumbricid_traits)
    
    diplopoda_traits <- traits %>% 
                     filter(trait_name %in% c("Body_length", "Habitat", "Humidity_preference"))  
    diplopoda_ind  <- myIndices(DF = df[df$className == "Diplopoda",], 
                     IDresol = "Espèce", TR = diplopoda_traits)

    arachnida_traits <- traits %>% 
                             filter(trait_name %in% c("Body_length", "Habitat", "Motion_strategies"))  
    arachnida_ind  <- myIndices(DF = df[df$className == "Arachnida",], 
                                IDresol = "Espèce", TR = arachnida_traits)

    
    
    