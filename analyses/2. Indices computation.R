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
                   h = T, sep = ";") 
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
    ## By taxonomic group
    lumbricid_tr <- traits %>% 
                       filter(trait_name %in% c("Body_length", "Habitat", "ecological_strategy"))
    lumbricid_ind <- myIndices(DF = df[df$orderName == "Crassiclitellata",], 
                     IDresol = "Espece", TR = lumbricid_tr)
    
    ## By Guild (e.g. detritivores)
    detritivore_tr <- traits %>% 
                     filter(trait_name %in% c("Body_length", "Habitat"))  
    detritivore_ind  <- myIndices(DF = df[df$className %in% c("Diplopoda", "Isopoda", "Clitellata"),], 
                     IDresol = "Espece", TR = detritivore_tr)

    ## By trait
    BL <- filter(traits, trait_name %in% c("Body_length", "Habitat"))  
    BL_ind  <- myIndices(DF = df, IDresol = "Espece", TR = BL)  
    
wrkdataset <- df[, c(1, 3:5)] %>%
  left_join(lumbricid_ind$alpha) %>%
  filter(method %in% c("tri manuel")) %>%
  replace(is.na(.), 0) %>%
  group_by(gradient, alti) %>%
  summarise(meanMass = mean(mass))


ggplot(wrkdataset, aes(x=as.numeric(alti), y=as.numeric(meanMass), color= gradient))+
  geom_point()+
  #facet_wrap(.~gradient, scales = "free_y")+
  theme_bw()
    