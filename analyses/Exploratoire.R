#####Analyses Exploratoires#####
################################

#########################################################
#I- DATA PREPARATION########
# @title
# Data preparation 
# 
# @description
# Préparation du jeu de données à traiter
# 
# @objectif
# Fusionner les jeux de données, homogénéiser la taxo, préparer les colonnes d'intérêt 
#
# @details
# 0. Interrogation de la base de données Eco&Sols et de INaturalist pour récupérer les données d'occurrence
# 1. Fusion des données et homogénéisation taxonomique
# 2. Export d'un fichier de données propres


# Libraries
librarian::shelf(tidyr, dplyr, ggplot2, rinat, RODBC, stringr)

#rgnparser::install_gnparser()
library(rgnparser)

#Importation de la bdd
read.csv("data/raw-data/Macrofaune_Orchamp_2021_2022.csv", header=T,row.names=NULL, sep=";")->AccessDB
str(AccessDB)

## Connection to Orchamp_global project on INaturalist
### Getting data
inat_orchamp <- get_inat_obs_project(158034, type="observations",raw=T)  # project_id=158034  pour Orchamp_global

### Data refining
inat_orchamp <- inat_orchamp %>%
  as_tibble() %>%
  select(taxon.name, taxon.rank, description) %>%
  rename(INat = description) %>%
  filter(!is.na(INat))

# Data preparation
## Merging Mike's team and INat identifications
df <- AccessDB %>% 
  left_join(inat_orchamp, by = "INat") %>%
  mutate(name = ifelse(is.na(taxon.name), `Valid.Name`, taxon.name)) 
#####
my_taxonChecker <- function(V) {
  
  librarian::shelf(tidyr, dplyr, rgbif, "Rekyt/rtaxref", "inbo/inborutils", "ropensci/rgnparser", stringr, forcats)
  
  
  # parse taxon name to clean the list to obtain the most simple canonical name
  uniqueNames00 <- unique(V)
  uniqueNames0 <- gn_parse_tidy(uniqueNames00)$canonicalsimple
  uniqueNames0 <- unique(V)
  nb_unique <- length(uniqueNames0)  
  harmo_colnames <- c("initial", "canonic", "id", "rankName", "referenceName", 
                      "familyName", "orderName", "className", "phylumName")
  
  # create an empty dataframe to store TaxRef taxonomy
  taxo_harmo <- as.data.frame(matrix(data=NA, 
                                     nrow = nb_unique, 
                                     ncol = 9,
                                     dimnames = list(1:nb_unique, harmo_colnames)))
  taxo_harmo$initial <- uniqueNames0
  taxo_harmo$canonic <- ifelse(is.na(word(uniqueNames0, 1, 2)), 
                               uniqueNames0, 
                               word(uniqueNames0, 1, 2))  # keep only the canonical binomial name (no subsp) 
  
  
  
  # function to retrieve taxonomy from TaxRef to each canonical name
  taxref_chk <- function(X){
    chk0 <- rt_taxa_search(sciname = X) 
    R <- str_count(X ,"\\W+") + 1  # is a species or a higher taxonomic level (2 => species)
    ifelse(ncol(chk0) == 1,
           chk <- rep(NA, 7),
           ifelse(R >= 2,
                  ifelse(!rankName %in% "Espèce",
                         chk <- rep(NA, 7),
                         chk <- chk0 %>%
                           filter(rankName == "Espèce") %>%
                           select(id, rankName, referenceName, familyName, orderName, className, phylumName),
                         chk <- chk0 %>%
                           filter(! rankName %in% c("Espèce", "Sous-Espèce", "Variété")) %>%
                           filter(scientificName == X) %>%
                           select(id, rankName, referenceName, familyName, orderName, className, phylumName)
                  )
           ))
    chk
  }
  
  
  taxref_chk <- function(X){
    chk0 <- rt_taxa_search(sciname = X) 
    ifelse(ncol(chk0) == 1,
           chk <- rep(NA, 7),
           ifelse(nrow(chk0) == 1,
                  chk <- chk0 %>%
                    select(id, rankName, referenceName, familyName, orderName, className, phylumName),
                  chk <- rep(NA, 7)
           )
    )
    chk
  }
  
  
  
  # loop to attribute TaxRef taxo to all canonical names
  for(i in 1:nb_unique){
    taxo_harmo[i,3:9] <- taxref_chk(X = taxo_harmo$canonic[i])
  }
  
  # look to taxa that are not present in TaxRef
  taxo_unaligned <- taxo_harmo %>%
    filter(is.na(id)) %>%
    select(initial, canonic) %>%
    rename(unalign = canonic)
  
  # Find unaligned taxa in a global taxonomic backbone (gbif)
  gbif_align <- gbif_species_name_match(taxo_unaligned, "unalign") %>%
    rename(rankName = rank,
           referenceName = scientificName,
           familyName = family,
           orderName = order,
           className = class,
           phylumName = phylum,
           scientificName = unalign, 
           id = usageKey) %>%
    select(!c("kingdom", "genus", "confidence", "synonym", "status","matchType" ))%>%
    mutate(rankName  = fct_recode(rankName, "Espèce" = "SPECIES", "Genre" = "GENUS", "Famille" = "FAMILY"))
  
  taxo_harmo <- taxo_harmo %>%
    filter(!is.na(id)) 
  taxo_harmo_def <- bind_rows(list(taxref = taxo_harmo, gbif = gbif_align), .id = "referential") %>%
    distinct()
  taxo_harmo_def
}
######

## Getting valid and homogenized taxonomic names
uniqueNames_raw <- tibble(name = unique(df$name)) %>% 
  filter(!grepl("Larve", name), !grepl("vide", name))
valid_names <- my_taxonChecker(uniqueNames_raw$name)  # Function stored at: "~/analyses/functions/my_taxonChecker function code"

## Merging the taxonomic backbone to the observations
df1 <- left_join(df, valid_names, by= c("Valid.Name" = "initial")) %>%
  select(!c(`Valid.Name`, taxon.name, taxon.rank, name,  scientificName)) %>%
  mutate(INat = ifelse(is.na(INat), 0, 1)) %>%
  separate(id_plot, c("gradient", "alti"))

## Save dataset
write.csv(df1, file = paste0("data/derived-data/clean_data_" , as.character(Sys.Date()) , ".csv"))

################################################################################
################################################################################

#II- Indices computations#######

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
df_clean <- read.csv("data/derived-data/clean_data_2023-04-20.csv", 
               h = T, sep = ",") 
df_clean$rankName <-  fct_recode(df_clean$rankName, "Famille" = "Sous-Famille",
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



