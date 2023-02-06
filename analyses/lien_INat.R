# @title
# INat la Tania 
# 
# @description
# Identification des invertébrés dans les pièges barber de la Tania
# 
# @objectif
# Tester le gain d'utiliser INat pour identifier les invertébrés que je ne sais pas ID 
#
# @details
# 0. Interrogation de INaturalist pour récupérer les données d'occurrence


# Libraries
librarian::shelf(tidyr, dplyr, ggplot2, rinat, rgbif, "inbo/inborutils")

south_lat <- 45.405840
west_long <- 6.594312
north_lat <- 45.433315
east_long <- 6.618400 

bounds <- c(south_lat, west_long, north_lat, east_long)

ct <- get_inat_obs(taxon_name = "Animalia", bounds = bounds) %>%   
  filter(user_name == "Mike Hedde") %>%
  select(scientific_name, description) %>%
  rename(INat = description)

###
# A continuer: comparer le niveau d'identification gagné sachant qu'il faut ~1h par placette pour faire les photos et les charger sur INat

test <- readxl::read_xlsx("data/raw-data/test_inat_tania.xlsx") 

test2 <- test %>% 
  left_join(ct) %>%
  mutate(name = ifelse(is.na(scientific_name), `Valid Name`, scientific_name)) %>%
  gbif_species_name_match(name = "name") %>%
  select("id_sample", "Replicate number", "id_plot", "abundance", "stade" , "Valid Name",  "scientificName", "rank", "order", "class", "family", "genus")

ecosols <- length(unique(test2$`Valid Name`))
inat <- length(unique(test2$scientificName))

(inat-ecosols)/inat*100   # % augmentation du nb de taxon identifié

