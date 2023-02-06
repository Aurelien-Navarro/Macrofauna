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
librarian::shelf(tidyr, dplyr, ggplot2, rinat)

south_lat <- 45.405840
west_long <- 6.594312
north_lat <- 45.433315
east_long <- 6.618400 

bounds <- c(south_lat, west_long, north_lat, east_long)

ct <- get_inat_obs(taxon_name = "Animalia", bounds = bounds) %>%   
  filter(user_name == "Mike Hedde") %>%
  select(scientific_name, description)

###
# A continuer: comparer le niveau d'identification gagné sachant qu'il faut ~1h par placette pour faire les photos et les charger sur INat
