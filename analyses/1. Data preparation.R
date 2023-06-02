# @title
# Data preparation 
# 
# @description
# Préparation du jeu de données à traite
# 
# @objectif
# Fusionner les jeux de données, homogénéiser la taxo, préparer les colonnes d'intérêt 
#
# @details
# 0. Interrogation de la base de données Eco&Sols et de INaturalist pour récupérer les données d'occurrence
# 1. Fusion des données et homogénéisation taxonomique
# 2. Export d'un fichier de données propres


# Libraries
librarian::shelf(tidyr, dplyr, ggplot2, rinat, RODBC, stringr, rgnparser)

# Database queries 
<<<<<<< HEAD
  ## Alternative: importing data from a csv file
  #df0 <- read.csv("data/raw-data/Macrofaune_Orchamp_2021_2022.csv", h=T, sep = ";")
  
  ## Better way : Connection to Mike's Access database
      ### Set up driver info and database path
      DRIVERINFO <- "driver={Microsoft Access Driver (*.mdb, *.accdb)};"
      MDBPATH <- "data/raw-data/fds_230425.accdb"
      PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
      
      ### Establish connection
      
      channel <- odbcDriverConnect(PATH)
      sqlTables(channel)
      
      
      ### Load Orchamp data into R dataframe
      
=======
## Alternative: importing data from a csv file
#df0 <- read.csv("data/raw-data/Macrofaune_Orchamp_2021_2022.csv", h=T, sep = ";")

## Better way : Connection to Mike's Access database
### Set up driver info and database path
#DRIVERINFO <- "driver={CData ODBC Driver for Microsoft Access};"
DRIVERINFO <- "driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "data/raw-data/fds_230530.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

### Establish connection

channel <- odbcDriverConnect(PATH)
sqlTables(channel)


### Load Orchamp data into R dataframe


df0 <- sqlFetch(channel,"Orchamp_matrix")
df0 <- rename(df0, Valid.Name="Valid Name")

### Close and remove channel
close(channel)
rm(channel)

## Connection to Orchamp_global project on INaturalist
### Getting data
inat_orchamp0 <- get_inat_obs_project(158034, type="observations",raw=T)  # project_id=158034  pour Orchamp_global

### Data refining
inat_orchamp <- inat_orchamp0 %>%
  as_tibble() %>%
  select(taxon.name, description) %>%
  rename(INat = description) %>%
  filter(!is.na(INat))

#Modifications about old classificaton
sub('Philonthina', 'Philonthus', inat_orchamp$taxon.name)->inat_orchamp$taxon.name

>>>>>>> 115dcdd95c2f72e695a85daf16eb70556342f60c


# Data preparation
## Merging Mike's team and INat identifications
notINat <- df0 %>% 
  filter(!grepl("-", INat))%>%
  mutate(taxon.name = gn_parse_tidy(Valid.Name)$canonicalsimple)

df <- df0 %>% 
  filter(grepl("-", INat)) %>%
  left_join(inat_orchamp, by = "INat", relationship = "many-to-many") %>%
  bind_rows(notINat) %>%
  mutate(name = ifelse(is.na(taxon.name) == T, Valid.Name, taxon.name))


## Getting valid and homogenized taxonomic names
uniqueNames_raw <- tibble(name = unique(df$name))

#Importation de my taxon checker function 
source("analyses/functions/my_taxonChecker function code.R")
valid_names <- my_taxonChecker(uniqueNames_raw$name)  

## Merging the taxonomic backbone to the observations
df1 <- left_join(df, valid_names, by = c("taxon.name" = "initial")) %>%
  select(!c(canonic, Valid.Name, taxon.name, scientificName)) %>%
  mutate(INat = ifelse(is.na(INat), 0, 1)) %>%
  separate(id_plot, c("gradient", "alti"))

## Save dataset
write.csv(df1, file = paste0("data/derived-data/Esp/clean_data_" , as.character(Sys.Date()) , ".csv"))
