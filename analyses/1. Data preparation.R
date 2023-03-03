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
librarian::shelf(tidyr, dplyr, ggplot2, rinat, RODBC, stringr)

# Database queries 
  ## Connection to Mike's Access database
      ### Set up driver info and database path
      DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
      MDBPATH <- "C:/Users/Hedde/Nextcloud/Hedde M/1. Travaux/AQR/0. Bases de Données/db communautés/fds_230228.accdb"
      #MDBPATH <- "C:/Users/heddemic/Nextcloud2/Hedde M/1. Travaux/AQR/0. Bases de Données/db communautés/fds_230228.accdb"  # Laptop
      PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
      
      ### Establish connection
      channel <- odbcDriverConnect(PATH)
      
      ### Load Orchamp data into R dataframe
      df0 <- sqlFetch(channel,"Orchamp_matrix")
      
      ### Close and remove channel
      close(channel)
      rm(channel)

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
  df <- df0 %>% 
    left_join(inat_orchamp, by = "INat") %>%
    mutate(name = ifelse(is.na(taxon.name), `Valid Name`, taxon.name)) 

  ## Getting valid and homogenized taxonomic names
    uniqueNames_raw <- tibble(name = unique(df$name)) %>% 
                              filter(!grepl("Larve", name), !grepl("vide", name))
    valid_names <- my_taxonChecker(uniqueNames_raw$name)  # Function stored at: "~/analyses/functions/my_taxonChecker function code"

  ## Merging the taxonomic backbone to the observations
    df1 <- left_join(df, valid_names, by= c("Valid Name" = "initial")) %>%
      select(!c(`Valid Name`, taxon.name, taxon.rank, name,  scientificName)) %>%
      mutate(INat = ifelse(is.na(INat), 0, 1)) %>%
      separate(id_sample, c("ANI", "yr", "nb"))%>%
      separate(id_plot, c("gradient", "alti"))
  
  ## Save dataset
    write.csv(df1, file = paste0("data/derived-data/clean_data_" , as.character(Sys.Date()) , ".csv"))

    