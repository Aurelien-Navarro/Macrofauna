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
  ## Alternative: importing data from a csv file
  #df0 <- read.csv("data/raw-data/Macrofaune_Orchamp_2021_2022.csv", h=T, sep = ";")
  
  ## Better way : Connection to Mike's Access database
      ### Set up driver info and database path
      #DRIVERINFO <- "driver={CData ODBC Driver for Microsoft Access};"
      DRIVERINFO <- "driver={Microsoft Access Driver (*.mdb, *.accdb)};"
      MDBPATH <- "data/raw-data/fds_230315.accdb"
      PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
      
      ### Establish connection
      
      channel <- odbcDriverConnect(PATH)
      sqlTables(channel)
      
      
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
    notINat <- df0 %>% 
      filter(!grepl("-", INat))
    df <- df0 %>% 
    filter(grepl("-", INat)) %>%
    left_join(inat_orchamp, by = "INat", relationship = "many-to-many") %>%
    mutate(name = ifelse(is.na(taxon.name), `Valid Name`, taxon.name)) %>%
    bind_rows(notINat)


  ## Getting valid and homogenized taxonomic names
    uniqueNames_raw <- tibble(name = unique(df$name)) %>% 
                              filter(!grepl("Larv", name), !grepl("vide", name))
    #Importation de my taxon checker function 
    source("analyses/functions/my_taxonChecker function code.R")
    
    #####la fonction####
    # @title
    # my_taxonChecker 
    # 
    # @description
    # Function to homogenize taxonomies used in different datasets
    # 
    # @objectif
    # Getting a valid list of taxa and of their higher rank levels
    #
    # @details
    # Analyse a vector containing taxonomic names (V) 
    # 1/ check regional (French) taxonomy using TaxRef
    # 2/ complete lacks with a global taxonomy (gbif) 
    
    
    my_taxonChecker <- function(V) {
      
      librarian::shelf(tidyr, dplyr, rgbif, "Rekyt/rtaxref", "inbo/inborutils", "ropensci/rgnparser", stringr, forcats)
      
      
      # parse taxon name to clean the list to obtain the most simple canonical name
      uniqueNames00 <- V %>% 
        #drop_na() %>%
        unique()
      uniqueNames0 <- gn_parse_tidy(uniqueNames00)$canonicalsimple
      uniqueNames0 <- unique(uniqueNames0)
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
        rename(unalign = canonic) %>%
        drop_na()
      
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
        mutate(rankName  = fct_recode(rankName, "Espèce" = "SPECIES", "Genre" = "GENUS", "Famille" = "FAMILY", "Ordre" = "ORDER"))
      
      taxo_harmo <- taxo_harmo %>%
        filter(!is.na(id)) 
      taxo_harmo_def <- bind_rows(list(taxref = taxo_harmo, gbif = gbif_align), .id = "referential") %>%
        distinct()
      taxo_harmo_def
    }
    #####
    valid_names <- my_taxonChecker(uniqueNames_raw$name)  # Function stored at: "~/analyses/functions/my_taxonChecker function code"

  ## Merging the taxonomic backbone to the observations
    df1 <- left_join(df, valid_names, by= c("Valid Name" = "initial")) %>%
      select(!c(taxon.name, taxon.rank, name,  scientificName)) %>%
      mutate(INat = ifelse(is.na(INat), 0, 1)) %>%
      separate(id_plot, c("gradient", "alti"))
  
  ## Save dataset
    write.csv(df1, file = paste0("data/derived-data/clean_data_" , as.character(Sys.Date()) , ".csv"))

    