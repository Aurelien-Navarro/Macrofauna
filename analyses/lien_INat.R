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
librarian::shelf(tidyr, dplyr, ggplot2, rinat, rgbif, "Rekyt/rtaxref", "inbo/inborutils", RODBC)

# Connection to Mike's Access database
    ## Set up driver info and database path
    DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
    MDBPATH <- "C:/Users/Hedde/Nextcloud/Hedde M/1. Travaux/AQR/0. Bases de Données/db communautés/fds_230228.accdb"
    PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
    
    ## Establish connection
    channel <- odbcDriverConnect(PATH)
    
    ## Load Orchamp data into R dataframe
    df <- sqlFetch(channel,"Orchamp_matrix")
    
    ## Close and remove channel
    close(channel)
    rm(channel)

# Connection to Orchamp_global project on INaturalist
    ## Getting data
    inat_orchamp <- get_inat_obs_project(158034, type="observations",raw=T)  # project_id=158034  pour Orchamp_global

    # Data refining
    inat_orchamp <- inat_orchamp %>%
      as_tibble() %>%
      select(taxon.name, taxon.rank, description) %>%
      rename(INat = description) %>%
      filter(!is.na(INat))

# Merging Mike's team and INat identifications

test <- df %>% 
  left_join(inat_orchamp, by = "INat") %>%
  mutate(name = ifelse(is.na(taxon.name), `Valid Name`, taxon.name)) %>%
  gbif_species_name_match(name = "name") %>%
  select("id_sample", "Replicate number", "id_plot", "abundance", "stade" , "Valid Name",  "scientificName", "rank", "order", "class", "family", "genus")


test2 <- df %>% 
  left_join(inat_orchamp, by = "INat") %>%
  mutate(name = ifelse(is.na(taxon.name), `Valid Name`, taxon.name)) 

  taxo_harmo <- as.data.frame(matrix(data=NA, nrow = length(names_to_chk), ncol = 8,
                       dimnames = list(1:length(names_to_chk),
                                       c("scientificName", "id", "rankName", "referenceName", "familyName", "orderName", "className", "phylumName"))))
  taxo_harmo$scientificName <- unique(test2$name)
  
  taxref_chk <- function(X){
    chk <- rt_taxa_search(sciname = X) %>%
      select(id, rankName, referenceName, familyName, orderName, className, phylumName)
    chk
  }
  
  for(i in 1:nrow(taxo_harmo)){
  taxo_harmo[i,2:8] <- taxref_chk(taxo_harmo$scientificName[i])
  }

  