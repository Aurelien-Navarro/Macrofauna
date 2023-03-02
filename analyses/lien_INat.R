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
librarian::shelf(tidyr, dplyr, ggplot2, rinat, rgbif, "Rekyt/rtaxref", "inbo/inborutils", "ropensci/rgnparser", RODBC)

# Connection to Mike's Access database
    ## Set up driver info and database path
    DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
    MDBPATH <- "C:/Users/Hedde/Nextcloud/Hedde M/1. Travaux/AQR/0. Bases de Données/db communautés/fds_230228.accdb"
    MDBPATH <- "C:/Users/heddemic/Nextcloud2/Hedde M/1. Travaux/AQR/0. Bases de Données/db communautés/fds_230228.accdb"
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

# taxref fields to keep
fields_to_keep <- c("scientificName", "id", "rankName", "referenceName", 
                    "familyName", "orderName", "className", "phylumName")

# unique taxon names
uniqueNames_raw <- tibble(name = unique(test2$name)) %>% 
                          filter(!grepl("Larve", name))

# parse taxon name to clean the list to obtain the most simple canonical name
uniqueNames <- gn_parse_tidy(uniqueNames_raw$name)$canonicalsimple
nb_unique <- length(uniqueNames)

# create empty dataframe to store taxref taxonomy
taxo_harmo <- as.data.frame(matrix(data=NA, 
                                   nrow = nb_unique, 
                                   ncol = 8,
                                   dimnames = list(1:nb_unique, fields_to_keep)))
taxo_harmo$scientificName <- uniqueNames

# function to retrieve taxonomy from taxref to each canonical name
taxref_chk <- function(X){
  chk0 <- rt_taxa_search(sciname = X) 
  ifelse(ncol(chk0) > 1,
         chk <- chk0 %>%
                 arrange(parentId) %>%
                 filter(row_number()==1) %>%
                 select(id, rankName, referenceName, familyName, orderName, className, phylumName),
         chk <- rep(NA, 7))
  chk
}

# change Scarabaeoidae"=> "Scarabaeidae
taxo_harmo <- taxo_harmo %>%
  mutate(scientificName = ifelse(scientificName == "Scarabaeoidae", "Scarabaeidae", scientificName))

# loop to attribute taxref taxo to all canonical names
for(i in 1:nrow(taxo_harmo)){
  taxo_harmo[i,2:8] <- taxref_chk(taxo_harmo$scientificName[i])
  }


taxref_chk("Auchenorrhyncha")
rt_taxa_search(sciname = "Aporrectodea giardi giardi")
