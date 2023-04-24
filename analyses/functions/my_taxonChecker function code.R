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
  harmo_colnames <- c("initial", "canonic", "id", "rankName", "fullName", 
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
                    select(id, rankName, fullName, familyName, orderName, className, phylumName),
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
  gbif_align0 <- gbif_species_name_match(taxo_unaligned, "unalign") 
  gbif_align <- gbif_align0 %>% 
    rename(rankName = rank,
           fullName = scientificName,
           familyName = family,
           orderName = order,
           className = class,
           phylumName = phylum,
           scientificName = unalign, 
           id = usageKey) %>%
    select(!c("kingdom", "genus", "confidence", "synonym", "status","matchType" ))%>%
    mutate(rankName  = fct_recode(rankName, "Espèce" = "SPECIES", "Genre" = "GENUS", 
                                  "Famille" = "FAMILY", "Ordre" = "ORDER", "Classe" = "CLASS"))
  
  taxo_harmo <- taxo_harmo %>%
    filter(!is.na(id)) 
  taxo_harmo_def <- bind_rows(list(taxref = taxo_harmo, gbif = gbif_align), .id = "referential") %>%
    distinct()
  taxo_harmo_def
}
#####