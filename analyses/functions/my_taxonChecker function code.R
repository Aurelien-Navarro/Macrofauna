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
  uniqueNames0 <- gn_parse_tidy(V)$canonicalsimple
  nb_unique <- length(uniqueNames0)  
  harmo_colnames <- c("initial", "canonic", "id", "rankName", "referenceName", 
                      "familyName", "orderName", "className", "phylumName")
  
  # create an empty dataframe to store TaxRef taxonomy
  taxo_harmo <- as.data.frame(matrix(data=NA, 
                                     nrow = nb_unique, 
                                     ncol = 9,
                                     dimnames = list(1:nb_unique, harmo_colnames)))
  taxo_harmo$initial <- V
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
                  chk <- chk0 %>%
                    filter(rankName == "Espèce") %>%
                    select(id, rankName, referenceName, familyName, orderName, className, phylumName),
                  chk <- chk0 %>%
                    filter(! rankName %in% c("Espèce", "Sous-Espèce", "Variété")) %>%
                    filter(scientificName == X) %>%
                    select(id, rankName, referenceName, familyName, orderName, className, phylumName)
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