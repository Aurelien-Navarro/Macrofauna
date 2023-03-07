# @title
# IndComm
# 
# @description
# Calcul d'indices de communautés
# 
# @objectif
# Calculer des indices de communautés, globaux ou par taxon
#
# @details
# 0. Calcul de variables de communautés
#         - abondance, biomasse totale et par Grp
#         - diversité alpha taxo : 
##             - diversité équivalente pour q %in% 0, 1, 2
#         - diversité alpha fonctionnelle 
##             - définir une métrique fonctionnelle (body size ? trophic niche ?)


# Libraries
librarian::shelf(tidyverse, stringr, hillR, DarkDiv, FD, mFD, cati)

# Create a generic function with
      # DF = dataframe containing id_sample, canonic (taxon name), abundance and mass
      # TR = trait file from BETSI
      # IDresol = rank taxo at wich indices will be computed

myIndices <- function(DF, TR, IDresol){
  
# Data quality

    ## ID precision
    rankID <- DF %>% 
      select(abundance, stade, rankName) %>%
      filter(abundance>0) %>%
      group_by(rankName, stade) %>%
      summarize(rankNb = sum(abundance, na.omit = T)) %>%
      ungroup() %>%
      mutate(rankPrc = rankNb/sum(rankNb)*100) %>%
      arrange(desc(rankPrc))
    
    ## INat help
    INatID <- DF %>% 
      select(abundance, INat) %>%
      filter(abundance>0) %>%
      group_by(INat) %>%
      summarize(INatNb = sum(abundance, na.omit = T)) %>%
      mutate(INatPrc = INatNb/sum(INatNb)*100)

# Community parameters
    ## Ontogenic stages
    dvpStage <- DF %>% 
      select(id_sample, abundance, stade) %>%
      filter(abundance>0) %>%
      group_by(id_sample, stade) %>%
      summarize(stdNb = sum(abundance, na.omit = T)) %>%
      mutate(stdPrc = stdNb/sum(stdNb)*100) %>%
      pivot_wider(id_cols = id_sample, names_from = stade, 
                  values_from = stdPrc, values_fill = 0, values_fn = sum)

    ## Community abundance
    abTot <- DF %>% 
          select(id_sample, abundance) %>%
          group_by(id_sample) %>%
          summarize(ab = sum(abundance, na.omit = T))

    ## Community biomass
    massTot <- DF %>% 
          select(id_sample, mass) %>%
          group_by(id_sample) %>%
          summarize(mass = sum(mass, na.omit = T))

    ## Species mass
    indmass <- DF %>% 
      select(id_sample, canonic, mass, rankName) %>%
      filter(rankName == IDresol) %>%
      group_by(id_sample, canonic) %>%
      summarize(massMean = mean(mass, na.omit = T),
                massSD = sd(mass),
                massNb = length(mass))  
    #### cf cati, get "regional trait" (e.g. mass) by requiring Mike's database 
    funct<-c("mean(x, na.rm = TRUE)", "kurtosis(x, na.rm = TRUE)",
             "max(x, na.rm = TRUE) - min(x, na.rm = TRUE)" )
    par(mfrow = c(1,1))
    massDistri <- plotDistri(as.data.frame(indmass$massMean), rep("region", times = nrow(indmass)),
               indmass$canonic, plot.ask = F, multipanel = F)
    #sp_regional.ind<-ComIndex(traits = data.frame(massMean = indmass$massMean,
    #                                                 massMean0 = indmass$massMean), 
    #                          index = funct, 
    #                          sp = indmass$canonic,
    #                          nullmodels = "regional.ind", 
    #                          ind.plot = indmass$id_sample,
    #                          nperm = 9, print = FALSE)
    
# Diversity indices
    ## Alpha taxonomic diversity
    com <- DF %>% 
          select(id_sample, canonic, abundance, rankName) %>%
          filter(rankName == IDresol) %>%
          pivot_wider(id_cols = id_sample, names_from = canonic, names_sort = T,
                      values_from = abundance, values_fill = 0, values_fn = sum) 
        
        q0 <- hill_taxa(com[,-1], q = 0)
        q1 <- hill_taxa(com[,-1], q = 1)
        q2 <- hill_taxa(com[,-1], q = 2)
        
        alphaTaxo <- cbind(com[,1], q0, q1, q2)
        
    ## Dark diversity?     ddHyper <- DarkDiv::DarkDiv(x = "data", method = "Hypergeometric")
        
        ## Alpha functional diversity
          ### from mFD package
          ### need a trait matrix with vectors : 
              ###            canonic name, 
              ###            type (trait/strategy)
              ###            modality
              ###            value
          ### reco => use sqrt(Gower) instead of raw Gower distance to stdz PCoA axes
          ### Compute CWM, CWV, ... trait  FD::functcomp()
              TR1 <- TR %>%
                group_by(taxon_name, trait_name)%>%
                summarize(codedPrc = coded_trait_value/sum(coded_trait_value),
                          rawVal = mean(raw_trait_value, na.omit = T))
                
              # à finir pour le fuzzy coding
                            
              tr0 <- TR %>%
                 mutate(canonic = gn_parse_tidy(taxon_name)$canonicalsimple) %>%
                 filter(canonic %in% colnames(com)[-1]) %>%
                 select(canonic, trait_name, attribute_trait, raw_trait_value, coded_trait_value)  %>%
                 mutate(val = as.numeric(ifelse(is.na(coded_trait_value), 
                                               raw_trait_value, 
                                               coded_trait_value))) %>%
                      #mutate(trait_name = gsub("\_","",trait_name)) %>%
                      #mutate(trait_name = abbreviate(trait_name, 3, named = F)) %>%
                 mutate(new_trait_name = ifelse(!is.na(attribute_trait), 
                                                  paste(trait_name, "_", attribute_trait, sep = ""), 
                                                  trait_name)) %>%
                 pivot_wider(id_cols = canonic, names_from = new_trait_name, 
                            values_from = val, values_fn = mean)
               
               tr_completeness <- DF %>% 
                 filter(rankName == IDresol) %>%
                 select(canonic) %>%
                 distinct() %>%
                 left_join(tr0) %>%
                 mutate(across(c(2:ncol(.)), ~ifelse(is.na(.), 0, 1)))
               tr <- tr0 %>%
                 column_to_rownames(var = "canonic")
               com1 <- com[,-1] %>%
                 select(rownames(tr))
               CWM <- cbind(com[,1], functcomp(as.matrix(tr), as.matrix(com1)))
              ### Compute Gower distance between sp in the trait space : mFD::funct.dist()
              ### Compute functional diversity indices:  mFD::alpha.fd.hill(asb_sp_w = "abundance", sp_dist  = "gower dist", tau = "mean", q = 1)  # q for Hill number 
             
        
# Releasing all indices
alpha <- tibble(id_sample = unique(DF$id_sample)) %>%
  left_join(abTot) %>%
  left_join(massTot) %>%
  left_join(alphaTaxo) %>%
  left_join(CWM)
res <- list(rankID = rankID, iNatID = INatID, dvpStage = dvpStage,
            alpha = alpha, indmass = indmass, 
            massDistri = massDistri, 
            tr_completeness = tr_completeness)
res
}
