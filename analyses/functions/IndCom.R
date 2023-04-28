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
librarian::shelf(dplyr, forcats, stringr, hillR, FD, mFD, rgnparser, textshape, tidyr)

# Create a generic function with
      # DF = dataframe containing id_sample, name (taxon name), abundance and mass
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
          filter(!is.na(mass)) %>%
          group_by(id_sample) %>%
          summarize(mass = sum(as.numeric(mass), na.omit = T))

    ## Species mass
    indmass <- DF %>% 
      select(id_sample, name, mass, rankName) %>%
      filter(rankName == IDresol) %>%
      group_by(id_sample, name) %>%
      summarize(massMean = mean(as.numeric(mass), na.omit = T),
                massSD = sd(mass),
                massNb = length(mass))  
    #### cf cati, get "regional trait" (e.g. mass) by requiring Mike's database 
    funct<-c("mean(x, na.rm = TRUE)", "kurtosis(x, na.rm = TRUE)",
             "max(x, na.rm = TRUE) - min(x, na.rm = TRUE)" )
    par(mfrow = c(1,1))
    #massDistri <- plotDistri(as.data.frame(indmass$massMean), rep("region", times = nrow(indmass)),
    #           indmass$name, plot.ask = F, multipanel = F)
    #sp_regional.ind<-ComIndex(traits = data.frame(massMean = indmass$massMean,
    #                                                 massMean0 = indmass$massMean), 
    #                          index = funct, 
    #                          sp = indmass$name,
    #                          nullmodels = "regional.ind", 
    #                          ind.plot = indmass$id_sample,
    #                          nperm = 9, print = FALSE)
    
# Diversity indices
    ## Alpha taxonomic diversity
    com <- DF %>% 
          select(id_sample, name, abundance, rankName) %>%
          filter(rankName == IDresol) %>%
          pivot_wider(id_cols = id_sample, names_from = name, names_sort = T,
                      values_from = abundance, values_fill = 0, values_fn = sum) 
        
        q0 <- hill_taxa(com[,-1], q = 0)
        q1 <- hill_taxa(com[,-1], q = 1)
        q2 <- hill_taxa(com[,-1], q = 2)
        
        alphaHill <- cbind(com[,1], q0, q1, q2)
        
    ## Dark diversity?     ddHyper <- DarkDiv::DarkDiv(x = "data", method = "Hypergeometric")
        
        ## Alpha functional diversity
          ### from mFD package
          ### need a trait matrix with vectors : 
              ###            name name, 
              ###            type (trait/strategy)
              ###            modality
              ###            value
          ### reco => use sqrt(Gower) instead of raw Gower distance to stdz PCoA axes
          ### Compute CWM, CWV, ... trait  FD::functcomp()
             tr0 <- TR %>%
                mutate(name = gn_parse_tidy(taxon_name)$canonicalsimple) %>%
                filter(name %in% colnames(com)[-1]) %>%
                select(name, trait_name, raw_trait_value, attribute_trait, coded_trait_value)
              
              TR_cont <- tr0 %>%
                    filter(is.na(coded_trait_value)) %>%
                    group_by(name, trait_name) %>%
                    summarise(val = mean(as.numeric(raw_trait_value), na.omit = T)) %>%
                    pivot_wider(id_cols = name, names_from = trait_name, 
                                values_from = val)
              
              TR_discr0 <- tr0  %>%
                filter(!is.na(coded_trait_value)) %>%
                select(!raw_trait_value) %>%
                group_by(name, trait_name, attribute_trait)%>%
                summarise(sum_aff = sum(coded_trait_value)) %>%
                group_by(name, trait_name)%>%
                mutate(codedPrc = sum_aff/sum(sum_aff),
                       new_trait_name = paste(trait_name, attribute_trait, sep=""))
              
              shannon.entropy <- function(p) {
                  if (min(p) < 0 || sum(p) <= 0)
                    return(NA)
                  p.norm <- p[p>0]/sum(p)
                  -sum(log2(p.norm)*p.norm)
                }
                
              TR_discr_H <- TR_discr0 %>%
                mutate(trait_name = paste(trait_name, "H", sep = "")) %>%
                group_by(name, trait_name)%>%
                summarize(H = shannon.entropy(codedPrc)) %>%
                pivot_wider(id_cols = name, names_from = trait_name, 
                            values_from = H, values_fill = 0) 
                
                
              tr1 <- TR_discr0  %>%
                  pivot_wider(id_cols = name, names_from = new_trait_name, 
                            values_from = codedPrc, values_fill = 0) %>%
                  full_join(TR_discr_H) %>%
                  full_join(TR_cont) 
               
               tr_completeness <- DF %>% 
                 filter(rankName == IDresol) %>%
                 select(name) %>%
                 distinct() %>%
                 left_join(tr1) %>%
                 mutate(across(c(2:ncol(.)), ~ifelse(. == 0, 0, 1)))
               
               tr2 <- tr1 %>%
                 column_to_rownames("name")
               com1 <- com[,-1] %>%
                 select(rownames(tr2))
               CWM <- cbind(com[,1], functcomp(as.matrix(tr2), as.matrix(com1)))
              ### Compute Gower distance between sp in the trait space : mFD::funct.dist()
              ### Compute functional diversity indices:  mFD::alpha.fd.hill(asb_sp_w = "abundance", sp_dist  = "gower dist", tau = "mean", q = 1)  # q for Hill number 
             
        
# Releasing all indices
alpha <- tibble(id_sample = unique(DF$id_sample)) %>%
  left_join(abTot) %>%
  left_join(massTot) %>%
  left_join(alphaHill) %>%
  left_join(CWM)
res <- list(alpha = alpha, indmass = indmass, 
            rankID = rankID, iNatID = INatID, dvpStage = dvpStage,
            #massDistri = massDistri, 
            tr_completeness = tr_completeness)
res
}
