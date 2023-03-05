# @title
# IndComm sur données Orchamp
# 
# @description
# Calcul d'indices de communautés sur les données Orchamp
# 
# @objectif
# Calculer des indices de communautés, globaux ou par taxon
#
# @details
# 0. Import des données d'identification (fusion ID Eco&Sols et ID INat)
#         - définir des groupes (Grp) d'intérêt
##             - détritivores : vdt, diplo, iso 
##             - prédateurs : carabiques, araignées, ...(?)
##             - herbivores : criquets, ... (?)
# 1. Calcul de variables de communautés


# Libraries
librarian::shelf(tidyverse)

# Data load 
df <- read.csv("data/derived-data/clean_data_2023-03-05.csv", 
               h = T, sep = ",") %>%
        select(!c(1))
df$rankName <-  fct_recode(df$rankName, "Famille" = "Sous-Famille",
                           "Famille" = "Super-Famille",
                           "Classe" = "Infra-Classe",
                           "Genre" = "Sous-Genre",
                           "Ordre" = "Sous-Ordre", 
                           "Phylum" = "Sous-Phylum")
traits <- read.csv("data/raw-data/trait.csv", h = T, sep = ";")

# Indice computation
vdt_ind <- myIndices(DF = df[df$orderName == "Crassiclitellata",], 
                    IDresol = "Espèce", traits = traits)

ew_categ <- df %>%
            select(method, Replicate.number, id_sample, gradient, alti) %>%
            distinct() %>%
            left_join(vdt_ind$CWM) %>%
            replace(is.na(.),0)


