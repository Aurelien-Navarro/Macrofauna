#####Analyses Exploratoires#####
################################

#########################################################
#I- DATA PREPARATION########
# @title
# Data preparation 
# 
# @description
# Préparation du jeu de données à traiter
# 
# @objectif
# Fusionner les jeux de données, homogénéiser la taxo, préparer les colonnes d'intérêt 
#
# @details
# 0. Interrogation de la base de données Eco&Sols et de INaturalist pour récupérer les données d'occurrence
# 1. Fusion des données et homogénéisation taxonomique
# 2. Export d'un fichier de données propres


# Libraries
librarian::shelf(tidyr, dplyr, ggplot2, rinat, RODBC, stringr, vegan, tibble, esquisse, fossil, data.table)

#Importation donnees propres
read.csv("data/derived-data/clean_data_2023-04-25.csv", header = T, sep=",")->All_Orchamp

#Transformation en matrice------
tp<-All_Orchamp %>%
  filter(!grepl("0", abundance))%>%
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(gradient, name, alti)%>%
  summarise(tot = sum(abundance)) 
as.numeric(tp$tot)->tp$tot

pivot_wider(tp,
            id_cols = c('gradient','alti'),
            names_from = 'name', 
            values_from = 'tot',
            values_fill = 0)->matrice
#infos sur la matrice
str(matrice)
head(matrice)
summary(matrice)

##Sortie de la matrice 
#write.csv(matrice, file = paste0("data/derived-data/matrice_esp_" , as.character(Sys.Date()) , ".csv"))

#######
#NMDS--
#######
matrice%>%
  unite(id_plot, gradient, alti)->matrice_u

#Matrice sans les sites 
matrice_u[,2:314]->NMDSobj
     
# Vérifier si x contient des valeurs manquantes
if (any(is.na(tp))) {
  # Gérer les valeurs manquantes ici
  message("Il y a des valeurs manquantes dans x.")
} else {
  # Exécuter le code si x ne contient pas de valeurs manquantes
  message("x ne contient pas de valeurs manquantes.")
}
#Passage des id_plot en ligne
matrice_u %>%
  remove_rownames() %>%
  column_to_rownames(var = 'id_plot')->NMDSobj



#Run NMDS
  NMDS_allOrchamp=metaMDS(NMDSobj, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions
  stressplot(NMDS_allOrchamp)
  plot(NMDS_allOrchamp)
  ordiplot(NMDS_allOrchamp,type="n")
  orditorp(NMDS_allOrchamp,display="sites",cex=0.55,air=0.01) 
  
  #Avec couleurs par site d'échantillonnage 
  treat=c(rep("ARG",6),rep("ARM",2),rep("BOU",5), rep("CAU",8), rep("MOU",6), 
          rep("MSB",5), rep("PEC",2), rep("RIS",6), rep("TAN",5), rep("VAL",5),
          rep("VCHA",6), rep("VER",5), rep("VTN",5))
  ordiplot(NMDS_allOrchamp,type="n")
  ordihull(NMDS_allOrchamp,groups=treat,draw="polygon",col="grey90",label=F)
  orditorp(NMDS_allOrchamp,display="sites",col=c(rep("green",6),rep("blue",2),rep("red",5), rep("orange",8), rep("purple",6), 
                                              rep("black",5), rep("lightblue",2), rep("pink",6), rep("lightgreen",5), rep("brown",5),
                                              rep("darkblue",6), rep("darkgreen",5), rep("darkorange",5)),
           air=0.01,cex=0.5)  
   
  ##############
  #PIECHART-----
  ##############
  library(formattable)
  #install.packages("PieDonut")
  #library(PieDonut)
  #On prend All_Orchamp
  str(All_Orchamp)
  ORCPc <- All_Orchamp %>%
    filter(!className %in% c("Collembola"), !orderName %in% c("Diptera"))  
  
  #Donut chart of taxonomic distribution
  donut <- ORCPc %>%
    select(orderName, className, familyName, abundance) %>%
    mutate(orderName = ifelse(orderName == "", "NI", orderName)) %>%
    mutate(orderName = ifelse(is.na(orderName), "NI", orderName)) %>% 
    mutate(className = ifelse(is.na(className), "NI", className)) %>%
    mutate(familyName = ifelse(is.na(familyName), "NI", familyName)) %>%
    filter(!familyName %in% c("NI"))%>%
    mutate(className = as.factor(className), orderName = as.factor(orderName),
           familyName= as.factor(familyName))%>%  
    group_by(familyName, orderName, className) %>%
    summarise(n = sum(abundance)) 
str(donut)
as.data.frame(donut)->donut_df

# Pie Chart with Percentages
  slices <-donut_df$n
  lbls <-donut_df$familyName
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  pie(slices,labels = lbls,
      cex=0.6,
      edges = 200, radius = 0.8,
      col=rainbow(length(lbls)),
      main="Répartition taxonomique des effectifs échantillonnés")



