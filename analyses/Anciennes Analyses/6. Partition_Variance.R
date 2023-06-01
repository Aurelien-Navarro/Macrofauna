########################
#Analyse de variance----
########################

#librairies
library(vegan)
library(dplyr)
library(tibble)
library(tidyverse)
library(esquisse)
library(ggplot2)

#ESPECES-----


##Utilisation de la fonction adipart()
#besoin d'un tableau communautés, et d'un tableau avec les échelles en hierarchie
#decroissante


#tableau communautes-----
read.csv("data/derived-data/Esp/clean_data_2023-05-10.csv", h=T, sep=',')->ESP
ESP%>%
  filter(!gradient %in% c("BOU","CAU"))->ESP #Forêts du bout et Cauteret non terminés

  ##ORTHOPTERA-----
ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
  filter(method %in% "barber")%>%
  filter(orderName %in% "Orthoptera")%>% 
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice0

#passage de la colone id en index
ESP_matrice0%>%
  remove_rownames()%>%
  column_to_rownames('id_sample')->ESP_matrice

    ###tableau des echelles----


#echelle de l echantillon
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample



#echelle du plot (gradient et altitude)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle du gradient (localité)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0

habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(plot, by='id_plot')%>%
  left_join(localite, by='id_sample')%>%
  unite(habitat, Milieu, gradient)%>%
  select(c(id_sample, habitat))->milieu


#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (Alpes S ou AlpesN ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = case_when(gradient %in% c("MSB","VER","CAU")~"Pyr",
                            gradient %in% c("VCHA", "VTN", "MOU","RIS")~"AlpS",
                            gradient %in% c("VAL", "BOU", "TAN","PEC","ARG","ARM")~"AlpN"))%>%
  select(c(id_sample, massif))->massif


  
#Echelle totale(gamma)
ESP%>%
  distinct(id_sample, .keep_all=T)%>%
  add_column(Dispositif = "Orchamp")%>%
  select(c("id_sample","Dispositif"))->gamma


#Concaténation 

ESP_matrice0%>%
  inner_join(plot, by="id_sample")%>%
  inner_join(altitude, by="id_sample")%>%
  inner_join(milieu, by="id_sample")%>%
  inner_join(localite, by="id_sample")%>%
  inner_join(massif, by="id_sample")%>%
  inner_join(gamma, by="id_sample")%>%
  select(c(id_sample, id_plot, habitat, gradient, massif, Dispositif))%>%
  relocate(habitat, .before=gradient)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


    ###Partitition de variance addititve-----
adipart(ESP_matrice, echelle[,-3],
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
        )->addipartESP_ortho

addipartESP_ortho$statistic

write.csv(addipartESP_ortho$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_alpha" , as.character(Sys.Date()) , ".csv"))

    ###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod_ortho
Nullmod_ortho$statistic
write.csv(Nullmod_ortho$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats

niveaux<-addipartESP_ortho$statistic[c(1,6:9)]
factor1<-(c("1.Alpha", "2.B-Echan","3.B-Gradient","4.B-Gradient", "5.B-Massif"))

data.frame(x=factor1, y=niveaux)->plotvariance_ortho
plotvariance_ortho%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance_ortho

write.csv(plotvariance_ortho, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance_ortho, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des Orthoptères",
    subtitle = "Dispositif Orchamp, 2021-2022",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée")

ggplot(plotvariance_ortho) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_bar(stat = "identity")+
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique des orthoptères",
    subtitle = "Dispositif Orchamp, 2021-2022, pièges Barber"
  ) +
  theme_minimal()




  ##CARABIDAE------

ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
  filter(method %in% "barber")%>%
  filter(familyName %in% "Carabidae")%>% 
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice0

#passage de la colone id en index
ESP_matrice0%>%
  remove_rownames()%>%
  column_to_rownames('id_sample')->ESP_matrice

###tableau des echelles----


#echelle de l echantillon
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample



#echelle du plot (gradient et altitude)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0


habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(plot, by='id_plot')%>%
  left_join(localite, by='id_sample')%>%
  unite(habitat, Milieu, gradient)%>%
  select(c(id_sample, habitat))->milieu


#echelle du gradient (localité)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (Alpes S ou AlpesN ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = case_when(gradient %in% c("MSB","VER","CAU")~"Pyr",
                            gradient %in% c("VCHA", "VTN", "MOU","RIS")~"AlpS",
                            gradient %in% c("VAL", "BOU", "TAN","PEC","ARG","ARM")~"AlpN"))%>%
  select(c(id_sample, massif))->massif



#Echelle totale(gamma)
ESP%>%
  distinct(id_sample, .keep_all=T)%>%
  add_column(Dispositif = "Orchamp")%>%
  select(c("id_sample","Dispositif"))->gamma


#Concaténation 

ESP_matrice0%>%
  inner_join(plot, by="id_sample")%>%
  inner_join(altitude, by="id_sample")%>%
  inner_join(milieu, by="id_sample")%>%
  inner_join(localite, by="id_sample")%>%
  inner_join(massif, by="id_sample")%>%
  inner_join(gamma, by="id_sample")%>%
  select(c(id_sample, id_plot, habitat, gradient, massif, Dispositif))%>%
  relocate(habitat, .before=gradient)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


###Partitition de variance addititve-----
adipart(ESP_matrice, echelle[,-3],
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP_carab

addipartESP_carab$statistic

write.csv(addipartESP_carab$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Carabidae/carab_alpha" , as.character(Sys.Date()) , ".csv"))

###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod_carab
Nullmod_carab$statistic
write.csv(Nullmod_carab$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Carabidae/carab_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats
niveaux<-addipartESP_carab$statistic[c(1,6:9)]
factor1<-(c("1.Alpha", "2.B-Echan","3.B-Gradient","4.B-Gradient", "5.B-Massif"))
data.frame(x=factor1, y=niveaux)->plotvariance_carab
plotvariance_carab%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance_carab

write.csv(plotvariance_carab, file = paste0("outputs/PartitionVariance_fichiers csv/Carabidae/carab_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance_carab, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des Carabiques",
    subtitle = "Dispositif Orchamp, 2021-2022",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée",
    caption  = "mpg data from the ggplot2 package")

ggplot(plotvariance_carab) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_bar(stat = "identity") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique des Carabiques",
    subtitle = "Dispositif Orchamp, 2021-2022, pièges Barber"
  ) +
  theme_minimal()


    ##Vers de terre------

ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
  filter(method %in% c("tri manuel","tri manuel qualitatif","chasse à vue"))%>%
  filter(className %in% c("Clitellata"))%>% 
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice0

#passage de la colone id en index
ESP_matrice0%>%
  remove_rownames()%>%
  column_to_rownames('id_sample')->ESP_matrice

###tableau des echelles----


#echelle de l echantillon
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample



#echelle du plot (gradient et altitude)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0


habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(plot, by='id_plot')%>%
  left_join(localite, by='id_sample')%>%
  unite(habitat, Milieu, gradient)%>%
  select(c(id_sample, habitat))->milieu


#echelle du gradient (localité)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (Alpes S ou AlpesN ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = case_when(gradient %in% c("MSB","VER","CAU")~"Pyr",
                            gradient %in% c("VCHA", "VTN", "MOU","RIS")~"AlpS",
                            gradient %in% c("VAL", "BOU", "TAN","PEC","ARG","ARM")~"AlpN"))%>%
  select(c(id_sample, massif))->massif



#Echelle totale(gamma)
ESP%>%
  distinct(id_sample, .keep_all=T)%>%
  add_column(Dispositif = "Orchamp")%>%
  select(c("id_sample","Dispositif"))->gamma


#Concaténation 

ESP_matrice0%>%
  inner_join(plot, by="id_sample")%>%
  inner_join(altitude, by="id_sample")%>%
  inner_join(milieu, by="id_sample")%>%
  inner_join(localite, by="id_sample")%>%
  inner_join(massif, by="id_sample")%>%
  inner_join(gamma, by="id_sample")%>%
  select(c(id_sample, id_plot, habitat, gradient, massif, Dispositif))%>%
  relocate(habitat, .before=gradient)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


###Partitition de variance addititve-----
adipart(ESP_matrice, echelle[,-3],
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP_vdt

addipartESP_vdt$statistic

write.csv(addipartESP_vdt$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/vdt_alpha" , as.character(Sys.Date()) , ".csv"))

###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod_vdt
Nullmod_vdt$statistic
write.csv(Nullmod_vdt$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/vdt_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats
niveaux<-addipartESP_vdt$statistic[c(1,6:9)]
factor1<-(c("1.Alpha", "2.B-Echan","3.B-Gradient","4.B-Gradient", "5.B-Massif"))
data.frame(x=factor1, y=niveaux)->plotvariance_vdt
plotvariance_vdt%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance_vdt

write.csv(plotvariance_vdt, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/vdt_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance_vdt, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des Vers de terre",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée",
    caption  = "mpg data from the ggplot2 package")

ggplot(plotvariance_vdt) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_bar(stat = "identity") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique des vers de terre",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue"
  ) +
  theme_minimal()







##Diplopodes------





ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
  #filter(method %in% c("tri manuel","tri manuel qualitatif","chasse à vue"))%>%
  filter(className %in% c("Diplopoda"))%>% 
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice0

#passage de la colone id en index
ESP_matrice0%>%
  remove_rownames()%>%
  column_to_rownames('id_sample')->ESP_matrice

###tableau des echelles----


#echelle de l echantillon
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample



#echelle du plot (gradient et altitude)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0


habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(plot, by='id_plot')%>%
  left_join(localite, by='id_sample')%>%
  unite(habitat, Milieu, gradient)%>%
  select(c(id_sample, habitat))->milieu


#echelle du gradient (localité)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (Alpes S ou AlpesN ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = case_when(gradient %in% c("MSB","VER","CAU")~"Pyr",
                            gradient %in% c("VCHA", "VTN", "MOU","RIS")~"AlpS",
                            gradient %in% c("VAL", "BOU", "TAN","PEC","ARG","ARM")~"AlpN"))%>%
  select(c(id_sample, massif))->massif



#Echelle totale(gamma)
ESP%>%
  distinct(id_sample, .keep_all=T)%>%
  add_column(Dispositif = "Orchamp")%>%
  select(c("id_sample","Dispositif"))->gamma


#Concaténation 

ESP_matrice0%>%
  inner_join(plot, by="id_sample")%>%
  inner_join(altitude, by="id_sample")%>%
  inner_join(milieu, by="id_sample")%>%
  inner_join(localite, by="id_sample")%>%
  inner_join(massif, by="id_sample")%>%
  inner_join(gamma, by="id_sample")%>%
  select(c(id_sample, id_plot, habitat, gradient, massif, Dispositif))%>%
  relocate(habitat, .before=gradient)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


###Partitition de variance addititve-----
adipart(ESP_matrice, echelle[,-3],
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP_diplo

addipartESP_diplo$statistic

write.csv(addipartESP_diplo$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/diplo_alpha" , as.character(Sys.Date()) , ".csv"))

###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod_diplo
Nullmod_diplo$statistic
write.csv(Nullmod_diplo$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/diplo_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats
niveaux<-addipartESP_diplo$statistic[c(1,6:9)]
factor1<-(c("1.Alpha", "2.B-Echan","3.B-Gradient","4.B-Gradient", "5.B-Massif"))
data.frame(x=factor1, y=niveaux)->plotvariance_diplo
plotvariance_diplo%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance_diplo

write.csv(plotvariance_diplo, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/diplo_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance_diplo, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des Diplopodes",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée",
    caption  = "mpg data from the ggplot2 package")

ggplot(plotvariance_diplo) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_bar(stat = "identity") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique des Diplopodes",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue"
  ) +
  theme_minimal()


##GEOTRUPIDAE------

ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
  #filter(method %in% c("tri manuel","tri manuel qualitatif","chasse à vue"))%>%
  filter(familyName %in% c("Geotrupidae"))%>% 
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice0

#passage de la colone id en index
ESP_matrice0%>%
  remove_rownames()%>%
  column_to_rownames('id_sample')->ESP_matrice

###tableau des echelles----


#echelle de l echantillon
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample



#echelle du plot (gradient et altitude)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0


habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(plot, by='id_plot')%>%
  left_join(localite, by='id_sample')%>%
  unite(habitat, Milieu, gradient)%>%
  select(c(id_sample, habitat))->milieu


#echelle du gradient (localité)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (Alpes S ou AlpesN ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = case_when(gradient %in% c("MSB","VER","CAU")~"Pyr",
                            gradient %in% c("VCHA", "VTN", "MOU","RIS")~"AlpS",
                            gradient %in% c("VAL", "BOU", "TAN","PEC","ARG","ARM")~"AlpN"))%>%
  select(c(id_sample, massif))->massif



#Echelle totale(gamma)
ESP%>%
  distinct(id_sample, .keep_all=T)%>%
  add_column(Dispositif = "Orchamp")%>%
  select(c("id_sample","Dispositif"))->gamma


#Concaténation 

ESP_matrice0%>%
  inner_join(plot, by="id_sample")%>%
  inner_join(altitude, by="id_sample")%>%
  inner_join(milieu, by="id_sample")%>%
  inner_join(localite, by="id_sample")%>%
  inner_join(massif, by="id_sample")%>%
  inner_join(gamma, by="id_sample")%>%
  select(c(id_sample, id_plot, habitat, gradient, massif, Dispositif))%>%
  relocate(habitat, .before=gradient)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


###Partitition de variance addititve-----
adipart(ESP_matrice, echelle[,-3],
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP_geotrup

addipartESP_geotrup$statistic

write.csv(addipartESP_geotrup$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/geotrup_alpha" , as.character(Sys.Date()) , ".csv"))

###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod_geotrup
Nullmod_geotrup$statistic
write.csv(Nullmod_geotrup$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/geotrup_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats
niveaux<-addipartESP_geotrup$statistic[c(1,6:9)]
factor1<-(c("1.Alpha", "2.B-Echan","3.B-Gradient","4.B-Gradient", "5.B-Massif"))
data.frame(x=factor1, y=niveaux)->plotvariance_geotrup
plotvariance_geotrup%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance_geotrup

write.csv(plotvariance_geotrup, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/geotrup_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance_geotrup, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des Geotrupidae",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée",
    caption  = "mpg data from the ggplot2 package")

ggplot(plotvariance_geotrup) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_bar(stat = "identity") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique des Geotrupidae",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue"
  ) +
  theme_minimal()





    ##ISOPODES-----

ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
  #filter(method %in% c("tri manuel","tri manuel qualitatif","chasse à vue"))%>%
  filter(orderName %in% c("Isopoda"))%>% 
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice0

#passage de la colone id en index
ESP_matrice0%>%
  remove_rownames()%>%
  column_to_rownames('id_sample')->ESP_matrice

###tableau des echelles----


#echelle de l echantillon
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample



#echelle du plot (gradient et altitude)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0


habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(plot, by='id_plot')%>%
  left_join(localite, by='id_sample')%>%
  unite(habitat, Milieu, gradient)%>%
  select(c(id_sample, habitat))->milieu


#echelle du gradient (localité)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (Alpes S ou AlpesN ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = case_when(gradient %in% c("MSB","VER","CAU")~"Pyr",
                            gradient %in% c("VCHA", "VTN", "MOU","RIS")~"AlpS",
                            gradient %in% c("VAL", "BOU", "TAN","PEC","ARG","ARM")~"AlpN"))%>%
  select(c(id_sample, massif))->massif



#Echelle totale(gamma)
ESP%>%
  distinct(id_sample, .keep_all=T)%>%
  add_column(Dispositif = "Orchamp")%>%
  select(c("id_sample","Dispositif"))->gamma


#Concaténation 

ESP_matrice0%>%
  inner_join(plot, by="id_sample")%>%
  inner_join(altitude, by="id_sample")%>%
  inner_join(milieu, by="id_sample")%>%
  inner_join(localite, by="id_sample")%>%
  inner_join(massif, by="id_sample")%>%
  inner_join(gamma, by="id_sample")%>%
  select(c(id_sample, id_plot, habitat, gradient, massif, Dispositif))%>%
  relocate(habitat, .before=gradient)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


###Partitition de variance addititve-----
adipart(ESP_matrice, echelle[,-3],
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP_iso

addipartESP_iso$statistic

write.csv(addipartESP_iso$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/iso_alpha" , as.character(Sys.Date()) , ".csv"))

###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod_iso
Nullmod_iso$statistic
write.csv(Nullmod_iso$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/iso_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats
niveaux<-addipartESP_iso$statistic[c(1,6:9)]
factor1<-(c("1.Alpha", "2.B-Echan","3.B-Gradient","4.B-Gradient", "5.B-Massif"))
data.frame(x=factor1, y=niveaux)->plotvariance_iso
plotvariance_iso%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance_iso

write.csv(plotvariance_iso, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/iso_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance_iso, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des isoidae",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée",
    caption  = "mpg data from the ggplot2 package")

ggplot(plotvariance_iso) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_bar(stat = "identity") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique des isoidae",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue"
  ) +
  theme_minimal()

  ##DECOMPOSEURS-----


ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
  #filter(method %in% c("tri manuel","tri manuel qualitatif","chasse à vue"))%>%
  filter(orderName %in% c("Isopoda")|className %in% c("Diplopoda","Clitellata")|familyName%in%"Geotrupidae")%>% 
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(id_sample, name2)%>%
  summarise(tot = sum(abundance))->tp1
###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_sample',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->ESP_matrice0

#passage de la colone id en index
ESP_matrice0%>%
  remove_rownames()%>%
  column_to_rownames('id_sample')->ESP_matrice

###tableau des echelles----


#echelle de l echantillon
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(id_sample) ->sample



#echelle du plot (gradient et altitude)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  unite(id_plot, gradient, alti)%>%
  select(c(id_sample, id_plot)) ->plot

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0


habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(plot, by='id_plot')%>%
  left_join(localite, by='id_sample')%>%
  unite(habitat, Milieu, gradient)%>%
  select(c(id_sample, habitat))->milieu


#echelle du gradient (localité)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))->localite

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (Alpes S ou AlpesN ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = case_when(gradient %in% c("MSB","VER","CAU")~"Pyr",
                            gradient %in% c("VCHA", "VTN", "MOU","RIS")~"AlpS",
                            gradient %in% c("VAL", "BOU", "TAN","PEC","ARG","ARM")~"AlpN"))%>%
  select(c(id_sample, massif))->massif



#Echelle totale(gamma)
ESP%>%
  distinct(id_sample, .keep_all=T)%>%
  add_column(Dispositif = "Orchamp")%>%
  select(c("id_sample","Dispositif"))->gamma


#Concaténation 

ESP_matrice0%>%
  inner_join(plot, by="id_sample")%>%
  inner_join(altitude, by="id_sample")%>%
  inner_join(milieu, by="id_sample")%>%
  inner_join(localite, by="id_sample")%>%
  inner_join(massif, by="id_sample")%>%
  inner_join(gamma, by="id_sample")%>%
  select(c(id_sample, id_plot, habitat, gradient, massif, Dispositif))%>%
  relocate(habitat, .before=gradient)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


###Partitition de variance addititve-----
adipart(ESP_matrice, echelle[,-3],
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP_decompo

addipartESP_decompo$statistic

write.csv(addipartESP_decompo$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/decompo_alpha" , as.character(Sys.Date()) , ".csv"))

###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod_decompo
Nullmod_decompo$statistic
write.csv(Nullmod_decompo$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/decompo_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats
niveaux<-addipartESP_decompo$statistic[c(1,6:9)]
factor1<-(c("1.Alpha", "2.B-Echan","3.B-Gradient","4.B-Gradient", "5.B-Massif"))
data.frame(x=factor1, y=niveaux)->plotvariance_decompo
plotvariance_decompo%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance_decompo

write.csv(plotvariance_decompo, file = paste0("outputs/PartitionVariance_fichiers csv/Decompo/decompo_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance_decompo, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des decompoidae",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée",
    caption  = "mpg data from the ggplot2 package")

ggplot(plotvariance_decompo) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_bar(stat = "identity") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique des decompoidae",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue"
  ) +
  theme_minimal()



#Comparaison entre groupes-----
resum <- tibble(
echelle = c("1.Alpha", "2.Beta-échantillons","3.Beta-stations","4.Beta-gradients", "5.Beta-massifs"), 
G1246decompo = addipartESP_decompo$statistic[c(1,6:9)],
G6iso= addipartESP_iso$statistic[c(1,6:9)],
G4vdt = addipartESP_vdt$statistic[c(1,6:9)],
G5carab = addipartESP_carab$statistic[c(1,6:9)],
G3ortho = addipartESP_ortho$statistic[c(1,6:9)],
G2diplo = addipartESP_diplo$statistic[c(1,6:9)],
G1geotrup = addipartESP_geotrup$statistic[c(1,6:9)])

resum <- resum %>%
  pivot_longer(cols = 2:6, names_to = "taxo")
  
ggplot(resum) +
  aes(x = taxo, y = value, fill = echelle) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_grey(end=1)+
  facet_grid(echelle~., scales="free_y")+
  labs(
    x = "",
    y = "% de variance expliquée",
    title = "Partition de la variance taxonomique",
    subtitle = "Dispositif Orchamp, 2021-2022, 11 gradients"
  ) +
  theme_minimal()+
  theme(legend.position = "none")
