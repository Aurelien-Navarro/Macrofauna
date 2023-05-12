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


  ##ORTHOPTERA-----
ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "Espèce")%>%
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
adipart(ESP_matrice, echelle,
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
        )->addipartESP

addipartESP$statistic

write.csv(addipartESP$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_alpha" , as.character(Sys.Date()) , ".csv"))

    ###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod
Nullmod$statistic
write.csv(Nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats
niveaux<-c(sum(addipartESP$statistic[c(1,6)]),
           sum(addipartESP$statistic[c(2,7)]),
           sum(addipartESP$statistic[c(3,8)]),
           sum(addipartESP$statistic[c(4,9)]),
           sum(addipartESP$statistic[c(5,10)])
           )
sum(niveaux)->Sumniveaux
niveaufinal<-c(sum(addipartESP$statistic[c(1,6)])/Sumniveaux,
               sum(addipartESP$statistic[c(2,7)])/Sumniveaux,
               sum(addipartESP$statistic[c(3,8)])/Sumniveaux,
               sum(addipartESP$statistic[c(4,9)])/Sumniveaux,
               sum(addipartESP$statistic[c(5,10)])/Sumniveaux
               )
sum(niveaufinal)#doit faire 1
factor1<-(c("Echantillon","Station","Milieu","Gradient", "Massif"))
data.frame(x=factor1, y=niveaufinal)->plotvariance
plotvariance%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance

write.csv(plotvariance, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des Orthoptères",
    subtitle = "Dispositif Orchamp, 2021-2022",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée",
    caption  = "mpg data from the ggplot2 package")

ggplot(plotvariance) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique (diversité alpha) des orthoptères",
    subtitle = "Dispositif Orchamp, 2021-2022, pièges Barber"
  ) +
  theme_minimal()




  ##CARABIDAE------

ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "Espèce")%>%
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
adipart(ESP_matrice, echelle,
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP

addipartESP$statistic

write.csv(addipartESP$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_alpha" , as.character(Sys.Date()) , ".csv"))

###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod
Nullmod$statistic
write.csv(Nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats
niveaux<-c(sum(addipartESP$statistic[c(1,6)]),
           sum(addipartESP$statistic[c(2,7)]),
           sum(addipartESP$statistic[c(3,8)]),
           sum(addipartESP$statistic[c(4,9)]),
           sum(addipartESP$statistic[c(5,10)])
)
sum(niveaux)->Sumniveaux
niveaufinal<-c(sum(addipartESP$statistic[c(1,6)])/Sumniveaux,
               sum(addipartESP$statistic[c(2,7)])/Sumniveaux,
               sum(addipartESP$statistic[c(3,8)])/Sumniveaux,
               sum(addipartESP$statistic[c(4,9)])/Sumniveaux,
               sum(addipartESP$statistic[c(5,10)])/Sumniveaux
)
sum(niveaufinal)#doit faire 1
factor1<-(c("Echantillon","Station","Milieu","Gradient", "Massif"))
data.frame(x=factor1, y=niveaufinal)->plotvariance
plotvariance%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance

write.csv(plotvariance, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des Carabiques",
    subtitle = "Dispositif Orchamp, 2021-2022",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée",
    caption  = "mpg data from the ggplot2 package")

ggplot(plotvariance) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique (diversité alpha) des Carabiques",
    subtitle = "Dispositif Orchamp, 2021-2022, pièges Barber"
  ) +
  theme_minimal()


    ##DETRITVORES------

ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(rankName %in% "Espèce")%>%
  filter(method %in% c("tri manuel","tri manuel qualitatif","chasse à vue"))%>%
  filter(orderName %in% "Isopoda"|className%in%c("Diplopoda","Clitellata"))%>% 
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
adipart(ESP_matrice, echelle,
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP

addipartESP$statistic

write.csv(addipartESP$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_alpha" , as.character(Sys.Date()) , ".csv"))

###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod
Nullmod$statistic
write.csv(Nullmod$statistic, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

#creation d'un tableau pour les resultats
niveaux<-c(sum(addipartESP$statistic[c(1,6)]),
           sum(addipartESP$statistic[c(2,7)]),
           sum(addipartESP$statistic[c(3,8)]),
           sum(addipartESP$statistic[c(4,9)]),
           sum(addipartESP$statistic[c(5,10)])
)
sum(niveaux)->Sumniveaux
niveaufinal<-c(sum(addipartESP$statistic[c(1,6)])/Sumniveaux,
               sum(addipartESP$statistic[c(2,7)])/Sumniveaux,
               sum(addipartESP$statistic[c(3,8)])/Sumniveaux,
               sum(addipartESP$statistic[c(4,9)])/Sumniveaux,
               sum(addipartESP$statistic[c(5,10)])/Sumniveaux
)
sum(niveaufinal)#doit faire 1
factor1<-(c("Echantillon","Station","Milieu","Gradient", "Massif"))
data.frame(x=factor1, y=niveaufinal)->plotvariance
plotvariance%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance

write.csv(plotvariance, file = paste0("outputs/PartitionVariance_fichiers csv/Orthoptera/Orthopteres_niveau" , as.character(Sys.Date()) , ".csv"))

#plot du tableau

ggplot(plotvariance, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des Detritivores",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue",
    x        = "Echelles d'étude",
    y        = "Porportion de Variance expliquée",
    caption  = "mpg data from the ggplot2 package")

ggplot(plotvariance) +
  aes(x = Echelle, y = Variance_expliquee, fill = Echelle) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Echelle",
    y = "% de variance expliquee",
    title = "Partition de la variance taxonomique (diversité alpha) des Detritivores",
    subtitle = "Dispositif Orchamp, 2021-2022, Tri manuel, qualitatif et chasse à vue"
  ) +
  theme_minimal()






