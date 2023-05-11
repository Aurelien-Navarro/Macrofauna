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

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (alpe ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = ifelse(gradient %in% c("MSB","VER","CAU"),"Pyr", "Alp"))%>%
  select(c(id_sample, massif))->massif

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0
massif%>%
left_join(plot, by="id_sample")->PrHab

habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(PrHab, by='id_plot')%>%
  unite(milieumass, massif, Milieu, id_plot)%>%
  select(c(id_sample, milieumass))->milieu
  

#Concaténation 

ESP_matrice0%>%
  left_join(plot, by="id_sample")%>%
  left_join(altitude, by="id_sample")%>%
  left_join(localite, by="id_sample")%>%
  left_join(massif, by="id_sample")%>%
  left_join(milieu, by="id_sample")%>%
  select(c(id_plot, gradient, milieumass, massif))%>%
  relocate(milieumass, .before=id_plot)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


    ###Partitition de variance addititve-----
adipart(ESP_matrice, echelle,
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
        )->addipartESP

write.csv(addipartESP$statistic, file = paste0("outputs/PartitionVariance/Orthopteres" , as.character(Sys.Date()) , ".csv"))

    ###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod
write.csv(Nullmod$statistic, file = paste0("outputs/PartitionVariance/Orthopteres_nullmodel" , as.character(Sys.Date()) , ".csv"))

###Presentation des resultats----

niveaux<-c(sum(addipartESP$statistic[c(1:6)]),
           sum(addipartESP$statistic[c(2:7)]),
           sum(addipartESP$statistic[c(3:8)]),
           sum(addipartESP$statistic[c(4:9)]))
sum(niveaux)->Sumniveaux
niveaufinal<-c(sum(addipartESP$statistic[c(1:6)])/Sumniveaux,
               sum(addipartESP$statistic[c(2:7)])/Sumniveaux,
               sum(addipartESP$statistic[c(3:8)])/Sumniveaux,
               sum(addipartESP$statistic[c(4:9)])/Sumniveaux)
sum(niveaufinal)#doit faire 1
factor1<-(c("Intra station","Milieu","Station","Gradient"))
data.frame(x=factor1, y=niveaufinal)->plotvariance
plotvariance%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance

write.csv(plotvariance, file = paste0("outputs/PartitionVariance/Orthopteres_niveau" , as.character(Sys.Date()) , ".csv"))

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
    subtitle = "Orchamp"
  ) +
  theme_minimal()


  ##DECOMPOSEURS-----


ESP%>%
  filter(!grepl("0", abundance))%>%
  filter(orderName %in% "Isopoda" | className %in% c("Diplopoda","Clitellata"))%>% 
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

#echelle de l'altitude
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, alti)) ->altitude


#echelle du massif (alpe ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = ifelse(gradient %in% c("MSB","VER","CAU"),"Pyr", "Alp"))%>%
  select(c(id_sample, massif))->massif

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0
massif%>%
  left_join(plot, by="id_sample")->PrHab

habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(PrHab, by='id_plot')%>%
  unite(milieumass, massif, Milieu, id_plot)%>%
  select(c(id_sample, milieumass))->milieu


#Concaténation 

ESP_matrice0%>%
  left_join(plot, by="id_sample")%>%
  left_join(altitude, by="id_sample")%>%
  left_join(localite, by="id_sample")%>%
  left_join(massif, by="id_sample")%>%
  left_join(milieu, by="id_sample")%>%
  select(c(id_plot, gradient, milieumass, massif))%>%
  relocate(milieumass, .before=id_plot)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


    ###Partition de variance addititve-----
adipart(ESP_matrice, echelle,
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
)->addipartESP
write.csv(addipartESP$statistic, file = paste0("outputs/PartitionVariance/Decomposeurs" , as.character(Sys.Date()) , ".csv"))
    ###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod
write.csv(Nullmod$statistic, file = paste0("outputs/PartitionVariance/NullModelDecomposeurs" , as.character(Sys.Date()) , ".csv"))

    ###Presentation des resultats----

niveaux<-c(sum(addipartESP$statistic[c(1:6)]),
           sum(addipartESP$statistic[c(2:7)]),
           sum(addipartESP$statistic[c(3:8)]),
           sum(addipartESP$statistic[c(4:9)]))
sum(niveaux)->Sumniveaux
niveaufinal<-c(sum(addipartESP$statistic[c(1:6)])/Sumniveaux,
               sum(addipartESP$statistic[c(2:7)])/Sumniveaux,
               sum(addipartESP$statistic[c(3:8)])/Sumniveaux,
               sum(addipartESP$statistic[c(4:9)])/Sumniveaux)
sum(niveaufinal)#doit faire 1
factor1<-(c("Intra station","Milieu","Station","Gradient"))
data.frame(x=factor1, y=niveaufinal)->plotvariance
plotvariance%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance

write.csv(plotvariance, file = paste0("outputs/PartitionVariance/Decompo_niveau" , as.character(Sys.Date()) , ".csv"))


ggplot(plotvariance, aes(x='Echelle', y=Variance_expliquee, fill=Echelle, )) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(
    title    = "Partition de variance taxonomique des décomposeurs",
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
    title = "Partition de la variance taxonomique (diversité alpha) des décomposeurs",
    subtitle = "Orchamp"
  ) +
  theme_minimal()


  ##PREDATORS----

ESP%>%
  filter(!grepl("0", abundance))%>%
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


#echelle du massif (alpe ou pyr)
ESP%>%
  filter(!grepl("0", abundance))%>%
  distinct(id_sample, .keep_all=T)%>%
  select(c(id_sample, gradient))%>%
  mutate(massif = ifelse(gradient %in% c("MSB","VER","CAU"),"Pyr", "Alp"))%>%
  select(c(id_sample, massif))->massif

#echelle de l'habitat 
#importation du tableau qui possède les habitats
read.csv("data/raw-data/Envir/Habitat.csv", header = T, sep=",")->habit0
massif%>%
  left_join(plot, by="id_sample")->PrHab

habit0%>%
  select(c(Milieu, id_plot))%>%
  left_join(PrHab, by='id_plot')%>%
  unite(milieumass, massif, Milieu, id_plot)%>%
  select(c(id_sample, milieumass))->milieu


#Concaténation 

ESP_matrice0%>%
  left_join(plot, by="id_sample")%>%
  left_join(altitude, by="id_sample")%>%
  left_join(localite, by="id_sample")%>%
  left_join(massif, by="id_sample")%>%
  left_join(milieu, by="id_sample")%>%
  select(c(id_plot, gradient, milieumass, massif))%>%
  relocate(milieumass, .before=id_plot)->echelle#passage par matrice 0 afin de conserver l'ordre des colones id sample


    ###Partitition de variance addititve-----
adipart(ESP_matrice, echelle,
        index=c("richness","shannon","simpson"), 
        weights = c("unif","prop"),
        relative = T,
        nsimul = 150,
        )->addipartESP
write.csv(addipartESP$statistic, file = paste0("outputs/PartitionVariance/Carabidae" , as.character(Sys.Date()) , ".csv"))
###hierarchical null model hypothesis

hiersimu(ESP_matrice, echelle,  FUN=diversity, relative=TRUE, nsimul=150)->Nullmod
write.csv(Nullmod$statistic, file = paste0("outputs/PartitionVariance/NullModelCarabidae" , as.character(Sys.Date()) , ".csv"))
    ###Presentation des resultats----


niveaux<-c(sum(addipartESP$statistic[c(1:6)]),
           sum(addipartESP$statistic[c(2:7)]),
           sum(addipartESP$statistic[c(3:8)]),
           sum(addipartESP$statistic[c(4:9)]))
sum(niveaux)->Sumniveaux
niveaufinal<-c(sum(addipartESP$statistic[c(1:6)])/Sumniveaux,
               sum(addipartESP$statistic[c(2:7)])/Sumniveaux,
               sum(addipartESP$statistic[c(3:8)])/Sumniveaux,
               sum(addipartESP$statistic[c(4:9)])/Sumniveaux)
sum(niveaufinal)#doit faire 1
factor1<-(c("Intra station","Milieu","Station","Gradient"))
data.frame(x=factor1, y=niveaufinal)->plotvariance
plotvariance%>%
  rename(Echelle = x)%>%
  rename(Variance_expliquee = y)->plotvariance

write.csv(plotvariance, file = paste0("outputs/PartitionVariance/Carab_niveau" , as.character(Sys.Date()) , ".csv"))


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
    title = "Partition de la variance taxonomique (diversité alpha) des carabiques",
    subtitle = "Orchamp"
  ) +
  theme_minimal()

