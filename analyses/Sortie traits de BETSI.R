####SORTIE TRAITS DE BESTI####
#Script pour sortir les traits disponibles et potentiellement
#interessants de la BDD betsi en fonction des especes ou genre
#---------------

#packages----
library(dplyr)
library(tidyverse)
librarian::shelf(tidyr, dplyr, ggplot2, rinat, RODBC, stringr)
#------
#Importation de la BDD----
read.csv("data/raw-data/BETSI_220221.csv",header = T, sep=";")->BETSI
read.csv("data/raw-data/ew_traits.csv",header = T, sep=";")->traits_vdt
#------

    #Carabidae----

#Sortie des especes de carabiques dispo id sur INAT
inat_orchamp_carab <- get_inat_obs_project(grpid="carabidae_orchamp", type="observations",raw=T)
#Conservation de la colone taxon
inat_orchamp_carab_tax <- inat_orchamp_carab %>%
  as_tibble() %>%
  select(taxon.name, taxon.rank, description) %>%
  rename(INat = description) %>%
  filter(!is.na(INat))%>%
  distinct(taxon.name)

#Sortie des traits de BETSI pour les taxons references dans 'inat_orchamp_xxx_taxo'
## Je n ai pas trouve d autres moyens pour joindre des tableaux que inner join qui ne peut joindre
#plus de 3 tableaux a la fois, d ou la quantite de lignes

Carabidae_traits_dispo<-inner_join(
  filter(BETSI, grepl("Abax",taxon_name))%>%
  distinct(trait_name),
  filter(BETSI, grepl("Amara",taxon_name))%>%
  distinct(trait_name),
  filter(BETSI, grepl("Bembidion",taxon_name))%>%
  distinct(trait_name),
  by="trait_name")

Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Bradytus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Calathus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")

Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Carabus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Cicindela",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")

Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Cychrus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Cymindis",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Harpalus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Laemostenus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Microlestes",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Nebria",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Notiophilus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Poecilus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Pterostichus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Steropus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Carabidae_traits_dispo<-inner_join(
  Carabidae_traits_dispo,
  filter(BETSI, grepl("Trechus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Trichotichnus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")

#Sortie du taleau : 
write.csv2(Carabidae_traits_dispo, here::here("outputs","Carabidae_traits_dipos.csv"),row.names = FALSE ) 


    #Orthoptera-----

#Sortie des especes d'orthoptera dispo id sur INAT
inat_orchamp_ortho <- get_inat_obs_project(grpid="orthoptera_orchamp", type="observations",raw=T)
#Conservation de la colone taxon
inat_orchamp_ortho_tax <- inat_orchamp_ortho %>%
  as_tibble() %>%
  select(taxon.name, taxon.rank, description) %>%
  rename(INat = description) %>%
  filter(!is.na(INat))%>%
  distinct(taxon.name)

#Sortie des traits de BETSI pour les taxons references dans 'inat_orchamp_xxx_taxo'
Orthopera_traits_dispo<-inner_join(
  filter(BETSI, grepl("Arcyptera",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Anonconotus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Bohemanella",taxon_name))%>%
    distinct(trait_name),
    by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Chorthippus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Decticus",taxon_name))%>%
    distinct(trait_name),
    by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Ephippiger",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Euchorthippus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Euthystira",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Glyptobothrus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Gomphocerippus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Metrioptera",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Miramella",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Myrmeleotettix",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Nemobius",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Oedipoda",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Omocestus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Platycleis",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Podisma",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Pseudochorthippus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Psophus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Stauroderus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Orthopera_traits_dispo<-inner_join(
  Orthopera_traits_dispo,
  filter(BETSI, grepl("Stenobothrus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")

###remarque : je n'ai pas garde les familles et tribus, j'ai regarde les genres
#4 traits dispos pour les 26 genres

#Sortie du taleau : 
write.csv2(Orthopera_traits_dispo, here::here("outputs","Orthoptera_traits_dipos.csv"),row.names = FALSE ) 

    #Hymenoptera----
#/!\ BETSI ne semble pas répertorier d'hymeno...
#Le code suivant s'applique si BETSI a des hymeno. Possiblement transcrivable 
#avec une autre BDD. 
#Sortie des especes d'hymeno parasites dispo id sur INAT
inat_orchamp_hymeno <- get_inat_obs_project(grpid="hymenoptera_orchamp", type="observations",raw=T)
#Conservation de la colone taxon
inat_orchamp_hymeno_tax <- inat_orchamp_hymeno %>%
  as_tibble() %>%
  select(taxon.name, taxon.rank, description) %>%
  rename(INat = description) %>%
  filter(!is.na(INat))%>%
  distinct(taxon.name)

#Sortie des traits de BETSI pour les taxons references dans 'inat_orchamp_xxx_taxo'
#Ne fonctionne pas car pas d'hymeno sur BETSI.
Hymenoptera_traits_dispo<-inner_join(
  filter(BETSI, grepl("Aclista",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Amblyaspis",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Anommatium",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Basalys",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Belyta",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Bethylus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Brachyserphus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Elasmus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Embolemus",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Exallonyx",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Lyteba",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Meraporus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Miota",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Mutilla",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Pantoclis",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Paracodrus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Paramesius",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Psilus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Scelio",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Sparasion",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Sphecodes",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Spilomicrus",taxon_name))%>%
    distinct(trait_name),
  filter(BETSI, grepl("Trichopria",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")
Hymenoptera_traits_dispo<-inner_join(
  Hymenoptera_traits_dispo,
  filter(BETSI, grepl("Vespula",taxon_name))%>%
    distinct(trait_name),
  by="trait_name")

#Sortie du taleau : 
write.csv2(Hymenoptera_traits_dispo, here::here("outputs","Hymenoptera_traits_dipos.csv"),row.names = FALSE ) 

    #Lumbricidae----
#Sortie des especes de vdt dispo id sur INAT
inat_orchamp_vdt <- get_inat_obs_project(grpid="lumbricidae_orchamp", type="observations",raw=T)
#Conservation de la colone taxon
inat_orchamp_vdt_tax <- inat_orchamp_vdt %>%
  as_tibble() %>%
  select(taxon.name, taxon.rank, description) %>%
  rename(INat = description) %>%
  filter(!is.na(INat))%>%
  distinct(taxon.name)

#Sortie des traits dispos sur 'ew_traits' des especes identifiees sur Inat.

Vdt_traits_dispos<-bind_rows(
  traits_vdt%>%
  filter(grepl("Aporrectodea caliginosa",Name.in.Drilobase.2019)),
  traits_vdt%>%
  filter(grepl("Aporrectodea rosea",Name.in.Drilobase.2019)),
  traits_vdt%>%
    filter(grepl("Allolobophora chlorotica",Name.in.Drilobase.2019)),
  traits_vdt%>%
    filter(grepl("Lumbricus terrestris",Name.in.Drilobase.2019)),
  traits_vdt%>%
    filter(grepl("Lumbricus rubellus",Name.in.Drilobase.2019)),
  traits_vdt%>%
    filter(grepl("Octolasion lacteum",Name.in.Drilobase.2019)),
  )

#Sortie du tableau 
write.csv2(Vdt_traits_dispos, here::here("outputs","Lumbricidae_traits_dipos.csv"),row.names = FALSE ) 
