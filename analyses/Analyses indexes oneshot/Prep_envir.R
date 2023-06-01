#Creation du df env--------


#@objectif : créer un tableau avec les variables environnementales pour chaque site 


#Libraries
library(dplyr)
library(tidyr)


#Importation des differentes donnees deja recup par micakel-------
read.csv("data/raw-data/envir/Tmean.data2.csv", header = T, sep=",")->Temperatures
read.csv("data/raw-data/envir/phyto.data3.csv", header = T, sep=",")->Phyto
read.csv("data/raw-data/envir/BoisVivant.data4.csv", header = T, sep=",")->Arbres
read.csv("data/raw-data/envir/Anasolsup.data5.csv", header = T, sep=",")->AnaSol
read.csv("data/raw-data/envir/AnaFossePedo.data6.csv", header = T, sep=",")->FosseP
read.csv("data/raw-data/envir/par_aurel/Tmean_Prec1.csv", header = T, sep=";")->Tmean_rainf1
read.csv("data/raw-data/envir/Habitat.csv",header=T, sep=",")->Habit
read.csv("data/raw-data/envir/Romain/MODIS4ORCHAMP.csv",header=T, sep=",")->NDVI
readRDS("data/raw-data/envir/Romain/climatic.rds")->Climat
read.csv("data/raw-data/Envir/Julien/SafranPyr.csv")->SafranPyr
read.csv("data/raw-data/Envir/Julien/CrocusPyr.csv")->CrocusPyr

#----------------


#PARAMETRES PHYSIQUES-----

 ##TEMPERATURES----------
  ##Temperature moyenne de 2017-2018-2019 par site et par altitude
 
Temperatures %>%
  select(contains("2019")) %>%
  rowMeans()->Temperatures$Tmean
Temperatures%>%
  select(c("nom", "idsite", "codeplot", "idplot", "Tmean"))->Temp_final
  

  ##PRECIPIATION------
  ##Precipitations moyennes par site et par altitude

Tmean_rainf1%>%
    filter(year>="2019")%>%
    na.omit(Rainf)%>%
    group_by(site, alti)%>%
    summarise(Pmean=mean(Rainf))-> Rainfall_final
  ##Creation d'une colone codeplot comme pour la temperature
paste(Rainfall_final$site, Rainfall_final$alti, sep="_")->Rainfall_final$codeplot

  ##Concatenation des deux
left_join(Temp_final, Rainfall_final, by = "codeplot")%>%
  select(c(codeplot, Tmean, Pmean))->ENV

  ##DONNES CLIMATIQUES 
Climat%>%
  select(!c(idplot, gradient, elevation, year))->Climatfinal

#Ajout sur ENV
Climatfinal%>%
left_join(ENV, by = "codeplot")->ENV

#PARAMETRES BIO------


 ##Richesse veg------
  ###RS------
RS_veg_final<-Phyto%>%
  na.omit(lb_nom)%>%
  group_by(codeplot)%>%
  distinct(lb_nom)%>%
  summarize(Rveg=n())

  ###rajout de la Richesse veg au tableau final
  left_join(ENV, RS_veg_final, by = "codeplot")->ENV
  
  
  ##Caracteristiques arbres--------------
  TF_arbres_final<-Arbres%>%
    na.omit(diam)%>%
    filter(!grepl("0", diam))%>%
    select(c(codeplot, diam))%>%
    group_by(codeplot)%>%
    summarise(Diam_mean=mean(diam))
  
  ##rajout du diametre arbre au tableau environnement
  left_join(ENV, TF_arbres_final, by = "codeplot")->ENV
  
  ##NDVI
  NDVI%>%
    na.omit(c(NDVImin, NDVImax))%>%
    select(c(codeplot, NDVImin, NDVImax))%>%
    group_by(codeplot)%>%
    summarise(NDVImin=mean(NDVImin), NDVImax=mean(NDVImax))->NDVIfinal
  
  #rajout au tableau environnement 
  left_join(ENV, NDVIfinal, by='codeplot')->ENV
  
#LE SOL--------


  ##MO------
  ##% de MO moyen pour les 2 profondeurs
  AnaSol%>%
    filter(grepl("MO", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(PRCTMOmean=mean(value))->MO_final

  ###implementation sur ENV
  left_join(ENV, MO_final, by = "codeplot")->ENV

  ##le pH-------
  AnaSol%>%
    filter(grepl("pH", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(pHmean=mean(value))->pH_final
  
  ##implementation sur ENV
  left_join(ENV, pH_final, by = "codeplot")->ENV

  ##le carbone-----
  AnaSol%>%
    filter(grepl("Carbon", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(Cmean=mean(value))->Cmean_final
  
  ##implementation sur ENV
  left_join(ENV, Cmean_final, by = "codeplot")->ENV
  
  ##Le nitrogene-----
  AnaSol%>%
    filter(grepl("Nitrogen", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(Nmean=mean(value))->Nmean_final
  
  ##implementation sur ENV
  left_join(ENV, Nmean_final, by = "codeplot")->ENV
  
  
#Pedologie-----
  
  ##Aluminium-----
  FosseP%>%
    filter(grepl("Aluminium", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Almean=mean(valeur))->Almean_final
  
  ##implementation sur ENV
  left_join(ENV, Almean_final, by = "codeplot")->ENV
  
  ##Argile
  FosseP%>%
    filter(grepl("Argile", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Argilemean=mean(valeur))->Argilemean_final
  
  ##implementation sur ENV
  left_join(ENV, Argilemean_final, by = "codeplot")->ENV
  
  ##Calcaire-----
  FosseP%>%
    filter(grepl("Calcaire", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Calcmean=mean(valeur))->Calc_final
  
  ##implementation sur ENV
  left_join(ENV, Calc_final, by = "codeplot")->ENV
  
  ##Calcium
  
  FosseP%>%
    filter(grepl("Calcium", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Camean=mean(valeur))->Camean_final
  
  ##implementation sur ENV
  left_join(ENV, Camean_final, by = "codeplot")->ENV
  
  ##Fer
  
  FosseP%>%
    filter(grepl("Fer", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Fermean=mean(valeur))->fermean_final
  
  ##implementation sur ENV
  left_join(ENV, fermean_final, by = "codeplot")->ENV
  
  ##Limons fins-----
  
  FosseP%>%
    filter(grepl("Limons fins", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Limfinmean=mean(valeur))->Limfinmean_final
  
  ##implementation sur ENV
  left_join(ENV, Limfinmean_final, by = "codeplot")->ENV
  
  ##Limons grossiers----
  
  FosseP%>%
    filter(grepl("Limons grossiers", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Limgrosmean=mean(valeur))->Limgrosmean_final
  
  ##implementation sur ENV
  left_join(ENV, Limgrosmean_final, by = "codeplot")->ENV
  
  ##Magnesium
  
  FosseP%>%
    filter(grepl("Magnésium", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(mgmean=mean(valeur))->mgmean_final
  
  
  ##implementation sur ENV
  left_join(ENV, mgmean_final, by = "codeplot")->ENV
  
  ##Manganèse
  
  FosseP%>%
    filter(grepl("Manganèse", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Mnmean=mean(valeur))->Mnmean_final
  
  ##implementation sur ENV
  left_join(ENV, Mnmean_final, by = "codeplot")->ENV
  
  ##Phosphore ----
  FosseP%>%
    filter(grepl("Phosphore", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Phosmean=mean(valeur))->Phosmean_final
  
  ##implementation sur ENV
  left_join(ENV, Phosmean_final, by = "codeplot")->ENV
  
  ##Potassium----
  FosseP%>%
    filter(grepl("Potassium", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Kmean=mean(valeur))->Kmean_final
  
  ##implementation sur ENV
  left_join(ENV, Kmean_final, by = "codeplot")->ENV
  
  
  ##Sablesfins----
  FosseP%>%
    filter(grepl("Sables fins", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Sablefinmean=mean(valeur))->Sablefin_final
  
  ##implementation sur ENV
  left_join(ENV, Sablefin_final, by = "codeplot")->ENV
  
  ##Sables grossiers----
  FosseP%>%
    filter(grepl("Sables grossier", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Sablegrosmean=mean(valeur))->Sablegros_final
  
  ##implementation sur ENV
  left_join(ENV, Sablegros_final, by = "codeplot")->ENV
  
  ##Sodium-----
  FosseP%>%
    filter(grepl("Sodium", code_anasol))%>%
    na.omit(valeur)%>%
    group_by(codeplot)%>%
    summarise(Namean=mean(valeur))->Namean_final
  
  ##implementation sur ENV
  left_join(ENV, Namean_final, by = "codeplot")->ENV
  
  
  
  
  
  
  
  
  
  
  

#ENZYMES------
  ##PHOS
  AnaSol%>%
    filter(grepl("PHOS", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(PHOSmean=mean(value))->PHOSmean_final
  
  ##implementation sur ENV
  left_join(ENV, PHOSmean_final, by = "codeplot")->ENV
  
  ##NAG
  AnaSol%>%
    filter(grepl("NAG", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(NAGmean=mean(value))->NAGmean_final
  
  ##implementation sur ENV
  left_join(ENV, NAGmean_final, by = "codeplot")->ENV
  
  ##LAP
  AnaSol%>%
    filter(grepl("LAP", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(LAPmean=mean(value))->LAPmean_final
  
  ##implementation sur ENV
  left_join(ENV, LAPmean_final, by = "codeplot")->ENV
  
  ##AG
  AnaSol%>%
    filter(grepl("EEA α-Glucosidase", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(AGmean=mean(value))->AGmean_final
  
  ##implementation sur ENV
  left_join(ENV, AGmean_final, by = "codeplot")->ENV
  
  ##CB
  AnaSol%>%
    filter(grepl("EEA β-D-cellubiosidase", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(CBmean=mean(value))->CBmean_final
  
  ##implementation sur ENV
  left_join(ENV, CBmean_final, by = "codeplot")->ENV
  
  ##BG
  AnaSol%>%
    filter(grepl("EEA β-Glucosidase", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(BGmean=mean(value))->BGmean_final
  
  ##implementation sur ENV
  left_join(ENV, BGmean_final, by = "codeplot")->ENV
  
  ##XYL
  AnaSol%>%
    filter(grepl("EEA β-Xylosidase", code_anasol))%>%
    na.omit(value)%>%
    group_by(codeplot)%>%
    summarise(XYLmean=mean(value))->XYLmean_final
  
  ##implementation sur ENV
  left_join(ENV, XYLmean_final, by = "codeplot")->ENV

  
#Habitat, coordonnes et altitudes-----
  ##Via un df concu de tt pièces
  Habit %>%
    rename(codeplot = id_plot) %>%
    mutate(Milieu = ifelse(grepl("Milieu_ouvert", Milieu), 0, 1)) -> Habit_final
  ##implementation sur ENV
  full_join(ENV, Habit_final, by = "codeplot")->ENV
  

#Les pyrenees 
  #recoder les idplot
  CrocusPyr%>%
    mutate(id_plot = recode(idplot, "186" = "MSB_1370",
                            "187" = "MSB_1420",
                            "188"="MSB_1600",
                            "189"="MSB_2000",
                            "190"="MSB_2200",
                            "191"="MSB_2400",
                            "199"="VER_1200",
                            "200"="VER_1400F",
                            "201"="VER_1400P",
                            "202"="VER_1600",
                            "203"="VER_1800",
                            "204"="VER_2000"))->CrocusPyr
  
  SafranPyr%>%
    mutate(id_plot = recode(idplot, "186" = "MSB_1370",
                            "187" = "MSB_1420",
                            "188"="MSB_1600",
                            "189"="MSB_2000",
                            "190"="MSB_2200",
                            "191"="MSB_2400",
                            "199"="VER_1200",
                            "200"="VER_1400F",
                            "201"="VER_1400P",
                            "202"="VER_1600",
                            "203"="VER_1800",
                            "204"="VER_2000"))->SafranPyr
  #TG1
  CrocusPyr%>%
    group_by(id_plot)%>%
  summarise(TG1.degres.mean=mean(TG1))%>%
  rename(codeplot=id_plot)-> TG1f
  ENV%>%
    left_join(TG1f, by="codeplot")%>%
    mutate(TG1.degres.mean = coalesce(TG1.degres.mean.y, TG1.degres.mean.x)) %>%
    select(-TG1.degres.mean.y, -TG1.degres.mean.x)->ENV
  #TG4
  CrocusPyr%>%
    group_by(id_plot)%>%
    summarise(TG4.degres.mean=mean(TG4))%>%
    rename(codeplot=id_plot)-> TG4f
  ENV%>%
    left_join(TG4f, by="codeplot")%>%
    mutate(TG4.degres.mean = coalesce(TG4.degres.mean.y, TG4.degres.mean.x)) %>%
    select(-TG4.degres.mean.y, -TG4.degres.mean.x)->ENV
  #DSNDISBA
  CrocusPyr%>%
    group_by(id_plot)%>%
    summarise(DSN_T_ISBA.mean=mean(DSN_T_ISBA))%>%
    rename(codeplot=id_plot)-> DSN_T_ISBAf 
  ENV%>%
    left_join(DSN_T_ISBAf, by="codeplot")%>%
    mutate(DSN_T_ISBA.mean = coalesce(DSN_T_ISBA.mean.y, DSN_T_ISBA.mean.x)) %>%
    select(-DSN_T_ISBA.mean.y, -DSN_T_ISBA.mean.x)->ENV 
  #Rainf
  SafranPyr%>%
    group_by(id_plot)%>%
    summarise(PTotY.mean=mean(Rainf))%>%
    rename(codeplot=id_plot)-> rainff
  ENV%>%
    left_join(rainff, by="codeplot")%>%
    mutate(PTotY.mean = coalesce(PTotY.mean.y, PTotY.mean.x)) %>%
    select(-PTotY.mean.y, -PTotY.mean.x)->ENV 

  #Tairmean
  SafranPyr%>%
    group_by(id_plot)%>%
    summarise(TMeanY.mean=mean(TairMean))%>%
    rename(codeplot=id_plot)-> Tairf
  ENV%>%
    left_join(Tairf, by="codeplot")%>%
    mutate(TMeanY.mean = coalesce(TMeanY.mean.y, TMeanY.mean.x)) %>%
    select(-TMeanY.mean.y, -TMeanY.mean.x)->ENV 



  
#Save ENV--------------
  write.csv(ENV, file = paste0("data/derived-data/Envir/ENV_" , as.character(Sys.Date()) , ".csv"))

  
  #PREPARATION DE PHYTO
  read.csv("data/raw-data/envir/phyto.data3.csv",header=T, sep=",")->Phyto0
  read.csv("data/raw-data/envir/Julien/PhytoPyrenees.csv",header=T, sep=";")->Phyto1
  Phyto0%>%
    bind_rows(Phyto1)->Phyto
  write.csv(Phyto, file = paste0("data/derived-data/Envir/Phyto",".csv"))   
  