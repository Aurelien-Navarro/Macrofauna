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

#----------------


#PARAMETRES PHYSIQUES-----

 ##TEMPERATURES----------
  ##Temperature moyenne de 2017-2018-2019 par site et par altitude
  str(Temperatures$X2019.12)
Temperatures %>%
  select(contains(c("2019","2018","2017"))) %>%
  rowMeans()->Temperatures$Tmean
Temperatures%>%
  select(c("nom", "idsite", "codeplot", "idplot", "Tmean"))->Temp_final
  

  ##PRECIPIATION------
  ##Precipitations moyennes par site et par altitude

Tmean_rainf1%>%
    filter(year>="2017")%>%
    na.omit(Rainf)%>%
    group_by(site, alti)%>%
    summarise(Pmean=mean(Rainf))-> Rainfall_final
  ##Creation d'une colone codeplot comme pour la temperature
paste(Rainfall_final$site, Rainfall_final$alti, sep="_")->Rainfall_final$codeplot

  ##Concatenation des deux
left_join(Temp_final, Rainfall_final, by = "codeplot")%>%
  select(c(codeplot, Tmean, Pmean))->ENV


#PARAMETRES BIO------


 ##Richesse veg------
  ###RS------
RS_veg_final<-Phyto%>%
  na.omit(lb_nom)%>%
  group_by(codeplot)%>%
  distinct(lb_nom)%>%
  summarize(Rveg=n())

  ###rajout de la Richesse veg au tableau final
  left_join(ENV, RS_veg_final, by = "codeplot")%>%
    select(c(codeplot,
             Tmean,
             Pmean,
             Rveg))->ENV
  
  
  ##Caracteristiques arbres--------------
  TF_arbres_final<-Arbres%>%
    na.omit(diam)%>%
    filter(!grepl("0", diam))%>%
    select(c(codeplot, diam))%>%
    group_by(codeplot)%>%
    summarise(Diam_mean=mean(diam))
  
  ##rajout du diametre arbre au tableau environnement
  left_join(ENV, TF_arbres_final, by = "codeplot")%>%
    select(c(codeplot,
             Tmean,
             Pmean,
             Rveg,
             Diam_mean))->ENV
  
  
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
  left_join(ENV, Habit_final, by = "codeplot")->ENV
  

#Save ENV--------------
  write.csv(ENV, file = paste0("data/derived-data/ENV_" , as.character(Sys.Date()) , ".csv"))
