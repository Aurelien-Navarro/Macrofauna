#GDM affiné 

#Script pour insérer les différentes conditions au GDM et le produire 

#librairies
librarian::shelf(dplyr,vegan, ggplot2, betapart, gdm, tibble, tidyverse, esquisse)

#Importation des données
read.csv("data/derived-data/Envir/ENV_2023-05-23.csv", row.names = 1)->ENV
ENV%>%
  filter(!codeplot %in% c("BOU","CAU"))->ENV
read.csv("data/derived-data/Traits/traits_homo_2023-04-27.csv",header=T, sep=",")->TRAITS
read.csv("data/raw-data/envir/phyto.data3.csv",header=T, sep=",")->Phyto
Phyto%>%
  filter(!codeplot %in% c("BOU","CAU"))->Phyto
read.csv("data/derived-data/Esp/clean_data_2023-05-10.csv",header=T, sep=",")->ESP
ESP%>%
  filter(!gradient %in% c("BOU","CAU"))->ESP

#Donnes traits deja triees par 2.Indice Computation Guilde
read.csv("data/derived-data/Traits/detriti/detrialphaplot_2023-05-24.csv", h=T, sep=",")->DetritiT
read.csv("data/derived-data/Traits/predat/predatalphaplot_2023-05-24.csv", h=T, sep=",")->PredatT
read.csv("data/derived-data/Traits/herbi/herbialphaplot_2023-05-24.csv", h=T, sep=",")->herbiT

#Fonctions 
source("analyses/functions/my_gdm_function.R")
source("analyses/functions/my_gdm_function_TRAITS.R")
#GDM ESPECE------

  ##HERBIVORES----
  GDM_HERBI_ESP<-my_gdm_function(ENV=ENV,
                            COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                            PHYTO=Phyto,
                            Methode= "barber", #trop de NAs pour incorporer
                            Variables = c("id_plot","ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                           "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                          "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                          "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                          "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                          "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))

#J ai du selectionner des variables car certaines ont trop de Na
#et le modèle se retrouve avec des valeurs négatives qu'il ne peut traiter 

  
##PREDATEURS----
  GDM_PREDAT_ESP<-my_gdm_function(ENV=ENV,
                                COMM=ESP[ESP$familyName=="Carabidae",],
                                PHYTO=Phyto,
                                Methode= "barber",
                                Variables = c("id_plot","ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                              "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                              "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                              "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                              "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                              "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
    
  ##DECOMPOSEURS----
GDM_DECOMPO_ESP<-my_gdm_function(ENV=ENV,
                                COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                 ESP$className %in% c("Diplopoda","Clitellata")|
                                                 ESP$familyName %in% "Geotrupidae",],
                                PHYTO=Phyto,
                                Methode= c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                Variables = c("id_plot","ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                              "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                              "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                              "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                              "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                              "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
  
##PARASITES----
GDM_PARA_ESP<-my_gdm_function(ENV=ENV,
                              COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                              PHYTO=Phyto,
                              Methode= "barber", 
                              Variables = c("id_plot","ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                            "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                            "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                            "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                            "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                            "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
    



#GDM TRAITS------

  ##HERBIVORES----

GDM_HERBI_TRAITS<-my_gdm_function_TRAITS(ENV=ENV,
                               COMM=herbiT,
                               PHYTO=Phyto,
                               Variables = c("id_plot","ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                             "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                             "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                             "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                             "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                             "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))


  ##PREDATEURS----
GDM_PREDA_TRAITS<-my_gdm_function_TRAITS(ENV=ENV,
                                         COMM=PredatT,
                                         PHYTO=Phyto,
                                         Variables = c("id_plot","ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                       "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                       "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                       "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                       "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                       "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))


  ##DECOMPOSEURS----
GDM_DETRITI_TRAITS<-my_gdm_function_TRAITS(ENV=ENV,
                                         COMM=DetritiT,
                                         PHYTO=Phyto,
                                         Variables = c("id_plot","ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                       "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                       "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                       "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                       "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                       "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))






