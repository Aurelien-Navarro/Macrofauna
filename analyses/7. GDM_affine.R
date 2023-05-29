#GDM affiné 

#Script pour insérer les différentes conditions au GDM et le produire 
#J ai du selectionner des variables car certaines ont trop de NA
#et le modèle se retrouve avec des valeurs négatives qu'il ne peut traiter 


#librairies
librarian::shelf(dplyr,vegan, ggplot2, betapart, gdm, tibble, tidyverse, esquisse)

#Importation des données
read.csv("data/derived-data/Envir/ENV_2023-05-23.csv", row.names = 1)->ENV
ENV%>%
  filter(!codeplot %in% c("BOU","CAU"))->ENV
read.csv("data/raw-data/envir/phyto.data3.csv",header=T, sep=",")->Phyto
Phyto%>%
  filter(!codeplot %in% c("BOU","CAU"))->Phyto
read.csv("data/derived-data/Esp/clean_data_2023-05-10.csv",header=T, sep=",")->ESP
ESP%>%
  filter(!gradient %in% c("BOU","CAU"))->ESP
read.csv("data/derived-data/ECHELLE2023-05-25.csv",header =T, sep=",")->Echelle

#Donnes traits deja triees par 2.Indice Computation Guilde
read.csv("data/derived-data/Traits/detriti/detrialphaplot_2023-05-24.csv", h=T, sep=",")->DetritiT
read.csv("data/derived-data/Traits/predat/predatalphaplot_2023-05-24.csv", h=T, sep=",")->PredatT
read.csv("data/derived-data/Traits/herbi/herbialphaplot_2023-05-24.csv", h=T, sep=",")->herbiT

#Fonctions 
source("analyses/functions/my_gdm_function_GRAD.R")
source("analyses/functions/my_gdm_function_PLOT.R")
source("analyses/functions/my_gdm_function_MASSIF.R")
source("analyses/functions/my_gdm_function_SAMP.R")
source("analyses/functions/my_gdm_function_TRAITS.R")
#GDM ESPECE------

  ##HERBIVORES----
GDM_HERBI_ESP_EchSample<-my_gdm_function_SAMP(ENV=ENV,
                                            COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                                            PHYTO=Phyto,
                                            Methode= "barber",
                                            ECHELLE = Echelle,
                                            Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                          "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                          "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                          "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                          "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                          "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))


as.vector(GDM_HERBI_ESP_EchSample$coefficients)->H1
tapply(H1, ceiling(seq_along(V1)/3), sum)->coefH_sample



  
  
GDM_HERBI_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                                              COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                                              PHYTO=Phyto,
                                              Methode= "barber",
                                              Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                            "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                            "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                            "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                            "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                            "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
 
  
  
as.vector(GDM_HERBI_ESP_EchPlot$coefficients)->H2
  tapply(H2, ceiling(seq_along(V1)/3), sum)->coefH_plot
  
  GDM_HERBI_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                              COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                                              PHYTO=Phyto,
                                              Methode= "barber",
                                              ECHELLE = Echelle,
                                              Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                            "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                            "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                            "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                            "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                            "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
  as.vector(GDM_HERBI_ESP_EchGrad$coefficients)->H3
  tapply(H3, ceiling(seq_along(V1)/3), sum)->coefH_grad
  
  
  
  #Ne fonctionne pas à l'échelle du massif car par de donnes Env des 
  #pyrennees donc comparaison deux à deux de 2 sites inutile 
   GDM_HERBI_ESP_EchMass<-my_gdm_function_MASSIF(ENV=ENV,
                                              COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                                              PHYTO=Phyto,
                                              Methode= "barber",
                                              ECHELLE = Echelle,
                                              Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                            "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                            "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                            "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                            "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                            "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))

   
   
     
   ##PREDATEURS----
   GDM_PREDAT_ESP_EchSamp<-my_gdm_function_SAMP(ENV=ENV,
                                                COMM=ESP[ESP$familyName=="Carabidae",],
                                                PHYTO=Phyto,
                                                Methode= "barber",
                                                ECHELLE= Echelle,
                                                Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                              "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                              "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                              "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                              "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                              "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   
   
  
   as.vector(GDM_PREDAT_ESP_EchSamp$coefficients)->Pr1
   tapply(Pr1, ceiling(seq_along(V1)/3), sum)->coefPr_sample
   
   
   
   
   GDM_PREDAT_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                                COMM=ESP[ESP$familyName=="Carabidae",],
                                PHYTO=Phyto,
                                Methode= "barber",
                                Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                              "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                              "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                              "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                              "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                              "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   
   
   
   as.vector(GDM_PREDAT_ESP_EchPlot$coefficients)->Pr2
   tapply(Pr2, ceiling(seq_along(V1)/3), sum)->coefPr_plot
   
   
   GDM_PREDAT_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                                 COMM=ESP[ESP$familyName=="Carabidae",],
                                                 PHYTO=Phyto,
                                                 Methode= "barber",
                                                 ECHELLE = Echelle,
                                                 Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                               "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                               "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                               "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                               "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                               "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   
 
   as.vector(GDM_PREDAT_ESP_EchGrad$coefficients)->Pr3
   tapply(Pr3, ceiling(seq_along(V1)/3), sum)->coefPr_grad
   
    ##DECOMPOSEURS----
   GDM_DECOMPO_ESP_EchSamp<-my_gdm_function_SAMP(ENV=ENV,
                                            COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                        ESP$className %in% c("Diplopoda","Clitellata")|
                                                        ESP$familyName %in% "Geotrupidae",],
                                            PHYTO=Phyto,
                                            ECHELLE= Echelle,
                                            Methode= c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                            Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                          "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                          "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                          "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                          "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                          "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   
   as.vector(GDM_DECOMPO_ESP_EchSamp$coefficients)->D1
   tapply(D1, ceiling(seq_along(V1)/3), sum)->coefDecompo_ech
   
GDM_DECOMPO_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                                COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                 ESP$className %in% c("Diplopoda","Clitellata")|
                                                 ESP$familyName %in% "Geotrupidae",],
                                PHYTO=Phyto,
                                Methode= c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                              "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                              "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                              "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                              "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                              "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))


as.vector(GDM_DECOMPO_ESP_EchPlot$coefficients)->D2
tapply(D2, ceiling(seq_along(V1)/3), sum)->coefDecompo_plot

GDM_DECOMPO_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                              COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                          ESP$className %in% c("Diplopoda","Clitellata")|
                                                          ESP$familyName %in% "Geotrupidae",],
                                              PHYTO=Phyto,
                                              ECHELLE= Echelle,
                                              Methode= c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                              Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                            "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                            "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                            "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                            "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                            "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   as.vector(GDM_DECOMPO_ESP_EchGrad$coefficients)->D3
   tapply(D3, ceiling(seq_along(V1)/3), sum)->coefDecompo_grad
##PARASITES----
   GDM_PARA_ESP_EchSamp<-my_gdm_function_SAMP(ENV=ENV,
                                              COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                                              PHYTO=Phyto,
                                              Methode= "barber",
                                              ECHELLE = Echelle,
                                              Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                            "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                            "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                            "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                            "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                            "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   
   as.vector(GDM_PARA_ESP_EchSamp$coefficients)->Pa1
   tapply(Pa1, ceiling(seq_along(V1)/3), sum)->coefPara_samp
   
GDM_PARA_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                              COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                              PHYTO=Phyto,
                              Methode= "barber", 
                              Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                            "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                            "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                            "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                            "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                            "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
    
as.vector(GDM_PARA_ESP_EchPlot$coefficients)->Pa2
tapply(Pa2, ceiling(seq_along(V1)/3), sum)->coefPara_plot

GDM_PARA_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                           COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                                           PHYTO=Phyto,
                                           Methode= "barber",
                                           ECHELLE=Echelle,
                                           Variables = c("ndvi.mean","GDD_1cm.sum.mean","GDD_10cm.sum.mean",       
                                                         "CWD.sum.mean","FDD_1cm.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                         "DSN_T_ISBA.mean","TG1.degres.mean","TG4.degres.mean",
                                                         "DRT.air.mean","TMeanY.mean","TMeanRngD.mean","TSeason.mean",
                                                         "TRngY.mean","PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                         "Rveg", "PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))

as.vector(GDM_PARA_ESP_EchGrad$coefficients)->Pa3
tapply(Pa3, ceiling(seq_along(V1)/3), sum)->coefPara_grad

#####PLOT######

##Comparaison entre guildes 
#sample
t1<-data.frame(Predictors = GDM_HERBI_ESP_echSample$predictors, 
       coefPr_sample,coefPara_samp,coefH_sample)
#parcelle
t2<-data.frame(Predictors = GDM_HERBI_ESP_EchPlot$predictors, 
           coefPr_plot,coefPara_plot,coefH_plot,coefDecompo_plot)
#gradient
t3<-tibble(Predictors=GDM_HERBI_ESP_echSample$predictors, 
       coefPr_grad,coefPara_grad, coefDecompo_grad)


#Le plot 
Tbis <- tidyr::gather(t3, guildetrophique, valeur, -Predictors)
Tbis <- Tbis %>%
  group_by(guildetrophique) %>%
  slice_max(n = 3, order_by = valeur) %>%
  ungroup()%>%
  filter(valeur>0)


ggplot(Tbis, aes(x = guildetrophique, y = valeur, fill = Predictors)) +
  geom_bar(stat = "identity") +
  labs(x = "Predictors", y = "Coefficient") +
  scale_fill_hue(l = 40) 
theme_minimal()

##Comparaison entre echelles
#Herbi
t4<-tibble(Predictors=GDM_HERBI_ESP_echSample$predictors, 
           coefH_sample, coefH_plot)
#Preda
t5<-tibble(Predictors=GDM_HERBI_ESP_echSample$predictors, 
           coefPr_sample, coefPr_plot, coefPr_grad)
#Detriti
t6<-tibble(Predictors=GDM_HERBI_ESP_echSample$predictors, 
           coefDecompo_grad, coefDecompo_plot)
#Parasites
t7<-tibble(Predictors=GDM_HERBI_ESP_echSample$predictors, 
           coefPara_samp, coefPara_plot, coefPara_grad)

#Le plot 
Tbis <- tidyr::gather(t7, echelleetude, valeur, -Predictors)
Tbis <- Tbis %>%
  group_by(echelleetude) %>%
  slice_max(n = 3, order_by = valeur) %>%
  ungroup()%>%
  filter(valeur>0)


ggplot(Tbis, aes(x = echelleetude, y = valeur, fill = Predictors)) +
  geom_bar(stat = "identity") +
  labs(x = "Scale of study", y = "Coefficient") +
  scale_fill_hue(l = 40) 
theme_minimal()












