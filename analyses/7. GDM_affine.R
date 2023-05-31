#GDM affiné 

#Script pour insérer les différentes conditions au GDM et le produire 
#J ai du selectionner des variables car certaines ont trop de NA
#et le modèle se retrouve avec des valeurs négatives qu'il ne peut traiter 


#librairies
librarian::shelf(dplyr,vegan, ggplot2, betapart, gdm, tibble, tidyverse, esquisse)

#Importation des données
read.csv("data/derived-data/Envir/ENV_2023-05-23.csv", row.names = 1)->ENV
ENV%>%
  filter(grepl("ARG|ARM|RIS|VCHA|VAL|PEC|MOU|VTN|TAN", codeplot))->ENV
read.csv("data/raw-data/envir/phyto.data3.csv",header=T, sep=",")->Phyto
Phyto%>%
  filter(grepl("ARG|ARM|RIS|VCHA|VAL|PEC|MOU|VTN|TAN", codeplot))->Phyto
read.csv("data/derived-data/Esp/clean_data_2023-05-30.csv",header=T, sep=",")->ESP
ESP%>%
  filter(grepl("ARG|ARM|RIS|VCHA|VAL|PEC|MOU|VTN|TAN", gradient))->ESP
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
                                            Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                                          "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                          "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                                          "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                          "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))

as.vector(GDM_HERBI_ESP_EchSample$coefficients)->H1
tapply(H1, ceiling(seq_along(H1)/3), sum)->coefH_sample



  
  
GDM_HERBI_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                                              COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                                              PHYTO=Phyto,
                                              Methode= "barber",
                                              Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                                            "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                            "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                                            "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                            "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
 
  
  

as.vector(GDM_HERBI_ESP_EchPlot$coefficients)->H2
  tapply(H2, ceiling(seq_along(H2)/3), sum)->coefH_plot
  
GDM_HERBI_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                              COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                                              PHYTO=Phyto,
                                              Methode= "barber",
                                              ECHELLE = Echelle,
                                              Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                                            "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                            "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                                            "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                            "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
plot(GDM_HERBI_ESP_EchGrad)  
as.vector(GDM_HERBI_ESP_EchGrad$coefficients)->H3
  tapply(H3, ceiling(seq_along(H3)/3), sum)->coefH_grad
  
  
  
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
                                                Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                                              "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                              "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                                              "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                              "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   
   
  
   as.vector(GDM_PREDAT_ESP_EchSamp$coefficients)->Pr1
   tapply(Pr1, ceiling(seq_along(Pr1)/3), sum)->coefPr_sample
   
   
   
   
   GDM_PREDAT_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                                COMM=ESP[ESP$familyName=="Carabidae",],
                                PHYTO=Phyto,
                                Methode= "barber",
                                Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                              "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                              "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                              "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                              "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   
   
   
   as.vector(GDM_PREDAT_ESP_EchPlot$coefficients)->Pr2
   tapply(Pr2, ceiling(seq_along(Pr2)/3), sum)->coefPr_plot
   
   
   GDM_PREDAT_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                                 COMM=ESP[ESP$familyName=="Carabidae",],
                                                 PHYTO=Phyto,
                                                 Methode= "barber",
                                                 ECHELLE = Echelle,
                                                 Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                                               "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                               "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                                               "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                               "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   
 plot(GDM_PREDAT_ESP_EchGrad)
   as.vector(GDM_PREDAT_ESP_EchGrad$coefficients)->Pr3
   tapply(Pr3, ceiling(seq_along(Pr3)/3), sum)->coefPr_grad
   
    ##DECOMPOSEURS----
   GDM_DECOMPO_ESP_EchSamp<-my_gdm_function_SAMP(ENV=ENV,
                                                  COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                              ESP$className %in% c("Diplopoda","Clitellata")|
                                                              ESP$familyName %in% "Geotrupidae",],
                                                PHYTO=Phyto,
                                                ECHELLE= Echelle,
                                                Methode = c("tri manuel","tri manuel qualitatif","chasse a vue"),    
                                                Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                                              "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                              "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                                              "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                              "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean",
                                                              "Nmean","Almean","Argilemean","Calcmean","Camean","Fermean",
                                                              "Limfinmean","Limgrosmean","mgmean","Mnmean","Phosmean","Kmean",
                                                              "Sablefinmean","Sablegrosmean","Namean","Milieu","Alt","X_L93","Y_L93"))
                                                
   
   as.vector(GDM_DECOMPO_ESP_EchSamp$coefficients)->D1
   tapply(D1, ceiling(seq_along(D1)/3), sum)->coefDecompo_ech
   
DM_DECOMPO_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                                COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                 ESP$className %in% c("Diplopoda","Clitellata")|
                                                 ESP$familyName %in% "Geotrupidae",],
                                PHYTO=Phyto,
                                Methode= c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                               "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                               "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                               "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                               "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean",
                                               "Nmean","Almean","Argilemean","Calcmean","Camean","Fermean",
                                               "Limfinmean","Limgrosmean","mgmean","Mnmean","Phosmean","Kmean",
                                               "Sablefinmean","Sablegrosmean","Namean","Milieu","Alt","X_L93","Y_L93"))


as.vector(GDM_DECOMPO_ESP_EchPlot$coefficients)->D2
tapply(D2, ceiling(seq_along(D2)/3), sum)->coefDecompo_plot

GDM_DECOMPO_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                              COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                          ESP$className %in% c("Diplopoda","Clitellata")|
                                                          ESP$familyName %in% "Geotrupidae",],
                                              PHYTO=Phyto,
                                              ECHELLE= Echelle,
                                              Methode= c("tri manuel","chasse à vue","tri manuel qualitatif"),
                                              Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                                             "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                             "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                                             "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                             "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean",
                                                             "Nmean","Argilemean","Limfinmean","Limgrosmean",
                                                             "Sablefinmean","Sablegrosmean","Milieu","Alt","X_L93","Y_L93"))
plot(GDM_DECOMPO_ESP_EchGrad)   
as.vector(GDM_DECOMPO_ESP_EchGrad$coefficients)->D3
   tapply(D3, ceiling(seq_along(D3)/3), sum)->coefDecompo_grad
##PARASITES----
   GDM_PARA_ESP_EchSamp<-my_gdm_function_SAMP(ENV=ENV,
                                              COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                                              PHYTO=Phyto,
                                              Methode= "barber",
                                              ECHELLE = Echelle,
                                              Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                                             "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                             "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                                             "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                             "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
   plot(GDM_PARA_ESP_EchSamp)
   as.vector(GDM_PARA_ESP_EchSamp$coefficients)->Pa1
   tapply(Pa1, ceiling(seq_along(Pa1)/3), sum)->coefPara_samp
   
GDM_PARA_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                              COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                              PHYTO=Phyto,
                              Methode= "barber", 
                              Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                             "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                             "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                             "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                             "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
    
as.vector(GDM_PARA_ESP_EchPlot$coefficients)->Pa2
tapply(Pa2, ceiling(seq_along(Pa2)/3), sum)->coefPara_plot

GDM_PARA_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                           COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                                           PHYTO=Phyto,
                                           Methode= "barber",
                                           ECHELLE=Echelle,
                                           Variables = c("ndvi.mean","GDD_10cm.sum.mean",       
                                                         "CWD.sum.mean","FDD_10cm.sum.mean","solar.radiation.sum.mean",
                                                         "DRT.air.mean","TMeanY.mean","TSeason.mean",
                                                         "PTotY.mean","PSeason.mean","Tmean","Pmean",                   
                                                         "Rveg","NDVImin","NDVImax","PRCTMOmean","pHmean","Milieu","Alt","X_L93","Y_L93"))
plot(GDM_PARA_ESP_EchGrad)
as.vector(GDM_PARA_ESP_EchGrad$coefficients)->Pa3
tapply(Pa3, ceiling(seq_along(Pa3)/3), sum)->coefPara_grad

#####PLOT######




##Comparaison entre guildes 
#sample
t1<-data.frame(Predictors = GDM_HERBI_ESP_EchSample$predictors,
               Valeur = coefH_sample)
t2<-data.frame(Predictors= GDM_PREDAT_ESP_EchSamp$predictors,
               Valeur = coefPr_sample)
t3<-data.frame(Predictors = GDM_DECOMPO_ESP_EchSamp$predictors,
               Valeur = coefDecompo_ech)
t4<-data.frame(Predictors =GDM_PARA_ESP_EchSamp$predictors,
               Valeur = coefPara_samp)
#parcelle
t5<-data.frame(Predictors = GDM_HERBI_ESP_EchPlot$predictors, 
               Valeur = coefH_plot)
t6<-data.frame(Predictors = GDM_PREDAT_ESP_EchPlot$predictors, 
               Valeur = coefPr_plot)
t7<-data.frame(Predictors = GDM_DECOMPO_ESP_EchPlot$predictors, 
               Valeur = coefDecompo_plot)
t8<-data.frame(Predictors = GDM_PARA_ESP_EchPlot$predictors, 
               Valeur = coefPara_plot)
#gradient
t9<-data.frame(Predictors=GDM_HERBI_ESP_EchGrad$predictors, 
               Valeur = coefH_grad)
t10<-data.frame(Predictors=GDM_PREDAT_ESP_EchGrad$predictors, 
                Valeur = coefPr_grad)
t11<-data.frame(Predictors=GDM_DECOMPO_ESP_EchGrad$predictors, 
                   Valeur = coefDecompo_grad)
t12<-data.frame(Predictors=GDM_PARA_ESP_EchGrad$predictors, 
                Valeur = coefPara_grad)


Tbis<-t1
#Le plot 
sorted_indices <- order(-Tbis$Valeur)
Tbis <- Tbis[sorted_indices, c("Predictors", "Valeur")]
Tbis<-Tbis[1:3,] 


ggplot(Tbis, aes(x = Predictors, y = Valeur, fill = Predictors)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Trophic Guild", y = "Coefficient") +
  scale_fill_hue(c = 40) +
  geom_text(aes(label = Predictors), position = position_dodge(width = 1), vjust = -0.5)
  theme_minimal()


#####LE PLOT DES SPLINES######

#Herbivores
t8<-tibble(Ecological_Distance = GDM_HERBI_ESP_EchSample$ecological,
             Observed_Dissimilarity = GDM_HERBI_ESP_EchSample$observed )
t9<-tibble(Ecological_Distance = GDM_HERBI_ESP_EchPlot$ecological,
           Observed_Dissimilarity = GDM_HERBI_ESP_EchPlot$observed )
t10<-tibble(Ecological_Distance = GDM_HERBI_ESP_EchGrad$ecological,
            Observed_Dissimilarity = GDM_HERBI_ESP_EchGrad$observed )
#Carnivores
t11<-tibble(Ecological_Distance = GDM_PREDAT_ESP_EchSamp$ecological,
            Observed_Dissimilarity = GDM_PREDAT_ESP_EchSamp$observed )
t12<-tibble(Ecological_Distance = GDM_PREDAT_ESP_EchPlot$ecological,
            Observed_Dissimilarity = GDM_PREDAT_ESP_EchPlot$observed )
t13<-tibble(Ecological_Distance = GDM_PREDAT_ESP_EchGrad$ecological,
            Observed_Dissimilarity = GDM_PREDAT_ESP_EchGrad$observed )
#Detritivores
t14<-tibble(Ecological_Distance = GDM_DECOMPO_ESP_EchSamp$ecological,
            Observed_Dissimilarity = GDM_DECOMPO_ESP_EchSamp$observed )
t15<-tibble(Ecological_Distance = GDM_DECOMPO_ESP_EchPlot$ecological,
            Observed_Dissimilarity = GDM_DECOMPO_ESP_EchPlot$observed )
t16<-tibble(Ecological_Distance = GDM_DECOMPO_ESP_EchGrad$ecological,
            Observed_Dissimilarity = GDM_DECOMPO_ESP_EchGrad$observed )
#Parasits
t17<-tibble(Ecological_Distance = GDM_PARA_ESP_EchSamp$ecological,
            Observed_Dissimilarity = GDM_PARA_ESP_EchSamp$observed )
t18<-tibble(Ecological_Distance = GDM_PARA_ESP_EchPlot$ecological,
            Observed_Dissimilarity = GDM_PARA_ESP_EchPlot$observed )
t19<-tibble(Ecological_Distance = GDM_PARA_ESP_EchGrad$ecological,
            Observed_Dissimilarity = GDM_PARA_ESP_EchGrad$observed )


#Comparaison entre echelles
par(mfrow = c(2,2))

plot(GDM_DECOMPO_ESP_EchSamp)


