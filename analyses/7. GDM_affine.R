#GDM affiné 

#Script pour insérer les différentes conditions au GDM et le produire 
#J ai du selectionner des variables car certaines ont trop de NA
#et le modèle se retrouve avec des valeurs négatives qu'il ne peut traiter 


#librairies
librarian::shelf(dplyr,vegan, ggplot2, betapart, gdm, tibble, tidyverse, esquisse, gridExtra)

#Importation des données
read.csv("data/derived-data/Envir/ENV_2023-06-01.csv", row.names = 1)->ENV
ENV%>%
  filter(grepl("ARG|ARM|RIS|VCHA|VAL|PEC|MOU|VTN|TAN|MSB|VER", codeplot))->ENV
read.csv("data/derived-data/Envir/Phyto.csv",header=T, sep=",")->Phyto
Phyto%>%
  filter(grepl("ARG|ARM|RIS|VCHA|VAL|PEC|MOU|VTN|TAN|MSB|VER", codeplot))->Phyto
read.csv("data/derived-data/Esp/clean_data_2023-05-30.csv",header=T, sep=",")->ESP
ESP%>%
  filter(grepl("ARG|ARM|RIS|VCHA|VAL|PEC|MOU|VTN|TAN|MSB|VER", gradient))->ESP
read.csv("data/derived-data/ECHELLE2023-05-25.csv",header =T, sep=",")->Echelle
read.csv("data/derived-data/Envir/resp.csv",header=T, sep=",")->RICHSPEANIMO

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
                                            IDRESO = "Espèce",
                                            ECHELLE = Echelle,
                                            ANIMO = RICHSPEANIMO,
                                            Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                                          "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                          "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                          "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean", "Resp"))

as.vector(GDM_HERBI_ESP_EchSample$coefficients)->H1
tapply(H1, ceiling(seq_along(H1)/3), sum)->coefH_sample



  
  
GDM_HERBI_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                                              COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                                              PHYTO=Phyto,
                                              Methode= "barber",
                                              IDRESO = "Espèce",
                                              ANIMO= RICHSPEANIMO,
                                              Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                                            "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                            "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                            "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean", "Resp"))
 
  
  

as.vector(GDM_HERBI_ESP_EchPlot$coefficients)->H2
  tapply(H2, ceiling(seq_along(H2)/3), sum)->coefH_plot
  
GDM_HERBI_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                              COMM=ESP[ESP$orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae",],
                                              PHYTO=Phyto,
                                              Methode= "barber",
                                              IDRESO = "Espèce",
                                              ECHELLE = Echelle,
                                              ANIMO = RICHSPEANIMO,
                                              Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                                            "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                            "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                            "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean", "Resp"))

as.vector(GDM_HERBI_ESP_EchGrad$coefficients)->H3
  tapply(H3, ceiling(seq_along(H3)/3), sum)->coefH_grad
  
  

   

   
   
     
   ##PREDATEURS----
   GDM_PREDAT_ESP_EchSamp<-my_gdm_function_SAMP(ENV=ENV,
                                                COMM=ESP[ESP$familyName=="Carabidae",],
                                                PHYTO=Phyto,
                                                Methode= "barber",
                                                IDRESO = "Espèce",
                                                ECHELLE= Echelle,
                                                ANIMO = RICHSPEANIMO,
                                                Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                                              "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                              "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                              "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean", "Resp"))
   
   
  
   as.vector(GDM_PREDAT_ESP_EchSamp$coefficients)->Pr1
   tapply(Pr1, ceiling(seq_along(Pr1)/3), sum)->coefPr_sample
   
   
   
   
   GDM_PREDAT_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                                COMM=ESP[ESP$familyName=="Carabidae",],
                                PHYTO=Phyto,
                                IDRESO = "Espèce",
                                Methode= "barber",
                                ANIMO = RICHSPEANIMO,
                                Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                              "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                              "pHmean","Milieu","Alt","X_L93","Y_L93",
                                              "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean","Resp"))
   
   
   
   as.vector(GDM_PREDAT_ESP_EchPlot$coefficients)->Pr2
   tapply(Pr2, ceiling(seq_along(Pr2)/3), sum)->coefPr_plot
   
   
   GDM_PREDAT_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                                 COMM=ESP[ESP$familyName=="Carabidae",],
                                                 PHYTO=Phyto,
                                                IDRESO = "Espèce",
                                                 Methode= "barber",
                                                 ECHELLE = Echelle,
                                                ANIMO = RICHSPEANIMO,
                                                 Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                                               "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                               "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                               "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean","Resp"))
   

   as.vector(GDM_PREDAT_ESP_EchGrad$coefficients)->Pr3
   tapply(Pr3, ceiling(seq_along(Pr3)/3), sum)->coefPr_grad
   
    ##DECOMPOSEURS----
   GDM_DECOMPO_ESP_EchSamp<-my_gdm_function_SAMP(ENV=ENV,
                                                  COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                              ESP$className %in% c("Diplopoda","Clitellata")|
                                                              ESP$familyName %in% "Geotrupidae",],
                                                PHYTO=Phyto,
                                                IDRESO = "Espèce",
                                                ECHELLE= Echelle,
                                                ANIMO = RICHSPEANIMO,
                                                Methode = c("tri manuel","tri manuel qualitatif","chasse a vue","barber"),    
                                                Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean","Resp",                
                                                              "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                              "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                              "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean",
                                                              "Nmean","Almean","Argilemean","Calcmean","Camean","Fermean",
                                                              "Limfinmean","Limgrosmean","mgmean","Mnmean","Phosmean","Kmean",
                                                              "Sablefinmean","Sablegrosmean","Namean"))
                                                
   
   as.vector(GDM_DECOMPO_ESP_EchSamp$coefficients)->D1
   tapply(D1, ceiling(seq_along(D1)/3), sum)->coefDecompo_ech
   
GDM_DECOMPO_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                                COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                 ESP$className %in% c("Diplopoda","Clitellata")|
                                                 ESP$familyName %in% "Geotrupidae",],
                                PHYTO=Phyto,
                                IDRESO = "Espèce",
                                ANIMO = RICHSPEANIMO,
                                Methode= c("tri manuel","chasse à vue","tri manuel qualitatif","barber"),
                                Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                              "Rveg","NDVImin","NDVImax","PRCTMOmean","Resp",
                                              "pHmean","Milieu","Alt","X_L93","Y_L93",
                                              "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean",
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
                                              IDRESO = "Espèce",
                                              ECHELLE= Echelle,
                                              ANIMO = RICHSPEANIMO,
                                              Methode= c("tri manuel","chasse à vue","tri manuel qualitatif","barber"),
                                              Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean", "Resp",               
                                                            "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                            "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                            "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean",
                                                            "Nmean","Argilemean",
                                                            "Limfinmean","Limgrosmean",
                                                            "Sablefinmean","Sablegrosmean","Milieu","Alt","X_L93","Y_L93"))
  
as.vector(GDM_DECOMPO_ESP_EchGrad$coefficients)->D3
   tapply(D3, ceiling(seq_along(D3)/3), sum)->coefDecompo_grad
##PARASITES----
   GDM_PARA_ESP_EchSamp<-my_gdm_function_SAMP(ENV=ENV,
                                              COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                                              PHYTO=Phyto,
                                              IDRESO = "Genre",
                                              Methode= "barber",
                                              ECHELLE = Echelle,
                                              ANIMO = RICHSPEANIMO,
                                              Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                                            "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                            "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                            "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))

   as.vector(GDM_PARA_ESP_EchSamp$coefficients)->Pa1
   tapply(Pa1, ceiling(seq_along(Pa1)/3), sum)->coefPara_samp
   
GDM_PARA_ESP_EchPlot<-my_gdm_function_PLOT(ENV=ENV,
                              COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                              PHYTO=Phyto,
                              IDRESO = "Genre",
                              Methode= "barber", 
                              ANIMO = RICHSPEANIMO,
                              Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                            "Rveg","NDVImin","NDVImax","PRCTMOmean","Resp",
                                            "pHmean","Milieu","Alt","X_L93","Y_L93",
                                            "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))
    
as.vector(GDM_PARA_ESP_EchPlot$coefficients)->Pa2
tapply(Pa2, ceiling(seq_along(Pa2)/3), sum)->coefPara_plot

GDM_PARA_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                           COMM= ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",],
                                           PHYTO=Phyto,
                                           Methode= "barber",
                                           IDRESO = "Genre",
                                           ANIMO = RICHSPEANIMO,
                                           ECHELLE=Echelle,
                                           Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean","Resp",              
                                                         "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                         "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                         "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))

as.vector(GDM_PARA_ESP_EchGrad$coefficients)->Pa3
tapply(Pa3, ceiling(seq_along(Pa3)/3), sum)->coefPara_grad

#####PLOT######

##Comparaison entre guildes 
#sample
t1<-data.frame(Predictors = GDM_HERBI_ESP_EchSample$predictors,
               Valeur = coefH_sample, Guild= "Herbivores",Scale="Sample")
sorted_indices <- order(-t1$Valeur)
t1 <- t1[sorted_indices, c("Predictors", "Valeur","Guild", "Scale")]
t1<-t1[1:3,] 
t2<-data.frame(Predictors= GDM_PREDAT_ESP_EchSamp$predictors,
               Valeur = coefPr_sample, Guild = "Predators", Scale="Sample")
sorted_indices <- order(-t2$Valeur)
t2 <- t2[sorted_indices, c("Predictors", "Valeur", "Guild", "Scale")]
t2<-t2[1:3,] 
t3<-data.frame(Predictors = GDM_DECOMPO_ESP_EchSamp$predictors,
               Valeur = coefDecompo_ech, Guild = "Detritivores", Scale="Sample")
sorted_indices <- order(-t3$Valeur)
t3 <- t3[sorted_indices, c("Predictors", "Valeur","Guild", "Scale")]
t3<-t3[1:3,] 
t4<-data.frame(Predictors =GDM_PARA_ESP_EchSamp$predictors,
               Valeur = coefPara_samp, Guild="Parasitoids", Scale="Sample")
sorted_indices <- order(-t4$Valeur)
t4 <- t4[sorted_indices, c("Predictors", "Valeur","Guild", "Scale")]
t4<-t4[1:3,] 

Tsample<-bind_rows(t1,t2,t3,t4)
#parcelle
t5<-data.frame(Predictors = GDM_HERBI_ESP_EchPlot$predictors, 
               Valeur = coefH_plot, Guild="Herbivores", Scale="Plot")
sorted_indices <- order(-t5$Valeur)
t5 <- t5[sorted_indices, c("Predictors", "Valeur","Guild", "Scale")]
t5<-t5[1:3,] 
t6<-data.frame(Predictors = GDM_PREDAT_ESP_EchPlot$predictors, 
               Valeur = coefPr_plot, Guild="Predators", Scale="Plot")
sorted_indices <- order(-t6$Valeur)
t6 <- t6[sorted_indices, c("Predictors", "Valeur","Guild", "Scale")]
t6<-t6[1:3,] 
t7<-data.frame(Predictors = GDM_DECOMPO_ESP_EchPlot$predictors, 
               Valeur = coefDecompo_plot, Guild = "Detritivores", Scale="Plot")
sorted_indices <- order(-t7$Valeur)
t7 <- t7[sorted_indices, c("Predictors", "Valeur","Guild","Scale")]
t7<-t7[1:3,] 
t8<-data.frame(Predictors = GDM_PARA_ESP_EchPlot$predictors, 
               Valeur = coefPara_plot, Guild="Parasitoids", Scale="Plot")
sorted_indices <- order(-t8$Valeur)
t8 <- t8[sorted_indices, c("Predictors", "Valeur","Guild", "Scale")]
t8<-t8[1:3,] 
Tplot<-bind_rows(t5,t6,t7,t8)

#gradient
t9<-data.frame(Predictors=GDM_HERBI_ESP_EchGrad$predictors, 
               Valeur = coefH_grad, Guild="Herbivores", Scale="Site")
sorted_indices <- order(-t9$Valeur)
t9 <- t9[sorted_indices, c("Predictors", "Valeur","Guild","Scale")]
t9<-t9[1:3,] 
t10<-data.frame(Predictors=GDM_PREDAT_ESP_EchGrad$predictors, 
                Valeur = coefPr_grad, Guild="Predators", Scale="Site")
sorted_indices <- order(-t10$Valeur)
t10 <- t10[sorted_indices, c("Predictors", "Valeur","Guild", "Scale")]
t10<-t10[1:3,] 
t11<-data.frame(Predictors=GDM_DECOMPO_ESP_EchGrad$predictors, 
                   Valeur = coefDecompo_grad, Guild="Detritivores",Scale="Site")
sorted_indices <- order(-t11$Valeur)
t11 <- t11[sorted_indices, c("Predictors", "Valeur","Guild", "Scale")]
t11<-t11[1:3,] 
t12<-data.frame(Predictors=GDM_PARA_ESP_EchGrad$predictors, 
                Valeur = coefPara_grad, Guild="Parasitoids",Scale="Site")
sorted_indices <- order(-t12$Valeur)
t12 <- t12[sorted_indices, c("Predictors", "Valeur","Guild","Scale")]
t12<-t12[1:3,] 
Tgrad<-bind_rows(t9, t10, t11, t12)

THerbi<-bind_rows(t1, t5, t9)
TPredat<-bind_rows(t2, t6, t10)
Tdetri<-bind_rows(t3,t7,t11)
Tpara<-bind_rows(t4,t8,t12)


#Le plot 


ggplot(Tdetri, aes(x = Scale, y = Valeur, fill = Predictors)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Predictors", y = "Coefficient") +
  scale_fill_hue(c = 40) +
  geom_text(aes(label = Predictors), position = position_dodge(width = 1), vjust = -0.5) +
  ggtitle("Detritivores")+
  theme_minimal()
  

  
#####LE PLOT DES SPLINES######
x11()
plot(GDM_PARA_ESP_EchGrad, plot.layout=c(1,1))
