#GDM affiné 

#Script pour insérer les différentes conditions au GDM et le produire 
#J ai du selectionner des variables car certaines ont trop de NA
#et le modèle se retrouve avec des valeurs négatives qu'il ne peut traiter 


#librairies
librarian::shelf(dplyr,vegan, ggplot2, betapart, gdm, tibble, tidyverse, esquisse, gridExtra, cowplot)

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
                                                          "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))

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
                                                            "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))
 
  
  

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
                                                            "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))

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
                                                              "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))
   
   
  
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
                                              "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))
   
   
   
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
                                                               "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))
   

   as.vector(GDM_PREDAT_ESP_EchGrad$coefficients)->Pr3
   tapply(Pr3, ceiling(seq_along(Pr3)/3), sum)->coefPr_grad
   
    ##DECOMPOSEURS----
   GDM_DECOMPO_ESP_EchSamp<-my_gdm_function_SAMP(ENV=ENV,
                                                  COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                              ESP$className %in% c("Diplopoda","Clitellata"),],
                                                PHYTO=Phyto,
                                                IDRESO = "Espèce",
                                                ECHELLE= Echelle,
                                                ANIMO = RICHSPEANIMO,
                                                Methode = c("tri manuel","tri manuel qualitatif"),    
                                                Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
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
                                                 ESP$className %in% c("Diplopoda","Clitellata"),],
                                PHYTO=Phyto,
                                IDRESO = "Espèce",
                                ANIMO = RICHSPEANIMO,
                                Methode= c("tri manuel","tri manuel qualitatif"),
                                Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",                
                                              "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                              "pHmean","TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean",
                                              "Nmean","Almean","Argilemean","Calcmean","Camean","Fermean",
                                              "Limfinmean","Limgrosmean","mgmean","Mnmean","Phosmean","Kmean",
                                              "Sablefinmean","Sablegrosmean","Namean","Milieu","Alt","X_L93","Y_L93"))


as.vector(GDM_DECOMPO_ESP_EchPlot$coefficients)->D2
tapply(D2, ceiling(seq_along(D2)/3), sum)->coefDecompo_plot

GDM_DECOMPO_ESP_EchGrad<-my_gdm_function_GRAD(ENV=ENV,
                                              COMM= ESP[ESP$orderName %in% "Isopoda"|
                                                          ESP$className %in% c("Diplopoda","Clitellata"),],
                                              PHYTO=Phyto,
                                              IDRESO = "Espèce",
                                              ECHELLE= Echelle,
                                              ANIMO = RICHSPEANIMO,
                                              Methode= c("tri manuel","tri manuel qualitatif"),
                                              Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",               
                                                            "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                            "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                            "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean",
                                                            "Nmean","Argilemean","Limfinmean","Limgrosmean",
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
                                            "Rveg","NDVImin","NDVImax","PRCTMOmean",
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
                                           Variables = c("ndvi.mean","TMeanY.mean","PTotY.mean",              
                                                         "Rveg","NDVImin","NDVImax","PRCTMOmean",
                                                         "pHmean","Milieu","Alt","X_L93","Y_L93",
                                                         "TG1.degres.mean" ,"TG4.degres.mean","DSN_T_ISBA.mean"))

as.vector(GDM_PARA_ESP_EchGrad$coefficients)->Pa3
tapply(Pa3, ceiling(seq_along(Pa3)/3), sum)->coefPara_grad

#####PLOT######

##Comparaison entre guildes 
cate1<-c("Geo","E","PS","PS","E",
        "E","E","E","PS","CF",
        "CF","PS","PS","PS","E")
cate2<-c("Geo","E","PS","PS","E",
         "E","E","E",
         "PS","PS","PS","PS",
         "NC","NC","NC","NC","NC","NC",
         "HH","HH", 
         "NC","NC", "NC","NC",
         "HH", "HH", "NC","CF","CF","E")
cate3<-c("Geographic","Energy","Physical_Stressing","PS","E","E","E","E",
         "PS", "CF","CF","PS","PS","PS","N","HH","HH",
         "HH","HH","HH","E")
#sample
t1<-data.frame(Predictors = GDM_HERBI_ESP_EchSample$predictors,
               Valeur = coefH_sample, Guild= "Herbivores",Scale="3.β1-Sample", 
               Categorie = cate1)

t2<-data.frame(Predictors= GDM_PREDAT_ESP_EchSamp$predictors,
               Valeur = coefPr_sample, Guild = "Predators", Scale="3.β1-Sample",
               Categorie = cate1)

t3<-data.frame(Predictors = GDM_DECOMPO_ESP_EchSamp$predictors,
               Valeur = coefDecompo_ech, Guild = "Detritivores", Scale="3.β1-Sample",
               Categorie = cate2)

t4<-data.frame(Predictors =GDM_PARA_ESP_EchSamp$predictors,
               Valeur = coefPara_samp, Guild="Parasitoids", Scale="3.β1-Sample",
               Categorie = cate1)


Tsample<-bind_rows(t1,t2,t4)
#parcelle
t5<-data.frame(Predictors = GDM_HERBI_ESP_EchPlot$predictors, 
               Valeur = coefH_plot, Guild="Herbivores", Scale="2.β2-Plot",
               Categorie = cate1)

t6<-data.frame(Predictors = GDM_PREDAT_ESP_EchPlot$predictors, 
               Valeur = coefPr_plot, Guild="Predators", Scale="2.β2-Plot",
               Categorie = cate1)

t7<-data.frame(Predictors = GDM_DECOMPO_ESP_EchPlot$predictors, 
               Valeur = coefDecompo_plot, Guild = "Detritivores", Scale="2.β2-Plot",
               Categorie = cate2)

t8<-data.frame(Predictors = GDM_PARA_ESP_EchPlot$predictors, 
               Valeur = coefPara_plot, Guild="Parasitoids", Scale="2.β2-Plot",
               Categorie = cate1)

Tplot<-bind_rows(t5,t6,t7,t8)

#gradient
t9<-data.frame(Predictors=GDM_HERBI_ESP_EchGrad$predictors, 
               Valeur = coefH_grad, Guild="Herbivores", Scale="1.β3-Site",
               Categorie = cate1)
 
t10<-data.frame(Predictors=GDM_PREDAT_ESP_EchGrad$predictors, 
                Valeur = coefPr_grad, Guild="Predators", Scale="1.β3-Site",
                Categorie = cate1)

t11<-data.frame(Predictors=GDM_DECOMPO_ESP_EchGrad$predictors, 
                   Valeur = coefDecompo_grad, Guild="Detritivores",Scale="1.β3-Site",
                Categorie = cate3)

t12<-data.frame(Predictors=GDM_PARA_ESP_EchGrad$predictors, 
                Valeur = coefPara_grad, Guild="Parasitoids",Scale="1.β3-Site",
                Categorie = cate1)


Total<-bind_rows(t1, t2, t4, t5, t6, t7, t8, t9, t10, t11, t12)

recode(Total$Predictors, 
       Geographic="Geographic", 
       ndvi.mean="NDVIm",
       TMeanY.mean="Temp_Y",
       PTotY.mean="Precip_Y",
       Rveg="RS_veg",
       NDVImin="NDVImin",
       NDVImax="NDVImax", 
       PRCTMOmean="Organic_Matter",
       pHmean="pH",
       Milieu="Opening",
       Alt="Altitude",
       TG1.degres.mean="Temp_1cm",
       TG4.degres.mean="Temp_4cm",
       DSN_T_ISBA.mean="Snow_depth",
       matrix_1="Vegetal_Diss", 
       Nmean="Nitrogene",
       Almean="Aluminium",
       Argilemean="Clay",
       Calcmean="Limestone", 
       Camean="Calcium",
       Fermean="Iron",
       Limfinmean="Fine_silt",
       Limgrosmean="Coarse_silt",
       mgmean="Manganeze",
       Phosmean="Phosphorus", 
       Kmean="Potassium",
       Sablefinmean="Fine_sand", 
       Sablegrosmean="Coarse_sand",
       Namean="Sodium")->Total$Predictors
       
recode(Total$Categorie,
       Geo="Geographic",
       E="Energy_Related",
       CF="Complex_Factor",
       HH="Habitat_Heterogeneity",
       NC="Nutrient_Content",
       PS="Physical_Stressing")->Total$Categorie


#Le plot 



library(RColorBrewer)
palette_colors <- rep(brewer.pal(9, "Greys"), length.out = 30)

library(dplyr)

ggplot(Total, aes(x = Guild, y = Valeur, fill = Categorie, color = Predictors)) +
  geom_bar(stat = "identity", position = position_stack(), linewidth=0.1) +
  geom_text(data = subset(Total, Valeur != 0), aes(label = Predictors), position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ Scale, ncol = 1) +
  coord_flip()+
  labs(x = "Guilds", y = "Coefficient", fill = "Predictors") +
  scale_fill_manual(values = c("Physical_Stressing" = "lightpink", "Geographic" = "cyan2", "Energy_Related" = "lightgreen", "Complex_Factor" = "sienna1", "Nutrient_Content" = "khaki", "Habitat_Heterogeneity" = "mediumorchid1")) +
  scale_color_grey(start = 0.5, end = 0.5)+
  guides(fill = guide_legend(title = "Categorie"), color = "none") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#####LE PLOT DES SPLINES######
plot(GDM_PARA_ESP_EchGrad)

M1<-data.frame(Ecological= GDM_HERBI_ESP_EchSample$ecological,
               Observed= GDM_HERBI_ESP_EchSample$observed,
               Guild="Herbivore")
M2<-data.frame(Ecological=GDM_PREDAT_ESP_EchSamp$ecological,
               Observed=GDM_PREDAT_ESP_EchSamp$observed, 
               Guild="Predator")
M3<-data.frame(Ecological=GDM_DECOMPO_ESP_EchSamp$ecological,
               Observed=GDM_DECOMPO_ESP_EchSamp$observed, 
               Guild="Detritivore")
M4<-data.frame(Ecological=GDM_PARA_ESP_EchSamp$ecological,
               Observed=GDM_PARA_ESP_EchSamp$observed, 
               Guild="Parasitoid")

Sample<-bind_rows(M1, M2, M4)

M5<-data.frame(Ecological= GDM_HERBI_ESP_EchPlot$ecological,
               Observed= GDM_HERBI_ESP_EchPlot$observed,
               Guild="Herbivore")
M6<-data.frame(Ecological=GDM_PREDAT_ESP_EchPlot$ecological,
               Observed=GDM_PREDAT_ESP_EchPlot$observed, 
               Guild="Predator")
M7<-data.frame(Ecological=GDM_DECOMPO_ESP_EchPlot$ecological,
               Observed=GDM_DECOMPO_ESP_EchPlot$observed, 
               Guild="Detritivore")
M8<-data.frame(Ecological=GDM_PARA_ESP_EchPlot$ecological,
               Observed=GDM_PARA_ESP_EchPlot$observed, 
               Guild="Parasitoid")

Plot<-bind_rows(M5, M6, M7, M8)

M9<-data.frame(Ecological= GDM_HERBI_ESP_EchGrad$ecological,
               Observed= GDM_HERBI_ESP_EchGrad$observed,
               Guild="Herbivore")
M10<-data.frame(Ecological=GDM_PREDAT_ESP_EchGrad$ecological,
               Observed=GDM_PREDAT_ESP_EchGrad$observed, 
               Guild="Predator")
M11<-data.frame(Ecological=GDM_DECOMPO_ESP_EchGrad$ecological,
               Observed=GDM_DECOMPO_ESP_EchGrad$observed, 
               Guild="Detritivore")
M12<-data.frame(Ecological=GDM_PARA_ESP_EchGrad$ecological,
               Observed=GDM_PARA_ESP_EchGrad$observed, 
               Guild="Parasitoid")

Gradient<-bind_rows(M9, M10, M11, M12)


library(dplyr)

ggplot(Sample, aes(x = Ecological, y = Observed, color = Guild)) +
  labs(x = "Ecological Distance", y = "Observed dissimilarity", title = "Diversity - Distance relationships between samples", subtitle = "GAM fitting - Herbivores, Predators, Parasitoids") +
  geom_point(size = 0.5) +
  geom_smooth(data = filter(Sample, Guild %in% c("Herbivore")),
              method = "gam", aes(group = Guild), fill = "lightgreen", se = TRUE) +
  geom_smooth(data = filter(Sample, Guild %in% c("Predator")),
              method = "gam", aes(group = Guild), fill = "#F8766D", se = TRUE) +
  geom_smooth(data = filter(Sample, Guild == "Parasitoid"),
              method = "gam", aes(group = Guild), fill = "#F0E442", se = TRUE) +
  scale_color_manual(values = c("Herbivore" = "darkgreen", "Predator" = "red", "Detritivore" = "blue", "Parasitoid" = "orange")) +
  theme_minimal()

ggplot(Plot, aes(x = Ecological, y = Observed, color = Guild)) +
  labs(x = "Ecological Distance", y = "Observed dissimilarity", title="Diversity - Distance relationships between plots", subtitle="GAM fitting") +
  geom_point(size=0.5)+
  geom_smooth(data = filter(Plot, Guild %in% c("Herbivore")),
              method = "gam", aes(group = Guild), fill = "lightgreen", se = TRUE) +
  geom_smooth(data = filter(Plot, Guild %in% c("Predator")),
              method = "gam", aes(group = Guild), fill = "#F8766D", se = TRUE) +
  geom_smooth(data = filter(Plot, Guild == "Detritivore"),
              method = "gam", aes(group = Guild), fill = "#8494ff", se = TRUE) +
  geom_smooth(data = filter(Plot, Guild == "Parasitoid"),
              method = "gam", aes(group = Guild), fill = "orange", se = TRUE) +
  scale_color_manual(values= c("Herbivore" = "darkgreen", "Predator" = "red", "Detritivore" = "blue", "Parasitoid" = "orange")) +
  theme_minimal()

ggplot(Gradient, aes(x = Ecological, y = Observed, color = factor(Guild))) +
  geom_point(size = 0.5) +
  labs(x = "Ecological Distance", y = "Observed dissimilarity", title = "Diversity - Distance relationships between gradients", subtitle = "GLM fitting") +
  stat_smooth(data = filter(Gradient, Guild %in% c("Herbivore")),
              method = "glm",
              aes(group = Guild), fill = "lightgreen", se = TRUE) +
  stat_smooth(data = filter(Gradient, Guild %in% c("Predator")),
              method = "glm", 
              aes(group = Guild), fill = "#F8766D", se = TRUE) +
  stat_smooth(data = filter(Gradient, Guild == "Detritivore"),
              method = "glm",
              aes(group = Guild), fill = "#8494ff", se = TRUE) +
  stat_smooth(data = filter(Gradient, Guild == "Parasitoid"),
              method = "glm",  
              aes(group = Guild), fill = "#F0E442", se = TRUE) +
  scale_color_manual(values = c("Herbivore" = "darkgreen", "Predator" = "red", "Detritivore" = "blue", "Parasitoid" = "orange")) +
  theme_minimal()

#les R²
model <- gam(Observed ~ Ecological, data = M1)
summary(model)->sum
print(sum)
# 1 - exp(-sum$deviance / sum$null.deviance) #Pour les GLM



####GAMMA DIV#####
ESP[ESP$orderName %in% "Hymenoptera"&!ESP$familyName %in%"Formicidae",]->Herbi
Herbi%>%
  filter(method==c("barber"))%>%
  filter(rankName=="Espèce")->Herbi
tp<-Herbi %>%
  mutate(name2 = ifelse(name == "", "unid", name)) %>% ##supression des vides 
  group_by(X, name2)%>%
  summarise(tot = sum(abundance)) 
#---------------------------------------

#####transformation en matrice#### 
pivot_wider(tp,
            id_cols = c('X'),
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->matrice

# Calcul de la somme des lignes
as.data.frame(colSums(matrice))->somme
t(somme)->matrice2
matrice2[,-1]->matrice3


vegan::diversity(matrice3, index="shannon")->GPa  #shannon index pour chaque site et chaque methode     

data.frame(Guild=c("Herbivore","Predator","Detritivore","Parasitoid"),
           Gamma = c(GH, GPr, GD, GPa))->GAMMA
write.csv(GAMMA, file = paste0("outputs/GAMMA/GAMMA" , ".csv"))
