##############
#----GDM-----
#############

##@Objectif : Essais de GDM sur donnees ORCHAMP macrofaune

#Packages 
library(dplyr)
library(vegan)
library(ggplot2)
library(betapart)
library(cowplot)
library(gdm)
library(FactoMineR)
library(factoextra)
library(pander)
library(tibble)

#injection des DF 
read.csv("data/derived-data/envir/ENV_2023-05-05.csv", row.names = 1)->ENV
read.csv("data/derived-data/traits_homo_2023-04-27.csv",header=T, sep=",")->TRAITS
read.csv("data/raw-data/envir/phyto.data3.csv",header=T, sep=",")->Phyto

#preparation de phyto 

    ##faire fit phyto avec le reste
Phyto%>%
  rename(id_plot=codeplot)%>%
  inner_join(ENV, by='id_plot')->Phyto
    

Phyto%>%
  add_column(ab = 1)%>%
  na.omit(lb_nom)%>%
  group_by(id_plot, lb_nom)%>%
  summarise(tot = sum(ab))->tp


#####transformation en matrice#### 
pivot_wider(tp,
            id_cols = 'id_plot',
            names_from = 'lb_nom', 
            values_from = 'tot',
            values_fill = 0)->commu

#passage de id_plot en index 
commu%>%
  remove_rownames()%>%
  column_to_rownames(var='id_plot')->commu_ind
#dissimilarite vegetale

Dissvege <- vegdist(commu_ind, method="bray", na.rm=T)
as.matrix(Dissvege)->Dissvege

vegedis <- cbind(idplot,Dissvege)#besoin de l'idplot correspondant à la guidle !!

#DECOMPOSEURS-------------
    ##Echelle : le plot----
  
    ###Preparation des donnees------

#Sortir la colone id_plot
detrialphaplot %>% 
  select(id_plot) -> idplot

#Remplacement des NAs par 0 : souvent trop haut pour mesurer quelque chose
ENV%>%
  mutate_all( ~replace(., is.na(.), 0))->ENV

#adequation entre ENV et Dissvdt
#ENV%>%
  #rename(id_plot='codeplot')->ENV
detrialphaplot%>%
inner_join(ENV, by="id_plot")%>%
  select(c('id_plot','ab','mass','q0','q1','q2','Body_length'))->detrialphaplot
ENV%>%
  inner_join(detrialphaplot, by="id_plot")%>%
  select(!c('ab','mass','q0','q1','q2','Body_length'))->ENV



#passage de ID sample en lignes_index
as_tibble(detrialphaplot)->detrialphaplot
detrialphaplot %>%
  filter(!c(is.na(id_plot)))%>%
  remove_rownames() %>%
  column_to_rownames(var='id_plot')->TRAITSVDT

#Sortir la colone id_plot
detrialphaplot %>% 
  select(id_plot) -> idplot


      ###Creation de la matrice de dissimilarite -----
#On va prendre Bray-Curtis comme metrique

Dissvdt <- vegdist(TRAITSVDT, method="bray", na.rm=T)
as.matrix(Dissvdt)->Dissvdt


 
#rajouter la colone id plot à la matrice de dissimilarite
cbind(Dissvdt, idplot)->gmdist

#GDM----------

    ##creation de l'objet-------
gdmdata <- formatsitepair(bioData=gmdist, 
                          bioFormat=3, #diss matrix 
                          XColumn="X_L93", 
                          YColumn="Y_L93", 
                          predData=ENV,
                          distPreds = list(vegedis),
                          siteColumn="id_plot")

    ##application de la fonction GDM
gdm.1 <- gdm(data=gdmdata, geo=TRUE)
summary(gdm.1)

length(gdm.1) #ici 16

  ##Plot de GDM-----
#fitted models + I splines 
x11()
plot(gdm.1, plot.layout=c(2,4))

  ##customisation-----
gdm.1.splineDat <- isplineExtract(gdm.1)
str(gdm.1.splineDat)
  ##exemple de customisation pour une variable
x11()
plot(gdm.1.splineDat$x[,"Diam_mean"], 
     gdm.1.splineDat$y[,"Diam_mean"], 
     lwd=3,
     type="l", 
     xlab="Mean Diameter of surrounding trees", 
     ylab="Partial ecological distance")


  ##Prediction de la dissimilarite entre sites (ecart a la prediction) 
gdm.1.pred <- predict(object=gdm.1, data=gdmdata)
head(gdm.1.pred)
plot(gdmdata$distance, 
     gdm.1.pred, 
     xlab="Observed dissimilarity", 
     ylab="Predicted dissimilarity", 
     xlim=c(0,1), 
     ylim=c(0,1), 
     pch=20, 
     col=rgb(0,0,1,0.5))
lines(c(-1,2), c(-1,2))
