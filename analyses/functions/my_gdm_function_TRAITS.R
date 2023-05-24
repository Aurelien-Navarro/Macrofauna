#my_GDM_function_ Traits
#Fontions qui recoit les imputs et sortira les outputs
#specialisee pour les traits car pas les mêmes manip de COMM 

#librairies
librarian::shelf(dplyr,vegan, ggplot2, betapart, gdm, tibble, tidyverse)

#La fonction
my_gdm_function_TRAITS<-function(ENV, COMM, PHYTO, Variables){
 
   #Pas besoin de préciser la méthode car déja précisée lors 
  #de la sortie des traits sur 2.Indice computation guilde
  
  #preparation generale des tableaux
  ENV%>%
    rename(id_plot=codeplot)%>%
    select(Variables)%>%
    na.omit->ENV
  PHYTO%>%
    rename(id_plot=codeplot)->PHYTO
  
  
  
  #preparation de phyto 
  PHYTO%>%
    add_column(ab = 1)%>%
    group_by(id_plot, lb_nom)%>%
    summarise(tot = sum(ab))->tp
  ##transformation en matrice
  pivot_wider(tp,
              id_cols = 'id_plot',
              names_from = 'lb_nom', 
              values_from = 'tot',
              values_fill = 0)->commuphyto
  
  
  
  
  
  #Preparation de COMM
  
  COMM%>%
    select(!X)->matrice
    
  
  #Affinage
  
  matrice1 <- subset(matrice, id_plot %in% intersect(intersect(matrice$id_plot, commuphyto$id_plot), ENV$id_plot))
  commuphyto1 <- subset(commuphyto, id_plot %in% intersect(intersect(commuphyto$id_plot, matrice$id_plot), ENV$id_plot))
  ENV1 <- subset(ENV, id_plot %in% intersect(intersect(ENV$id_plot, matrice$id_plot), commuphyto$id_plot))
  
  
  
  ##dissimilarite vegetale
  
  
  
  commuphyto1$id_plot->id_plot
  as.data.frame(id_plot)->id_plot
  
  ##passage de id_plot en index 
  commuphyto1%>%
    remove_rownames()%>%
    column_to_rownames('id_plot')->commuphyto_ind
  Dissvege <- as.matrix(vegdist(commuphyto_ind, method="bray", na.rm=T))
  
  
  
  vegedis <- cbind(id_plot,Dissvege)
  
  
  
  ###Dissimilarite communaute 
  #Sortir la colone id_plot
  matrice1 %>% 
    select(id_plot) -> idplot2
  
  #on finit la matrice commu
  
  matrice1%>%
    remove_rownames()%>%
    column_to_rownames('id_plot')->matrice2
  
  
  DissComm <- vegdist(matrice2, method="bray", na.rm=T)
  as.matrix(DissComm)->DissComm
  
  #on replace id_plot
  cbind(DissComm, idplot2)->gdmdist
  
  
  
  
  
  #Le GDM 
  #objet pour le GDM
  gdmdata <- formatsitepair(bioData=gdmdist, 
                            bioFormat=3, #diss matrix 
                            XColumn="X_L93", 
                            YColumn="Y_L93", 
                            predData=ENV1,
                            distPreds = list(vegedis),
                            siteColumn="id_plot")
  
  na.omit(gdmdata)->gdmdata
  
  ##application de la fonction GDM
  gdm.1 <- gdm(data=gdmdata, geo=TRUE)
  summary(gdm.1)
  
  #validation du GDM
  gdm.crossvalidation(gdmdata,train.proportion=0.5, n.crossvalid.tests=1,
                      geo=FALSE, splines=NULL, knots=NULL)
  
  
  #plot du GDM
  
  plot(gdm.1, plot.layout=c(2,3))->plot
  
  #affinage splines
  gdm.1.splineDat <- isplineExtract(gdm.1)
  str(gdm.1.splineDat)
  
  
  
  return(gdm.1)
}