#my_GDM_function GRADIENT
#Fontions qui recoit les imputs et sortira les outputs

#librairies
librarian::shelf(dplyr,vegan, ggplot2, betapart, gdm, tibble, tidyverse)

#La fonction
my_gdm_function_GRAD<-function(ENV, COMM, PHYTO, Methode, Variables, ECHELLE){

  #preparation generale des tableaux
  #ECHELLE GRADIENT------
  ENV%>%
    rename(id_plot=codeplot)%>%
    inner_join(ECHELLE, by='id_plot',relationship
               = "many-to-many")%>%
    select(c(Variables,gradient))%>%
    group_by(gradient)%>%
    summarise(across(
      .cols = all_of(Variables), 
      .fns = mean,
      na.rm=T))%>%
    rename(echelle = gradient)%>%
    na.omit->ENV
  
  
  PHYTO%>%
    rename(id_plot=codeplot)%>%
    inner_join(ECHELLE, by='id_plot', relationship
               = "many-to-many")->PHYTO
  
  COMM%>%
    unite(id_plot, gradient, alti)%>%
    inner_join(ECHELLE, by='id_plot', relationship
               = "many-to-many")->COMM
  
  
  
  #preparation de phyto 
  PHYTO%>%
    add_column(ab = 1)%>%
    group_by(gradient, lb_nom)%>%
    rename(echelle = gradient)%>%
    summarise(tot = sum(ab))->tp
  ##transformation en matrice
  pivot_wider(tp,
              id_cols =echelle ,
              names_from = 'lb_nom', 
              values_from = 'tot',
              values_fill = 0)->commuphyto
  
  
  #Preparation de COMM
  
  COMM%>%
    filter(!grepl("0", abundance))%>%
    filter(method == Methode)%>%
    filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
    mutate(name2 = ifelse(name == "", "unid", name))%>% 
    group_by(gradient, name2)%>%
    rename(echelle =gradient) %>%
    summarise(tot = sum(abundance))->tp1
  
  ###transfo en matrice
  pivot_wider(tp1,
              id_cols = echelle,
              names_from = 'name2', 
              values_from = 'tot',
              values_fill = 0)->matrice
  
  #Affinage
  
  matrice1 <- subset(matrice, echelle %in% intersect(intersect(matrice$echelle, commuphyto$echelle), ENV$echelle))
  commuphyto1 <- subset(commuphyto, echelle %in% intersect(intersect(commuphyto$echelle, matrice$echelle), ENV$echelle))
  ENV1 <- subset(ENV, echelle %in% intersect(intersect(ENV$echelle, matrice$echelle), commuphyto$echelle))
  
  
  
  ##dissimilarite vegetale
  
  
  
  commuphyto1$echelle->echelle
  as.data.frame(echelle)->echelle
  
  ##passage de id_plot en index 
  commuphyto1%>%
    remove_rownames()%>%
    column_to_rownames('echelle')->commuphyto_ind
  Dissvege <- as.matrix(vegdist(commuphyto_ind, method="bray", na.rm=T))
  
  
  
  vegedis <- cbind(echelle,Dissvege)
  
  
  
  ###Dissimilarite communaute 
  #Sortir la colone id_plot
  matrice1 %>% 
    select(echelle) -> echelle2
  
  #on finit la matrice commu
  
  matrice1%>%
    remove_rownames()%>%
    column_to_rownames('echelle')->matrice2
  
  
  DissComm <- vegdist(matrice2, method="bray", na.rm=T)
  as.matrix(DissComm)->DissComm
  
  #on replace id_plot
  cbind(DissComm, echelle2)->gdmdist
  
  
  
  gdmdata <- formatsitepair(bioData=gdmdist, 
                            bioFormat=3, #diss matrix 
                            XColumn="X_L93", 
                            YColumn="Y_L93", 
                            predData=ENV1,
                            distPreds = list(vegedis),
                            siteColumn="echelle")
  
  na.omit(gdmdata)->gdmdata
  
  ##application de la fonction GDM
  gdm.1 <- gdm(data=gdmdata, geo=TRUE)
  summary(gdm.1)
  return(gdm.1)
  
  #validation du GDM
  gdm.crossvalidation(gdmdata,train.proportion=0.5, n.crossvalid.tests=1,
                      geo=FALSE, splines=NULL, knots=NULL)
  
  
  #plot du GDM
  
  plot(gdm.1, plot.layout=c(2,3))->plot
  
  #affinage splines
  gdm.1.splineDat <- isplineExtract(gdm.1)
  str(gdm.1.splineDat)
  
}