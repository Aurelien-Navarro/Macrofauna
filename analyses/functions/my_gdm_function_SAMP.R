#my_GDM_function SAMPLE
#Fontions qui recoit les imputs et sortira les outputs

#librairies
librarian::shelf(dplyr,vegan, ggplot2, betapart, gdm, tibble, tidyverse)

#La fonction
my_gdm_function_SAMP<-function(ENV, COMM, PHYTO, Methode, Variables, ECHELLE){
  
  #preparation generale des tableaux
  #ECHELLE GRADIENT------
  ENV%>%
    rename(id_plot=codeplot)%>%
    inner_join(ECHELLE, by='id_plot',relationship = "many-to-many" )%>%
    select(c(Variables,id_sample))%>%
    group_by(id_sample)%>%
    summarise(across(
      .cols = all_of(Variables), 
      .fns = mean,
      na.rm=T))%>%
    rename(echelle = id_sample)%>%
    na.omit->ENV
  
  
  PHYTO%>%
    rename(id_plot=codeplot)%>%
    inner_join(ECHELLE, by='id_plot', relationship = "many-to-many")->PHYTO
  
  COMM%>%
    unite(id_plot, gradient, alti)->COMM
  
  
  
  #preparation de phyto 
  PHYTO%>%
    group_by(id_sample, lb_nom)%>%
    rename(echelle = id_sample)%>%
    add_column(ab = 1)%>%
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
    filter(method %in% Methode)%>%
    filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
    mutate(name2 = ifelse(name == "", "unid", name))%>% 
    group_by(id_sample, name2)%>%
    rename(echelle =id_sample) %>%
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
                      geo=FALSE, splines=NULL, knots=NULL)->valid
  
  
  #plot du GDM
  
  plot(gdm.1, plot.layout=c(2,3))->plot
  
  #affinage splines
  gdm.1.splineDat <- isplineExtract(gdm.1)
  str(gdm.1.splineDat)
 
  
  
}