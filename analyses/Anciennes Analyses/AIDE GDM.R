#BROUILLON VOIR CE QUI VA PAS 
#
#Suppose etre supprime en fin de journee 
#
#Implementation de la fonction 'my gdm function" 
#En copié collé 

#Les tableaux
read.csv("data/derived-data/Envir/ENV_2023-05-22.csv", row.names = 1)->ENV
ENV%>%
  filter(!codeplot %in% c("BOU","CAU"))->ENV
read.csv("data/derived-data/Traits/traits_homo_2023-04-27.csv",header=T, sep=",")->TRAITS
read.csv("data/raw-data/envir/phyto.data3.csv",header=T, sep=",")->Phyto
Phyto%>%
  filter(!codeplot %in% c("BOU","CAU"))->Phyto
read.csv("data/derived-data/Esp/clean_data_2023-05-10.csv",header=T, sep=",")->ESP
ESP%>%
  filter(orderName == "Orthoptera"|ESP$familyName=="Chrysomelidae")%>%
  filter(!gradient %in% c("BOU","CAU"))->ESP


#preparation generale des tableaux
ENV%>%
  rename(id_plot=codeplot)%>%
  na.omit()->ENV
ESP%>%
  unite(id_plot, gradient, alti)->COMM
Phyto%>%
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
  filter(!grepl("0", abundance))%>%
  filter(method == "barber")%>%
  filter(rankName %in% "EspÃ¨ce"|rankName%in%"Espèce")%>%
  mutate(name2 = ifelse(name == "", "unid", name))%>% 
  group_by(id_plot, name2)%>%
  summarise(tot = sum(abundance))->tp1

###transfo en matrice
pivot_wider(tp1,
            id_cols = 'id_plot',
            names_from = 'name2', 
            values_from = 'tot',
            values_fill = 0)->matrice

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
                          siteColumn="id_plot",
                          sppColumn = 'id_plot')

na.omit(gdmdata)->gdmdata


##application de la fonction GDM
gdm.1 <- gdm(data=gdmdata, geo=TRUE)
summary(gdm.1)
