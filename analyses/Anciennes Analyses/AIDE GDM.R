Echelle%>%
  select(id_plot, id_sample)->ECHELLE2

ENV%>%
  rename(id_plot=codeplot)%>%
  inner_join(ECHELLE2, by="id_plot")%>%
  group_by(select(last_col()))%>%
  summarise_all(mean)%>% 
  select(all_of(Variables))%>%#on fait la moyenne sur l'échelle d étude
  na.omit->ENV

derniere_colonne <- names(ECHELLE2)[ncol(ECHELLE2)]


