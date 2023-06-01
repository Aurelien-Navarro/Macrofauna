#Calcul rapide de biodiversité pour le GDM

read.csv("data/derived-data/Esp/clean_data_2023-05-30.csv",header=T, sep=",")->ESP

Resp<-ESP%>%
  filter(rankName=="Espèce")%>%
  unite(id_plot, gradient, alti)%>%
  group_by(id_sample, id_plot)%>%
  distinct(name)%>%
  summarize(Resp=n())









write.csv(Resp, file = paste0("data/derived-data/Envir/Resp",".csv"))   
