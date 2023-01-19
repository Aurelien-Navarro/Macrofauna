#####Tests avec donnees TM####

#####SETTINGS####


install.packages("vegan")
library(vegan)
library(dplyr)
library(tibble)
library(ggplot2)

install.packages("esquisse")
library(esquisse)



#####Imortation JD#####
data<-read.csv("Donnees ORCHAMP.csv", sep=";", dec=".",header=TRUE)
str(data)

data%>%
  distinct(Valid.Name)%>%
  count(Valid.Name)

#####Richesse specifique#####

   

RSalt<-data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n());RSalt###creation de la RS

str(RSalt)
as.numeric(RSalt$nb)->RSalt$nb
as.vector(RSalt$id_plot)->RSalt$id_plot
str(RSalt)###Transformation des donnees pour barplot

ggplot(data=RSalt, aes(x=id_plot, y=nb), ylab="localite",xlab="Richesse specifique")+
  geom_bar(stat="identity")+
  theme_minimal()->bpRSalt
bpRSalt+coord_flip()###Barplot exploratoire

#######

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(1:6)->ARG;
  ggplot(ARG, aes(x=id_plot, y=nb))+geom_bar(stat="identity")
  
data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(7,8)->ARM;
  ggplot(ARM, aes(x=id_plot, y=nb))+geom_bar(stat="identity")

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(9:13)->MSB;
  ggplot(MSB, aes(x=id_plot, y=nb))+geom_bar(stat="identity")

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(14,15)->PEC;
  ggplot(PEC, aes(x=id_plot, y=nb))+geom_bar(stat="identity")

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(16:20)->TAN;
ggplot(TAN, aes(x=id_plot, y=nb))+geom_bar(stat="identity")

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(21:23)->VAL;
ggplot(VAL) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(VAL_1860 = "#F8766D", 
                               VAL_2050 = "#93AA00", VAL_2250 = "#00C19F")) +
  scale_color_manual(values = c(VAL_1860 = "#F8766D", 
                                VAL_2050 = "#93AA00", VAL_2250 = "#00C19F")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Ventoux", caption = "Altitudes") +
  theme_minimal()


data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(24:28)->VER;
ggplot(VER) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(VER_1200 = "#F8766D", 
                               VER_1400F = "#93AA00", VER_1400P = "#00C19F", VER_1800 = "#619CFF", VER_2000 = "#FF61C3")) +
  scale_color_manual(values = c(VER_1200 = "#F8766D", 
                                VER_1400F = "#93AA00", VER_1400P = "#00C19F", VER_1800 = "#619CFF", VER_2000 = "#FF61C3")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Lac Vert", caption = "Altitudes") +
  theme_minimal()


data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(29:33)->VTN;
ggplot(VTN) +
 aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
 geom_bar() +
 scale_fill_manual(values = c(VTN_1130 = "#F8766D", 
VTN_1340 = "#93AA00", VTN_1510 = "#00C19F", VTN_1675 = "#619CFF", VTN_1860 = "#FF61C3")) +
 scale_color_manual(values = c(VTN_1130 = "#F8766D", 
VTN_1340 = "#93AA00", VTN_1510 = "#00C19F", VTN_1675 = "#619CFF", VTN_1860 = "#FF61C3")) +
 labs(x = "Altitudes Croissantes", 
 y = "Richesse spécifique", subtitle = "Ventoux", caption = "Altitudes") +
 theme_minimal()







