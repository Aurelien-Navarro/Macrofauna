#####Tests avec donnees TM####

#####SETTINGS####
setwd("C:/AUREL/STAGE/M2/work")
getwd()

install.packages("vegan")
library(vegan)
library(dplyr)
library(tibble)
library(ggplot2)

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
str(RSalt)###Transformation des donnees pour parplot

ggplot(data=RSalt, aes(x=id_plot, y=nb), ylab="localite",xlab="Richesse specifique")+
  geom_bar(stat="identity")+
  theme_minimal()->bpRSalt
bpRSalt+coord_flip()###Barplot exploratoire

#######

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(1:6)->ARG 
  
data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(7,8)->ARM 

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(9:13)->MSB

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(14,15)->PEC

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(16:20)->TAN

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(21:23)->VAL 

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(24:28)->VER

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(29:33)->VTN 

x11()
par(mfrow=c(3,3))
plotARG<-ggplot(ARG, aes(x=id_plot, y=nb))+geom_bar(stat="identity")
plotARM<-ggplot(ARM, aes(x=id_plot, y=nb))+geom_bar(stat="identity")
plotMSB<-ggplot(MSB, aes(x=id_plot, y=nb))+geom_bar(stat="identity")
plotPEC<-ggplot(PEC, aes(x=id_plot, y=nb))+geom_bar(stat="identity")
plotTAN<-ggplot(TAN, aes(x=id_plot, y=nb))+geom_bar(stat="identity")
plotVAL<-ggplot(VAL, aes(x=id_plot, y=nb))+geom_bar(stat="identity")
plotVER<-ggplot(VER, aes(x=id_plot, y=nb))+geom_bar(stat="identity")
plotVTN<-ggplot(VTN, aes(x=id_plot, y=nb))+geom_bar(stat="identity")

x11()
layout(matrix(1:9,3,3))
plotVTN
plotARG
plotARM
plotMSB
plotPEC
plotTAN
plotVAL
plotVER

###Rajout divers



