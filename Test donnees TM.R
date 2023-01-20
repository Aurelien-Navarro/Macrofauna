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

#############################
#####Richesse specifique#####
#############################
   
#####Creation de la RS
RSalt<-data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n());RSalt

#####Transformation des donnees
str(RSalt)
as.numeric(RSalt$nb)->RSalt$nb
as.vector(RSalt$id_plot)->RSalt$id_plot
str(RSalt)

#####Barplot exploratoire####
ggplot(data=RSalt, aes(x=id_plot, y=nb), ylab="localite",xlab="Richesse specifique")+
  geom_bar(stat="identity")+
  theme_minimal()->bpRSalt
bpRSalt+coord_flip()
##############################

########Creation des plots pour chaque site####
  ####Argentiere####
data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(1:6)->ARG
      ####ggplot####
ggplot(ARG) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(ARG_1420 = "#F8766D", 
                               ARG_1620 = "#93AA00", ARG_1790 = "#00C19F", ARG_2000 = "#619CFF", ARG_2230 = "#FF61C3", ARG_2400="#CC0033")) +
  scale_color_manual(values = c(ARG_1420 = "#F8766D", 
                                ARG_1620 = "#93AA00", ARG_1790 = "#00C19F", ARG_2000 = "#619CFF", ARG_2230 = "#FF61C3", ARG_2400="#CC0033")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Argentiere", caption = "Altitudes") +
  theme_minimal()


  ####Armenaz####  
data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(7,8)->ARM
      ####plot####
ggplot(ARM) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(ARM_1520 = "#F8766D", 
                               ARM_1750 = "#93AA00")) +
  scale_color_manual(values = c(ARM_1520 = "#F8766D", 
                                ARM_1750 = "#93AA00")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Armenaz", caption = "Altitudes") +
  theme_minimal()

  ####Massif Saint Barthelemy####
data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(9:13)->MSB
      ####ggplot####
ggplot(MSB) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(MSB_1370 = "#F8766D", 
                               MBS_1420 = "#93AA00", MSB_1890 = "#00C19F", MSB_2020 = "#619CFF", MSB_2260 = "#FF61C3")) +
  scale_color_manual(values = c(MSB_1370 = "#F8766D", 
                                MBS_1420 = "#93AA00", MSB_1890 = "#00C19F", MSB_2020 = "#619CFF", MSB_2260 = "#FF61C3")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Massif Saint Barthelemy", caption = "Altitudes") +
  theme_minimal()

  ####Pecloz####

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(14,15)->PEC
      ####ggplot####
ggplot(PEC) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(PEC_1152 = "#F8766D", 
                               PEC_1375 = "#93AA00")) +
  scale_color_manual(values = c(PEC_1152 = "#F8766D", 
                                PEC_1375 = "#93AA00")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Pecloz", caption = "Altitudes") +
  theme_minimal()

  #####Tania####

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(16:20)->TAN
    ####ggplot####
ggplot(TAN) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(TAN_1420 = "#F8766D", 
                               TAN_1700 = "#93AA00", TAN_1890 = "#00C19F", TAN_2100 = "#619CFF", TAN_2300 = "#FF61C3")) +
  scale_color_manual(values = c(TAN_1420 = "#F8766D", 
                                TAN_1700 = "#93AA00", TAN_1890 = "#00C19F", TAN_2100 = "#619CFF", TAN_2300 = "#FF61C3")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Tania", caption = "Altitudes") +
  theme_minimal()

  #####Valoire####

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(21:23)->VAL
      ####ggplot####
ggplot(VAL) +
  aes(x = id_plot, fill = id_plot, colour = id_plot, weight = nb) +
  geom_bar() +
  scale_fill_manual(values = c(VAL_1860 = "#F8766D", 
                               VAL_2050 = "#93AA00", VAL_2250 = "#00C19F")) +
  scale_color_manual(values = c(VAL_1860 = "#F8766D", 
                                VAL_2050 = "#93AA00", VAL_2250 = "#00C19F")) +
  labs(x = "Altitudes Croissantes", 
       y = "Richesse spécifique", subtitle = "Valoire", caption = "Altitudes") +
  theme_minimal()

  #####Lac vert####

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(24:28)->VER
      ####ggplot####
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

  #####Ventoux####

data%>%
  group_by(id_plot)%>%
  distinct(Valid.Name)%>%
  dplyr::summarize(nb=n())%>%
  slice(29:33)->VTN
        ####ggplot####
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

###########################################

#############################
#####Diversite de Shannon####
#############################





