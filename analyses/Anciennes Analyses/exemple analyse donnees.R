
# ---------------------------------------------------
# Working directory
# ---------------------------------------------------
setwd("C:/Users/heddemic/Documents/Codes R/Macrofauna")  #ordi portable
# ---------------------------------------------------


# ---------------------------------------------------
# Libraries
# ---------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(formattable)
library(openxlsx)
library(cowplot)
library(stringr)
# ---------------------------------------------------

# ---------------------------------------------------
# Chargement donnees
# ---------------------------------------------------
mydata <- read.csv("data/raw-data/Donnees ORCHAMP.csv", h=T , sep = ";" )
# ---------------------------------------------------

# ---------------------------------------------------
# Mise en forme des données
# ---------------------------------------------------
mydataTM <- mydata %>%
  filter(method == "tri manuel")  %>%
  filter(!Class %in% c("Collembola", "Protura", "Symphyla"), !Order %in% c("Acari", "Enchytraeida"))
# ---------------------------------------------------

# ---------------------------------------------------
# Calcul Densité d'activité par répétition
# ---------------------------------------------------
ab_mean <- mydataTM %>%
  select(Replicate.number, id_site, altitude, Order, abundance) %>%
  group_by(id_site, altitude, Replicate.number) %>%
  summarise(ab_rep = sum(abundance)*16) %>%
  group_by(id_site, altitude) %>%
  summarise(ab_mean = mean(ab_rep), 
            ab_sd = sd(ab_rep), 
            eff = length(ab_rep), 
            se = ab_sd/sqrt(eff)) 
  
# ---------------------------------------------------

# ---------------------------------------------------
# Représentation
# ---------------------------------------------------

      # ---------------------------------------------------
      # Densité d'activité par station
      # ---------------------------------------------------
      jpeg("figures/TM_total_ab_site.jpeg", width = 1100, height = 700)
      TM_total_ab_site = ggplot(ab_mean, aes(x = altitude, y = ab_mean))+
                        geom_point(stat = "identity", fill = "darkorange")+
                        geom_errorbar(
                          aes(ymin = ab_mean - se, ymax = ab_mean + se), 
                          position = position_dodge2(padding = 0.5))+
                        labs(y = "Densité moyenne d'individus \ncollectés par site (individus m-2)", 
                             x = "Altitude (m)")+
                        facet_grid(id_site~., scales = "free_y")+
                        theme_bw()+
                        theme(legend.position = c(0.8, 0.75),
                              text = element_text(size=15))
      TM_total_ab_site
      dev.off()
      # ---------------------------------------------------
