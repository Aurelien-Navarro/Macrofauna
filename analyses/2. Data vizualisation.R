# @title
# INat la Tania 
# 
# @description
# Identification des invertébrés dans les pièges barber de la Tania
# 
# @objectif
# Tester le gain d'utiliser INat pour identifier les invertébrés que je ne sais pas ID 
#
# @details
# 0. Interrogation de la base de données Eco&Sols et de INaturalist pour récupérer les données d'occurrence
# 1. Fusion des données et homogénéisation taxonomique
# 2. Export d'un fichier de données propres


# Libraries
librarian::shelf(tidyr, dplyr, ggplot2, rinat, RODBC, stringr)

# Data load 
df <- read.csv("data/derived-data/clean_data_2023-03-03.csv", h = T, sep = ",") %>%
  select(!c(X, ANI))

# Total abundance per station for various groups
ab_comput <- function(df) {
  df %>%
  group_by(gradient, alti, method, Replicate.number) %>%
  summarize(tot_ab = sum(abundance)) %>%
  filter(method %in% c("barber", "tri manuel")) %>%
  mutate(tot_ab = ifelse(is.na(tot_ab), 0, tot_ab)) %>%
  group_by(gradient, alti, method) %>%
  summarize(ab_mean = mean(tot_ab, na.omit = T), 
            ab_sd = sd(tot_ab), 
            eff = length(tot_ab), 
            se = ab_sd/sqrt(eff))
  }


ab_tot       <- ab_comput(df)%>%
                      mutate(grp = "all")
ab_carabidae <- ab_comput(df[df$familyName == "Carabidae",])%>%
                      mutate(grp = "Carabidae")
ab_ortho     <- ab_comput(df[df$orderName == "Orthoptera",])%>%
                      mutate(grp = "Orthoptera")
ab_vdt       <- ab_comput(df[df$orderName == "Crassiclitellata",])%>%
                      mutate(grp = "VdT")


ggplot(data = ab_tot[ab_tot$method == "tri manuel",], 
       aes(y = ab_mean, x = as.numeric(alti), color = gradient))+
  geom_point(stat = "identity", fill = "darkorange")+
  geom_errorbar(
    aes(ymin = ab_mean - se, ymax = ab_mean + se), 
    position = position_dodge2(padding = 0.5))+
  labs(y = "Densité moyenne d'individus \ncollectés par site (individus m-2)", 
       x = "Altitude (m)")+
  #scale_y_continuous(trans='log10') +
  ylim(c(0, 20))+
  geom_smooth(se = T)+
  theme_bw()

ggplot(data = ab_ortho[ab_ortho$method == "barber",], 
       aes(y = ab_mean, x = as.numeric(alti), color = gradient))+
  geom_point(stat = "identity", fill = "darkorange")+
  geom_errorbar(
    aes(ymin = ab_mean - se, ymax = ab_mean + se), 
    position = position_dodge2(padding = 0.5))+
  labs(y = "Densité d'activité moyenne \n (individus par piège)", 
       x = "Altitude (m)")+
  #scale_y_continuous(trans='log10') +
  ylim(c(0, 15))+
  geom_smooth(se = T)+
  theme_bw()
