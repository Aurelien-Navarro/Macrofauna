#-------------
#ACP##########
#-------------

#@objectif : creer une ACP entre mes stations et les variables environnementales
#sur la base des traits fonctionnels 

#librairies 
library(FactoMineR)
library(factoextra)
library(missMDA)
library(corrplot)
library(ade4)
library(tibble)

#Importation des jeux de donnée 
read.csv('data/derived-data/ENV_2023-04-28.csv', h=T, sep=",")->ENV
read.csv("data/derived-data/traits/traits_homo_2023-04-27.csv",header=T, sep=",")->traits
read.csv("data/derived-data/Esp/matrice_esp_2023-04-25.csv",h=T, sep=",")->esp

#Preparations des donnees
esp%>%
  unite(codeplot, gradient, alti)->esp
#Passage stations en ligne
esp%>%
  filter(!c(is.na(codeplot)))%>%
  remove_rownames()%>%
  column_to_rownames(var='codeplot')%>%
  select(!c('X'))->esp_ACP


##passage station en lignes
ENV %>%
  filter(!c(is.na(codeplot)))%>%
  remove_rownames()%>%
  column_to_rownames(var='codeplot')%>%
  select(!c('X','X_L93','Y_L93'))->ENV_ACP

##Passage des stations en ligne

as_tibble(detrialphamean)->detrialphamean
detrialphamean %>%
  dplyr::select(id_plot, everything()) %>%
  tibble::column_to_rownames(var = "id_plot") -> detrialphaACP

#ACP ENVIRONNEMENTALE-----------


PCA(ENV_ACP, scale.unit = TRUE, ncp = 5, graph = TRUE)->PCA
    ###exploration des donnees
eig.val <- get_eigenvalue(PCA)
eig.val
fviz_eig(PCA, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(PCA);var

fviz_pca_var(PCA, col.var = "black")

    ###qualité de representation des variables 
head(var$cos2, 4)

    ###visualisation desdites qualités

corrplot(var$cos2, is.corr=FALSE)

    ### bar plot qualité de reprentation variables
fviz_cos2(PCA, choice = "var", axes = 1:2)
#Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(PCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

#Contribution des var aux axes principaux 
head(var$contrib, 5)

corrplot(var$contrib, is.corr=FALSE) 

#Contribution à PC1 et PC2 
fviz_contrib(PCA, choice = "var", axes = 1:2, top = 10)

#Variables les plus contributives
fviz_pca_var(PCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#Les individus-----
ind <- get_pca_ind(PCA);ind
# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)

#Ind par valeur de cos2
fviz_pca_ind (PCA, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

  ##Qualité de représentation
fviz_cos2(PCA, choice = "ind")
  ## Contribution totale sur PC1 et PC2
fviz_contrib(PCA, choice = "ind", axes = 1:2)

  ##colorer ind par leur site d'appartenance

color <- substr(ENV$codeplot, 1, nchar(ENV$codeplot) - 5)


fviz_pca_ind(PCA, # Montre les points seulement (mais pas le "text")
             col.ind = color, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07","red",'black',"green",'purple',"blue","brown","pink"),
             addEllipses = TRUE,# Ellipses de concentration
             mean.point = FALSE,
             ellipse.type = "confidence",
             legend.title = "Sites",
             labelsize= 2,
             pointsize= 1,
             pointshape = 15,
             title = "Principal Component Analysis, environemental variables",
             subtitle = "Orchamp plots",
             caption = "Source: factoextra",
             xlab = "PC1", ylab = "PC2",
             legend.position = "top"
)

      ###Biplot, tres peu utile, tres vite le bazar
fviz_pca_biplot(PCA, repel = TRUE,
                col.var = "grey", # Couleur des variables
                col.ind = "orange"  # Couleur des individues
)

#ACP espèces-----

PCA(esp_ACP, scale.unit = TRUE, ncp = 5, graph = TRUE)->PCA_esp

###exploration des donnees
eig.val <- get_eigenvalue(PCA_esp)
eig.val
fviz_eig(PCA_esp, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(PCA_esp);var

fviz_pca_var(PCA_esp, col.var = "black")

###qualité de representation des variables 
head(var$cos2, 4)

###visualisation desdites qualités

corrplot(var$cos2, is.corr=FALSE)

### bar plot qualité de reprentation variables
fviz_cos2(PCA_esp, choice = "var", axes = 1:2)
#Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(PCA_esp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

#Contribution des var aux axes principaux 
head(var$contrib, 5)

corrplot(var$contrib, is.corr=FALSE) 

#Contribution à PC1 et PC2 
fviz_contrib(PCA_esp, choice = "var", axes = 1:2, top = 10)

#Variables les plus contributives
fviz_pca_var(PCA_esp, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#Les individus-----
ind <- get_pca_ind(PCA_esp);ind
# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)

#Ind par valeur de cos2
fviz_pca_ind (PCA_esp, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

##Qualité de représentation
fviz_cos2(PCA_esp, choice = "ind")
## Contribution totale sur PC1 et PC2
fviz_contrib(PCA_esp, choice = "ind", axes = 1:2)

##colorer ind par leur site d'appartenance

color <- substr(esp$codeplot, 1, nchar(esp$codeplot) - 5)


fviz_pca_ind(PCA_esp, # Montre les points seulement (mais pas le "text")
             col.ind = color, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07","red",'black',"green",'purple',"blue","brown","pink","lightgreen","darkred","darkblue","darkgreen"),
             addEllipses = TRUE,# Ellipses de concentration
             mean.point = FALSE,
             ellipse.type = "confidence",
             legend.title = "Sites",
             labelsize= 2,
             pointsize= 1,
             pointshape = 15,
             title = "Principal Component Analysis, specific variables",
             subtitle = "Orchamp plots",
             caption = "Source: factoextra",
             xlab = "PC1", ylab = "PC2",
             legend.position = "top"
)

###Biplot, tres peu utile, tres vite le bazar
#fviz_pca_biplot(PCA_esp, repel = TRUE,
                #col.var = "grey", # Couleur des variables
                #col.ind = "orange"  # Couleur des individus)

#ACP traits------

PCA(detrialphaACP, scale.unit = TRUE, ncp = 5, graph = TRUE)->PCA_traits

###exploration des donnees
eig.val <- get_eigenvalue(PCA_traits)
eig.val
fviz_eig(PCA_traits, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(PCA_traits);var

fviz_pca_var(PCA_traits, col.var = "black")

###qualité de representation des variables 
head(var$cos2, 4)

###visualisation desdites qualités

corrplot(var$cos2, is.corr=FALSE)

### bar plot qualité de reprentation variables
fviz_cos2(PCA_traits, choice = "var", axes = 1:2)
#Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(PCA_traits, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

#Contribution des var aux axes principaux 
head(var$contrib, 6)

corrplot(var$contrib, is.corr=FALSE) 

#Contribution à PC1 et PC2 
fviz_contrib(PCA_traits, choice = "var", axes = 1:2, top = 10)

#Variables les plus contributives
fviz_pca_var(PCA_traits, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#Les individus-----
ind <- get_pca_ind(PCA_traits);ind
# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)

#Ind par valeur de cos2
fviz_pca_ind (PCA_traits, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

##Qualité de représentation
fviz_cos2(PCA_traits, choice = "ind")
## Contribution totale sur PC1 et PC2
fviz_contrib(PCA_traits, choice = "ind", axes = 1:2)

##colorer ind par leur site d'appartenance

color <- substr(detrialphamean$id_plot, 1, nchar(detrialphamean$id_plot) - 5)


fviz_pca_ind(PCA_traits, # Montre les points seulement (mais pas le "text")
             col.ind = color, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07","red",'black',"green",'purple',"blue","brown","pink","lightgreen","darkred","darkblue","darkgreen"),
             addEllipses = TRUE,# Ellipses de concentration
             mean.point = FALSE,
             ellipse.type = "confidence",
             legend.title = "Sites",
             labelsize= 2,
             pointsize= 1,
             pointshape = 15,
             title = "Principal Component Analysis, specific variables",
             subtitle = "Orchamp plots",
             caption = "Source: factoextra",
             xlab = "PC1", ylab = "PC2",
             legend.position = "top"
)

###Biplot, tres peu utile, tres vite le bazar
fviz_pca_biplot(PCA_traits, repel = TRUE,
col.var = "black", # Couleur des variables
col.ind = "orange",)  # Couleur des individus)
