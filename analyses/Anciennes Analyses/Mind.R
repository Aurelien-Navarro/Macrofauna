#MASTER MIND#

library(ggplot2)
library(dplyr)
library(tidyverse)

colors<-c("red","orange","yellow","green","blue","purple","pink","brown")
selected<-sample(colors, 4)

a<-"yellow"
b<-"brown"

A<-data.frame(Couleur="A", Place= 1, Essai=c(1:8))
B<-data.frame(Couleur="B",Place=2, Essai=c(1:8))
C<-data.frame(Couleur="C",Place=3, Essai=c(1:8))
D<-data.frame(Couleur="D",Place=4, Essai=c(1:8))
bind_rows(A, B, C, D)->PrPlot
as.numeric(PrPlot$Essai)->PrPlot$Essai


ggplot(PrPlot, aes(x="Place", y="Essai"))+
  geom_point()+
  theme_minimal()




if(a==selected[1]||b==selected[2]){
print("yellow")
  }else{
  print("nope")
}

 


