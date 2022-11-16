
#Fija tu área de trabajo we, yo ya no lo hago porque ya lo tengo

library(tidyverse)
library(readr)


baset<-read.csv("~/GitHub/Data-Science/Datasets/Teams.csv") #Lee la base 
view(baset)

meets<-baset %>%
  filter(teamID == "NYN") %>%
  select(W,L,R,RA,yearID)#Como solo que remos la parte de los meet de NY solo 
#seleccionamos ese equipo con las variables que nos interesa.

view(meets)
