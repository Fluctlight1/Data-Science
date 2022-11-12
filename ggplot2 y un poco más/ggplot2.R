# Clase 26 de Octubre de 2022

install.packages("tidyverse")
library(tidyverse)
install.packages("devtools")
install.packages("usethis")
library(usethis)
library(devtools)
devtools::install_github("beanumber/mdsr")
library(mdsr)
library(ggplot2)

getwd()
read.csv

?CIACountries

base<-CIACountries
names(CIACountries)
head(CIACountries)
tail(CIACountries)
View(CIACountries)

#Descriptivo
summary(CIACountries)
summary(CIACountries$pop)
sd(CIACountries$pop)
var(CIACountries$pop)
min(CIACountries$pop)
max(CIACountries)
median(CIACountries)
table(CIACountries)

# 3.1.1 Aesthetics
#data:CIACountries

#Educacion
summary(CIACountries$educ)

#histograma
a<-ggplot(data=CIACountries, aes(x=educ))
a+geom_histogram(bindwidth =1)
a+geom_density(kernel ="gaussian")

#Diagrama de caja 
a+geom_boxplot()
aa<-ggplot(data=CIACountries, aes(x=net_users, y=educ))
aa+geom_boxplot()

#GDP
summary(CIACountries$gdp)

#Histograma

b<-ggplot(data=CIACountries, aes(gdp))
b+geom_histogram(bandwidth =1000)
b+geom_density(kernel ="gaussian")

#Diagrama de Caja 
b+geom_boxplot()

bb<-ggplot(data=CIACountries, aes(x=net_users, y=gdp))
bb+geom_boxplot()

#Diagrama de Dispersion 

g<-ggplot(data=CIACountries, aes(y=gdp, x=educ))
g+geom_point(size=3)
g+geom_point(aes(color=net_users), size=3)

install.packages("RColorBrewer")
library(RColorBrewer)
g+geom_point(aes(color=espectral), size=3)
g+geom_text(aes(color=net_users, size=roadways))

g+geom_text(aes(label=country, color=net_users), size=3)
g+geom_text(aes(label=country, color=net_users), size=3)


#3.1.2 Scales
g+geom_point(aes( color=net_users, size=roadways))+
  coord_trans(y="log10")

#3.1.4 Facets
g+geom_point(alpha=0.9,aes(size=roadaways))+
  coord_trans(y="log10")+
  facet_wrap(∼net_users, nrow=1)+
  theme(legend.position = "top")

#Media de GDP por categoría de net_users 
CIACountries %>% group_by(net_users) %>% summarise(mean(gdp))
CIACountries %>% group_by(net_users) %>% summarise(median(gdp))

#3.1.5 Layers 
#Data:MedicareCharges 
?MedicareCharges

#library(dplyr)
names(MedicareCharges)
View(MedicareCharges)
nrow(MedicareCharges)
table(MedicareCharges$stateProvider)
base_medi<-MedicareCharges

table(MedicareCharges$drg)

#Usamos filter para seleccionar datos (sub-base)
ChargesNJ<-MedicareCharges %>%
  filter(stateProvider == "NJ")

p<-ggplot( data=ChargesNJ, aes(x=drg, y=mean_charge))+
  geom_col(fill="gray")+
  ylab("Statewide Averange Charge ($)")+
  xlab("Medical Procedure (DRG)")+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=rel(0.5)))
p

p<-ggplot( data=ChargesNJ, aes(x=reorder(drg, mean_charge), y=mean_charge))+
  geom_col(fill="gray")+
  ylab("Statewide Averange Charge ($)")+
  xlab("Medical Procedure (DRG)")+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=rel(0.5)))

p+geom_point(data=MedicareCharges, size=1, alpha=0.3)

#3.2.1 Univariate Displays
#data: SAT
#Histograms and density plots 

names(SAT_2010)
g<-ggplot(data=SAT_2010, aes(x=math))

g+geom_histogram(binwidth = 10)+labs(x="Averange math SAT score")

g+geom_density(adjust=0.3)

g<-ggplot(data=SAT_2010, aes(x=expenditure, y=math))
gg+geom_point()

gg<-gg+geom_smooth(method="lm", se=FALSE)+
  xlab("Averange expenditure per student ($1000)")+
  ylab("Averange score on math SAT")

#Usamos mutate para generar una nueva variable 
SAT_2010<-SAT_2010 %>%
  mutate(
    SAT_rate=cut(
      sat_pct,
      breaks=c(0,30,60,100),
      labels=c("low", "medium", "high")
    )
  )
names(SAT_2010)
gg<-gg %+% SAT_2010 #>+> para incluir la nueva variable en las graficas
gg+aes(color=SAT_rate)

gg+facet_wrap(∼SAT_rate)+geom_point()
