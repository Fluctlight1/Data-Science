#Despues de instalar tidyverse y cargar la base 


nrow(base)
#para saber el nombre de las variables
names(base)
#para identificar los primeros 5 o 6 renglones(registros) de la base
head(base)
#para identificar los 煤ltimos 5 o 6 renglones(registros) de la base
tail(base)
#para solicitar o llamar los renglones que estemos analizando o que nos interese revisar. se pueden cambiar los numeros para llamar diferentes combinaciones (l贸gica de matrices)
base[1:10,15:18]
base[1:7,21:50]
base[1:5,33:43]
#para renglones especificos de forma discontinua incluir la letra "c" 
base[c(1:10, 325),c(6,25,34,98)]
base[c(1:7, 205),c(10,14,16,18)]
base[c(7:17, 50),c(3,6,30,60)]


#Para ver el resumen de los datos
install.packages("summarytools")
library(summarytools)

#descriptivas de variables cuantitativas
install.packages("moments")
library(moments)

listaFunciones=list(n=length,
                    media=mean,
                    desv_est=sd,
                    asimetria=skewness,
                    curtos=kurtosis,
                    min=min,
                    mediana=median,
                    max=max)
install.packages("flextable")
library(flextable)


base %>%
  summarise_at(vars(ing_cor),listaFunciones) %>%
  mutate(variable="Ingreso-cor") %>%
  select(last_col(), everything()) %>%
  flextable() %>%
  fontsize(size=10, part="all") %>%
  colformat_double(j=-2, digit=2) %>%
  autofit

#los 5 numeros (min, 25th, mediana, 75th, max) (cuartiles)
fivenum(base$ing_cor)

#Ahora: Que pasa si ponderamos 
####Analisis de la encuesta completa usando los factores de expansi贸n
install.packages("survey")
library(survey)
#Arroja el dato del factor de expansi贸n (en este caso, los hogares)
#Cada hogar tiene la representaci髇 de un grupo especifico
#Factor de expansi贸n (Es el ponderador) le da el peso a cada uno de los integrantes de a cuerdo al grupo que representa
summary(base$factor)
#Para saber el dato real o total de los datos
sum(base$factor)
#despues de correr el comando anterior (INTERPRETACI覰)
#El m醲imo de hogares representa al menos 9 hogares diiferentes
#Al menos el 25% (o menos) representa 170 hogares diferentes.... etc

#Qu茅 debemos hacer para tener en cuenta el factor de expansi贸n?
#Declaro el dise帽o de muestreopara lograr alcanzar el total de la poblaci贸n
diseno<-svydesign(id=~upm, strata=~est_dis, data=base, weights=~factor)
diseno
names(diseno)

#Estimaci贸n del ingreso corriente a nivel nacional (total promedio)
ingreso_nal_prom<-svymean(~ing_cor, diseno, na.rm=TRUE)
ingreso_nal_prom
# na.rm=TRUE en el c贸digo anterior nos indica que se remuevan loa NA para poder hacer el c谩lculo


#lo anterior nos arroj贸 la media ponderada (total) y el error estandar  

cuantiles<-svyquantile(~ing_cor,diseno, c(.25,.5,.75))
cuantiles

base$entidad<-substr(base$folioviv,1,2)
base$Nhog<-1
ingreso_ent_prom<-svyby(~ing_cor,denominator=~Nhog, by=~entidad, diseno, svyratio) 
#Por entidad federativa
ingreso_ent_prom

base$entidad<-substr(base$folioviv,1,2)
base$Nhog<-1
ingreso_ent_prom<-svyby(~ing_cor,denominator=~Nhog, by=~entidad, diseno, svyratio) 
ingreso_ent_prom



