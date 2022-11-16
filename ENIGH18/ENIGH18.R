#CLASE 07 / NOV / 2022

install.packages("tidyverse")
library(tidyverse)
devtools::install_github("beanumber/mdsr")
library(mdsr)
library(ggplot2)

#Exploración de la base concentradohogar de la ENIGH_2018
#Abrimos la base de datos que está en formato .dbf
#instalamos el paquete que se necesita

install.packages("foreign")
library(foreign)

#Fija el directorio de trabajo (en el escritorio de la computadora)
setwd("C:/Users/UAMI/Desktop")

#generamos un objeto (la base de datos)
concentrado<-read.dbf("concentradohogar.dbf")

concentrado

?Foreign

#Explorar la muestra (sin ponderadores)
#tablas de frecuencia
tama�o_loc_freq<-table(concentrado$tam_loc)
tama�o_loc_freq
prop.table(tama�o_loc_freq)
install.packages("summarytools")
library(summarytools)
freq(concentrado$tam_loc)
freq(concentrado$est_socio)

#Descriptivas de variables cuantitativas
install.packages("moments")
library(moments)

#sin el tidyverse podemos calcular estaditicas, por ejemplo
#media
mean(concentrado$edad_jefe)
median(concentrado$edad_jefe)

#con tidyverse podemos dar formato y hacer tablas más presentables 
#Empezamos por declarar una función que contiene todas las estadisticas que queremos 

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

#Estadisticas de la edad del jefe de familia 
concentrado %>%
  summarise_at(vars(edad_jefe),listaFunciones) %>%
  mutate(variable="edad_jefe") %>%
  select(last_col(), everything()) %>%
  flextable() %>%
  fontsize(size=10, part="all") %>%
  colformat_double(j=-2, digit=2) %>%
  autofit

#Estadisitias de la edad del gasto corriente
concentrado %>%
  summarise_at(vars(ing_cor),listaFunciones) %>%
  mutate(variable="ingreso_corriente") %>%
  select(last_col(), everything()) %>%
  flextable() %>%
  fontsize(size=10, part="all") %>%
  colformat_double(j=-2, digit=2) %>%
  autofit
