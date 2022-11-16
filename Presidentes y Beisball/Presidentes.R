#Clase 14 de Noviembre de 2022 (Lunes)
#Data wrangling on ne table 
#Manejo de base de datos 

library (tidyverse)

#Pregunto en que directorio me encuentro
getwd()
#Fijamos el area de trabajo  
setwd("~/GitHub/Data-Science/Datasets")

#Leemos la base de datos 
basep<-read.csv("presidential.csv")
view(basep)   #Para ver los datos en la tabla.


#Comandos importantes (Todos son instalando tidyverse)
#select()  para seleccionar columnas o variables.
#filter() para seleccionar renglones o registros
#mutate()  para generar o transformar variables
#arrange() para ordenar renglones 
#summarize() para calcular estadisticas 

#SELECCIONAR VARIABLES y Generamos una base nueva 
subp<-select(basep,name,party)

#Seleccionamos renglones (solo presidentes democratas)
democratas<-filter(basep,party=="Democratic")   #cuando ponemos 2 iguales (==) estamos comparando

#Podemos mezclar ambos comandos para seleccionar renglones y columnas en un solo comando
#Seleccionamos solo el nombre de democratas 
#No voy a generar una nueva base 
select(filter(basep,party=="Democratic"),name)

#Generamos una nueva variable en la que vamos a calcular los tiempos de duración de cada presidente. 
#el comando "lubridate" trabaja con fechas y tiempos 
install.packages("lubridate")
library(lubridate)
basep <-basep %>%
  mutate(dias=interval(start, end)/ddays(1)) %>%
  mutate(anios=interval(start,end)/dyears(1))

basep

#Renombrar las variables que todas esten en ingles
basep<-basep %>%
  rename(days=dias) %>% 
  rename(years=anios)

#Ordenando la base de datos 
basep %>%      #(Acomoda los datos de los años de menor a mayor)
  arrange(years)

basep %>%
  arrange(desc(years))     #Acomoda los datos de los años de mayor a menor.

#Dos variables como criterio para ordenar 
basep %>%
  arrange(desc(years), party)

basep %>%
  arrange(party,desc(years))

#Estadisticas de resumen 
basep %>%
  summarize(
    N=n(),
    inicio=min(year(start)),
    final=max(year(end)),
    num_democ=sum(party=="Democratic"),
    anios=sum(years)
  )

#Estadisticas de resumen por grupo (paratidos)
basep %>%
  group_by(party) %>%
  summarize(
    N=n(),
    inicio=min(year(start)),
    final=max(year(end)),
    num_democ=sum(party=="Democratic"),
    anios=sum(years),
    tiempo_promedio=mean(years)
  )
