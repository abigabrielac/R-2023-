# Practica Calificada N$B!k(B 2 - Grupo 4 ----
## Integrantes:
## - Gabriela Calvo
## - Mauricio Ibanez
## - Cesar Nunez
## - Cristian Orellana

# PREGUNTA 1 ----
## PREGUNTA 1.A.----
##install.packages("gganimate")
##install.packages("gifski")
##install.packages("av")
##install.packages("transformr")

library(tidyverse)
library(lubridate)
library(gapminder)
library(gganimate)

## Descargamos la data directamente desde el link provisto y lo almacenamos como un tible
setwd("D:/1. Documentos/1. Estudios/7. Diplomado PUCP Data Science/04. Fundamentos de R/R-2023-Diplomado/BD")
link <- "https://files.minsa.gob.pe/s/eRqxR35ZCxrzNgr/download"
data <- read_delim(link, delim =";" )

## Al revisar la data, notamos que considera la informacion individual de todos 
## los casos positivos desde el inicio de la pandemia para todos los departamentos
## del Peru. Por lo tanto, generamos un filtro a la data para quedarnos unicamente
## con los departamentos sugeridos.

data <- data |> 
  filter(DEPARTAMENTO == "APURIMAC" | DEPARTAMENTO == "AMAZONAS" | DEPARTAMENTO == "AYACUCHO" | DEPARTAMENTO == "HUANCAVELICA" | DEPARTAMENTO == "PASCO" | DEPARTAMENTO == "TACNA" )

## Ahora, generamos las variables de anio y mes necesarias para luego generar la 
## variable n_mes que contiene el numero de mes desde el inicio de la pandemia.
data <- data |> 
  mutate(anio = year(ymd(FECHA_RESULTADO)), mes = month(ymd(FECHA_RESULTADO))) |> 
  mutate(n_mes = case_when(anio == 2020 ~ mes - 2,
                           anio == 2021 ~ mes - 2 + 12,
                           anio == 2022 ~ mes - 2 + 24, 
                           anio == 2023 ~ mes - 2 + 36,
                           TRUE ~ 99))

## Ahora, generamos un nuevo tibble con el objetivo de que contenga tres variables
## que utilizaremos para el graico dinamico: DEPARTAMENTO, n_mes y n_casos. Esta
## ultima la generamos a partir de la funcion count (que contabiliza el numero de
## casos para cada departamento y cada mes).
data_2 <- data |> 
  count(DEPARTAMENTO, n_mes) |> 
  rename(casos_mes = n)

## Como la pregunta nos pide realizar un grafico entre el mes 1 y 21 desde el 
## inicio de la pandemia, generamos un filtro a la nueva base de datos generada.
data_2 <- data_2 |> 
  filter(n_mes >= 1 & n_mes <=21)

## Ya que tenemos la data lista para el grafico, generamos el grafico dinamico con 
## las librerias ggplot() y gganimate(). Para ello, utilizamos las capas: base, 
## aesthetic, geom, coordinates y las extensiones de gganimate()
gif_P1A <- data_2 |> 
  ggplot() + 
  aes(x = n_mes, y = casos_mes, color = DEPARTAMENTO) + 
  geom_line() +
  labs(x = 'Numero de mes desde inicio de la pandemia',
       y = 'Numero de casos positivos al mes') +
  transition_reveal(n_mes) +
  labs(title = "Evolucion del numero de casos positivos segun departamento",
       subtitle = "Mes: {round(frame_along, , digits = 0)}")

## Finalmente, guardamos el grafico animado como un archivo gif
anim_save("gif_P1A.gif", gif_P1A)


## Analisis ----
## El grafico muestra la evolucion de los casos positivos de COVID-19 en seis 
## regiones del pais desde marzo del 2020 hasta diciembre del 2021. A partir de 
## este se desprenden las siguientes observaciones:
##  - Se observan dos pico de contagios en las seis regiones que corresponden a 
##    las olas 1 y 2. La primera ola ocurre entre los meses de agosto y octubre 
##    2020. En cambio, la segunda ola ocurre entre los meses de enero a mayo 2021.
##  - Tacna fue la region con mas casos positivos en la primera ola. Sin embargo, 
##    registro un numero de casos menor en la segunda ola.
##  - Apurimac fue la region con mas casos positivos durante la segunda ola y tuvo
##    un incremento notorio en casos respecto a la primera ola 
##  - Regiones como Pasco y Ayacucho presentaron similar numero de casos positivos
##    tanto en la primera como en la segunda ola.


## PREGUNTA 1.B.----

## Primero  creamos una nueva columna con la media movil por 3 meses con la funcion rollmean
## Agrupamos los resultados por departamento, para que la media movil se aplique seg?n esa variable
## Para aplicar la funci?n rollmean, nombramos a la nueva variable, designamos sobre la variable 
## que se va a trabajar y de acuerdo al n?mero de periodos.
## En este caso ser?a cada tres meses.
library(zoo)

data_2 <- data_2 |> 
  mutate(Macroregion = case_when (DEPARTAMENTO == "AMAZONAS" ~ "Selva",
                                  DEPARTAMENTO == "APURIMAC" ~ "Sur",
                                  DEPARTAMENTO == "TACNA" ~ "Sur",
                                  DEPARTAMENTO == "AYACUCHO" ~ "Centro",
                                  DEPARTAMENTO == "HUANCAVELICA" ~ "Centro",
                                  DEPARTAMENTO == "PASCO" ~ "Centro"))

data_3 <- data_2 |> 
  subset(select = -c(DEPARTAMENTO)) |> 
  group_by(Macroregion, n_mes) |> 
  summarise(casos_mes = sum(casos_mes)) |> 
  mutate(mes_MA=rollmean(x=casos_mes, 3, na.pad =TRUE, align = "right", 0))


## Ahora, generamos la variable macroregion y designamos de acuerdo a lo solicitado. 
## Usamos la funci?n case_when para generar la nueva columa seg?n las condiciones.


## Tomamos como insumo el codigo del gif 1A y en vez de que el grafico nos presente 
## el n? de casos por mes, nos presentara la media movil por cada tres meses.
## De acuerdo a la macroregion.

gif_P1B <-  data_3 |> 
  ggplot(aes(x = n_mes, y = mes_MA, color = Macroregion))+ 
  geom_line() +
  labs(x = 'Numero de mes desde inicio de la pandemia',
       y = 'Media movil (3 meses)') +
  transition_reveal(n_mes) +
  labs(title = "Evolucion de la media movil de casos positivos por macroregion",
       subtitle = "Mes: {round(frame_along, , digits = 0)}")


## Finalmente, guardamos el grafico animado como un archivo gif
anim_save("gif_P1B.gif", gif_P1B)

## Analisis ----
## El grafico muestra la evolucion de los casos positivos de COVID-19, por macroregion,
## en funcion de la media movil de un periodo de 3 meses, desde marzo del 2020 hasta diciembre del 2021. 
## A partir de este se desprenden las siguientes observaciones:


##  - Se observan 2 picos de contagio segun la media movil, en Julio del 2020 y febrero del 2021.
##    Para el caso de la macroregion centro y selva, el primer pico es más alto que el segundo. Y, en el caso, 
##    de la macroregion sur se da al revés.
##  - El primer pico de decenso se da en el mes de octubre de 2020, luego el media de casos aumenta y
##    finalmente decrece a partir del mes de marzo de 2021.
##  No existe mayor diferencia entre el primer gráfico y el presente, salvo que la agrupación se da por macroregiones.

# PREGUNTA 2 ----

## a) Diagnostico de valores perdidos ------

#Para abrir bases de datos en formatos (SAS,spss,stata) instalamos el paquete haven

install.packages("haven")
library(haven)

#Se utiliza la funcion "read_dta()" para leer el archivo de Stata

datos <- read_dta("D:/QLab/Fundamentos en R/PC2/PER_2021_LAPOP_AmericasBarometer_v1.2_w.dta")

#Se especifica la variables que se quiere obtener de la BD "datos"

sublapop <- subset(datos, select = c("q2", "q1tb", "prov1t", "b2", "it1", "cses6n", "ur1new", "ing4", "gi0n", "anestg"))

#se instlaa el paquete pacman para instalar y abrir los paquetes necesarios para el analisis requerido.

install.packages("pacman")
library(pacman)
p_load("VIM","DEoptimR","minqa","nloptr","simputation","mice","tidyverse","DMwR2","naniar")

#Con el analisis descriptivo de las variables seleccionadas, se  observa la cantidad
#de valores pertidos (NA's), siendo el mas resaltante b2 con 1547 .

summary(sublapop)

#Se verifica que si existen valores perdidos
anyNA(sublapop)
any_na(sublapop)

#Se hace un conteo general de la cantidad de NA's que hay en la BD
n_miss(sublapop)

#Se calcula el  porcentaje de valores perdidos en toda la BD.
prop_miss(sublapop)
pct_miss(sublapop)
#Considerando el porcentaje con respecto al total de toda la BD se tiene un 6.069783%
#lo cual no representa (segun los parametros) mayor relevancia.

#N??mero total de valroes perdidos.
n_complete(sublapop)

#Se realiza el analisis de valores peridos por variables, donde se observa que mas del 50% 
#de los datos de la variable b2 son datos perdidos (NA's)

miss_var_summary(sublapop)

miss_var_table(sublapop)

#Lo observamos ahora en candidades brutas.
miss_case_summary(sublapop)


vis_miss(sublapop)

#se observa el en un grafico los datos perdidos, donde se verifica que la variblre b2 representa
#un problema.

gg_miss_case(sublapop)

#se puede observar ciertas relaciones entre los valores perdidos
#en respectivas varibles.
gg_miss_upset(sublapop)

a=aggr(sublapop,numbers=T)
a
summary(a)
aggr(sublapop,numbers=T,sortcomb=TRUE,sortvar=TRUE,only.miss=TRUE)

# Verificando el patron VP visualmente

## Mecanismo completamente aleatorio (MCAR)?  
## O mecanismo aleatorio (MAR)?

matrixplot(sublapop)
# Foto de la dataset en colores.
# Rojo: Los datos faltantes
# Escala de grises segun valores de vectores numericos
# Para poder inspeccionar necesitamos que la grafica sea interactiva y utilizamos x11()
x11()
matrixplot(sublapop)
# Analicemos la variable b2: sera MCAR o MAR?
# MCAR: Valores perdidos no debera coincidir con ningun patron en las otras variables. Aleatoriamente
# MAR: Valores perdidos coinciden con ciertos valores de otras variables. 
# Es MCAR no sigue ningun patron.. 
# 

## b) Evaluacion de las varibles ing4 e it1------

##  Analisis grafico ----

# Podemos hacer una prueba mas minuciosa con un boxplot

# Probemos con dos variables una con 16 dotasos perdidos(ing4) y la otra
# con 107 casos perdidos (it1)


VIM::pbox(sublapop[4:8], pos=1)

# Si los dos boxplot son iguales. No hay diferencia. MCAR
# Si son diferentes: Estan asociados. MAR
# Conclusion es MCAR.

##   Prueba de hipotesis ----

# (Comparacion de medias en dos grupos) 
# El grupo es la variable que presenta perdidos. 

# Prueba t de medias (evaluar el mecanismo de la variable ing4)
# H0: No hay diferencia
# H1: Hay diferencia 
t.test(ing4 ~ is.na(it1), data=sublapop)
# Si el p valor < 0.05, rechaza la H0 y concluyes que hay diferencia. MAR
# Si el p valor > 0.05, no se rechaza la H0 y concluyes que no hay diferencia. MCAR

# El p valor es mayor a 0.05 por lo que no se rechaza la H0,
# no hay diferencias por tanto es MCAR.





