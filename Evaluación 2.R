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

## Al revisar la data, notamos que considera la informaci??n individual de todos 
## los casos positivos desde el inicio de la pandemia para todos los departamentos
## del Per??. Por lo tanto, generamos un filtro a la data para quedarnos ??nicamente
## con los departamentos sugeridos.

data <- data |> 
  filter(DEPARTAMENTO == "APURIMAC" | DEPARTAMENTO == "AMAZONAS" | DEPARTAMENTO == "AYACUCHO" | DEPARTAMENTO == "HUANCAVELICA" | DEPARTAMENTO == "PASCO" | DEPARTAMENTO == "TACNA" )

## Ahora, generamos las variables de a??o y mes necesarias para luego generar la 
## variable n_mes que contiene el n??mero de mes desde el inicio de la pandemia.
data <- data |> 
  mutate(anio = year(ymd(FECHA_RESULTADO)), mes = month(ymd(FECHA_RESULTADO))) |> 
  mutate(n_mes = case_when(anio == 2020 ~ mes - 2,
                           anio == 2021 ~ mes - 2 + 12,
                           anio == 2022 ~ mes - 2 + 24, 
                           anio == 2023 ~ mes - 2 + 36,
                           TRUE ~ 99))

## Ahora, generamos un nuevo tibble con el objetivo de que contenga tres variables
## que utilizaremos para el gr??fico din??mico: DEPARTAMENTO, n_mes y n_casos. Esta
## ??ltima la generamos a partir de la funci??n count (que contabiliza el n??mero de
## casos para cada departamento y cada mes).
data_2 <- data |> 
  count(DEPARTAMENTO, n_mes) |> 
  rename(casos_mes = n)

## Como la pregunta nos pide realizar un gr??fico entre el mes 1 y 21 desde el 
## inicio de la pandemia, generamos un filtro a la nueva base de datos generada.
data_2 <- data_2 |> 
  filter(n_mes >= 1 & n_mes <=21)

## Ya que tenemos la data lista para el gr??fico, generamos el gr??fico din??mico con 
## las librer??as ggplot() y gganimate(). Para ello, utilizamos las capas: base, 
## aesthetic, geom, coordinates y las extensiones de gganimate()
gif_P1A <- data_2 |> 
  ggplot() + 
  aes(x = n_mes, y = casos_mes, color = DEPARTAMENTO) + 
  geom_line() +
  labs(x = 'N??mero de mes desde inicio de la pandemia',
       y = 'N??mero de casos positivos al mes') +
  transition_reveal(n_mes) +
  labs(title = "Evoluci??n del n??mero de casos positivos seg??n departamento",
       subtitle = "Mes: {round(frame_along, , digits = 0)}")

## Finalmente, guardamos el gr??fico animado como un archivo gif
anim_save("gif_P1A.gif", gif_P1A)


## An??lisis ----
## El gr??fico muestra la evoluci??n de los casos positivos de COVID-19 en seis 
## regiones del pa??s desde marzo del 2020 hasta diciembre del 2021. A partir de 
## este se desprenden las siguientes observaciones:
## ???  Se observan dos pico de contagios en las seis regiones que corresponden a 
##    las olas 1 y 2. La primera ola ocurre entre los meses de agosto y octubre 
##    2020. En cambio, la segunda ola ocurre entre los meses de enero a mayo 2021.
## ???  Tacna fue la regi??n con m??s casos positivos en la primera ola. Sin embargo, 
##    registr?? un n??mero de casos menor en la segunda ola.
## ???  Apur??mac fue la regi??n con m??s casos positivos durante la segunda ola y tuvo
##    un incremento notorio en casos respecto a la primera ola 
## ???  Regiones como Pasco y Ayacucho presentaron similar n??mero de casos positivos
##    tanto en la primera como en la segunda ola.


## PREGUNTA 1.B.----

## Primero  creamos una nueva columna con la media movil por 3 meses con la funcion rollmean
## Agrupamos los resultados por departamento, para que la media movil se aplique según esa variable
## Para aplicar la función rollmean, nombramos a la nueva variable, designamos sobre la variable 
## que se va a trabajar y de acuerdo al número de periodos.
## En este caso sería cada tres meses.
data_2 <- data_2 |> group_by(DEPARTAMENTO)|>  mutate(mes_MA=rollmean(x=casos_mes, 3, na.pad =TRUE, align = "right", 0))


## Ahora, generamos la variable macroregion y designamos de acuerdo a lo solicitado. 
## Usamos la función case_when para generar la nueva columa según las condiciones.
data_2 <- data_2 |> 
  mutate(Macroregion = case_when (DEPARTAMENTO == "AMAZONAS" ~ "Selva",
                                  DEPARTAMENTO == "APURIMAC" ~ "Sur",
                                  DEPARTAMENTO == "TACNA" ~ "Sur",
                                  DEPARTAMENTO == "AYACUCHO" ~ "Centro",
                                  DEPARTAMENTO == "HUANCAVELICA" ~ "Centro",
                                  DEPARTAMENTO == "PASCO" ~ "Centro"))

## Tomamos como insumo el codigo del gif 1A y en vez de que el grafico nos presente 
## el nº de casos por mes, nos presentara la media movil por cada tres meses.
## De acuerdo a la macroregion.

gif_P1B <- data_2 |> 
  ggplot(aes(x = n_mes, y = mes_MA, color = Macroregion))+ 
  geom_line() +
  labs(x = 'Numero de mes desde inicio de la pandemia',
       y = 'Media móvil (3 meses)') +
  transition_reveal(n_mes) +
  labs(title = "Evolucion de la media movil de casos positivos por macroregion",
       subtitle = "Mes: {round(frame_along, , digits = 0)}")


## Finalmente, guardamos el grafico animado como un archivo gif
anim_save("gif_P1B.gif", gif_P1B)



# PREGUNTA 2 ----

