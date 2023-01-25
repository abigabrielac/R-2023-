# Pr??ctica Calificada N° 2 - Grupo 4 ----
# Integrantes:
# - Gabriela Calvo
# - Mauricio Ib????ez
# - C??sar N????ez
# - Cristian Orellana

# PREGUNTA 1 ----
## PREGUNTA 1.A.----
#install.packages("gganimate")
#install.packages("gifski")
#install.packages("av")
#install.packages("transformr")

library(tidyverse)
library(lubridate)
library(gapminder)
library(gganimate)

# Descargamos la data y lo guardamos en el 
setwd("D:/1. Documentos/1. Estudios/7. Diplomado PUCP Data Science/04. Fundamentos de R/R-2023-Diplomado/BD")
data <- read_delim("positivos_covid.csv", delim =";" ) 

data <- data |> 
  filter(DEPARTAMENTO == "APURIMAC" | DEPARTAMENTO == "AMAZONAS" | DEPARTAMENTO == "AYACUCHO" | DEPARTAMENTO == "HUANCAVELICA" | DEPARTAMENTO == "PASCO" | DEPARTAMENTO == "TACNA" )

data <- data |> 
  mutate(anio = year(ymd(FECHA_RESULTADO)), mes = month(ymd(FECHA_RESULTADO))) |> 
  mutate(n_mes = case_when(anio == 2020 ~ mes - 2,
                           anio == 2021 ~ mes - 2 + 12,
                           anio == 2022 ~ mes - 2 + 24, 
                           anio == 2023 ~ mes - 2 + 36,
                           TRUE ~ 99))


data_2 <- data |> 
  count(DEPARTAMENTO, n_mes) |> 
  rename(casos_mes = n)


data_2 <- data_2 |> 
  filter(n_mes >= 1 & n_mes <=21)

data_2 |> 
  ggplot() + 
  aes(x = n_mes, y = casos_mes, color = DEPARTAMENTO) + 
  geom_line() +
  labs(x = 'N??mero de mes desde inicio de la pandemia',
       y = 'N??mero de casos positivos al mes') +
  transition_reveal(n_mes) +
  labs(title = "Evoluci??n del n??mero de casos positivos seg??n departamento",
       subtitle = "Mes: {round(frame_along, , digits = 0)}")


# An??lisis
# Ola1 (entre el mes 5 (agosto 2020) y 7 (octubre 2020)) y Ola2 (mes 11 (enero 2021) y mes 15 (mayo 2021))
# Ola 1: tacna el m??s jodido en Ola 1, pero en Ola 2 estuvo mejor 
# Ola 2: Apur??mac el m??s jodido, pero no se vio tan afectado en Ola 1


## PREGUNTA 1.B.----


# PREGUNTA 2 ----

