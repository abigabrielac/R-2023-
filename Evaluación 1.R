#Evaluacion 1 ----
library(tidyverse)
##Pregunta 1 ----
###Pregunta 1.a) ----

##Pregunta 2  ----
###Pregunta 2.a)  ----

#Seteamos la dirección donde se encuentran las bases
setwd("C:/Users/User/Dropbox/Otros/Cursos/Diplomado ciencias de datos/3. Fundamentos R/Clase 3")

#Importamos las bases
copas_matches <- read_csv("WorldCupMatches.csv")
copas <- read_csv("WorldCups.csv")

###Pregunta 2.b)  ----
###Pregunta 2.c)  ----


###Pregunta 2.d)  ----

# Primero: señalamos que vamos a generar un conteo por referee por lo que utilizamoss el "group_by"
# Segundo: Indicamos que queremos el número de veces que aparecen 
# Tercero: ordenamos en forma descendente

copas_matches |> 
  group_by(Referee)  |>  
  summarise(partidos_arbitrados=n()) |>  
  arrange(desc(partidos_arbitrados))

# El Referee que más partidos arbitró fue: Ravshan IRMATOV (UZB)

###Pregunta 2.e)  ----

# Primero: creamos variable que cuente el total de goles por partido
# Segundo: ordenamos de forma descendente la base para ver que partido tuvo más goles

copas_matches |> 
  mutate(total_goles =  `Home Team Goals` + `Away Team Goals`)  |>  
  arrange(desc(total_goles))

# El partido con más goles fue en 1954 Austria vs Suiza, en total 12 goles

###Pregunta 2.f)  ----

# Primero se verifica que en ambas bases la variable llave de unión se llame igual

names(copas_matches)
names(copas)

#Segundo: se crea una nueva base con las columnas de interés

new_copa <- copas_matches  |>  
  left_join(copas[,c(1:5)], by = "Year")


##Pregunta 3  ----
###Pregunta 3.a)  ----
library(tidyverse)
library(readxl)

# Seteamos el directorio asociado a nuestro repositorio de GitHub clonado a nuestra PC
setwd("D:/1. Documentos/1. Estudios/7. Diplomado PUCP Data Science/04. Fundamentos de R/R-2023-Diplomado/BD")

# Nos descargamos la base de datos del Github. 
# Esta base de datos recoge la información registrada por todas las entidades 
# públicas que han realizado compras directas de bienes y servicios por menores 
# de 9 UIT durante diciembre de 2022. 
bd <- read_xlsx("CONOSCE_ORDENESCOMPRADICIEMBRE2022_0.xlsx")

# Nuestra base de datos contiene 18 variables, las cuales se pueden observar a continuación:
names(bd)
# [1] "ENTIDAD"                       "RUC_ENTIDAD"                   "FECHA_REGISTRO"                "FECHA_DE_EMISION"             
# [5] "FECHA_COMPROMISO_PRESUPUESTAL" "FECHA_DE_NOTIFICACION"         "TIPOORDEN"                     "NRO_DE_ORDEN"                 
# [9] "ORDEN"                         "DESCRIPCION_ORDEN"             "MONEDA"                        "MONTO_TOTAL_ORDEN_ORIGINAL"   
# [13] "OBJETOCONTRACTUAL"             "ESTADOCONTRATACION"            "TIPODECONTRATACION"            "DEPARTAMENTO__ENTIDAD"        
# [17] "RUC_CONTRATISTA"               "NOMBRE_RAZON_CONTRATISTA"

# Debido a que en nuestro tema de trabajo nos interesa conocer únicamente la 
# información asociada al departamento de Lima y para compras menores a 9 UIT, 
# por lo que realizamos un filtro:
bd <- bd |> filter(DEPARTAMENTO__ENTIDAD== "LIMA") 
bd <- bd |> filter(MONTO_TOTAL_ORDEN_ORIGINAL <= 9*4400) 

### Pregunta 3.b) ----
# Ahora armamos la tabla de datos descriptivos para la variable numérica "MONTO_TOTAL_ORDEN_ORIGINAL", que recoge los montos de las órdenes de compra:
# Mínimo, máximo, media, mediana, desviación estándar y rango intercuartílico
bd |> 
  summarise(minimo=min(MONTO_TOTAL_ORDEN_ORIGINAL), maximo=max(MONTO_TOTAL_ORDEN_ORIGINAL) , media=mean(MONTO_TOTAL_ORDEN_ORIGINAL), mediana = median(MONTO_TOTAL_ORDEN_ORIGINAL) , sd(MONTO_TOTAL_ORDEN_ORIGINAL) , IQR(MONTO_TOTAL_ORDEN_ORIGINAL))


### Pregunta 3.c)  ----
# Ahora armamos una tabla de frecuencias para la variable "TIPODECONTRATACION"

bd |> 
  count(TIPODECONTRATACION) |> 
  mutate(Pct. = prop.table((n))*100) 

