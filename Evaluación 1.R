#Evaluacion 1 ----
##Pregunta 1 ----
###Pregunta 1.1 ----



# Pregunta 3
## Pregunta 3.a.
library(tidyverse)
library(readxl)

# Seteamos el directorio asociado a nuestro repositorio de GitHub clonado a nuestra PC
setwd("D:/1. Documentos/1. Estudios/7. Diplomado PUCP Data Science/04. Fundamentos de R/R-2023-Diplomado/BD")

# Nos descargamos la base de datos del Github. 
### Esta base de datos recoge la información registrada por todas las entidades 
### públicas que han realizado compras directas de bienes y servicios por menores 
### de 9 UIT durante diciembre de 2022. 
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

## Pregunta 3.b.
# Ahora armamos la tabla de datos descriptivos para la variable numérica "MONTO_TOTAL_ORDEN_ORIGINAL", que recoge los montos de las órdenes de compra:
## Mínimo, máximo, media, mediana, desviación estándar y rango intercuartílico
bd |> 
  summarise(minimo=min(MONTO_TOTAL_ORDEN_ORIGINAL), maximo=max(MONTO_TOTAL_ORDEN_ORIGINAL) , media=mean(MONTO_TOTAL_ORDEN_ORIGINAL), mediana = median(MONTO_TOTAL_ORDEN_ORIGINAL) , sd(MONTO_TOTAL_ORDEN_ORIGINAL) , IQR(MONTO_TOTAL_ORDEN_ORIGINAL))


## Pregunta 3.c
# Ahora armamos una tabla de frecuencias para la variable "TIPODECONTRATACION"

bd |> 
  count(TIPODECONTRATACION) |> 
  mutate(Pct. = prop.table((n))*100) 


