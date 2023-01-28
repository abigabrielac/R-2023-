### 1. Descarga y limpieza de la data ----
#install.packages("mapsPERU")
#install.packages("sf")
library(tidyverse)
library(readxl)
library(mapsPERU)
library(dplyr)
library(sf)

# Seteamos el directorio asociado a nuestro repositorio de GitHub clonado a nuestra PC
setwd("C:/Users/User/Documents/GitHub/R-2023-Diplomado/BD")

# Nos descargamos la base de datos del Github. 
# Esta base de datos recoge la informaci??n registrada por todas las entidades 
# p??blicas que han realizado compras directas de bienes y servicios por menores 
# de 9 UIT durante diciembre de 2022. 
bd_dic <- read_xlsx("CONOSCE_ORDENESCOMPRADICIEMBRE2022_0.xlsx")
bd_nov <- read_xlsx("CONOSCE_ORDENESCOMPRANOVIEMBRE2022_0.xlsx")
bd_oct <- read_xlsx("CONOSCE_ORDENESCOMPRAOCTUBRE2022_0.xlsx")
#bd_sep <- read_xlsx("CONOSCE_ORDENESCOMPRASETIEMBRE2022_0.xlsx")
#bd_ago <- read_xlsx("CONOSCE_ORDENESCOMPRAAGOSTO2022_0.xlsx")
#bd_jul <- read_xlsx("CONOSCE_ORDENESCOMPRAJULIO2022_0.xlsx")
#bd_jun <- read_xlsx("CONOSCE_ORDENESCOMPRAJUNIO2022_0.xlsx")
#bd_may <- read_xlsx("CONOSCE_ORDENESCOMPRAMAYO2022_0.xlsx")
#bd_abr <- read_xlsx("CONOSCE_ORDENESCOMPRAABRIL2022_0.xlsx")
#bd_mar <- read_xlsx("CONOSCE_ORDENESCOMPRAMARZO2022_0.xlsx")
#bd_feb <- read_xlsx("CONOSCE_ORDENESCOMPRAFEBRERO2022_0.xlsx")
#bd_ene <- read_xlsx("CONOSCE_ORDENESCOMPRAENERO2022_0.xlsx")

#bd_lima <- rbind(bd_dic,bd_nov, bd_oct , bd_sep, bd_ago, bd_jul, bd_jun, bd_may, bd_abr, bd_mar, bd_feb, bd_ene)
bd_lima <- rbind(bd_dic,bd_nov, bd_oct)


# Debido a que en nuestro tema de trabajo nos interesa conocer ??nicamente la 
# informaci??n asociada al departamento de Lima y para compras menores a 9 UIT, 
# por lo que realizamos un filtro:
bd_lima <- bd_lima |> 
  filter(DEPARTAMENTO__ENTIDAD== "LIMA") 
bd_lima <- bd_lima |> 
  filter(MONTO_TOTAL_ORDEN_ORIGINAL <= 9*4400) 

# Ahora generamos una base de datos alternativa para quedarnos solo con los RUC 
# y nombres de entidades que son de Lima Metropolitana
# bd_munis <- bd_lima |> 
#  subset(select = c(ENTIDAD, RUC_ENTIDAD)) |> 
#  filter(!duplicated(RUC_ENTIDAD)) |> 
#  filter(substr(ENTIDAD, 1, 13) == "MUNICIPALIDAD")

lista_munis <- read_xlsx("lista_munis.xlsx")

# Extraemos la informaci??n de las fechas de emisi??n de las ordenes de compra/servicio
bd_final <- bd_lima |> 
  left_join(lista_munis, by= "RUC_ENTIDAD") |> 
  filter(LIMA_MET =="Si") |> 
  subset(select = -c(19)) |> 
  mutate(anio_emision = substr(FECHA_DE_EMISION, 1, 4), mes_emision = as.numeric(strftime(as.Date(FECHA_DE_EMISION), "%m")))

### 2. Descriptivos generales ----
# Gr??fico de todo el ??ltimo trimestre
bd_final |> 
  filter(anio_emision == "2022" & (mes_emision == 10 | mes_emision ==11 | mes_emision == 12)) |> 
  count(mes_emision , TIPOORDEN) |> 
  ggplot() + aes(x = mes_emision , y = n) + geom_line() + geom_point() + aes(colour =TIPOORDEN) 

# Gr??fico del 2022 completo
bd_final |> 
  filter(anio_emision == "2022") |> 
  count(mes_emision , TIPOORDEN) |> 
  ggplot() + aes(x = mes_emision , y = n) + geom_line() + geom_point() + aes(colour =TIPOORDEN) 

# Cuadro de frecuencias por municipalidad en el ??ltimo trimestre
bd_final |> 
  filter(anio_emision == "2022" & (mes_emision == 10 | mes_emision ==11 | mes_emision == 12)) |> 
  group_by(ENTIDAD.x, TIPOORDEN) |> 
  summarise(n = n()) |> 
  arrange(desc(n))

# Cuadro de montos promedio de por municipalidad en el ??ltimo trimestre
bd_final |> 
  filter(anio_emision == "2022" & (mes_emision == 10 | mes_emision ==11 | mes_emision == 12)) |> 
  group_by(ENTIDAD.x , TIPOORDEN) |> 
  summarise(media = mean(MONTO_TOTAL_ORDEN_ORIGINAL)) |> 
  arrange(desc(media))



### 4. Concentración proveedores ----

# Se define bse de concentración de proveedores
bd_conc_s <-  bd_final |> 
  filter(anio_emision == "2022" & (mes_emision == 10 | mes_emision ==11 | mes_emision == 12) & TIPOORDEN == "Orden de Servicio" )  |> 
  group_by(ENTIDAD.x) |>
  mutate(momto_trim = sum(MONTO_TOTAL_ORDEN_ORIGINAL),  n_ordenes = n(), n_distintas =  n_distinct(RUC_CONTRATISTA), ratio_conc = n_distintas/n_ordenes, inv_ratio = 1 - ratio_conc) |> 
  filter(!duplicated(UBIGEO)) |> 
  select(1,16,20,21:28) |> 
  arrange(ENTIDAD.x)


#Se visualiza en mapa
mapa_lim <- map_DIST |> 
  filter(DEPARTAMENTO == "Lima" & PROVINCIA == "Lima") |> 
  rename(UBIGEO = COD_DISTRITO)

mapa_conc_s <- left_join(bd_conc_s, mapa_lim, by="UBIGEO") 



