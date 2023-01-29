### Descarga y limpieza de la data ----
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

### Descriptivos generales ----
# Gr??fico de todo el ultimo trimestre
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
### Mapas distritales ----

#se filtra en bd_final las ordenes de compra y servico emitidas en lso meses de octubre, noviembre y diciembre
library(dplyr)
bd_final<-bd_final %>% filter(mes_emision == 10 | mes_emision == 11 | mes_emision == 12)

#Se crea una columna solo con el nombre de los distritos, paa lo cual limpiamos 
#la inscripci?jn "MUNICIPALIDAD DISTRITAL DE"
library(dplyr)
bd_final <- bd_final %>%
  mutate(NOMBdist = sub("MUNICIPALIDAD DISTRITAL DE", "", ENTIDAD.x))

### 1. Cantidad de ordenes por distrito, segun tipo ----

map_1 <- bd_final |> count(ENTIDAD.x, TIPOORDEN) #creamos un nuevo df (Municipalidad, tipo de orden, Cantidad)

map_1["ENTIDAD.x"] <- map_1["ENTIDAD.x"] |>
  mutate(ENTIDAD.x = gsub("MUNICIPALIDAD DISTRITAL DE ", "", ENTIDAD.x)) |>
  mutate(ENTIDAD.x = gsub("MUNICIPALIDAD METROPOLITANA DE ", "", ENTIDAD.x)) |>
  mutate(ENTIDAD.x = gsub(pattern = "\\((.*?)\\)", "", ENTIDAD.x)) |>
  mutate(ENTIDAD.x = gsub("- LIMA", "", ENTIDAD.x)) 
#Para el merge con la base de datos sobre distritos de Lima, uniformizamos la informacion. 
#Por ese motivo, nos quedamos solo con el nombre de los distritos quitando las palabras que les anteceden.


colnames(DF_MAPA) <- c("DISTRITO", "Tipoorden", "Cantidad") #renombramos las variables del DF para el merge por DISTRITO

map_lima <- dplyr::filter(map_DIST, REGION == "Lima Metropolitana") #de la base de datos por distrito, 
#filtramos la provincia de Lima Metropolitana

map_lima$DISTRITO <- toupper(map_lima$DISTRITO) #Ponemos en altas los valores de la variable distrito para el merge

db_lima_OS <- merge(x = map_lima, y = df_map_os, by = "DISTRITO", all.x = TRUE)
#Juntamos las bases de datos: la que contiene la informacion de los distritos y la que posee la informacion
#sobre tipos de ordenes y la cantidad por distrito.

###1.1. Cantidad de Ordenes de Servicio, por distrito ----
OS = ggplot(db_lima_OS, aes(geometry = geometry)) +
  geom_sf(aes(fill = Cantidad)) +
  ggtitle("Imagen 1. Cantidad de Órdenes de Servicio IV Trim")+
  labs(x = "", y = "")+
  scale_fill_gradient("Cantidad de ordenes de servicio",low = "#FCFFDD" , high = "#26185F", na.value = "white")
OS #Visualizacion de datos para OS

###1.2. Cantidad de Ordenes de Compra, por distrito----
db_lima_OC <- merge(x = map_lima, y = df_map_oc, by = "DISTRITO", all.x = TRUE)

OC=ggplot(db_lima_OC, aes(geometry = geometry)) +
  geom_sf(aes(fill = Cantidad)) +
  ggtitle("Imagen 2. Cantidad de Órdenes de Compra IV Trim")+
  labs(x = "", y = "")+
  scale_fill_gradient("Cantidad de ordenes de compra", low = "yellow", high = "red", na.value = "white") 
OC #Visualizacion de datos para OC


### 2. Montos designados por distrito, según tipo ----

# Calculamos el total de monto desembOlsado por Municipalidad Distrital y por el
#tipo de orden (compra , servicio)

bd_final_summary <- bd_final %>%
  group_by(NOMBdist, TIPOORDEN) %>%
  summarize(MONTO_TOTAL_ORDEN_ORIGINAL = sum(MONTO_TOTAL_ORDEN_ORIGINAL))

# Oredenamos por montos totales desembolsados
bd_final_summary <- bd_final_summary %>%
  arrange(desc(MONTO_TOTAL_ORDEN_ORIGINAL))

#Borramos en la base de datos resumen "- LIMA" y consideeramos a la Municipalidad
#metropolitana de Lima como  Lima.

bd_final_summary$NOMBdist <- gsub("MUNICIPALIDAD METROPOLITANA DE ","",bd_final_summary$NOMBdist)
bd_final_summary$NOMBdist <- sub(" - LIMA","",bd_final_summary$NOMBdist)
bd_final_summary$NOMBdist <- gsub("\\(|\\)","",bd_final_summary$NOMBdist)
bd_final_summary$NOMBdist <- gsub("CHOSICA","",bd_final_summary$NOMBdist)
bd_final_summary$NOMBdist <- gsub("LAS PALMERAS","",bd_final_summary$NOMBdist)

#Sacamos un resumen solo de las ordenes de servico.
bd_final_summary_servicio <- bd_final_summary %>%
  filter(TIPOORDEN == "Orden de Servicio")

#Sacamos un resumen soo de las ordenes de compra
bd_final_summary_compra <- bd_final_summary %>%
  filter(TIPOORDEN == "Orden de Compra") 

#cambiamos de nombre la columna NOMBdist por NOMBDIST, para homogenizar
colnames(bd_final_summary_servicio)[colnames(bd_final_summary_servicio)=="NOMBdist"] <- "NOMBDIST"

colnames(bd_final_summary_compra)[colnames(bd_final_summary_compra)=="NOMBdist"] <- "NOMBDIST"


#Se descarga y abren los paquetes que se requieren para dibujar los mapas

install.packages("pacman")
library(pacman)
p_load("sf","purrr","ggplot2","ggrepel","tidyverse","readxl")


#Se accede a los archivos que se usar?n para graficar los mapas

dirmapas <- "C:/Users/crist/Documents/GitHub/R-2023-Diplomado/BD" #La direcci?n de tu directorio de trabajo
setwd(dirmapas)
peru_d <- st_read("LIMITE_DISTRITAL_2020_INEI_geogpsperu_juansuyo_931381206.shp") #Este comando permite leer el shapefile y 'transformarlo' en un data frame
peru_d

#Probamos si funciona la base de datos de los mapas
ggplot(data = peru_d) +
  geom_sf()

#filtramos la base de datos peru_d en solo la provincia de Lima
peru_d_lima<-peru_d %>% filter(NOMBPROV == "LIMA")

#Borramos los espacios en los nombres de distritos para poder realizar los
#emparejamientos
peru_d_lima$NOMBDIST <- trimws(peru_d_lima$NOMBDIST)
bd_final_summary_servicio$NOMBDIST <- trimws(bd_final_summary_servicio$NOMBDIST)


#creamos las columas de coordenadas en una data frame
data_servicio <- merge(peru_d_lima, bd_final_summary_servicio, by.x = "NOMBDIST", by.y = "NOMBDIST", 
                       suffixes = c("_x","_y"), 
                       all.x = TRUE, 
                       sort = TRUE, 
                       nomatch = NULL)
#simpificamos el nombre de la columa MONTO
colnames(data_servicio)[colnames(data_servicio)=="MONTO_TOTAL_ORDEN_ORIGINAL"] <- "MONTO_TOTAL"


bd_final_summary_compra$NOMBDIST <- trimws(bd_final_summary_compra$NOMBDIST)

data_compra <- merge(peru_d_lima, bd_final_summary_compra, by.x = "NOMBDIST", by.y = "NOMBDIST", 
                     suffixes = c("_x","_y"), 
                     all.x = TRUE, 
                     sort = TRUE, 
                     nomatch = NULL)

#Graficar solo LA PROVINCIA de Lima

ggplot(data = peru_d %>%
         filter(NOMBPROV=="LIMA")) +
  geom_sf() 

### 2.1. Mapa distrital por monto total de ordenes de servicio----

library(dplyr)

# Agrupar los datos por NOMBDIST
merged_data_grouped <- data_servicio %>% group_by(NOMBDIST)

# Calcular la posici?n media de la geometr?a para cada grupo
merged_data_grouped <- merged_data_grouped %>% summarize(x = mean(st_coordinates(data_servicio[["geometry"]])[,1]),
                                                         y = mean(st_coordinates(data_servicio[["geometry"]])[,2]),
                                                         MONTO_TOTAL = mean(MONTO_TOTAL))

merged_data_filtered <- merged_data_grouped %>% filter(MONTO_TOTAL!= 0 & !is.na(MONTO_TOTAL))




library(dplyr)

#Creamos los rangos, para diferenciar los distritos que gastaron 
#m?s, en el cuatro trimestres del 2022

merged_data_filtered <- merged_data_filtered %>% 
  mutate(fill = cut(MONTO_TOTAL, 
                    breaks = c(0, 5000000, 10000000, 15000000, Inf), 
                    labels = c("yellow", "green", "orange", "red")))

data_servicio <- data_servicio %>% 
  mutate(fill = cut(MONTO_TOTAL, 
                    breaks = c(0, 5000000, 10000000, 15000000, Inf), 
                    labels = c("yellow", "green", "orange", "red")))


#mapa
ggplot(data = merged_data_filtered) +
  geom_sf(data = data_servicio, aes(fill = fill)) +
  ggtitle("Distritos de Lima - Monto Total - Orden de Servicio (IV Trim 2022)") +
  scale_fill_manual(values = c("yellow", "green","orange", "red"),na.value = "transparent", labels = c("Bajo", "Medio","Alto", "Muy alto")) +
  labs(fill = "Exposici?n")

#grafico de barras
ggplot(merged_data_filtered, aes(x = MONTO_TOTAL/1000, y = reorder(NOMBDIST, MONTO_TOTAL), fill = fill)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_y_discrete(limits = rev(levels(merged_data_filtered$NOMBDIST))) +
  ggtitle("Distritos de Lima - Monto Total - Orden de Servicio (IV Trim 2022)") +
  scale_fill_manual(values = c("yellow", "green","orange", "red"),na.value = "white", labels = c("Bajo", "Medio","Alto", "Muy alto")) + 
  labs(x = "Monto en Miles", y = "Distritos", fill = "Exposici?n") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#La municipalidad metropolitana de Lima fue la qeu se situo en 
#el rango muy alto, sobrepasando el monto d elos 15 millones en 
#monto pagoado en ordenes de servicio.
#seguido por el distrito de miraflores y Lurigancho(Chosica)


### 2.2. Mapa distrital por monto total de ordenes de compra----

library(dplyr)

# Agrupar los datos por NOMBDIST
merged_data_grouped2 <- data_compra %>% group_by(NOMBDIST)

# Calcular la posici?n media de la geometr?a para cada grupo
merged_data_grouped2 <- merged_data_grouped2 %>% summarize(x = mean(st_coordinates(data_compra[["geometry"]])[,1]),
                                                           y = mean(st_coordinates(data_compra[["geometry"]])[,2]),
                                                           MONTO_TOTAL_ORDEN_ORIGINAL = mean(MONTO_TOTAL_ORDEN_ORIGINAL))


#filtramos los distritos sin informaci?n y cambiamos de nombre la columna 
merged_data_filtered2 <- merged_data_grouped2 %>% filter(MONTO_TOTAL_ORDEN_ORIGINAL != 0 & !is.na(MONTO_TOTAL_ORDEN_ORIGINAL))
colnames(merged_data_filtered2)[colnames(merged_data_filtered2)=="MONTO_TOTAL_ORDEN_ORIGINAL"] <- "MONTO_TOTAL"


#se crean los rangos, pero por la difencia en montos, ahora se considera muy 
#alto los superiores a 1500 0000 soles
merged_data_filtered2 <- merged_data_filtered2 %>% 
  mutate(fill = cut(MONTO_TOTAL, 
                    breaks = c(0, 500000, 1000000, 1500000, Inf), 
                    labels = c("yellow", "green", "orange", "red")))


colnames(data_compra)[colnames(data_compra)=="MONTO_TOTAL_ORDEN_ORIGINAL"] <- "MONTO_TOTAL"

data_compra <- data_compra %>% 
  mutate(fill = cut(MONTO_TOTAL, 
                    breaks = c(0, 500000, 1000000, 1500000, Inf), 
                    labels = c("yellow", "green", "orange", "red")))



ggplot(data = merged_data_filtered2) +
  geom_sf(data = data_compra, aes(fill = fill)) +
  ggtitle("Distritos de Lima - Monto Total - Orden de Compra (IV Trim 2022)") +
  scale_fill_manual(values = c("yellow", "green","orange", "red"),na.value = "transparent", labels = c("Bajo", "Medio","Alto", "Muy alto")) +
  labs(fill = "Exposici?n")


ggplot(merged_data_filtered2, aes(x = MONTO_TOTAL/1000, y = reorder(NOMBDIST, MONTO_TOTAL), fill = fill)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_y_discrete(limits = rev(levels(merged_data_filtered2$NOMBDIST))) +
  ggtitle("Distritos de Lima - Monto Total - Orden de Compra (IV Trim 2022)") +
  scale_fill_manual(values = c("yellow", "green","orange", "red"),na.value = "white", labels = c("Bajo", "Medio","Alto", "Muy alto")) + 
  labs(x = "Monto en Miles", y = "Distritos", fill = "Exposici?n") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#La municipalidad de Miraflores fue la que se situo en 
#el rango muy alto, sobrepasando el monto de los 1500 000 en 
#monto pagoado en ordenes de compra.
#seguido por el distrito de Villa Maria del Triunfo y Lima.


### 3. Concentración proveedores ----

# Se define bse de concentración de proveedores
bd_conc_s <-  bd_final |> 
  filter(anio_emision == "2022" & (mes_emision == 10 | mes_emision ==11 | mes_emision == 12) & TIPOORDEN == "Orden de Servicio" )  |> 
  group_by(ENTIDAD.x) |>
  mutate(momto_trim = sum(MONTO_TOTAL_ORDEN_ORIGINAL),  n_ordenes = n(), n_distintas =  n_distinct(RUC_CONTRATISTA), ratio_conc = n_distintas/n_ordenes, inv_ratio = (1 - ratio_conc)) |> 
  filter(!duplicated(UBIGEO)) |> 
  select(1,16,20,21:29) |> 
  arrange(ENTIDAD.x)

bd_conc_c <-  bd_final |> 
  filter(anio_emision == "2022" & (mes_emision == 10 | mes_emision ==11 | mes_emision == 12) & TIPOORDEN == "Orden de Compra" )  |> 
  group_by(ENTIDAD.x) |>
  mutate(momto_trim = sum(MONTO_TOTAL_ORDEN_ORIGINAL),  n_ordenes = n(), n_distintas =  n_distinct(RUC_CONTRATISTA), ratio_conc = n_distintas/n_ordenes, inv_ratio = (1 - ratio_conc)) |> 
  filter(!duplicated(UBIGEO)) |> 
  select(1,16,20,21:29) |> 
  arrange(ENTIDAD.x)

#Se visualiza en mapa
mapa_lim <- map_DIST |> 
  filter(DEPARTAMENTO == "Lima" & PROVINCIA == "Lima") |> 
  rename(UBIGEO = COD_DISTRITO)

mapa_conc_s <- left_join(mapa_lim, bd_conc_s, by="UBIGEO") |> 
  arrange(desc(ratio_conc)) |> 
  mutate(cat_conc=case_when(ratio_conc>0.2 & ratio_conc<=0.4~"1 a 2 proveedores por cada 5 contratos",
                            ratio_conc>0.4 & ratio_conc<=0.6~"2 a 3 proveedores por cada 5 contratos",
                            ratio_conc>0.6 & ratio_conc<=0.8~"3 a 4 proveedores por cada 5 contratos",
                            ratio_conc>0.8~ "5 proveedores por cada 5 contratos",
                            TRUE ~ "Sin información") )

mapa_conc_c <- left_join(mapa_lim, bd_conc_c, by="UBIGEO") |> 
  arrange(desc(ratio_conc)) |> 
  mutate(cat_conc=case_when(ratio_conc>0.2 & ratio_conc<=0.4~"De 1 a 2 por cada 5 contratos",
                            ratio_conc>0.4 & ratio_conc<=0.6~"De 2 a 3 por cada 5 contratos",
                            ratio_conc>0.6 & ratio_conc<=0.8~"De 3 a 4 por cada 5 contratos",
                            ratio_conc>0.8~ "De 5 por cada 5 contratos",
                            TRUE ~ "Sin información"  ) )



#factor(mapa_conc_s$cat_conc, levels = c("1 a 2 proveedores por cada 5 contratos","2 a 3 proveedores por cada 5 contratos", "3 a 4 proveedores por cada 5 contratos"," 5 proveedores por cada 5 contratos" ))
colores_s <- c("#034e7b", "#3690c0","#74a9cf", "#d0d1e6", "white")
colores_c <- c("#cc4c02", "#fe9929","#fed98e", "#ffffd4", "white")

###3.1.Concentracion de proveedores por Ordenes de Servicios----
mapa_conc_s |> 
  ggplot() +
  aes(geometry=geometry) +
  scale_fill_manual(values=colores_s)+
  geom_sf(aes(fill=cat_conc)) +
  labs(title = "Imagen 5. Concentración de proveedores por órdenes de servicio")+
  guides(fill=guide_legend(title="N° de proveedores distintos adjudicados"))

###3.2.Concentracion de proveedores por Ordenes de Compras----

mapa_conc_c |>
  ggplot() +
  aes(geometry=geometry) +
  scale_fill_manual(values=colores_c)+
  #scale_fill_brewer(palette = "RdGy", na.value = "white")+
  geom_sf(aes(fill=cat_conc))+
  labs(title = "Imagen 6. Concentración de proveedores por órdenes de compra")+
  guides(fill=guide_legend(title="N° de proveedores distintos adjudicados"))
