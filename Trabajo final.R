### Descarga y limpieza de la data ----
#install.packages("mapsPERU")
#install.packages("sf")
library(tidyverse)
library(mapsPERU)
library(dplyr)
library(sf)
library(openxlsx)

# Como primer paso del codigo, importamos los archivos xlsx del repositorio de 
# GitHub del grupo, utilizando la funci??n read.xlsx de la libreria openxlsx
# Para ello, definimos los links de las tres bases de datos que utilizaremos
link_dic <- "https://github.com/abigabrielac/R-2023-Diplomado/raw/main/BD/CONOSCE_ORDENESCOMPRADICIEMBRE2022_0.xlsx"
link_nov <- "https://github.com/abigabrielac/R-2023-Diplomado/raw/main/BD/CONOSCE_ORDENESCOMPRANOVIEMBRE2022_0.xlsx"
link_oct <- "https://github.com/abigabrielac/R-2023-Diplomado/raw/main/BD/CONOSCE_ORDENESCOMPRAOCTUBRE2022_0.xlsx"

# A continuaci??n, importamos y juntamos las tres bases de inter??s con la funci??n 
# read.xlsx y rbind. Asimismo, debido a que en nuestro tema de trabajo nos interesa 
# conocer unicamente la  informacion asociada al departamento de Lima y para  
# compras menores a 9 UIT, realizamos un filtro. 
bd_osce <- rbind(read.xlsx(link_dic),
                 read.xlsx(link_nov),
                 read.xlsx(link_oct)) |> 
  filter(DEPARTAMENTO__ENTIDAD == "LIMA") |>
  filter(MONTO_TOTAL_ORDEN_ORIGINAL <= 9 * 4400)

# En la base de datos importada identificamos que contamos con muchas entidades 
# p??blicas ubicadas en el departamento de Lima. Por lo tanto, como nos interesa
# quedarnos ??nicamente con las municipalidades de Lima Metropolitana, utilizamos
# un excel generado por nosotros para la obtenci??n de las 43 municipalidades dis
# -tritales de Lima Metropolitana. Esto se hizo porque la base de datos no permite 
# diferencias provincias de Lima (solo permite filtrar por departamento).

# OJO: si bien esto se podr??a generar en el mismo R, convenimos que era m??s pr??ctico
# utilizar una base alternativa que contar con un c??digo de R que filtre espec??ficamente
# las 43 municipalidades (esto ser??a muy largo). Asimismo, esta base recoge los 
# c??digos ubigeo de los distritos, lo cual nos servir?? m??s adelante para los mapas.

link_munis <- "https://github.com/abigabrielac/R-2023-Diplomado/raw/main/BD/lista_munis.xlsx"
mun_list <- read.xlsx(link_munis)

# Finalmente, obtenemos la base de datos final para el trabajo al combinar las 
# bases del osce para el ??ltimo trimestre de 2022 (bd_osce) con la base de datos
# de municipalidades (mun_list). Para ello utilizamos la funci??n left_join y filter.
# Asimismo, generamos las variables anio_emision y mes_emision que nos serviran  
# para identificar el mes de la emision de las ordenes de compra y servicios. 
# Debido a que nos interesa en particular el ultimo trimestre del a??o, nos quedamos
# con las ordenes emitidas en octubre, noviembre y diciembre. 
bd_final <- bd_osce |> 
  left_join(mun_list, by= "RUC_ENTIDAD") |> 
  filter(LIMA_MET =="Si") |> 
  subset(select = -c(19)) |> 
  mutate(fecha_emision = as.numeric(as.Date(FECHA_DE_EMISION), "%Y-%m-%d"),
         fecha_registro = as.numeric(as.Date(FECHA_REGISTRO), "%Y-%m-%d"),
         anio_emision = substr(FECHA_DE_EMISION, 1, 4),
         mes_emision = as.numeric(strftime(as.Date(FECHA_DE_EMISION), "%m")),
         mes_registro = as.numeric(strftime(as.Date(FECHA_REGISTRO), "%m")),
         dia_registro = as.numeric(strftime(as.Date(FECHA_REGISTRO), "%d")))  |> 
  filter(anio_emision == 2022 & (mes_emision == 10 | mes_emision == 11 | mes_emision == 12))

### Descriptivos generales ----

# Cuadro de frecuencias de os y oc por municipalidad en el ultimo trimestre
cuadro1 <- bd_final |> 
  group_by(ENTIDAD.x, TIPOORDEN) |> 
  summarise(n = n()) |> 
  spread(key = TIPOORDEN, value = n) |> 
  mutate_all(~ ifelse(is.na(.), 0, .)) |> 
  mutate(n_ords = `Orden de Servicio` + `Orden de Compra`) |> 
  arrange(desc(n_ords))|> 
  print( n = 23) 

# Cuadro de montos promedio de por municipalidad en el ultimo trimestre
cuadro2 <- bd_final |> 
  group_by(ENTIDAD.x , TIPOORDEN) |> 
  summarise(media = mean(MONTO_TOTAL_ORDEN_ORIGINAL)) |> 
  spread(key = TIPOORDEN, value = media) |> 
  arrange(desc(`Orden de Servicio`))

### Mapas distritales ---- 
#(mapsPERU): borrar coordenadas y el fondo plomo 

# Utilizando el paquete mapsPERU cargamos la base de datos y filtramos para los distritos de Lima Metropolitana
map_lima <- map_DIST |> #Cargamos la base de datos sobre los distritos del Peru
  dplyr::filter(REGION == "Lima Metropolitana") |> #filtramos la provincia de Lima Metropolitana,
  rename(UBIGEO = COD_DISTRITO ) #renombramos la variable del DF para el merge por UBIGEO

#### 0. Mapas sobre el registro  ----

# Generamos la variable de registro tard??o de las ??rdenes, utilizando la funci??n
# mutate y case_when siguiendo la definici??n de la Directiva del OSCE.

bd_registro <- bd_final |> 
  mutate(reg_a_tiempo = case_when(mes_emision == mes_registro ~ 100 ,
                                  mes_emision == 10 & dia_registro < 9 ~ 100,
                                  mes_emision == 11 & dia_registro < 8 ~ 100,
                                  mes_emision == 12 & dia_registro < 6 ~ 100,
                                  TRUE ~ 0)) |> 
  group_by(UBIGEO, ENTIDAD.x, TIPOORDEN ) |> 
  summarise(pct_reg_a_tiempo = mean(reg_a_tiempo)) 

map_reg_os <- bd_registro |> 
  filter(TIPOORDEN == "Orden de Servicio")  #cantidad de ordenes de servicio por ubigeo

map_reg_oc <- bd_registro |> 
  filter(TIPOORDEN == "Orden de Compra") #cantidad de ordenes de compras por ubigeo


db_lima_reg_os <- merge(x = map_lima, y = map_reg_os, by = "UBIGEO", all.x = TRUE) |>  #Juntamos las bases de datos: 
  arrange(desc(pct_reg_a_tiempo)) |> 
  mutate(cat_registro = case_when(pct_reg_a_tiempo<33 ~"1. Menos 33% de las OS registradas a tiempo",
                                  pct_reg_a_tiempo<66 & pct_reg_a_tiempo>=33 ~"2. Entre 33% y 66% de las OS registradas a tiempo",
                                  pct_reg_a_tiempo>=66 ~"3. M??s del 66% de las OS registradas a tiempo",
                                  TRUE ~ "Sin informaci??n")) # Creamos una variable categ??rica que nos permita hacer la leyenda del mapa

db_lima_reg_oc <- merge(x = map_lima, y = map_reg_oc, by = "UBIGEO", all.x = TRUE) |> #Juntamos las bases de datos: 
  arrange(desc(pct_reg_a_tiempo)) |> 
  mutate(cat_registro = case_when(pct_reg_a_tiempo<33 ~"1. Menos 33% de las OC registradas a tiempo",
                                  pct_reg_a_tiempo<66 & pct_reg_a_tiempo>=33 ~"2. Entre 33% y 66% de las OC registradas a tiempo",
                                  pct_reg_a_tiempo>=66 ~"3. M??s del 66% de las OC registradas a tiempo",
                                  TRUE ~ "Sin informaci??n")) # Creamos una variable categ??rica que nos permita hacer la leyenda del mapa


colores_s <- c("#74a9cf", "#3690c0", "#034e7b", "white") #Definimos manualmente los colores del mapa
colores_c <- c("#fed98e", "#fe9929","#cc4c02", "white") #Definimos manualmente los colores del mapa

plot_os <- db_lima_reg_os |> 
  ggplot() +
  aes(geometry=geometry) +
  scale_fill_manual(values=colores_s)+
  geom_sf(aes(fill=cat_registro)) +
  labs(title = "Imagen 1. Porcentaje de ordenes de servicio registradas a tiempo")+
  guides(fill=guide_legend(title="Porcentaje de ??rdenes de servicio registradas a tiempo"))

plot_oc <- db_lima_reg_oc |> 
  ggplot() +
  aes(geometry=geometry) +
  scale_fill_manual(values=colores_c)+
  geom_sf(aes(fill=cat_registro)) +
  labs(title = "Imagen 2. Porcentaje de ordenes de compra registradas a tiempo")+
  guides(fill=guide_legend(title="Porcentaje de ??rdenes de compra registradas a tiempo")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

library(plotly)

plotly::ggplotly(plot_os) 
plotly::ggplotly(plot_oc) 


#### 1. Cantidad de ordenes por distrito, segun tipo ----

map_1 <- bd_final |> count(UBIGEO, TIPOORDEN)#Para facilitar el manejo de data, filtramos por "Ubigeo"

colnames(map_1) <- c("UBIGEO", "Tipoorden", "Cantidad")# Renombramos variables

#creamos los dataframes para combinarlos con el dataframe que contiene la informacion sobre los distritos
df_map_os <- filter(map_1, Tipoorden == "Orden de Servicio")#cantidad de ordenes de servicio por ubigeo
df_map_oc <- filter(map_1, Tipoorden == "Orden de Compra")#cantidad de ordenes de compras por ubigeo

##### 1.1. Cantidad de Ordenes de Servicio, por distrito ----
db_lima_OS <- merge(x = map_lima, y = df_map_os, by = "UBIGEO", all.x = TRUE) #Juntamos las bases de datos: 
#la que contiene la informacion de los distritos y la que posee la informacion 
#sobre las ordenes de servicio y la cantidad por distrito.


OS = ggplot(db_lima_OS, aes(geometry = geometry)) + #creamos el mapa
  geom_sf(aes(fill = Cantidad)) +
  ggtitle("Imagen 1. Cantidad de ??rdenes de Servicio IV Trim")+
  labs(x = "", y = "")+
  scale_fill_gradient("Cantidad de ordenes de servicio",low = "#FCFFDD" , high = "#26185F", na.value = "white")
OS #Visualizacion de datos para OS

##### 1.2. Cantidad de Ordenes de Compra, por distrito----
db_lima_OC <- merge(x = map_lima, y = df_map_oc, by = "UBIGEO", all.x = TRUE) #Juntamos las bases de datos: 
#la que contiene la informacion de los distritos y la que posee la informacion 
#sobre las ordenes de compra y la cantidad por distrito.

OC=ggplot(db_lima_OC, aes(geometry = geometry)) + #creamos el mapa
  geom_sf(aes(fill = Cantidad)) +
  ggtitle("Imagen 2. Cantidad de ??rdenes de Compra IV Trim")+
  labs(x = "", y = "")+
  scale_fill_gradient("Cantidad de ordenes de compra", low = "yellow", high = "red", na.value = "white") 
OC #Visualizacion de datos para OC


####  2. Montos designados por distrito, segun tipo de orden  ----

# Para iniciar el analisis, se calcula los montos totales por distrito y por tipo de orden.
# para obtener este reporte se agrupa la base de datos por UBIGEO y tipo de oden.
#luego se suman los montos correspondients.


monto_oyc <- bd_final %>%
  group_by(UBIGEO, TIPOORDEN) %>%
  summarize(MONTO_TOTAL_ORDEN_ORIGINAL = sum(MONTO_TOTAL_ORDEN_ORIGINAL))

colnames(monto_oyc) <- c("UBIGEO", "Tipoorden", "Monto_Total") #renombrar las columnas obtenidas, para una mejor
#de los datos

#Como reultado del dataframe otenido anteriormente, filtramos y obtenemos dos
#dataframe dividos por el tipo de orden.

df_monto_os <- filter(monto_oyc, Tipoorden == "Orden de Servicio")#cantidad de ordenes de servicio por ubigeo
df_monto_oc <- filter(monto_oyc, Tipoorden == "Orden de Compra")#cantidad de ordenes de compras por ubigeo

##### 2.1. Montos totales de Ordenes de Servicio, por distrito ----

db_lima_monto_OS <- merge(x = map_lima, y = df_monto_os, by = "UBIGEO", all.x = TRUE) #Juntamos las bases de datos: 

#la que contiene la informacion de los distritos y la que posee la informacion 
#sobre las ordenes de servicio y los montos totales por distrito.

#Se dibuja el mapa corresondiente, con el gradiente de colores que diferencia 
#el monto total gastado en cada distrito de Lima Metropolitana.

monto_OS = ggplot(db_lima_monto_OS, aes(geometry = geometry)) + 
  geom_sf(aes(fill = Monto_Total)) +
  ggtitle("Imagen 5. Montos totales de Servicio IV Trim") +
  labs(x = "", y = "") +
  scale_fill_gradient("MONTO TOTAL", labels = function(x) format(x, scientific = FALSE, big.mark = "."),
                      low = "#FCFFDD", high = "#26185F", na.value = "white") +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.ticks=element_blank(), panel.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
monto_OS


#Gr?fico de barras
#Para evitar que se grafiquen los distritos sin informacion, borramos los distritos que no 
#presentan informai?n para el periodo en an?lisis.

db_lima_monto_OS_clean <- na.omit(db_lima_monto_OS)


library(stringr)

db_lima_monto_OS_clean$DISTRITO <- str_wrap(db_lima_monto_OS_clean$DISTRITO, width = 30)

# utilizamos la funci?n str_wrap de la librer?a stringr para limitar la longitud 
#de cada nombre de distrito a un n?mero espec?fico de caracteres (30), lo que asegurar? que los 
#nombres de los distritos no se corten en varias l?neas

monto_OS_barras = ggplot(db_lima_monto_OS_clean, aes(x=Monto_Total/1000, y= reorder(DISTRITO,Monto_Total),fill= Monto_Total)) + 
  
  geom_bar(stat = "identity",width = 0.6) + # agrupaci?n por distrito
  ggtitle("Imagen 6. Montos totales de Servicio IV Trim") +
  labs(x = "Monto Total en miles de soles", y = "Distrito") +
  scale_fill_gradient("MONTO TOTAL", labels = function(x) format(x, scientific = FALSE, big.mark = "."),
                      low = "#FCFFDD", high = "#26185F", na.value = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "white"))

monto_OS_barras

#Como se observa en las imagenes 5 y 6, el distrito con mayor monto total por ordenes de servicio
#es Lima Metropolitana, lo cual se justifica por u gran envergadura. Asimismo se aprecia los los 
#cuatro distrisos que lo acompanan son:Lurigancho(Chosica), Miraflores, Comas y San Borja, con 
#montos que no superan el 50% de lo claculado para el distrito de LIma.

##### 2.2. Montos totales de Ordenes de Compra, por distrito ----

db_lima_monto_OC <- merge(x = map_lima, y = df_monto_oc, by = "UBIGEO", all.x = TRUE) #Juntamos las bases de datos: 

##la que contiene la informacion de los distritos y la que posee la informacion 
#sobre las ordenes de compra y los montos totales por distrito.

#Se dibuja el mapa corresondiente, con el gradiente de colores que diferencia 
#el monto total gastado en cada distrito de Lima Metropolitana.


monto_OC = ggplot(db_lima_monto_OC, aes(geometry = geometry)) + #creamos el mapa
  geom_sf(aes(fill = Monto_Total)) +
  ggtitle("Imagen 7. Montos totales de Compra IV Trim") +
  labs(x = "", y = "") +
  scale_fill_gradient("MONTO TOTAL", labels = function(x) format(x, scientific = FALSE, big.mark = "."),
                      low = "yellow", high = "red", na.value = "white") +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.ticks=element_blank(), panel.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
monto_OC

#Gr?fico de barras
#Para evitar que se grafiquen los distritos sin informacion, borramos los distritos que no 
#presentan informai?n para el periodo en an?lisis.
db_lima_monto_OC_clean <- na.omit(db_lima_monto_OC)

# utilizamos la funci?n str_wrap de la librer?a stringr para limitar la longitud 
#de cada nombre de distrito a un n?mero espec?fico de caracteres (30), lo que asegurar? que los 
#nombres de los distritos no se corten en varias l?neas


library(stringr)

db_lima_monto_OC_clean$DISTRITO <- str_wrap(db_lima_monto_OC_clean$DISTRITO, width = 30)


monto_OC_barras = ggplot(db_lima_monto_OC_clean, aes(x=Monto_Total/1000, y= reorder(DISTRITO,Monto_Total),fill= Monto_Total)) + # creamos el gr?fico de barras
  
  geom_bar(stat = "identity",width = 0.6) + # agrupaci?n por distrito
  ggtitle("Imagen 8. Montos totales de Compra IV Trim") +
  labs(x = "Monto Total en miles de soles", y = "Distrito") +
  scale_fill_gradient("MONTO TOTAL", labels = function(x) format(x, scientific = FALSE, big.mark = "."),
                      low = "yellow", high = "red", na.value = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "white"))

monto_OC_barras

#Como se observa en las imagenes 7 y 8, el distrito con mayor monto total por ordenes de compra
#es Villa Maria del Triundo que ha sobrepasado el umbral de 1 500 000 nuvos soles, sequido de cerca
#por Lima Metropolitana, Luriganco (Chosica), San Juan de Luriganco y San Borja.

sum(db_lima_monto_OS_clean$Monto_Total)

sum(db_lima_monto_OC_clean$Monto_Total)

####  3. Concentraci??n proveedores ----

# Se define bse de concentraci??n de proveedores
bd_conc_s <-  bd_final |> 
  filter(TIPOORDEN == "Orden de Servicio" )  |> 
  group_by(ENTIDAD.x) |>
  mutate(momto_trim = sum(MONTO_TOTAL_ORDEN_ORIGINAL),  n_ordenes = n(), n_distintas =  n_distinct(RUC_CONTRATISTA), ratio_conc = n_distintas/n_ordenes, inv_ratio = (1 - ratio_conc)) |> 
  filter(!duplicated(UBIGEO)) |> 
  select(1,16,20,21:29) |> 
  arrange(ENTIDAD.x)

bd_conc_c <-  bd_final |> 
  filter(TIPOORDEN == "Orden de Compra" )  |> 
  group_by(ENTIDAD.x) |>
  mutate(momto_trim = sum(MONTO_TOTAL_ORDEN_ORIGINAL),  n_ordenes = n(), n_distintas =  n_distinct(RUC_CONTRATISTA), ratio_conc = n_distintas/n_ordenes, inv_ratio = (1 - ratio_conc)) |> 
  filter(!duplicated(UBIGEO)) |> 
  select(1,16,20,21:29) |> 
  arrange(ENTIDAD.x)

#PLOT OS
mapa_conc_s <- left_join(mapa_lim, bd_conc_s, by="UBIGEO") |> 
  arrange(desc(ratio_conc)) |> 
  mutate(cat_conc=case_when(ratio_conc>0.2 & ratio_conc<=0.4~"1 a 2 proveedores por cada 5 contratos",
                            ratio_conc>0.4 & ratio_conc<=0.6~"2 a 3 proveedores por cada 5 contratos",
                            ratio_conc>0.6 & ratio_conc<=0.8~"3 a 4 proveedores por cada 5 contratos",
                            ratio_conc>0.8~ "5 proveedores por cada 5 contratos",
                            TRUE ~ "Sin informaci??n") )
#PLOT OC

mapa_conc_c <- left_join(mapa_lim, bd_conc_c, by="UBIGEO") |> 
  arrange(desc(ratio_conc)) |> 
  mutate(cat_conc=case_when(ratio_conc>0.2 & ratio_conc<=0.4~"De 1 a 2 por cada 5 contratos",
                            ratio_conc>0.4 & ratio_conc<=0.6~"De 2 a 3 por cada 5 contratos",
                            ratio_conc>0.6 & ratio_conc<=0.8~"De 3 a 4 por cada 5 contratos",
                            ratio_conc>0.8~ "De 5 por cada 5 contratos",
                            TRUE ~ "Sin informaci??n"  ) )


#factor(mapa_conc_s$cat_conc, levels = c("1 a 2 proveedores por cada 5 contratos","2 a 3 
#proveedores por cada 5 contratos", "3 a 4 proveedores por cada 5 contratos"," 5 proveedores por cada 5 contratos" ))

colores_s <- c("#034e7b", "#3690c0","#74a9cf", "#d0d1e6", "white")
colores_c <- c("#cc4c02", "#fe9929","#fed98e", "#ffffd4", "white")

##### 3.1.Concentracion de proveedores por Ordenes de Servicios----
mapa_conc_s |> 
  ggplot() +
  aes(geometry=geometry) +
  scale_fill_manual(values=colores_s)+
  geom_sf(aes(fill=cat_conc)) +
  labs(title = "Imagen 5. Concentraci??n de proveedores por ??rdenes de servicio")+
  guides(fill=guide_legend(title="N?? de proveedores distintos adjudicados"))

##### 3.2.Concentracion de proveedores por Ordenes de Compras----

mapa_conc_c |>
  ggplot() +
  aes(geometry=geometry) +
  scale_fill_manual(values=colores_c)+
  #scale_fill_brewer(palette = "RdGy", na.value = "white")+
  geom_sf(aes(fill=cat_conc))+
  labs(title = "Imagen 6. Concentraci??n de proveedores por ??rdenes de compra")+
  guides(fill=guide_legend(title="N?? de proveedores distintos adjudicados"))
