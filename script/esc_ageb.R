## Análisis de población en edad de estudiar y ubicación de escuelas 

# Cargamos librerías a utilizar 
library(tidyverse)
library(sf)
library(sp)
library(tmap)

# Cargar datos 
# Censo de población y vivienda 
# Mapa a nivel ageb
# Mapa DENUE
# Mapa municipal 
# Mapa de ejes 
censo2020 <- read_csv("data/agebmnaCENSO_TAB2020.csv")
# agebmap <- st_read("data", "27a")
agebmap <- read_rds("data/agebmap.rds")
# denueTAB <- rgdal::readOGR("data", "denue_inegi_27_", encoding ="latin1")
denueTAB <- read_rds("data/denueTAB.rds")
denueESC <- read_rds("data/denueESC.rds")
# munmapa <- rgdal::readOGR("data", "27mun", encoding ="latin1")
# Cambiar a mapas a objetos SF para trabajarlos con tidyerse 
# denueTAB <- st_as_sf(denueTAB)
# munmapa <- st_as_sf(munmapa) %>%
#  filter(NOMGEO %in% c("Centro", "Nacajuca"))
munmapa <- read_rds("data/munmapa.rds")
ejesmapa <- st_read("data", "27e") 
# Seleccionar solo las vialidades de interés 
ejesmapa <- ejesmapa %>% 
  filter(CVE_MUN %in% c("004", "013")) %>% 
  filter(TIPOVIAL %in% c("Avenida", "Periférico", 
                         "Calzada", "Eje Vial", 
                         "Carretera", "Boulevard"))

# Limpieza y procesado de Censo2020 para obtener datos a nivel AGEB
# de los municipios de Centro (Villahermosa) y Nacajuca
# vector con variables de interés 
variables <- c("POBTOT", "POBFEM", "POBMAS", 
               "P_3A5", "P_3A5_F", "P_3A5_M",
               "P_6A11", "P_6A11_F", "P_6A11_M", 
               "P_12A14", "P_12A14_F", "P_12A14_M", 
               "P_15A17", "P_15A17_F", "P_15A17_M", 
               "P_18A24", "P_18A24_F", "P_18A24_M", 
               "P3A5_NOA", "P3A5_NOA_F", "P3A5_NOA_M", 
               "P6A11_NOA", "P6A11_NOAF", "P6A11_NOAM",
               "P12A14NOA", "P12A14NOAF","P12A14NOAM", 
               "P15A17A", "P15A17A_F", "P15A17A_M", 
               "P18A24A", "P18A24A_F", "P18A24A_M", 
               "GRAPROES", "GRAPROES_F", "GRAPROES_M",
               "CVEGEO"
)

ageb2020data <- censo2020 %>%
  filter(NOM_LOC == "Total AGEB urbana", NOM_MUN %in% c("Centro", 
                                                        "Nacajuca")) %>%
  mutate(CVEGEO = paste(ENTIDAD, MUN, LOC, AGEB, sep = "")) %>%
  select(any_of(variables))

# Reemplazar * por NA 
ageb2020data[ageb2020data == "*"] <- NA

# Cambiar datos chr a dbl con la función APPLY
# Definir vector con columnas a transformar
vec <- c(1:36)
ageb2020data[ ,vec] <- apply(ageb2020data[ , vec, drop=F], 2,
                            function(x) as.double(as.character(x)))
# Eliminar datos innecesarios 
rm(variables, vec, censo2020)

glimpse(ageb2020data)

## Limpieza y procesado de denueTAB para filtrar solo valores instituciones 
# educativas de Villahermosa y Nacajuca 
# Actividades de interes
scian <- c(611111, 611112, # Preescolar 1 privado 2 particular
           611121, 611122, # Primaria
           611131, 611132, # Secundaria general
           611141, 611142, # Secundaria técnica
           611151, 611152, # Media técnica
           611161, 611162, # Media superior
           611211, 611212, # Técnica superior
           611311, 611312) # Educación superior  

# Variables de interés
variables <- c(
  "nom_estab", "nombre_act", 
  "nomb_asent", "geometry", 
  "CVEGEO"
)

# Crear base de datos filtrando municipios de interés, actividades de interés 
denueESC <- denueTAB %>%
  filter(municipio %in% c("Centro", "Nacajuca"), codigo_act %in% scian) %>%
  # Crear variable CVEGEO para unir a la ageb2020data
  mutate(CVEGEO = paste(cve_ent, cve_mun, cve_loc, ageb, sep = "")) %>%
  select(any_of(variables)) 
rm(denueTAB, scian, variables)

# Crear nuevas categorías para el nombre de actividad 
denueESC$tipo_esc <- "a"
denueESC$tipo_esc <- with(denueESC, 
                          ifelse(denueESC$nombre_act == "Escuelas de educación preescolar del sector público",
                                 "PREE_PUB", tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación preescolar del sector privado",
                                 "PREE_PRI", tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación primaria del sector privado" ,
                                 "PRIM_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación primaria del sector público",
                                 "PRIM_PUB", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación secundaria general del sector público",
                                 "SEC_PUB", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación secundaria general del sector privado",
                                 "SEC_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación secundaria técnica del sector privado" ,
                                 "SECTEC_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación secundaria técnica del sector público" ,
                                 "SECTEC_PUB", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación media superior del sector público"  ,
                                 "EMS_PUB", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación media superior del sector privado"  ,
                                 "EMS_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación media técnica terminal del sector privado"  ,
                                 "EMSTEC_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación técnica superior del sector privado"  ,
                                 "TECSUP_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación técnica superior del sector público",
                                 "TECSUP_PUB", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación superior del sector privado" ,
                                 "ESCSUP_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación superior del sector público" ,
                                 "ESCSUP_PUB", denueESC$tipo_esc))

glimpse(denueESC)
head(denueESC)

# Crear tabla con el numero de escuelas por AGEB 
denueAGEB <- denueESC %>%
  # Se convierte a tibble para descartar que datos espaciales causen error 
  as.tibble() %>% 
  select(tipo_esc, CVEGEO) %>% 
  count(CVEGEO, tipo_esc) %>%
  pivot_wider(names_from = "tipo_esc", 
              values_from = "n", 
              values_fill = 0) %>%
  mutate(total = rowSums(.[2:16]),
         ) # obtener total de escuelas por AGEB

# Eliminar datos que ya no se utilizaran 
rm(variables, scian, denueTAB)

glimpse(denueAGEB)
head(denueAGEB)

# Agregar denueAGEB a AGEBdata2020
datos <- ageb2020data %>%
  full_join(denueAGEB, by = "CVEGEO")
rm(ageb2020data, denueAGEB)

glimpse(datos)

# Elegir solo datos para preescolares y primarias publicas y privadas 
pres_prim <- datos %>%
  select(CVEGEO, POBTOT, 
         P_3A5, #P_3A5_F, P_3A5_M, 
         P_6A11, #P_6A11_F, P_6A11_M, 
         #P3A5_NOA, #P3A5_NOA_F, P3A5_NOA_M, 
         #P6A11_NOA, #P6A11_NOAF, P6A11_NOAM, 
         PREE_PUB, PREE_PRI, 
         PRIM_PUB, PRIM_PRI) %>%
  # Se crean variables con valores totales 
  # Según las variables elegidas 
  mutate(TOTAL = rowSums(.[5:8], na.rm = TRUE), 
         TOTALPRIV = rowSums(.[c(6, 8)], na.rm = TRUE),
         TOTALPUB = rowSums(.[c(5, 7)], na.rm = TRUE), 
         TOTALPRIM = rowSums(. [c(7,8)], na.rm = TRUE), 
         TOTALPREES = rowSums(. [c(5,6)], na.rm = TRUE)
         )

rm(datos)
glimpse(pres_prim)

# Crear denue_PRES_PRIM solo con preescolares y primarias de dataframe denueESC
denue_PRES_PRIM <- denueESC %>%
  filter(tipo_esc %in% c("PREE_PUB", 
                         "PREE_PRI", 
                         "PRIM_PUB", 
                         "PRIM_PRI"))
# Remover elementos utilizados
rm(denueESC)

# Unir datos a mapa 
mapa_pree_prim <- agebmap %>%
  filter(CVE_MUN %in% c("004", "013")) %>%
  left_join(pres_prim, by = "CVEGEO") %>%
  select(-CVE_ENT, -CVE_LOC, -CVE_AGEB) 
rm(agebmap)

# Conocer las AGEB que se pierden de tabla datos al unir con capa espacial 
pres_prim$CVEGEO[pres_prim$CVEGEO %in% mapa_pree_prim$CVEGEO == FALSE]
# Crear vector con datos que coinciden para filtrar 
sobrantes <- pres_prim$CVEGEO[pres_prim$CVEGEO %in% mapa_pree_prim$CVEGEO == TRUE] 

# Eliminar de datos AGEB que no se integran a mapadatos 
pres_prim <- pres_prim %>%
  filter(CVEGEO %in% sobrantes)
rm(sobrantes)
# Comprobar eliminación de sobrantes
pres_prim$CVEGEO[pres_prim$CVEGEO %in% mapa_pree_prim$CVEGEO == FALSE] 

glimpse(pres_prim)
glimpse(mapa_pree_prim)



## Análisis Exploratorio de Datos 
# Resumen de principales variables 
summary(pres_prim)
summarytools::stview(summarytools::dfSummary(pres_prim[, 2:13])) 

# Cuales son las AGEB con más niños en edad de estudiar prim y pres 
tmap_mode("plot")
tmap_mode("view")
pres_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTAL_ESC = TOTAL) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTAL_ALUM)) %>%
  select(-P_3A5, -P_6A11) %>%
  head(5)

# Mapa de AGEB con mas población 
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTAL) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTAL_ALUM)) %>%
  head(5) %>%
  tm_shape() +
  tm_polygons(col = "TOTAL_ALUM", 
              alpha = 0.3,
              style = "jenks", n = 5) +
  tm_shape(denue_PRES_PRIM) + 
  tm_dots(col = "tipo_esc", 
          popup.vars = TRUE) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Mapa de AGEB con mas población para IMPRIMIR
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTAL) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTAL_ALUM)) %>%
  tm_shape() +
  tm_polygons(col = "TOTAL_ALUM", 
              alpha = 0.3,
              style = "jenks", n = 5) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager")

# Cuales son las AGEB con mas escuelas
pres_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTAL_ESC = TOTAL) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTAL_ESC)) %>%
  select(-P_3A5, -P_6A11) %>%
  head(5)

# Mapa de AGEB con mas escuelas 
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTAL_ESC = TOTAL) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTAL_ESC)) %>%
  select(-P_3A5, -P_6A11) %>%
  head(5) %>%
  tm_shape() +
  tm_polygons(col = "TOTAL_ESC",
              popup.vars = TRUE,
              alpha = 0.3,
              style = "jenks", n = 3) +
  tm_shape(denue_PRES_PRIM) + 
  tm_dots(col = "tipo_esc", 
          popup.vars = TRUE) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Mapa de AGEB con mas escuelas PARA IMPRIMIR
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTAL_ESC = TOTAL) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTAL_ESC)) %>%
  select(-P_3A5, -P_6A11) %>%
  tm_shape() +
  tm_polygons(col = "TOTAL_ESC",
              popup.vars = TRUE,
              alpha = 0.3,
              style = "jenks", n = 5) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 


# Cuales son las AGEB con más escuelas públicas
# Tabla
pres_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTALPUB) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTALPUB)) %>%
  select(-P_3A5, -P_6A11) %>%
  head(5)

# Mapa de AGEB con mas escuelas públicas
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTALPUB) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTALPUB)) %>%
  select(-P_3A5, -P_6A11) %>%
  head(10) %>%
  tm_shape() +
  tm_polygons(col = "TOTALPUB",
              popup.vars = TRUE,
              alpha = 0.3,
              style = "jenks", n = 3) +
  tm_shape(denue_PRES_PRIM) + 
  tm_dots(col = "tipo_esc", 
          popup.vars = TRUE) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Mapa de AGEB con mas escuelas públicas IMPRIMIR
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTALPUB) %>%
  tm_shape() +
  tm_polygons(col = "TOTALPUB",
              popup.vars = TRUE,
              palette = "YlOrRd",
              alpha = 0.6,
              style = "jenks", n = 5) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 


# Cuales son las AGEB con más escuelas primarias públicas
# Tabla
pres_prim %>% 
  select(CVEGEO, P_6A11, PRIM_PUB) %>%
  arrange(desc(PRIM_PUB)) %>%
  head(5)

# Mapa de AGEB con mas escuelas primarias públicas
mapa_pree_prim %>% 
  select(CVEGEO, P_6A11, PRIM_PUB) %>%
  arrange(desc(PRIM_PUB)) %>%
  head(10) %>%
  tm_shape() +
  tm_polygons(col = "PRIM_PUB",
              popup.vars = TRUE,
              alpha = 0.3,
              style = "jenks", n = 3) +
  tm_shape(denue_PRES_PRIM) + 
  tm_dots(col = "tipo_esc", 
          popup.vars = TRUE) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Mapa de AGEB con mas escuelas primarias públicas IMPRIMIR
mapa_pree_prim %>% 
  select(CVEGEO, P_6A11, PRIM_PUB) %>%
  tm_shape() +
  tm_polygons(col = "PRIM_PUB",
              popup.vars = TRUE,
              alpha = 0.6,
              palette = "YlOrRd",
              style = "jenks", n = 5) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Cuales son las AGEB con más escuelas preescolares públicas
# Tabla
pres_prim %>% 
  select(CVEGEO, P_3A5, PREE_PUB) %>%
  arrange(desc(PREE_PUB)) %>%
  head(5)

# Mapa de AGEB con mas escuelas preescolar públicas
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, PREE_PUB) %>%
  arrange(desc(PREE_PUB)) %>%
  head(5) %>%
  tm_shape() +
  tm_polygons(col = "PREE_PUB",
              popup.vars = TRUE,
              palette = "YlOrRd",
              alpha = 0.6,
              style = "jenks", n = 5) +
  tm_shape(denue_PRES_PRIM) + 
  tm_dots(col = "tipo_esc", 
          popup.vars = TRUE) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Mapa de AGEB con mas escuelas preescolar públicas IMPRIMIR
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, PREE_PUB) %>%
  tm_shape() +
  tm_polygons(col = "PREE_PUB",
              popup.vars = TRUE,
              palette = "YlOrRd",
              alpha = 0.6,
              style = "jenks", n = 3) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Cuales son las AGEB con más escuelas privadas
# Tabla
pres_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTALPRIV) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTALPRIV)) %>%
  select(-P_3A5, -P_6A11) %>%
  head(5)

# Mapa de AGEB con mas escuelas privadas
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTALPRIV) %>%
  rowwise() %>%
  mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
  arrange(desc(TOTALPRIV)) %>%
  select(-P_3A5, -P_6A11) %>%
  head(10) %>%
  tm_shape() +
  tm_polygons(col = "TOTALPRIV",
              popup.vars = TRUE,
              palette = "YlOrRd",
              alpha = 0.6,
              style = "jenks", n = 3) +
  tm_shape(denue_PRES_PRIM) + 
  tm_dots(col = "tipo_esc", 
          popup.vars = TRUE) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Mapa de AGEB con mas escuelas privadas IMPRIMIR
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, P_6A11, TOTALPRIV) %>%
  filter(TOTALPRIV %in% c(1, 2, 3, 4)) %>%
  tm_shape() +
  tm_polygons(col = "TOTALPRIV",
              popup.vars = TRUE,
              palette = "YlOrRd",
              alpha = 0.6,
              style = "jenks", n = 3) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Cuales son las AGEB con más escuelas primarias privadas
# Tabla
pres_prim %>% 
  select(CVEGEO, P_6A11, PRIM_PRI) %>%
  arrange(desc(PRIM_PRI)) %>%
  head(5)


# Mapa de AGEB con mas escuelas primarias PRIVADAS
mapa_pree_prim %>% 
  select(CVEGEO, P_6A11, PRIM_PRI) %>%
  arrange(desc(PRIM_PRI)) %>%
  head(10) %>%
  tm_shape() +
  tm_polygons(col = "PRIM_PRI",
              popup.vars = TRUE,
              alpha = 0.3,
              style = "jenks", n = 3) +
  tm_shape(denue_PRES_PRIM) + 
  tm_dots(col = "tipo_esc", 
          popup.vars = TRUE) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Mapa de AGEB con mas escuelas primarias privadas IMPRIMIR
mapa_pree_prim %>% 
  select(CVEGEO, P_6A11, PRIM_PRI) %>%
  filter(PRIM_PRI %in% c(1, 2)) %>%
  tm_shape() +
  tm_polygons(col = "PRIM_PRI",
              popup.vars = TRUE,
              alpha = 0.6,
              palette = "YlOrRd",
              style = "jenks", n = 2) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Cuales son las AGEB con más escuelas preescolares PRIVADOS
# Tabla
pres_prim %>% 
  select(CVEGEO, P_3A5, PREE_PRI) %>%
  arrange(desc(PREE_PRI)) %>%
  head(5)

# Mapa de AGEB con mas escuelas preescolar privados
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, PREE_PRI) %>%
  arrange(desc(PREE_PRI)) %>%
  head(5) %>%
  tm_shape() +
  tm_polygons(col = "PREE_PRI",
              popup.vars = TRUE,
              palette = "YlOrRd",
              alpha = 0.6,
              style = "jenks", n = 3) +
  tm_shape(denue_PRES_PRIM) + 
  tm_dots(col = "tipo_esc", 
          popup.vars = TRUE) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Mapa de AGEB con mas escuelas preescolar privadas IMPRIMIR
mapa_pree_prim %>% 
  select(CVEGEO, P_3A5, PREE_PRI) %>%
  filter(PREE_PRI %in% c(1, 2,3)) %>%
  tm_shape() +
  tm_polygons(col = "PREE_PRI",
              popup.vars = TRUE,
              palette = "YlOrRd",
              alpha = 0.6,
              style = "jenks", n = 3) +
  tm_shape(munmapa) + 
  tm_borders() +
  tm_basemap("CartoDB.Voyager") # Tipo de mapa interactivo 

# Correlaciones 
pres_prim[, 2:13] %>% 
  as.matrix() %>%
  Hmisc::rcorr()

# Gráfica de correlación 
pres_prim[, 2:13] %>% 
  as.matrix() %>%
  cor(use = "complete.obs") %>%
  ggcorrplot::ggcorrplot()


tmap_mode("plot")  
# Crear mapas bivariado con la función de Stefano De Sabbata
# Mapa de AGEB con mas escuelas 
bivariate_choropleth(
  # Data frame con los datos a utilizar, la cual se adapta a la función
  mapa_pree_prim %>% 
    # Seleccionar variables para sumatorias
    select(CVEGEO, P_3A5, P_6A11, TOTAL_ESC = TOTAL) %>%
    # Usar función para hacer sumas en filas
    rowwise() %>%
    mutate(TOTAL_ALUM = sum(P_3A5, P_6A11)) %>%
    # Reemplazar NA para que la función bivariate no arroje error
    replace_na(list(TOTAL_ALUM = 0, TOTAL_ESC = 0)) %>%
    # Convertir de objeto sf a objeto espacial
    as_Spatial(),
  # Se seleccionan las variables a utilizar
  c("TOTAL_ALUM", "TOTAL_ESC"))

# Mapa de ageb con niños de 3 a 5 años y preescolares PUBLICOS 
bivariate_choropleth(
  # data frame
  mapa_pree_prim %>% 
    # Reemplazar NA para evitar error
    replace_na(list(P_3A5 = 0, PREE_PUB = 0)) %>%
    # Convertir de objeto sf a sp 
    as_Spatial(),
  # Elegir variables 
  c("P_3A5", "PREE_PUB")
)

# Mapa de ageb con niños de 6 a 11 y primarias publicas
bivariate_choropleth(
  # dataframe 
  mapa_pree_prim %>%
    # Reemplazar NA para evitar error de la función 
    replace_na(list(P_6A11 = 0, PRIM_PUB = 0)) %>%
    # Convertir objeto sf a sp 
    as_Spatial(), 
  # Elegir variables para el mapa 
  c("P_6A11", "PRIM_PUB")
)
