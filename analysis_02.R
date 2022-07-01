# Librerías
library(tidyverse)
library(data.table)


## Adquisición de datos
da00 <- fread("220116COVID19MEXICO.csv")

glimpse(da00)

# Cambio a tipo factor de variable ENTIDAD_RES y MUNICIPIO_RES
da00 <- da00 %>% 
  mutate(ENTIDAD_RES = as.factor(ENTIDAD_RES),
         MUNICIPIO_RES = as.factor(MUNICIPIO_RES))

levels(da00$ENTIDAD_RES) <- c("01", "02", "03", "04", "05", "06", "07", "08",
                              "09", "10", "11", "12", "13", "14", "15", "16",
                              "17", "18", "19", "20", "21", "22", "23", "24",
                              "25", "26", "27", "28", "29", "30", "31", "32")

# Suma de positivos
length(which(da00$CLASIFICACION_FINAL %in% c(1:3)))


# Adquisición de Nomenclatura de estados
estados <- read_csv("code_ENTIDAD.csv")

# Preparación de data frame para integración
estados <- estados %>% 
  mutate(ENTIDAD_RES = as.factor(CLAVE_ENTIDAD)) %>% 
  select(-CLAVE_ENTIDAD,
         -ABREVIATURA,
         ENTIDAD_RES,
         estado = ENTIDAD_FEDERATIVA)

levels(estados$ENTIDAD_RES)

# Integración con data frame de COVID19
da000 <- da00 %>% 
  left_join(estados, by = "ENTIDAD_RES")

# Verificación de suma de positivos
length(which(da000$CLASIFICACION_FINAL %in% c(1:3)))


## Integración de base datos de positivos
# Mapper de positivos
es_positivo <- as_mapper(~ ifelse(.x %in% c(1:3), 1, 0))

## Base de Datos municipios seleccionados de GUANAJUATO solo POSITIVOS
munic_gto_01 <- da000 %>% 
  filter(estado == "GUANAJUATO") %>% 
  mutate(caso_positivo = es_positivo(CLASIFICACION_FINAL)) %>% 
  filter(caso_positivo == 1) %>% 
  filter(MUNICIPIO_RES %in% c("3", "7", "15", "17", "20", "27", "37")) %>%
  mutate(municipio = recode(MUNICIPIO_RES,
                            "3" = "SAN MIGUEL DE ALLENDE",
                            "7" = "CELAYA",
                            "15" = "GUANAJUATO",
                            "17" = "IRAPUATO",
                            "20" = "LEÓN",
                            "27" = "SALAMANCA",
                            "37" = "SILAO DE LA VICTORIA")) %>% 
  select(fecha = FECHA_INGRESO,
         municipio,
         caso_positivo) %>% 
  group_by(municipio, fecha) %>% 
  arrange(fecha) %>% 
  summarize(positivos_dia = sum(caso_positivo))
  
# Adición de variable promedio móvil 7 días
munic_gto_02 <- munic_gto_01 %>% 
  mutate(prom_mov_7 = zoo::rollmean(x = positivos_dia, 
                                    k = 7, 
                                    fill = NA)) %>% 
  drop_na()
  
# Anidación por municipio
nest_munic_gto <- munic_gto_02 %>% 
  nest(-municipio)

# Generación de gráficas
grafica_por_municipio <- nest_munic_gto %>% 
  mutate(grafica = map2(data, municipio,
                        ~ ggplot(.x, 
                                 aes(x = fecha, 
                                     y = positivos_dia)) +
                          geom_line(linetype = "dashed", 
                                    color = "darkgrey") +
                          geom_line(aes(y = prom_mov_7),
                                    color = "blue") +
                          labs(title = "Casos Nuevos Diarios Confirmados de COVID19",
                               subtitle = paste("Municipio", {.y}),
                               x = "Fecha",
                               y = "Casos diarios (móvil 7 días)") +
                          scale_x_date(date_breaks = "3 month")))

# Impresión de las gráficas
grafica_por_municipio %>% 
  pluck("grafica") %>% 
  print()
































