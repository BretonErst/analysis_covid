# Librerías
library(tidyverse)
library(data.table)


## Adquisición de datos
da00 <- fread("220118COVID19MEXICO.csv")

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

# Base Nacional solo POSITIVOS DIARIOS
da01 <- da000 %>% 
  mutate(caso_positivo = es_positivo(CLASIFICACION_FINAL)) %>% 
  filter(caso_positivo == 1) %>% 
  select(fecha = FECHA_INGRESO,
         estado,
         caso_positivo) %>% 
  group_by(estado, fecha) %>% 
  arrange(fecha) %>% 
  summarize(positivos_dia = sum(caso_positivo)) 

sum(da01$positivos_dia)


# Adición de promedio móvil 7 días
da02 <- da01 %>% 
  mutate(prom_mov_7 = zoo::rollmean(positivos_dia,
                                    k = 7,
                                    fill = NA)) %>%
  drop_na()


# Gráfica de Casos Nuevos NACIONAL
da000 %>% 
  mutate(caso_positivo = es_positivo(CLASIFICACION_FINAL)) %>% 
  filter(caso_positivo == 1) %>% 
  select(fecha = FECHA_INGRESO, 
         caso_positivo) %>% 
  group_by(fecha, caso_positivo) %>% 
  arrange(fecha) %>% 
  summarize(positivos_dia = sum(caso_positivo)) %>% 
  ungroup() %>% 
  mutate(prom_mov_7 = zoo::rollmean(x = positivos_dia,
                                    k = 7, 
                                    fill = NA)) %>% 
  drop_na() %>% 
  ggplot(aes(x = fecha, 
             y = positivos_dia)) +
    geom_line(linetype = "dashed",
              color = "darkgrey") +
    geom_line(aes(y = prom_mov_7),
              color = "blue") +
    labs(title = "Casos Nuevos Diarios Confirmados de COVID-19",
         subtitle = "México Nacional",
         x = "Fecha",
         y = "Casos confirmados (móvil 7 días)") +
    scale_x_date(date_breaks = "3 month")


# Anidado por ESTADO
nest_da01 <- da02 %>% 
  nest(-estado)

# Preparación de gráfica por ESTADO
plots_por_estado <- nest_da01 %>% 
  mutate(graficas = map2(data, estado,
                      ~ ggplot(.x,
                               aes(x = fecha, 
                                   y = positivos_dia)) +
                        geom_line(linetype = "dashed",
                                  color = "darkgrey") +
                        geom_line(aes(y = prom_mov_7),
                                  color = "blue") +
                        labs(title = "Casos Nuevos Diarios Confirmados de COVID-19",
                             subtitle = paste("Estado ", {.y}),
                             x = "Fecha",
                             y = "Casos confirmados (móvil 7 días)") + 
                        scale_x_date(date_breaks = "3 month")))


# Imprimir gráficas
plots_por_estado %>% 
  pluck("graficas") %>% 
  print()









































