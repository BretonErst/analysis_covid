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


table(da000$RESULTADO_LAB)


## Integración de base datos de positivos
# Mapper de positivos
es_positivo <- as_mapper(~ ifelse(.x %in% c(1:3), 1, 0))


## Base de Datos Nacional para DEFUNCIONES
# Definición de benchmarks
mex_def <- c(50000, 100000, 150000, 200000, 250000, 300000)

# Base de defunciones
nac_defunciones <- tibble(da000) %>% 
  mutate(defuncion = es_positivo(CLASIFICACION_FINAL)) %>% 
  filter(defuncion == 1) %>% 
  select(fecha = FECHA_DEF,
         defuncion) %>% 
  filter(fecha != '9999-99-99') %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  arrange(fecha) %>% 
  mutate(defun_acum = cumsum(defuncion))
  

# Días desde primera muerte hasta primera marka
fecha_marks$fecha[1] - min(nac_defunciones$fecha)

# Marcas de fecha
fecha_marks <- nac_defunciones %>% 
  filter(defun_acum %in% mex_def) %>% 
  mutate(dias = fecha - lag(fecha)) %>% 
  select(fecha, defun_acum, dias)
  

# Plot de defunciones
nac_defunciones %>% 
  ggplot(aes(x = fecha, 
             y = defuncion)) +
    geom_jitter(alpha = 0.15,
                shape = 1) +
    labs(title = "Defunciones por COVID 19",
         subtitle = "México Nacional",
         x = "Fecha",
         y = NULL) +
    geom_vline(xintercept = as.Date(fecha_marks$fecha),
               color = "darkred",
               lwd = 1.3) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())































