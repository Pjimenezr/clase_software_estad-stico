library(tidyverse)
library(data.table)
library(microbenchmark)

if (!require("dplyr")) install.packages("dplyr")

source("Functions.R")

urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)

# Ejercicio 1 -------------------------------------------------------------
nombres_archivos <- extract_name(urls)
print(nombres_archivos) 

descargas <- map2(urls, nombres_archivos, ~ download_esi_data(.x, .y, "data"))


# --- Ejercicio 2 -------------------------------------------------------------

rutas <- list.files("data", full.names = TRUE)

lista_datos <- map(rutas, read_esi_data)

names(lista_datos) <- nombres_archivos


# --- Ejercicio 3 -------------------------------------------------------------

esi_completa <- bind_rows(lista_datos, .id = "version")

tabla_personas_hogares <- map_dfr(lista_datos, function(tabla) {
  tibble(
    n_personas = n_distinct(tabla$idrph, na.rm = TRUE),
    n_hogares = n_distinct(tabla$id_identificacion, na.rm = TRUE)
  )
}, .id = "version")

print(head(tabla_personas_hogares))

tabla_ingresos <- map_dfr(lista_datos, function(tabla) {
  
  tabla_filtrada <- tabla %>% filter(ocup_ref == 1)
  
  tibble(
    minimo = min(tabla_filtrada$ing_t_p, na.rm = TRUE),
    maximo = max(tabla_filtrada$ing_t_p, na.rm = TRUE),
    media = mean(tabla_filtrada$ing_t_p, na.rm = TRUE),
    percentil_10 = quantile(tabla_filtrada$ing_t_p, 0.10, na.rm = TRUE),
    percentil_90 = quantile(tabla_filtrada$ing_t_p, 0.90, na.rm = TRUE)
  )
}, .id = "version")

print(head(tabla_ingresos))

write_csv(tabla_personas_hogares, "outputs/tabla_personas_hogares.csv")
write_csv(tabla_ingresos, "outputs/tabla_ingresos.csv")


# --- Ejercicio 4 -------------------------------------------------------------

lista_dt <- map(lista_datos, as.data.table)

dt_consolidada <- as.data.table(esi_completa)

benchmark <- microbenchmark(
  
  opcion_lista_purrr = {
    map(lista_datos, function(df) {
      df %>% 
        filter(ocup_ref == 1) %>% 
        summarise(media = mean(ing_t_p, na.rm = TRUE))
    })
  },
  
  opcion_dplyr_full = {
    esi_completa %>%
      filter(ocup_ref == 1) %>%
      group_by(version) %>%
      summarise(media = mean(ing_t_p, na.rm = TRUE))
  },
  
  opcion_lista_dt = {
    lapply(lista_dt, function(dt) {
      dt[ocup_ref == 1, .(media = mean(ing_t_p, na.rm = TRUE))]
    })
  },
 
  opcion_dt_full = {
    dt_consolidada[ocup_ref == 1, .(media = mean(ing_t_p, na.rm = TRUE)), by = version]
  },
  
  times = 5 
)

print(benchmark)

#¿Cuál estrategia es más eficiente y por qué?
#La estrategia 4, es la más eficiente de todas. Al tener todas las bases juntas en un solo objeto, el paquete puede optimizar mucho mejor los cálculos en memoria

#¿Existen diferencias relevantes entre dplyr y data.table?
#Sí, se notan diferencias, sobre todo cuando aumenta la cantidad de datos.Aunque dplyr es súper fácil de leer y escribir, data.table está diseñado específicamente para manipular grandes volúmenes de datos reduciendo los tiempos de procesamiento de forma significativa

#¿Usar map o apilar tablas genera diferencias notorias?
#Sí, genera diferencias bien notorias. Usar map obliga a R a iterar sobre cada tabla por separado, lo cual suele ser más lento porque repite el proceso muchas veces.En comparación, si apilamos las tablas, podemos aprovechar la vectorización, que es cuando R opera sobre toda la columna de una sola vez.


