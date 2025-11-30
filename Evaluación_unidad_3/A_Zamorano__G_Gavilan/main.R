library(dplyr)
library(purrr)
library(readr)
library(data.table)
library(microbenchmark)
library(stringr)
library(ggplot2)

source("functions.R")

urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)
###cabe aclarar que la "version" que aparecen en nuestras tablas resumen corresponden a la posición del los urls que pertenecen al vector url
### por ejemplo, version 1 es la ESI del año 2018, version 2 es la ESI del 2019 y así sucesivamente.

# 3.1 ---------------------------------------------------------------------

file_names <- map_chr(urls, extract_name)
file_names
# tabla de personas y hogares ---------------------------------------------
tabla_personas_hogares <- map_df(lista_bases, personas_hogar, .id ="version")
print(tabla_personas_hogares)


# tabla estadísticos de ingresos del trabajo principal --------------------

tabla_ingresos_principal<-map_df(lista_bases, estadisticos_ingresos, .id ="version" )
print(tabla_ingresos_principal)
# item 6 ------------------------------------------------------------------


datos_stack_df <- bind_rows(lista_esi, .id = "archivo")

lista_esi_dt <- map(lista_esi, as.data.table)

datos_stack_dt <- as.data.table(datos_stack_df)


f_list_dplyr <- function() {
  map(lista_esi, ~ .x %>% 
        summarise(media = mean(ing_t_p, na.rm=TRUE), 
                  sd = sd(ing_t_p, na.rm=TRUE),
                  cv = sd(ing_t_p, na.rm=TRUE)/mean(ing_t_p, na.rm=TRUE)))
}

f_stack_dplyr <- function() {
  datos_stack_df %>%
    group_by(archivo) %>%
    summarise(media = mean(ing_t_p, na.rm=TRUE), 
              sd = sd(ing_t_p, na.rm=TRUE),
              cv = sd(ing_t_p, na.rm=TRUE)/mean(ing_t_p, na.rm=TRUE))
}

f_list_dt <- function() {
  lapply(lista_esi_dt, function(dt) {
    dt[, .(media = mean(ing_t_p, na.rm=TRUE),
           sd = sd(ing_t_p, na.rm=TRUE),
           cv = sd(ing_t_p, na.rm=TRUE)/mean(ing_t_p, na.rm=TRUE))]
  })
}

f_stack_dt <- function() {
  datos_stack_dt[, .(media = mean(ing_t_p, na.rm=TRUE),
                     sd = sd(ing_t_p, na.rm=TRUE),
                     cv = sd(ing_t_p, na.rm=TRUE)/mean(ing_t_p, na.rm=TRUE)), 
                 by = archivo]
}

message("Ejecutando Benchmark (puede tardar )...")
res_bench <- microbenchmark(
  "Lista_dplyr" = f_list_dplyr(),
  "Stack_dplyr" = f_stack_dplyr(),
  "Lista_DT"    = f_list_dt(),
  "Stack_DT"    = f_stack_dt(),
  times = 5 
)

print(res_bench)
write_csv(as.data.frame(summary(res_bench)), "outputs/benchmark_resultados.csv")

if(require("ggplot2")) {
  gg <- autoplot(res_bench) + labs(title = "Comparación de Eficiencia: dplyr vs data.table")
  ggsave("outputs/benchmark_plot.png", gg)
}


# Respuestas --------------------------------------------------------------


#¿Cuál estrategia es más eficiente y por qué?
#La estrategia más eficiente fue Lista + data.table (Lista_DT), con una media de 46.73 ms y un mínimo de 35.08 ms
#Esto ocurre porque data.table está  optimizado para manipular datos en memoria, reduciendo los tiempos de procesamiento


#¿Existen diferencias relevantes entre dplyr y data.table?
#Sí, existen grandes diferencias,Lista_dt obtuvo una media de 46.7 ms, siendo el doble de rapida que lista_dplyr, con 96.6ms


#¿Usar map o apilar tablas genera diferencias notorias?
#Si, genera diferencias,en dplyr, generar las tablas apiladas fue mucho mas eficiente con 67.7 ms de media, en comparacion con la lista (96.6 ms)
#En el caso de data_table, trabajar con la lista fue mas eficiente, con una media de 49.73 ms, en comparacion con la tabla apilada que obtuvo 66.6 segundos



