library(purrr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
library(tibble)
library(microbenchmark)

# Ejercicio 1 -------------------------------------------------------------

url <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)

extract_name

file_names <- purrr::map_chr(url, extract_name)
file_names

download_esi_data

purrr::map2(
  url,
  file_names,
  ~download_esi_data(.x, .y, "data")
)

files <- list.files("data", full.names = TRUE)
files

bases <- purrr::map(
  bases,
  ~ .x %>% dplyr::mutate(
    id_identificacion = as.character(id_identificacion),
    idrph = as.character(idrph),
    ing_t_p = as.numeric(ing_t_p)   # <--- IMPORTANTE
  )
)

bases <- purrr::map(files, read_esi_data)
bases


# Ejercicio 5 -------------------------------------------------------------

tabla1 <- purrr::map2_df(
  bases,      
  files,   
  ~ tibble(
    version  = .y,
    personas = dplyr::n_distinct(.x$idrph),
    hogares  = dplyr::n_distinct(.x$id_identificacion)
  )
)

tabla1

tabla2 <- purrr::map2_df(
  bases,
  files,
  ~ .x %>%
    dplyr::filter(ocup_ref == 1) %>%
    dplyr::summarise(
      version    = .y,
      minimo     = min(ing_t_p, na.rm = TRUE),
      maximo     = max(ing_t_p, na.rm = TRUE),
      media      = mean(ing_t_p, na.rm = TRUE),
      p10        = quantile(ing_t_p, probs = 0.10, na.rm = TRUE),
      p90        = quantile(ing_t_p, probs = 0.90, na.rm = TRUE)
    )
)

tabla2


# Ejercicio 6 -------------------------------------------------------------
bases <- purrr::map(files, read_esi_data)

bases <- purrr::map(
  bases,
  ~ .x %>% dplyr::mutate(
    id_identificacion = as.character(id_identificacion),
    idrph = as.character(idrph),
    ing_t_p = as.numeric(ing_t_p)
  )
)

bases_apiladas <- dplyr::bind_rows(bases)

bases_dt <- data.table::as.data.table(bases_apiladas)

estr1 <- function() {
  purrr::map(
    bases,
    ~ .x %>%
      dplyr::filter(ocup_ref == 1) %>%
      dplyr::summarise(
        media = mean(ing_t_p, na.rm = TRUE),
        sd    = sd(ing_t_p, na.rm = TRUE),
        cv    = sd(ing_t_p, na.rm = TRUE) / mean(ing_t_p, na.rm = TRUE)
      )
  )
}

estr2 <- function() {
  bases_apiladas %>%
    dplyr::filter(ocup_ref == 1) %>%
    dplyr::summarise(
      media = mean(ing_t_p, na.rm = TRUE),
      sd    = sd(ing_t_p, na.rm = TRUE),
      cv    = sd(ing_t_p, na.rm = TRUE) / mean(ing_t_p, na.rm = TRUE)
    )
}

estr3 <- function() {
  purrr::map(
    bases,
    function(tabla) {
      dt <- data.table::as.data.table(tabla)
      dt[ocup_ref == 1,
         .(
           media = mean(ing_t_p, na.rm = TRUE),
           sd    = sd(ing_t_p, na.rm = TRUE),
           cv    = sd(ing_t_p, na.rm = TRUE) / mean(ing_t_p, na.rm = TRUE)
         )]
    }
  )
}

estr4 <- function() {
  bases_dt[
    ocup_ref == 1,
    .(
      media = mean(ing_t_p, na.rm = TRUE),
      sd    = sd(ing_t_p, na.rm = TRUE),
      cv    = sd(ing_t_p, na.rm = TRUE) / mean(ing_t_p, na.rm = TRUE)
    )
  ]
}

resultado_benchmark <- microbenchmark::microbenchmark(
  lista_purrr       = estr1(),
  apilada_dplyr     = estr2(),
  lista_datatable = estr3(),
  apilada_datatable = estr4(),
  times = 5
)
print(resultado_benchmark)

# Interpretacion ----------------------------------------------------------

##1)La estrategia más rápida es la que usa data.table con todas las bases apiladas en una sola tabla.
##Esto sucede porque data.table trabaja directamente sobre un único conjunto de datos y no necesita repetir pasos para cada año.
##Además, el modo en que maneja las memoria le permite filtrar y calcular los indicadores en menos tiempo que las otras alternativas

##2)Sí, hay diferencias.
##En tareas de filtrado y cálculo de estadísticas, data.table suele ser más rápido que dplyr.
##La razón principal es que data.table trabaja modificando los datos “en el mismo lugar”, sin generar tantas copias intermedias.
##Dplyr es más cómodo para leer el código, pero por dentro realiza más pasos, lo que en bases grandes se nota en el tiempo de ejecución.
##Por eso, cuando el enfoque es rendimiento, data.table tiende a dar mejores tiempos.

##3)Sí, cambia bastante.
##Cuando trabajas con map(), estás repitiendo el proceso una vez por cada base: filtrar, calcular media, desviación estándar, etc. Se hace todo varias veces.
##En cambio, cuando apilas todas las bases en una sola, haces el filtrado y los cálculos solo una vez.
##Eso reduce el trabajo total y, en consecuencia, el tiempo de ejecución.
##La diferencia se nota más cuando se usa data.table, que funciona mejor cuando recibe una tabla grande completa. pinpun
