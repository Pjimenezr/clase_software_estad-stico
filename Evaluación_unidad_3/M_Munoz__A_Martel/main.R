# main.R

if (!require("dplyr")) install.packages("dplyr") 
if (!require("purrr")) install.packages("purrr")
if (!require("data.table")) install.packages("data.table")
if (!require("microbenchmark")) install.packages("microbenchmark")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyverse")) install.packages("tidyverse")

library(dplyr)
library(tidyverse)
library(purrr)
library(data.table)
library(microbenchmark)
library(stringr)

source("functions.R")

urls <- c("https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)

options(timeout = 600)

if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
}

# --- 1. Ejercicio 1: Descarga Automatizada de Datos ---
file_names <- purrr::map_chr(urls, extract_name) 
purrr::walk2(urls, file_names, download_esi_data, directory = "data") 
list.files("data")
# --- 2. Ejercicio 2: Lectura de Archivos ---
file_paths <- list.files("data", full.names = TRUE)
esi_list <- purrr::map(file_paths, read_esi_data)
names(esi_list) <- purrr::map_chr(esi_list, ~ unique(.x$version))
# --- 3. Ejercicio 3: Construcción de Indicadores ---
tabla_personas_hogares <- purrr::map_dfr(esi_list, function(df) {
  df %>%
    dplyr::summarise(
      version = unique(version),
      num_personas = n_distinct(idrph),
      num_hogares = n_distinct(id_identificacion)
    )
})
write.csv(tabla_personas_hogares, "outputs/tabla_1_personas_hogares.csv", row.names = FALSE)

tabla_estadisticos_ingresos <- purrr::map_dfr(names(esi_list), function(name) {
  calculate_income_stats(esi_list[[name]], name)
})
write.csv(tabla_estadisticos_ingresos, "outputs/tabla_2_estadisticos_ingresos.csv", row.names = FALSE)


# --- 4. Ejercicio 4: Comparación de Eficiencia ---
df_apilado <- data.table::rbindlist(esi_list, fill = TRUE, use.names = TRUE) %>% 
  as.data.frame()

expr_list <- list(
  "E1_Lista_Purrr_Dplyr" = quote({ purrr::map_dfr(esi_list, calc_stats_dplyr) }),
  "E2_Apiladas_Dplyr" = quote({ df_apilado %>% calc_stats_dplyr() }),
  "E3_Lista_Purrr_Data.table" = quote({ purrr::map_dfr(esi_list, calc_stats_datatable) }),
  "E4_Apiladas_Data.table" = quote({ data.table::as.data.table(df_apilado) %>% calc_stats_datatable() })
)

bench_results <- microbenchmark::microbenchmark(
  list = expr_list,
  times = 5,
  unit = "ms" 
)

write.csv(as.data.frame(bench_results), "outputs/microbenchmark_results.csv", row.names = FALSE)


