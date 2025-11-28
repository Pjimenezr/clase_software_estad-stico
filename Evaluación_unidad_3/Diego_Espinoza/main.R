source("functions.R")

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("data.table")) install.packages("data.table")
if (!require("janitor")) install.packages("janitor")
if (!require("microbenchmark")) install.packages("microbenchmark")

library(tidyverse)
library(data.table)
library(janitor)
library(microbenchmark)


urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)

versions <- c(2022, 2021, 2020, 2019, 2018)


file_names <- vapply(urls, extract_name, FUN.VALUE = character(1))
print(file_names)


paths <- mapply(
  FUN      = download_esi_data,
  url      = urls,
  file_name = file_names,
  MoreArgs = list(directory = "data")
)

print(paths)

esi_list <- lapply(paths, read_esi_data)

esi_list <- Map(
  f = function(df, ver) {
    df %>% mutate(version = ver)
  },
  df  = esi_list,
  ver = versions
)

esi_dt_list <- lapply(esi_list, as.data.table)


names(esi_list[[1]])[1:60]

names(esi_list[[1]])[grepl("ing", names(esi_list[[1]]))]

id_persona_var <- "id_identificacion"
id_hogar_var   <- "idrph"
ingreso_var    <- "ing_t_p"

if (!ingreso_var %in% names(esi_list[[1]])) {
  cat("Ojo: no existe la columna", ingreso_var, "en la base. Variables con 'ing' en el nombre:\n")
  print(names(esi_list[[1]])[grepl("ing", names(esi_list[[1]]))])
}


resumen_por_version_dplyr <- function(df) {
  df %>%
    summarise(
      version    = first(version),
      n_personas = n_distinct(.data[[id_persona_var]]),
      n_hogares  = n_distinct(.data[[id_hogar_var]]),
      ing_media  = mean(.data[[ingreso_var]], na.rm = TRUE),
      ing_sd     = sd(.data[[ingreso_var]], na.rm = TRUE),
      ing_cv     = get_cv(.data[[ingreso_var]], na_rm = TRUE)
    )
}

tabla1_dplyr <- bind_rows(
  lapply(esi_list, resumen_por_version_dplyr)
)

tabla1_dplyr


resumen_por_version_dt <- function(dt) {
  dt <- as.data.table(dt)
  
  dt[ , .(
    n_personas = uniqueN(get(id_persona_var)),
    n_hogares  = uniqueN(get(id_hogar_var)),
    ing_media  = mean(get(ingreso_var), na.rm = TRUE),
    ing_sd     = sd(get(ingreso_var), na.rm = TRUE),
    ing_cv     = get_cv(get(ingreso_var), na_rm = TRUE)
  ), by = version]
}

tabla1_dt <- rbindlist(
  lapply(esi_dt_list, resumen_por_version_dt)
)

tabla1_dt


bm_tablas <- microbenchmark(
  dplyr = {
    bind_rows(
      lapply(esi_list, resumen_por_version_dplyr)
    )
  },
  data_table = {
    rbindlist(
      lapply(esi_dt_list, resumen_por_version_dt)
    )
  },
  times = 20
)

bm_tablas


estrategia1_dplyr_lista <- function(esi_list) {
  bind_rows(
    lapply(esi_list, resumen_por_version_dplyr)
  )
}

estrategia2_dplyr_apilado <- function(esi_list) {
  df_all <- bind_rows(esi_list)
  
  df_all %>%
    group_by(version) %>%
    summarise(
      n_personas = n_distinct(.data[[id_persona_var]]),
      n_hogares  = n_distinct(.data[[id_hogar_var]]),
      ing_media  = mean(.data[[ingreso_var]], na.rm = TRUE),
      ing_sd     = sd(.data[[ingreso_var]], na.rm = TRUE),
      ing_cv     = get_cv(.data[[ingreso_var]], na_rm = TRUE),
      .groups    = "drop"
    )
}

estrategia3_dt_lista <- function(esi_dt_list) {
  rbindlist(
    lapply(esi_dt_list, resumen_por_version_dt)
  )
}

estrategia4_dt_apilado <- function(esi_dt_list) {
  
  dt_all <- rbindlist(esi_dt_list, use.names = TRUE, fill = TRUE)
  
  dt_all[ , .(
    n_personas = uniqueN(get(id_persona_var)),
    n_hogares  = uniqueN(get(id_hogar_var)),
    ing_media  = mean(get(ingreso_var), na.rm = TRUE),
    ing_sd     = sd(get(ingreso_var), na.rm = TRUE),
    ing_cv     = get_cv(get(ingreso_var), na_rm = TRUE)
  ), by = version]
}


tab1_e1 <- estrategia1_dplyr_lista(esi_list)
tab1_e2 <- estrategia2_dplyr_apilado(esi_list)
tab1_e3 <- estrategia3_dt_lista(esi_dt_list)
tab1_e4 <- estrategia4_dt_apilado(esi_dt_list)

tab1_e1
tab1_e2
tab1_e3
tab1_e4

bm_estrategias <- microbenchmark(
  dplyr_lista   = estrategia1_dplyr_lista(esi_list),
  dplyr_apilado = estrategia2_dplyr_apilado(esi_list),
  dt_lista      = estrategia3_dt_lista(esi_dt_list),
  dt_apilado    = estrategia4_dt_apilado(esi_dt_list),
  times = 20
)

bm_estrategias

if (!dir.exists("outputs")) dir.create("outputs")

tabla_final <- estrategia4_dt_apilado(esi_dt_list)
readr::write_csv(tabla_final, "outputs/tabla_resumen_ingresos.csv")

bm_df <- as.data.frame(bm_estrategias)
readr::write_csv(bm_df, "outputs/benchmark_estrategias.csv")
