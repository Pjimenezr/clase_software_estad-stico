# functions.R

if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("purrr")) install.packages("purrr")
if (!require("data.table")) install.packages("data.table")

library(dplyr)
library(stringr)
library(purrr)
library(data.table)

extract_name <- function(url) {
  nombre_archivo <- stringr::str_extract(url, "esi[-_]\\d{4}[-_a-z]*\\.csv")
  nombre_archivo <- stringr::str_remove(nombre_archivo, "\\?.*")
  nombre_archivo <- stringr::str_replace_all(nombre_archivo, "-", "_")
  return(nombre_archivo)
}

download_esi_data <- function(url, file_name, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  ruta_destino <- file.path(directory, file_name)
  tryCatch({
    download.file(url, destfile = ruta_destino, mode = "wb", quiet = TRUE) 
  }, error = function(e) {
    warning(paste("Error al descargar", file_name, ":", e$message))
  })
}

read_esi_data <- function(path) {
  df_leido <- data.table::fread(path, data.table = FALSE)
  names(df_leido) <- tolower(names(df_leido))
  version_name <- basename(path) %>%
    stringr::str_extract("esi[-_]\\d{4}")
  df_leido$version <- version_name
  return(df_leido)
}


calculate_income_stats <- function(df, version_name) {
  df_filtrado <- df %>%
    filter(ocup_ref == 1) 
  stats <- df_filtrado %>%
    summarise(
      version = version_name,
      minimo = min(ing_t_p, na.rm = TRUE),                   
      maximo = max(ing_t_p, na.rm = TRUE),                   
      media = mean(ing_t_p, na.rm = TRUE),                   
      p10 = quantile(ing_t_p, 0.10, na.rm = TRUE),          
      p90 = quantile(ing_t_p, 0.90, na.rm = TRUE)           
    )
  return(stats)
}

calc_stats_dplyr <- function(df) {
  df %>%
    filter(ocup_ref == 1) %>%
    summarise(
      media = mean(ing_t_p, na.rm = TRUE),
      desv_estandar = sd(ing_t_p, na.rm = TRUE)
    ) %>%
    mutate(
      cv = desv_estandar / media 
    )
}

calc_stats_datatable <- function(df) {
  dt <- data.table::as.data.table(df)
  stats <- dt[ocup_ref == 1, .(
    media = mean(ing_t_p, na.rm = TRUE),
    desv_estandar = sd(ing_t_p, na.rm = TRUE)
  )]
  stats[, cv := desv_estandar / media]
  return(stats)
}

