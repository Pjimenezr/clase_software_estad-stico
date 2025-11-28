library(stringr)
library(readr)
library(data.table)
library(purrr)
library(tidyverse)

# extraer nombres ---------------------------------------------------------


extract_name <- function(url){
    str_extract(url, "[^/]+(?=\\?)")
  }


# descarga ----------------------------------------------------------------


download_esi_data <- function(url, file_name, directory = "data") {
  if(!dir.exists(directory)){
    dir.create(directory)
  }
  destino_path <- file.path(directory, file_name)
  download.file(url, destfile = destino_path)
}

descargas <- map2(urls, file_names, ~ download_esi_data(.x, .y, directory = "data"))

read_esi_data <- function(path) {
  lineas <- readLines(path, n = 5)
  n_punto_coma <- sum(str_count(lineas, ";"))
  n_coma <- sum(str_count(lineas, ","))
  sep_detectado <- if (n_punto_coma > n_coma) ";" else ","
  message(paste("Leyendo", basename(path), "| Separador detectado:", sep_detectado))
  
  df <- read_delim(path, delim = sep_detectado, show_col_types = FALSE, name_repair = "minimal")
  
  return(df)
}