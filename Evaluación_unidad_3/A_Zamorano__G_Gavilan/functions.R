library(stringr)
library(readr)
library(data.table)
library(purrr)
library(tidyverse)

# urls --------------------------------------------------------------------

urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)
# extraer nombres ---------------------------------------------------------


extract_name <- function(url){
    str_extract(url, "[^/]+(?=\\?)")
  }
file_names <- map_chr(urls, extract_name)
file_names

# descarga ----------------------------------------------------------------

download_esi_data <- function(url, file_name, directory = "data") {
  if(!dir.exists(directory)){
    dir.create(directory)
  }
  destino_path <- file.path(directory, file_name)
  download.file(url, destfile = destino_path)
}

descargas <- map2(urls, file_names, ~ download_esi_data(.x, .y, directory = "data"))

# lectura de archivos -----------------------------------------------------


read_esi_data <- function(path) {
  lineas <- readLines(path, n = 3)
  texto <- paste(lineas, collapse = "")
  sep_detectado <- if (str_detect(texto, ";")) ";" else ","
  message(paste("Leyendo", basename(path), "| Separador detectado:", sep_detectado))
  df <- data.table::fread(path, sep= sep_detectado)
  return(df)
}

# cargar bases en una lista -----------------------------------------------
rutas_archivos <- list.files("data", pattern = "csv", full.names = TRUE)
lista_bases <- map(rutas_archivos, read_esi_data)
str(lista_bases, max.level = 1)

# personas y hogares ------------------------------------------------------
personasxhogar<-function(df){
  df %>% summarise(
    numero_personas = n_distinct(idrph),
    numero_hogares = n_distinct(id_identificacion)
  )
}

