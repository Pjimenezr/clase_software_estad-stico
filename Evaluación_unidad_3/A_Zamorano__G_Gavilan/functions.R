if (!require("dplyr")) install.packages("dplyr")
if (!require("purrr")) install.packages("purrr")
if (!require("readr")) install.packages("readr")
if (!require("data.table")) install.packages("data.table")
if (!require("microbenchmark")) install.packages("microbenchmark")
if (!require("stringr")) install.packages("stringr")
if (!require("ggplot2")) install.packages("ggplot2")
library(dplyr)
library(purrr)
library(readr)
library(data.table)
library(microbenchmark)
library(stringr)
library(ggplot2)

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
personas_hogar<-function(df){
  df %>% summarise(
    numero_personas = n_distinct(idrph),
    numero_hogares = n_distinct(id_identificacion)
  )
}
# estadísticos de ingreso del trabajo principal -----------------------------------------------------------------------

estadisticos_ingresos <- function(df) {
  df %>% filter(ocup_ref == 1) %>% 
    summarise(
      minimo = min(ing_t_p, na.rm = TRUE),
      maximo = max(ing_t_p, na.rm = TRUE),
      media  = mean(ing_t_p, na.rm = TRUE),
      p10    = quantile(ing_t_p, 0.10, na.rm = TRUE),
      p90    = quantile(ing_t_p, 0.90, na.rm = TRUE))
}

