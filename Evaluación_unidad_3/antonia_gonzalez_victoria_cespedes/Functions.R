library(tidyverse)
library(data.table)

# --- Funciones ---

extract_name <- function(url) {
  nombre_archivos <- str_extract(url, "esi.*\\.csv")
  return(nombre_archivos)
}

download_esi_data <- function(url, nombres_archivos, directory) {
  
  ruta_destino <- file.path(directory, nombres_archivos)
  
  download.file(url, destfile = ruta_destino, mode = "wb")
}

read_esi_data <- function(path) {

  primera_fila <- readLines(path, n = 1)
  
  if (grepl(";", primera_fila)) {
    separador <- ";"
  } else {
    separador <- ","
  }
  
  datos <- read_delim(path, delim = separador, show_col_types = FALSE)
  
  return(datos)
}

