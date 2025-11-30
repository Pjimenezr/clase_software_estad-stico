###

if (!require("dplyr")) install.packages("dplyr")
if (!require("purrr")) install.packages("purrr")
if (!require("data.table")) install.packages("data.table")
if (!require("microbenchmark")) install.packages("microbenchmark")

library(dplyr)
library(purrr)
library(data.table)
library(microbenchmark)



#### Ejercicio 1 (apartado número 3 de las instrucciones)
### 1.1
extract_name <- function(url) {
  nombre <- sub(".*/([^/]+\\.csv).*", "\\1", url)
  return(nombre)
}

### 3.2

download_esi_data <- function(url, file_name, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
    message("Directorio creado: ", directory)
    }
ruta_completa <- file.path(directory, file_name)

tryCatch({
  download.file(url, destfile = ruta_completa, mode = "wb")
  message("✓ Descargado exitosamente: ", file_name)
}, error = function(e) {
  message("✗ Error al descargar ", file_name, ": ", e$message)
})
}



#### Ejercicio 2 (es el número 4 en la pauta de instrucciones) #######
### lo de la función de descarga masiva lo he puesto (punto 3,3 de la pauta) en el main profe

read_esi_data <- function(path) {
  primera_linea <- readLines(path, n = 1, warn = FALSE)
  if (grepl(";", primera_linea)) {
    separador <- ";"
  } else if (grepl(",", primera_linea)) {
    separador <- ","
  } else if (grepl("\t", primera_linea)) {
    separador <- "\t"
  } else {
    separador <- ","
  }
  
  datos <- read.csv(path, sep = separador, stringsAsFactors = FALSE, 
                    encoding = "UTF-8")
  
  message("✓ Archivo leído con separador: '", separador, "' - ", 
          nrow(datos), " filas, ", ncol(datos), " columnas")
  
  return(datos)
}






