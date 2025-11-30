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

