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
### 3.1
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
### lo de la función de descarga masiva lo he puesto (punto 3.3 de la pauta) en el main profe
## 4.1

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

### igual que antes, el apartado 4.2 (lo de cargar las bases en una lista) lo he puesto en el main

##### Ejercicio 3: hacer las tablas e indicadores
### 5.1

crear_tabla_personas_hogares <- function(lista_datos, nombres_archivos) {
  # Usa map2_df para iterar sobre datos y nombres simultáneamente
  resultados <- map2_df(lista_datos, nombres_archivos, function(datos, nombre) {
    # Crea un tibble (tabla moderna) con los resultados
    tibble(
      version = nombre,  # Nombre del archivo
      num_personas = n_distinct(datos$idrph),  # Personas únicas
      num_hogares = n_distinct(datos$id_identificacion)  # Hogares únicos
    )
  })
  
  return(resultados)
}

## 5.2
crear_tabla_ingresos <- function(lista_datos, nombres_archivos) {
  resultados <- map2_df(lista_datos, nombres_archivos, function(datos, nombre) {
    datos_ocupados <- datos %>% 
      filter(ocup_ref == 1)
    tibble(
      version = nombre,
      minimo = min(datos_ocupados$ing_t_p, na.rm = TRUE),
      maximo = max(datos_ocupados$ing_t_p, na.rm = TRUE),
      media = mean(datos_ocupados$ing_t_p, na.rm = TRUE),
      percentil_10 = quantile(datos_ocupados$ing_t_p, 0.10, na.rm = TRUE),
      percentil_90 = quantile(datos_ocupados$ing_t_p, 0.90, na.rm = TRUE)
    )
  })
  
  return(resultados)
}



