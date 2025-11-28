# functions.R
library(stringr)
library(data.table)
library(readr)

extract_name <- function(url) {
  nombre <- str_extract(url, "[\\w-]+\\.csv") 
  return(nombre)
}

download_esi_data <- function(url, file_name, directory) {
  ruta_destino <- file.path(directory, file_name)
  
  # Solo descargamos si no existe (ahorra tiempo)
  if (!file.exists(ruta_destino)) {
    message("Descargando: ", file_name)
    download.file(url, destfile = ruta_destino, mode = "wb")
  }
}

# --- 3. Lectura Robusta (Usando data.table) ---
# El PDF pide detectar separador[cite: 1364].
# La función fread() de data.table hace esto AUTOMÁTICAMENTE.
# Es la forma más eficiente vista en clase_data.table.pdf.
read_esi_data <- function(path) {
  # fread detecta si es separador ";" o "," solo.
  datos <- data.table::fread(path) 
  return(datos)
}
