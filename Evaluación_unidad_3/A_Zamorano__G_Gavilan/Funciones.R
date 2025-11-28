library(stringr)
library(readr)
library(data.table)

extract_name <- function(url) {
  nombre_sucio <- basename(url)
  nombre_limpio <- str_remove(nombre_sucio, "\\?.*$")
  nombre_final <- str_replace(nombre_limpio, "\\.cav$", ".csv")
  
  return(nombre_final)
}


download_esi_data <- function(url, file_name, directory = "data") {
  if (!dir.exists(directory)) {
    dir.create(directory)
  }
  file_path <- file.path(directory, file_name)
  message(paste("Descargando:", file_name, "..."))
  tryCatch({
    download.file(url, destfile = file_path, mode = "wb", quiet = TRUE)
    message(paste("OK:", file_name))
  }, error = function(e) {
    message(paste("ERROR descargando:", file_name))
  })
}


read_esi_data <- function(path) {
  lineas <- readLines(path, n = 5)
  n_punto_coma <- sum(str_count(lineas, ";"))
  n_coma <- sum(str_count(lineas, ","))
  sep_detectado <- if (n_punto_coma > n_coma) ";" else ","
  message(paste("Leyendo", basename(path), "| Separador detectado:", sep_detectado))
  
  df <- read_delim(path, delim = sep_detectado, show_col_types = FALSE, name_repair = "minimal")
  
  return(df)
}