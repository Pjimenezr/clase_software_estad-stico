paquetes <- c("dplyr", "purrr", "data.table", "microbenchmark")
nuevos <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(nuevos)) install.packages(nuevos)

library(dplyr)
library(purrr)
library(data.table)
library(microbenchmark)


extract_name <- function(url) {
  sub("\\?.*", "", basename(url))
}

download_esi_data <- function(url, file_name, directory) {
  if (!dir.exists(directory)) dir.create(directory)
  
  ruta_final <- file.path(directory, file_name)
  
  if (!file.exists(ruta_final)) {
    cat("Bajando:", file_name, "\n")
    try(download.file(url, destfile = ruta_final, mode = "wb"), silent = TRUE)
  } else {
    cat("Ya existe:", file_name, "\n")
  }
}

read_esi_data <- function(path) {
  linea <- readLines(path, n = 1)
  
  ptos_coma <- sum(unlist(strsplit(linea, "")) == ";")
  comas <- sum(unlist(strsplit(linea, "")) == ",")
  
  separador <- ifelse(ptos_coma > comas, ";", ",")
  
  cat("Leyendo:", basename(path), "con", separador, "\n")
  
  data.table::fread(path, sep = separador, data.table = FALSE)
}
