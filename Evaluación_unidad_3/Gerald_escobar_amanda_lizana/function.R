# Ejercicios 1 -------------------------------------------------------------
## Ejercicio 1.1
extract_name <- function(url){
  url %>% 
    str_replace("\\?.+$", "")  %>%
    basename()
}
extract_name

##Ejercicio 1.2

download_esi_data <- function(url, file_name, directory) {
  path <- file.path(directory, file_name)
  download.file(url, path, mode = "wb")
}
download_esi_data


# ejercicio4.1 ------------------------------------------------------------
read_esi_data <- function(path) {
  first_line <- readLines(path, n = 1)
  sep <- if (stringr::str_detect(first_line, ";")) ";" else ","
  data.table::fread(path, sep = sep)
}
#hola