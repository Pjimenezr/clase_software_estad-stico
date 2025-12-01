
# 3 -----------------------------------------------------------------------

# 3.1 ---------------------------------------------

#Función de extracción
extract_name <- function(url) {
  
  file_with_params <- stringr::str_extract(url, "[^/]+$")
  
  file_name <- stringr::str_replace(file_with_params, pattern = "\\?.*", replacement = "")
  
  return(file_name)
}

#file_names <- purrr::map_chr(urls, extract_name)
#file_names


# 3.2 ----------------------------------------------

download_esi_data <- function(url, file_name, directory = "data") {
  
  target_dir <- file.path(directory)
  
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  destination_path <- file.path(target_dir, file_name)
  
  download.file(url = url, 
                destfile = destination_path, 
                mode = "wb", 
                quiet = TRUE,
                method = "auto")
  
  message(paste("Descarga completa:", file_name, "guardado en:", destination_path))
}

# 3.3 ----------------------------------------------

#options(timeout = 300)

#purrr::walk2(
#  .x = urls, 
#  .y = file_names, 
#  .f = download_esi_data,
#  directory = "data" 
#)


# 4 -----------------------------------------------------------------------

# 4.1 ---------------------------------------------------------------------

read_esi_data <- function(path) {
# Usamos fread de data.table porque detecta separadores de forma automática
data <- data.table::fread(path, showProgress = FALSE)
  
# Convertir a tibble para que nos funcione con dplyr %>% 
data <- as_tibble(data)
  
# Estandarizar nombres como buena práctica
names(data) <- tolower(names(data))

if("id_identificacion" %in% names(data)){
  data$id_identificacion <- as.numeric(data$id_identificacion)
}
return(data)
}


# 4.2 ---------------------------------------------------------------------

file_paths <- list.files("data", full.names = TRUE)

message(paste("Archivos encontrados:", paste(basename(file_paths), collapse = ", ")))

esi_data_list <- purrr::map(file_paths, read_esi_data)

file_names_clean <- basename(file_paths)
names(esi_data_list) <- file_names_clean



# 5.1 Función para Tabla 1 (Personas y Hogares) -------------------------
calc_counts <- function(data, version) {
  data %>%
    summarise(
      version = version,
      n_personas = n_distinct(idrph),
      n_hogares = n_distinct(id_identificacion)
    )
}



# 5.2 Función para Tabla 2 (Estadísticos Ingresos) ----------------------
calc_income_stats <- function(data, version) {
  data %>%
    filter(ocup_ref == 1) %>%
    summarise(
      version = version,
      min_ing = min(ing_t_p, na.rm = TRUE),
      max_ing = max(ing_t_p, na.rm = TRUE),
      media_ing = mean(ing_t_p, na.rm = TRUE),
      p10 = quantile(ing_t_p, 0.10, na.rm = TRUE),
      p90 = quantile(ing_t_p, 0.90, na.rm = TRUE)
    )
}

