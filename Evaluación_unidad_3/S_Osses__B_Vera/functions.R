
# 3 -----------------------------------------------------------------------

# 3.1 ---------------------------------------------

#Función de extracción
extract_name <- function(url) {
  
  file_with_params <- stringr::str_extract(url, "[^/]+$")
  
  file_name <- stringr::str_replace(file_with_params, pattern = "\\?.*", replacement = "")
  
  return(file_name)
}

file_names <- purrr::map_chr(urls, extract_name)

file_names


# 3.2 ----------------------------------------------

download_esi_data <- function(url, file_name, directory = "datasb") {
  
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
#NO USAR AUN

options(timeout = 300)

purrr::walk2(
  .x = urls, 
  .y = file_names, 
  .f = download_esi_data,
  directory = "datasb" 
)




