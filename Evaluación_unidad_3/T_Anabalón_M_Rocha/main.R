# =====================================================================
# main.R 
# =====================================================================

# Cargar librerías y funciones
source("functions.R")

# =====================================================================
# 1. URLs 
# =====================================================================

urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)

# =====================================================================
# 3.1 Nombres de archivos
# =====================================================================

file_names <- purrr::map_chr(urls, extract_name)
print(file_names)

# =====================================================================
# 3.2—3.3 Descarga de archivos
# =====================================================================
# ---------------------------------------------------------------------
# 2. download_esi_data
# ---------------------------------------------------------------------

download_esi_data <- function(url, file_name, directory = "data") {
  
  
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  dest_path <- file.path(directory, file_name)
  
 
  options(timeout = max(600, getOption("timeout")))  # 10 minutos
  

  metodo <- if (.Platform$OS.type == "windows") "wininet" else "curl"
  
  # ------------------------------------------------------------------
  # Reintentos automáticos (hasta 3 veces)
  # ------------------------------------------------------------------
  intentos_max <- 3
  intento <- 1
  exito <- FALSE
  
  while (intento <= intentos_max && !exito) {
    
    message("Intento ", intento, " descargando: ", file_name)
    
    tryCatch({
      
      download.file(
        url,
        destfile = dest_path,
        mode = "wb",
        method = metodo
      )
      
      exito <- TRUE
      message("✔ Archivo descargado correctamente: ", file_name)
      
    }, error = function(e) {
      
      message("⚠ Error en intento ", intento, ": ", e$message)
      
      if (intento == intentos_max) {
        stop("❌ Falló la descarga después de ", intentos_max, " intentos.")
      }
      
     
      Sys.sleep(3)
    })
    
    intento <- intento + 1
  }
}

purrr::pwalk(list(urls, file_names, "data"), download_esi_data)

# =====================================================================
# 4.2 Leer todas las bases
# =====================================================================

data_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)

esi_list <- purrr::map(data_files, read_esi_data)
names(esi_list) <- gsub("\\.csv", "", basename(data_files))

# =====================================================================
# 5.1 TABLA 1: Personas y hogares
# =====================================================================

tabla1 <- purrr::map2_dfr(
  esi_list,
  names(esi_list),
  calculate_p_h
)

print(tabla1)

# =====================================================================
# 5.2 TABLA 2: Estadísticos de ingreso
# =====================================================================

names(esi_list$`esi-2018-personas`)
names(esi_list[[1]])

tabla2 <- purrr::map2_dfr(
  esi_list,
  names(esi_list),
  calculate_income_stats
)

print(tabla2)

# =====================================================================
# 6. Comparación de eficiencia con microbenchmark
# =====================================================================

esi_combined_df <- dplyr::bind_rows(esi_list)
esi_combined_dt <- as.data.table(esi_combined_df)

efficiency_comparison <- microbenchmark(
  purrr_list = purrr::map2_dfr(esi_list, names(esi_list), calculate_income_stats),
  
  dplyr_combined = esi_combined_df %>%
    filter(ocup_ref == 1, ing_t_p > 0) %>%
    group_by(version) %>%
    summarise(
      media = mean(ing_t_p),
      sd = sd(ing_t_p),
      cv = sd(ing_t_p) / mean(ing_t_p)
    ),
  
  dt_list = purrr::map_dfr(esi_list, calc_dt_list),
  
  dt_combined = esi_combined_dt[
    ocup_ref == 1 & ing_t_p > 0,
    .(
      media = mean(ing_t_p),
      sd = sd(ing_t_p),
      cv = sd(ing_t_p) / mean(ing_t_p)
    ),
    by = version
  ],
  
  times = 5
)

print(efficiency_comparison)

