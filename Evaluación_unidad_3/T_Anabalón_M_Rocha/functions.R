# =====================================================================
# functions.R — Funciones para Evaluación Unidad 3
# =====================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  stringr,
  dplyr,
  purrr,
  tibble,
  data.table,
  microbenchmark
)

# ---------------------------------------------------------------------
# 1. extract_name(url)
# ---------------------------------------------------------------------

extract_name <- function(url) {
  name <- stringr::str_extract(url, "(?<=/)[^/\\?]+(?=\\?)")
  return(name)
}

# ---------------------------------------------------------------------
# 2. download_esi_data
# ---------------------------------------------------------------------

download_esi_data <- function(url, file_name, directory = "data") {
  
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  dest_path <- file.path(directory, file_name)
  
  download.file(url, destfile = dest_path, mode = "wb")
  
  message("Descargado: ", file_name)
}

# ---------------------------------------------------------------------
# 3. read_esi_data — LECTURA ROBUSTA Y ESTANDARIZACIÓN COMPLETA
# ---------------------------------------------------------------------

read_esi_data <- function(path) {
  
  # Detectar separador automáticamente
  first_line <- readLines(path, n = 1)
  sep <- if (grepl(";", first_line)) ";" else ","
  
  df <- read.table(
    path,
    sep = sep,
    header = TRUE,
    fill = TRUE,
    quote = "",
    encoding = "Latin1"
  )
  
  df <- tibble::as_tibble(df)
  names(df) <- tolower(make.names(names(df), unique = TRUE))
  
  
  # ----------------------------------------------------------
  # Estandarización de ID PERSONA (idrph)
  # ----------------------------------------------------------
  
  id_persona_candidates <- c("idrph", "id_rph", "folio", "idpersona", "id_persona")
  
  found_idp <- intersect(id_persona_candidates, names(df))
  
  if (length(found_idp) > 0) {
    df$idrph <- df[[found_idp[1]]]
  } else {
    df$idrph <- NA
  }
  
  
  # ----------------------------------------------------------
  # Estandarización de ID HOGAR
  # ----------------------------------------------------------
  
  id_hogar_candidates <- c("id_identificacion", "id_hogar", "id_vivienda", "id_hog")
  
  found_hogar <- intersect(id_hogar_candidates, names(df))
  
  if (length(found_hogar) > 0) {
    df$id_identificacion <- df[[found_hogar[1]]]
  } else {
    df$id_identificacion <- NA
  }
  
  
  # ----------------------------------------------------------
  # Estandarización de OCUP_REF
  # ----------------------------------------------------------
  
  ocup_candidates <- c(
    "ocup_ref", "ocup.ref",
    "ocupacion_ref", "ocupacion",
    "ocupado_ref", "ocupado"
  )
  
  found_ocup <- intersect(ocup_candidates, names(df))
  
  if (length(found_ocup) > 0) {
    df$ocup_ref <- df[[found_ocup[1]]]
  } else {
    df$ocup_ref <- NA
  }
  
  
  # ----------------------------------------------------------
  # Estandarización de INGRESO: ing_t_p
  # ----------------------------------------------------------
  
  income_candidates <- c(
    "ing_t_p", "ing.tp", "ing_tp",
    "ing_tprin", "ing_t_prin", "ing_tprincipal",
    "ingreso_principal", "ingreso_trabajo", "ingtrabajo",
    "yprinc", "y_princ", "y_principal",
    "ylabpr", "ylab_prin", "ylabprincipal",
    "yt_prin", "ytp", "y_ingprinc",
    "ing_lab", "ing_lab_principal"
  )
  
  found_income <- intersect(income_candidates, names(df))
  
  if (length(found_income) > 0) {
    df$ing_t_p <- suppressWarnings(as.numeric(df[[found_income[1]]]))
  } else {
    df$ing_t_p <- NA
  }
  
  
  # ----------------------------------------------------------
  # Agregar versión del archivo
  # ----------------------------------------------------------
  
  df$version <- gsub("\\.csv", "", basename(path))
  
  return(df)
}

# ---------------------------------------------------------------------
# 4. Tabla 1 — Personas y Hogares
# ---------------------------------------------------------------------

calculate_p_h <- function(data, version_name) {
  data %>% summarise(
    version = version_name,
    n_personas = n_distinct(idrph),
    n_hogares = n_distinct(id_identificacion)
  )
}

# ---------------------------------------------------------------------
# 5. Tabla 2 — Estadísticos de ingreso
# ---------------------------------------------------------------------

calculate_income_stats <- function(data, version_name) {
  data %>%
    filter(
      ocup_ref == 1,
      !is.na(ing_t_p),
      ing_t_p > 0
    ) %>%
    summarise(
      version = version_name,
      minimo = min(ing_t_p),
      maximo = max(ing_t_p),
      media = mean(ing_t_p),
      p10 = quantile(ing_t_p, 0.10),
      p90 = quantile(ing_t_p, 0.90)
    )
}

# ---------------------------------------------------------------------
# 6. Función para estrategia data.table
# ---------------------------------------------------------------------

calc_dt_list <- function(data) {
  dt <- as.data.table(data)
  dt <- dt[ocup_ref == 1 & ing_t_p > 0]
  
  dt[, .(
    media = mean(ing_t_p),
    sd = sd(ing_t_p),
    cv = sd(ing_t_p) / mean(ing_t_p)
  )]
}

