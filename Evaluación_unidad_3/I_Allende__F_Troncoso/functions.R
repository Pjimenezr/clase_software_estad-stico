############################################################
# functions.R – Evaluación Unidad 3
############################################################

# ------------------------------
# Cargar paquetes automáticamente
# ------------------------------
load_packages <- function(pkgs) {
  for (p in pkgs) {
    if (!require(p, character.only = TRUE)) {
      install.packages(p)
      library(p, character.only = TRUE)
    }
  }
}

# ------------------------------
# Ejercicio 1: extracción de nombre
# ------------------------------
extract_name <- function(url) {
  gsub(".+/(.+)\\?.+", "\\1", url)
}

# ------------------------------
# Ejercicio 1: descarga con URL
# ------------------------------
download_esi_data <- function(url, file_name, directory = "data") {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  dest <- file.path(directory, file_name)
  
  # Si el archivo ya existe, no lo descargamos de nuevo
  if (file.exists(dest)) {
    message("Ya existe, no se descarga de nuevo: ", file_name)
    return(invisible(dest))
  }
  
  message("Descargando: ", file_name)
  
  tryCatch(
    {
      download.file(url, destfile = dest, mode = "wb", quiet = FALSE)
    },
    error = function(e) {
      # Si falló, borramos el archivo incompleto
      if (file.exists(dest)) file.remove(dest)
      stop("Error al descargar ", file_name, ": ", conditionMessage(e))
    }
  )
  
  invisible(dest)
}

# ------------------------------
# Ejercicio 2: lectura robusta
# ------------------------------
read_esi_data <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE)
  
  if (grepl(";", first_line)) delim <- ";"
  else if (grepl("\t", first_line)) delim <- "\t"
  else delim <- ","
  
  readr::read_delim(path, delim = delim, show_col_types = FALSE)
}

# ------------------------------
# Ejercicio 2: leer todas las tablas
# ------------------------------
read_all_esi <- function(directory = "data") {
  files <- list.files(directory, full.names = TRUE)
  tablas <- purrr::map(files, read_esi_data)
  
  names(tablas) <- gsub("\\.csv.*$", "", basename(files))
  tablas
}

# ------------------------------
# Ejercicio 3: personas y hogares
# ------------------------------
tabla_personas_hogares <- function(lista_tablas) {
  purrr::imap_dfr(lista_tablas, ~ {
    dplyr::summarise(
      .x,
      version = .y,
      n_personas = dplyr::n_distinct(idrph),
      n_hogares  = dplyr::n_distinct(id_identificacion)
    )
  })
}

# ------------------------------
# Ejercicio 3: estadísticos ingresos
# ------------------------------
tabla_ingresos_trabajo <- function(lista_tablas) {
  purrr::imap_dfr(lista_tablas, ~ {
    base <- dplyr::filter(.x, ocup_ref == 1)
    dplyr::summarise(
      base,
      version = .y,
      min   = min(ing_t_p, na.rm = TRUE),
      max   = max(ing_t_p, na.rm = TRUE),
      media = mean(ing_t_p, na.rm = TRUE),
      p10   = quantile(ing_t_p, 0.10, na.rm = TRUE),
      p90   = quantile(ing_t_p, 0.90, na.rm = TRUE)
    )
  })
}

# ------------------------------
# Ejercicio 4: eficiencia
# ------------------------------
# dplyr + lista
calc_stats_dplyr_list <- function(lista) {
  purrr::imap_dfr(lista, ~ {
    d <- dplyr::filter(.x, ocup_ref == 1)
    dplyr::summarise(
      d,
      version = .y,
      mean = mean(ing_t_p, na.rm = TRUE),
      sd   = sd(ing_t_p, na.rm = TRUE),
      cv   = sd / mean
    )
  })
}

# dplyr + apilado
calc_stats_dplyr_apilado <- function(lista) {
  apilado <- purrr::imap_dfr(lista, ~ dplyr::mutate(.x, version = .y))
  apilado |>
    dplyr::filter(ocup_ref == 1) |>
    dplyr::group_by(version) |>
    dplyr::summarise(
      mean = mean(ing_t_p, na.rm = TRUE),
      sd   = sd(ing_t_p, na.rm = TRUE),
      cv   = sd / mean
    )
}

# data.table + lista
calc_stats_dt_list <- function(lista) {
  lista_dt <- lapply(lista, data.table::as.data.table)
  res <- list()
  
  for (n in names(lista_dt)) {
    dt <- lista_dt[[n]][ocup_ref == 1]
    m  <- dt[, mean(ing_t_p, na.rm = TRUE)]
    s  <- dt[, sd(ing_t_p, na.rm = TRUE)]
    res[[n]] <- data.table::data.table(
      version = n,
      mean = m,
      sd   = s,
      cv   = s / m
    )
  }
  data.table::rbindlist(res)
}

# data.table + apilado
calc_stats_dt_apilado <- function(lista) {
  lista_dt <- lapply(lista, data.table::as.data.table)
  apilado <- data.table::rbindlist(
    mapply(function(dt, nm){ dt[, version := nm]; dt }, 
           lista_dt, names(lista_dt), SIMPLIFY = FALSE),
    fill = TRUE
  )
  apilado[ocup_ref == 1,
          .(
            mean = mean(ing_t_p, na.rm = TRUE),
            sd   = sd(ing_t_p, na.rm = TRUE),
            cv   = sd(ing_t_p, na.rm = TRUE) / mean(ing_t_p, na.rm = TRUE)
          ),
          by = version]
}

# Benchmark
run_benchmark <- function(lista) {
  microbenchmark::microbenchmark(
    dplyr_lista   = calc_stats_dplyr_list(lista),
    dplyr_apilado = calc_stats_dplyr_apilado(lista),
    dt_lista      = calc_stats_dt_list(lista),
    dt_apilado    = calc_stats_dt_apilado(lista),
    times = 5
  )
}
