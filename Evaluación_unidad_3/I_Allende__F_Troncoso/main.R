############################################################
# main.R – Evaluación Unidad 3
############################################################

rm(list = ls())

# ------------------------------
# Cargar funciones
# ------------------------------
source("functions.R")

# ------------------------------
# Cargar paquetes
# ------------------------------
pkgs <- c("dplyr", "readr", "purrr", "data.table", "stringr", "microbenchmark")
load_packages(pkgs)

# ------------------------------
# URLs oficiales del enunciado
# ------------------------------
urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)

# ------------------------------
# Ejercicio 1: obtener nombres
# ------------------------------
file_names <- purrr::map_chr(urls, extract_name)

# ------------------------------
# Ejercicio 1: descargar todo
# ------------------------------
purrr::walk2(urls, file_names, ~ download_esi_data(.x, .y, "data"))

# ------------------------------
# Ejercicio 2: leer todas las bases
# ------------------------------
lista_tablas <- read_all_esi("data")

# ------------------------------
# Ejercicio 3: tablas
# ------------------------------
tabla1 <- tabla_personas_hogares(lista_tablas)
tabla2 <- tabla_ingresos_trabajo(lista_tablas)

# Guardar
if (!dir.exists("outputs")) dir.create("outputs")

readr::write_csv(tabla1, "outputs/tabla_personas_hogares.csv")
readr::write_csv(tabla2, "outputs/tabla_ingresos_trabajo.csv")

# ------------------------------
# Ejercicio 4: eficiencia
# ------------------------------
bench <- run_benchmark(lista_tablas)
summary_bench <- summary(bench)

saveRDS(bench, "outputs/resultados_benchmark.rds")
readr::write_csv(as.data.frame(summary_bench), "outputs/benchmark_resumen.csv")

print(tabla1)
print(tabla2)
print(summary_bench)
