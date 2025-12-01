#MAIN

# cargar funciones
source("functions.R")

#3.

urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?download=true"
)

file_names <- purrr::map_chr(urls, extract_name)

purrr::walk2(urls, file_names, ~ download_esi_data(.x, .y, "data"))

paths <- list.files("data", full.names = TRUE)
lista_tablas <- purrr::map(paths, read_esi_data)
names(lista_tablas) <- sub("\\.csv$", "", file_names)

#4.

dir.create("outputs", showWarnings = FALSE)

tabla1 <- build_table1(lista_tablas, names(lista_tablas))
write_csv(tabla1, "outputs/tabla1_personas_hogares.csv")

tabla2 <- build_table2(lista_tablas, names(lista_tablas))
write_csv(tabla2, "outputs/tabla2_ingresos.csv")

#5.

bench <- benchmark_strategies(lista_tablas)
sink("outputs/benchmark.txt")
print(bench)
sink()

#6.

cat(item6_text(bench))
