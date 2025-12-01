# main.R
source("functions.R")

urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?download=true"
)

file_names <- purrr::map_chr(urls, extract_name)

walk2(urls, file_names, ~ download_esi_data(.x, .y, "data"))

paths <- list.files("data", full.names = TRUE)
lista_tablas <- purrr::map(paths, read_esi_data)
names(lista_tablas) <- sub("\\.csv$", "", file_names)

dir.create("outputs", showWarnings = FALSE)

tabla1 <- build_table1(lista_tablas, names(lista_tablas))
write_csv(tabla1, "outputs/tabla1_personas_hogares.csv")

tabla2 <- build_table2(lista_tablas, names(lista_tablas))
write_csv(tabla2, "outputs/tabla2_ingresos.csv")

bench <- benchmark_strategies(lista_tablas)

sink("outputs/benchmark.txt")
print(bench)
sink()

texto_item6 <- "
La estrategia más eficiente es 'apilar tablas + data.table', ya que presenta el menor tiempo
promedio en las 5 iteraciones del benchmark. Data.table trabaja por referencia, no copia datos
y está optimizado en C, por lo que es más rápido que dplyr. Además, procesar una sola tabla
apilada es más eficiente que usar map(), que ejecuta la operación repetidamente para cada año.
"

writeLines(texto_item6, "outputs/item6_conclusion.txt")
cat(texto_item6)

