### En este va el script principal, el que ejecuta el flujo como tal.

rm(list = ls())
source("functions.R")

cat("Librerías cargadas correctamente\n")
cat("Versión de R:", R.version.string, "\n\n")


### Parte 1, lo de la descarga de los datos
### esto es lo del ejercicio 1 (apartado 3)

cat("\n=============================================\n")
cat("   EJERCICIO 1: DESCARGA DE DATOS\n")
cat("=============================================\n\n")


urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)


cat("Extrayendo nombres de archivos...\n")
file_names <- map_chr(urls, extract_name)

cat("Archivos a descargar:\n")
print(file_names)
cat("\n")


cat("Iniciando descarga de archivos...\n")
cat("Esto puede tardar varios minutos dependiendo de tu conexión.\n\n")

walk2(urls, file_names, ~download_esi_data(.x, .y, "data"))

cat("\n✓ Descarga completada\n")

######## parte 2 ##########

cat("\n=============================================\n")
cat("   EJERCICIO 2: LECTURA DE DATOS\n")
cat("=============================================\n\n")


rutas_archivos <- file.path("data", file_names)


cat("Leyendo archivos...\n\n")
lista_datos <- map(rutas_archivos, read_esi_data)

names(lista_datos) <- file_names

cat("\n✓ Todos los archivos cargados exitosamente\n")
cat("Bases de datos disponibles:\n")
print(names(lista_datos))

#### la parte 3, la de los indicadores ####


cat("\n=============================================\n")
cat("   EJERCICIO 3: INDICADORES\n")
cat("=============================================\n\n")


######### Tabla 1 - Personas y hogares #####
cat("Generando Tabla 1: Conteo de personas y hogares...\n")
tabla_1 <- crear_tabla_personas_hogares(lista_datos, file_names)

cat("\n--- TABLA 1: PERSONAS Y HOGARES ---\n")
print(tabla_1)

####### Tabla 2 - Estadísticos de ingresos ##########
cat("\n\nGenerando Tabla 2: Estadísticos de ingresos...\n")
tabla_2 <- crear_tabla_ingresos(lista_datos, file_names)

cat("\n--- TABLA 2: ESTADÍSTICOS DE INGRESOS ---\n")
print(tabla_2)

### ahora guardar las tablas en outputs
cat("\n\nGuardando resultados en carpeta outputs/...\n")


if (!dir.exists("outputs")) {
  dir.create("outputs")
}


write.csv(tabla_1, "outputs/tabla_personas_hogares.csv", row.names = FALSE)
write.csv(tabla_2, "outputs/tabla_ingresos.csv", row.names = FALSE)

cat("✓ Tablas guardadas en:\n")
cat("  - outputs/tabla_personas_hogares.csv\n")
cat("  - outputs/tabla_ingresos.csv\n")


#### lo de la eficiencia

cat("\n=============================================\n")
cat("   EJERCICIO 4: COMPARACIÓN DE EFICIENCIA\n")
cat("=============================================\n\n")


cat("Preparando datos para el benchmark...\n")

lista_datos_con_version <- map2(lista_datos, file_names, function(datos, nombre) {
  datos$version <- nombre
  return(datos)
})

datos_apilados <- bind_rows(lista_datos_con_version)
datos_apilados_dt <- as.data.table(datos_apilados)

cat("Datos preparados:\n")
cat("  - Lista con", length(lista_datos), "datasets\n")
cat("  - Dataset apilado con", nrow(datos_apilados), "filas\n\n")


## se ejecuta el benchmark en cada caso

cat("Ejecutando benchmark (5 iteraciones)...\n")
cat("Esto puede tardar un momento...\n\n")

resultados_benchmark <- microbenchmark(
  
  ########### lista + purrr #####
  lista_purrr = {
    calcular_stats_purrr(lista_datos)
  },
  
  ####### apiladas y dplyr #######
  apilado_dplyr = {
    calcular_stats_dplyr(datos_apilados)
  },
  
  ###### lista y data.table #######
  lista_datatable = {
    calcular_stats_datatable_lista(lista_datos)
  },
  
  #### apiladas y data.table ######
  apilado_datatable = {
    calcular_stats_datatable_apilado(datos_apilados_dt)
  },
  
  times = 5
)




cat("\n--- RESULTADOS DEL BENCHMARK ---\n\n")
print(resultados_benchmark)

cat("\n\nGuardando resultados del benchmark...\n")
saveRDS(resultados_benchmark, "outputs/benchmark_resultados.rds")


resumen_benchmark <- summary(resultados_benchmark)
write.csv(resumen_benchmark, "outputs/benchmark_resumen.csv", row.names = FALSE)

cat("✓ Resultados del benchmark guardados en:\n")
cat("  - outputs/benchmark_resultados.rds\n")
cat("  - outputs/benchmark_resumen.csv\n")










