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




