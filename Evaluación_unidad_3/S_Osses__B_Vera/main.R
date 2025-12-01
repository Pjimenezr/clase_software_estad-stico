library(stringr)
library(purrr)
library(dplyr)
library(readr)
library(microbenchmark)
library(data.table)
source("functions.R")

urls <- c("https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
          "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)


# 3.1 Extraemos los nombres -----------------------------------------------------------

file_names <- map_chr(urls, extract_name)

# 3.3 Descarga masiva -----------------------------------------------------

options(timeout = 600) 
purrr::walk2(
  .x = urls, 
  .y = file_names, 
  .f = download_esi_data,
  directory = "data" 
)

# 4.2 Lectura de los datos ------------------------------------------------

file_paths <- list.files("data", full.names = TRUE)
esi_data_list <- map(file_paths, read_esi_data)

names(esi_data_list) <- file_names #asignamos los nombres a cada elemento de la lista para dejarlos como identificadores


# 5. Indicadores ----------------------------------------------------------

# Tabla 1: Personas y Hogares
tabla_resumen_hogares <- purrr::map_df(
  .x = names(esi_data_list),
  .f = ~calc_counts(data = esi_data_list[[.x]], version = .x)  #llamamos la función creada en el archivo functions
)
 
 print(tabla_resumen_hogares) #en caso se quiera verificar la salida.

# Tabla 2: Ingresos (ocup_ref = 1)
tabla_estadisticos_ingresos <- purrr::map_df(
  .x = names(esi_data_list),
  .f = ~calc_income_stats(data = esi_data_list[[.x]], version = .x) #llamamos la función creada en el archivo functions
)

 print(tabla_estadisticos_ingresos) # en caso que se quiera verificar la salida.

 

# 6. Eficiencia -----------------------------------------------------------


 # 6. Eficiencia -----------------------------------------------------------
 
 # Preparamos los datos apilados
 esi_df_all <- dplyr::bind_rows(esi_data_list, .id = "version")
 esi_dt_all <- data.table::as.data.table(esi_df_all)
 
 message("Iniciando Benchmark completo (Media, SD y CV)...")
 
 benchmark_results <- microbenchmark::microbenchmark(
   
   # Estrategia 1: Lista + purrr
   "Lista + purrr" = {
     purrr::map(esi_data_list, function(df) {
       x <- df$ing_t_p[which(df$ocup_ref == 1)]
       # Calculamos las 3 cosas que pide la guía
       list(mean = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE), cv = sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE))
     })
   },
   
   # Estrategia 2: Apilado + dplyr
   "Apilado + dplyr" = {
     esi_df_all %>% 
       filter(ocup_ref == 1) %>% 
       group_by(version) %>% 
       summarise(
         media = mean(ing_t_p, na.rm = TRUE),
         sd = sd(ing_t_p, na.rm = TRUE),
         cv = sd(ing_t_p, na.rm = TRUE) / mean(ing_t_p, na.rm = TRUE)
       )
   },
   
   # Estrategia 3: Lista + data.table
   "Lista + data.table" = {
     lapply(esi_data_list, function(dt) {
       if (!data.table::is.data.table(dt)) data.table::setDT(dt)
       dt[ocup_ref == 1, .(
         media = mean(ing_t_p, na.rm=TRUE),
         sd = sd(ing_t_p, na.rm=TRUE),
         cv = sd(ing_t_p, na.rm=TRUE)/mean(ing_t_p, na.rm=TRUE)
       )]
     })
   },
   
   # Estrategia 4: Apilado + data.table
   "Apilado + data.table" = {
     esi_dt_all[ocup_ref == 1, .(
       media = mean(ing_t_p, na.rm=TRUE),
       sd = sd(ing_t_p, na.rm=TRUE),
       cv = sd(ing_t_p, na.rm=TRUE)/mean(ing_t_p, na.rm=TRUE)
     ), by = version]
   },
   
   times = 5
 )
 
 print(benchmark_results)


#1. Cuál estrategia es más eficiente y por qué?  Según los resultados obtenidos en el microbenchmark, la estrategia más eficiente en promedio fue "Lista + purrr" (Media: 6.85 ms), seguida muy de cerca por "Apilado + data.table" si observamos la mediana (7.32 ms).
#La estrategia más eficiente es Tablas Apiladas + data.table (7.3280 ms). 
#Es la más rápida porque utiliza la librería data.table, que es altamente optimizada y programada en C, 
#lo que le permite procesar el conjunto de datos unificado (apilado) en una sola operación muy veloz, minimizando la sobrecarga.
 
##2. ¿Existen diferencias relevantes entre dplyr y data.table?
##Sí, existen diferencias muy relevantes. La versión apilada de data.table (7.3280 ms) es aproximadamente 23 veces más rápida que la versión apilada de dplyr (173.1330 ms).
##sta gran diferencia demuestra que data.table es una herramienta superior para tareas intensivas de manipulación y resumen sobre grandes volúmenes de datos.
 
###3. ¿Usar map o apilar tablas genera diferencias notorias?
###Sí, genera diferencias notorias. El impacto depende de la librería: con librerías eficientes como data.table, la estrategia de apilar las tablas (7.3280 ms) es el doble de rápida que usar map sobre la lista (17.0538 ms).
###Sin embargo, si se utiliza una librería más lenta como dplyr, usar map sobre la lista de tablas pequeñas es en realidad la opción más rápida, ya que evita la sobrecarga de procesar un único data frame gigante.


# 7. Generación de Archivos en carpeta outputs ----------------------------

# 1. Crear la carpeta si no existe
if (!dir.exists("outputs")) {
  dir.create("outputs")
  message("Carpeta 'outputs' creada exitosamente.")
}

# 2. Guardar las tablas en formato CSV
readr::write_csv(tabla_resumen_hogares, "outputs/tabla_resumen_hogares.csv")
readr::write_csv(tabla_estadisticos_ingresos, "outputs/tabla_estadisticos_ingresos.csv")

# 3. Guardar el gráfico del Benchmark
png("outputs/grafico_eficiencia.png", width = 800, height = 600)
boxplot(benchmark_results, main = "Comparación de Tiempos: dplyr vs data.table")
dev.off() # Importante: esto cierra el archivo y lo guarda

message("¡Listo! Archivos guardados en la carpeta 'outputs/'")
