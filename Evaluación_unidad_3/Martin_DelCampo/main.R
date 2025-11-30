
urls <- c(
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2022/esi_2022.csv?sfvrsn=9c939ccc_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi_2021.csv?sfvrsn=bead03fb_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020-personas.csv?sfvrsn=66c7160_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019-personas.csv?sfvrsn=7d8832d9_4&download=true",
  "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018-personas.csv?sfvrsn=8a269cc6_4&download=true"
)

#sacar nombre y descargar
nombres_archivos <- map_chr(urls, extract_name)
walk2(urls, nombres_archivos, ~ download_esi_data(.x, .y, "data"))

#cargar datos
rutas <- file.path("data", nombres_archivos)
lista_esi <- map(rutas, read_esi_data)
names(lista_esi) <- nombres_archivos

#carpeta outputs
if (!dir.exists("outputs")) dir.create("outputs")

#tabla 1
tabla1 <- map_df(names(lista_esi), function(n) {
  d <- lista_esi[[n]]
  names(d) <- tolower(names(d))
  
  tibble(
    version = n,
    n_personas = n_distinct(d$idrph),
    n_hogares = n_distinct(d$id_identificacion)
  )
})
print(tabla1)
write.csv(tabla1, "outputs/tabla1.csv")

#tabla 2
tabla2 <- map_df(names(lista_esi), function(n) {
  d <- lista_esi[[n]]
  names(d) <- tolower(names(d))
  
  filtro <- d %>% filter(ocup_ref == 1)
  
  tibble(
    version = n,
    min = min(filtro$ing_t_p, na.rm = T),
    max = max(filtro$ing_t_p, na.rm = T),
    media = mean(filtro$ing_t_p, na.rm = T),
    p10 = quantile(filtro$ing_t_p, 0.1, na.rm = T),
    p90 = quantile(filtro$ing_t_p, 0.9, na.rm = T)
  )
})
print(tabla2)
write.csv(tabla2, "outputs/tabla2.csv")

#comparacion tiempos(Benchmark)

lista_esi <- map(lista_esi, function(df) {
  names(df) <- tolower(names(df))
  if("id_identificacion" %in% names(df)) {
    df$id_identificacion <- as.numeric(df$id_identificacion)
  }
  return(df)
})

datos_juntos <- bind_rows(lista_esi, .id = "version")
names(datos_juntos) <- tolower(names(datos_juntos))

lista_dt <- map(lista_esi, as.data.table)
dt_juntos <- as.data.table(datos_juntos)

res_bench <- microbenchmark(
  "Lista_purrr" = {
    map(lista_esi, function(df) {
      names(df) <- tolower(names(df))
      x <- df[df$ocup_ref == 1, ]
      c(mean(x$ing_t_p, na.rm=T), sd(x$ing_t_p, na.rm=T))
    })
  },
  "Apilada_dplyr" = {
    datos_juntos %>%
      filter(ocup_ref == 1) %>%
      group_by(version) %>%
      summarise(m = mean(ing_t_p, na.rm=T), s = sd(ing_t_p, na.rm=T))
  },
  "Lista_datatable" = {
    lapply(lista_dt, function(dt) {
      x <- copy(dt)
      setnames(x, tolower(names(x)))
      x[ocup_ref == 1, .(m = mean(ing_t_p, na.rm=T), s = sd(ing_t_p, na.rm=T))]
    })
  },
  "Apilada_datatable" = {
    dt_juntos[ocup_ref == 1, .(m = mean(ing_t_p, na.rm=T), s = sd(ing_t_p, na.rm=T)), by = version]
  },
  times = 5
)

print(res_bench)
sink("outputs/benchmark.txt")
print(res_bench)
sink()


# Preguntas ---------------------------------------------------------------

#1. Viendo el resultado de res_bench, la que ganó fue la Apilada + data.table, ya que al tener todo junto en dt_juntos 
# el paquete data.table hace todo el trabajo de una sola vez, no copia datos de un lado a otro como las otras opciones.

#2. En el código, con dplyr el proceso se estructura paso a paso mediante el uso de filter, group_by y summarise, lo cual
# resulta más extenso y en mas lineas, mientras que con el data.table se ejecuta todo en una única línea usando corchetes 
# siendo mas compacto y ordenado al leer el codigo.

#3. Hay una diferencia clara porque cambia la forma de trabajar, al usar map sobre la lista, se obliga al programa 
# a procesar "uno por uno" cada año, lo cual es más lento. en cambio, al apilar todo en datos_juntos, se transforma 
# la tarea en una sola operación y no repitiendo los mismos cálculos, por lo que las estrategias apiladas resultan ser más rápidas.