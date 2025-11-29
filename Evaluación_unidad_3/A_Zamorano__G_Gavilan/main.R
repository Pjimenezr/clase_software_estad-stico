library(dplyr)
library(purrr)
library(readr)
library(data.table)
library(microbenchmark)
library(stringr)

source("functions.R")
###cabe aclarar que la "version" que aparecen en nuestras tablas resumen corresponden a la posición del los urls que pertenecen al vector url
### por ejemplo, version 1 es la ESI del año 2018, version 2 es la ESI del 2019 y así sucesivamente.
# tabla de personas y hogares ---------------------------------------------
tabla_personas_hogares <- map_df(lista_bases, personas_hogar, .id ="version")
print(tabla_personas_hogares)


# tabla estadísticos de ingresos del trabajo principal --------------------

tabla_ingresos_principal<-map_df(lista_bases, estadisticos_ingresos, .id ="version" )
print(tabla_ingresos_principal)
