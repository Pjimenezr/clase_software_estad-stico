###

if (!require("dplyr")) install.packages("dplyr")
if (!require("purrr")) install.packages("purrr")
if (!require("data.table")) install.packages("data.table")
if (!require("microbenchmark")) install.packages("microbenchmark")

library(dplyr)
library(purrr)
library(data.table)
library(microbenchmark)



#### Ejercicio 1

extract_name <- function(url) {
  nombre <- sub(".*/([^/]+\\.csv).*", "\\1", url)
  return(nombre)
}





