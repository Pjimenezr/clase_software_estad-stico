library(tidyverse)
library(data.table)
library(janitor)

extract_name <- function(url) {
  name <- sub("^.+/([^/\\?]+)\\?.*$", "\\1", url)
  return(name)
}

download_esi_data <- function(url, file_name, directory = "data") {
  
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  dest_path <- file.path(directory, file_name)
  
  download.file(url = url, destfile = dest_path, mode = "wb")
  
  message("Descargado: ", dest_path)
  return(dest_path)
}

read_esi_data <- function(path) {
  
  first_line <- readLines(path, n = 1, encoding = "UTF-8")
  
  if (stringr::str_detect(first_line, ";")) {
    delim <- ";"
  } else if (stringr::str_detect(first_line, "\t")) {
    delim <- "\t"
  } else {
    delim <- ","
  }
  
  df <- readr::read_delim(
    file = path,
    delim = delim,
    locale = readr::locale(encoding = "Latin1"),
    guess_max = 100000
  ) %>%
    janitor::clean_names()
  
  return(df)
}

get_cv <- function(x, na_rm = FALSE) {
  
  if (na_rm) {
    x <- x[!is.na(x)]
  }
  
  m <- mean(x)
  s <- sd(x)
  
  cv <- s / m
  return(cv)
}
