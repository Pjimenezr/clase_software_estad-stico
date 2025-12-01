# functions.R
library(purrr)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(tibble)
library(data.table)
library(microbenchmark)

extract_name <- function(url){
  name <- basename(url)
  name <- sub("\\?.*$", "", name)
  return(name)
}

download_esi_data <- function(url, file_name, directory = "data"){
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  dest <- file.path(directory, file_name)
  tryCatch({
    download.file(url, dest, mode = "wb", quiet = TRUE)
  }, error = function(e){
    download.file(url, dest, method = "curl", mode = "wb", quiet = TRUE)
  })
  invisible(dest)
}

detect_delim <- function(path){
  lines <- readLines(path, n = 5, warn = FALSE)
  txt <- paste(lines, collapse = "\n")
  counts <- c(
    comma     = stringr::str_count(txt, ","),
    semicolon = stringr::str_count(txt, ";"),
    tab       = stringr::str_count(txt, "\t"),
    pipe      = stringr::str_count(txt, "\\|")
  )
  delim_map <- c(comma = ",", semicolon = ";", tab = "\t", pipe = "|")
  return(delim_map[names(which.max(counts))])
}

read_esi_data <- function(path){
  delim <- detect_delim(path)
  df <- readr::read_delim(path, delim = delim,
                          guess_max = 20000,
                          locale = locale(encoding = "Latin1"),
                          show_col_types = FALSE)
  names(df) <- janitor::make_clean_names(names(df))
  return(as.data.frame(df))
}

build_table1 <- function(tbl_list, version_names){
  purrr::imap_dfr(tbl_list, function(df, idx){
    nm <- names(df)
    col_persona <- nm[str_detect(nm, "idrph|id_persona|id_rph|persona")][1]
    col_hogar   <- nm[str_detect(nm, "id_identificacion|id_hogar|folio|hogar")][1]
    tibble(
      version = version_names[idx],
      n_personas = n_distinct(df[[col_persona]]),
      n_hogares  = n_distinct(df[[col_hogar]])
    )
  })
}

build_table2 <- function(tbl_list, version_names){
  purrr::imap_dfr(tbl_list, function(df, idx){
    nm <- names(df)
    col_ing  <- nm[str_detect(nm, "^ing_t_p$|ing_t|ingreso")][1]
    col_ocup <- nm[str_detect(nm, "ocup_ref|ocup")][1]
    sub <- df[df[[col_ocup]] == 1, , drop = FALSE]
    ing <- suppressWarnings(as.numeric(sub[[col_ing]]))
    ing <- ing[!is.na(ing)]
    tibble(
      version = version_names[idx],
      min  = min(ing),
      max  = max(ing),
      mean = mean(ing),
      p10  = quantile(ing, 0.10),
      p90  = quantile(ing, 0.90)
    )
  })
}

benchmark_strategies <- function(tbl_list){
  
  strategy1 <- function(){
    purrr::map(tbl_list, function(df){
      nm <- names(df)
      col_ing  <- nm[str_detect(nm, "^ing_t|ingreso")][1]
      col_ocup <- nm[str_detect(nm, "ocup")][1]
      ing <- df[df[[col_ocup]] == 1, col_ing] |> as.numeric()
      ing <- ing[!is.na(ing)]
      tibble(mean = mean(ing), sd = sd(ing), cv = sd(ing)/mean(ing))
    }) |> bind_rows()
  }
  
  strategy2 <- function(){
    combined <- bind_rows(tbl_list, .id = "version")
    nm <- names(combined)
    col_ing  <- nm[str_detect(nm, "^ing_t|ingreso")][1]
    col_ocup <- nm[str_detect(nm, "ocup")][1]
    combined %>%
      filter(.data[[col_ocup]] == 1) %>%
      group_by(version) %>%
      summarise(
        mean = mean(as.numeric(.data[[col_ing]]), na.rm = TRUE),
        sd   = sd(as.numeric(.data[[col_ing]]), na.rm = TRUE),
        cv   = sd/mean
      )
  }
  
  strategy3 <- function(){
    rbindlist(
      lapply(tbl_list, function(df){
        DT <- as.data.table(df)
        nm <- names(DT)
        col_ing  <- nm[str_detect(nm, "^ing_t|ingreso")][1]
        col_ocup <- nm[str_detect(nm, "ocup")][1]
        sub <- DT[get(col_ocup) == 1, .(ing = as.numeric(get(col_ing)))]
        sub <- sub[!is.na(ing)]
        data.table(
          mean = mean(sub$ing),
          sd   = sd(sub$ing),
          cv   = sd(sub$ing)/mean(sub$ing)
        )
      }),
      fill = TRUE
    )
  }
  
  strategy4 <- function(){
    DT <- rbindlist(tbl_list, idcol = "version", fill = TRUE)
    nm <- names(DT)
    col_ing  <- nm[str_detect(nm, "^ing_t|ingreso")][1]
    col_ocup <- nm[str_detect(nm, "ocup")][1]
    DT[, (col_ing) := as.numeric(get(col_ing))]
    DT[get(col_ocup) == 1,
       .(mean = mean(get(col_ing), na.rm = TRUE),
         sd   = sd(get(col_ing), na.rm = TRUE)),
       by = version
    ][, cv := sd/mean]
  }
  
  microbenchmark(
    lista_purrr      = strategy1(),
    apilar_dplyr     = strategy2(),
    lista_datatable  = strategy3(),
    apilar_datatable = strategy4(),
    times = 5
  )
}

