#Importem els paquets necessàris

library(dplyr)
library(stringr)
library(tibble)
library(stringi)
library(readxl)
library(knitr)
library(kableExtra)
library(readr)


#Lectura de dades
llegir <- function(arxiu) {
  
  any_v <- as.integer(str_extract(basename(arxiu), "\\d{4}"))
  
  linies <- readLines(arxiu, warn = FALSE)
  linies <- stri_enc_toutf8(linies)
  linies <- str_trim(linies)
  linies <- linies[linies != ""]
  
  categoria_actual <- "General"
  res <- list()
  
  i <- 1
  while (i <= length(linies)) {
    l <- linies[i]
    if (str_detect(l, "^[A-Za-zÁÉÍÓÚáéíóúñÑ ]+$") && !str_detect(l, "^\\d{5}")) {
      cat_tmp <- l
      j <- i + 1
      while(j <= length(linies) && !str_detect(linies[j], "^\\d{5}")) {
        cat_tmp <- str_c(cat_tmp, str_trim(linies[j]), sep = " ")
        j <- j + 1
      }
      categoria_actual <- str_squish(cat_tmp)
      i <- j
      next
    }
    
    if (str_detect(l, "^\\d{5}$")) {
      numero <- l
      if (i + 2 <= length(linies) && 
          str_detect(linies[i+1], "^[\\d\\.,]+$") && 
          str_detect(linies[i+2], "EUROS")) {
        
        premi_valor <- as.numeric(str_remove_all(linies[i+1], "\\.|,"))
        res[[length(res) + 1]] <- tibble(
          numero = numero,
          lletra = NA_character_,
          premi = premi_valor,
          categoria = categoria_actual,
          any = any_v
        )
        i <- i + 3
        next
      }
    }
    
    if (str_detect(l, "^\\d{5}")) {
      numero <- str_extract(l, "^\\d{5}")
      lletra <- ifelse(str_detect(l, "\\bt\\b"), "t", NA_character_)
      premi_valor <- as.numeric(str_extract(l, "\\d+$"))
      
      res[[length(res) + 1]] <- tibble(
        numero = numero,
        lletra = lletra,
        premi = premi_valor,
        categoria = categoria_actual,
        any = any_v
      )
      i <- i + 1
      next
    }
    
    i <- i + 1
  }
  
  return(bind_rows(res))
}

#Processament de les dades
nateja <- function(dataset) {
  dataset %>%
    bind_rows(.) %>%
    mutate(
      premi = as.numeric(premi),
      categoria = str_trim(categoria),
      categoria = str_to_lower(categoria),
      categoria = recode(
        categoria, 
        # errores de codificación
        "veinti n mil"   = "veintiún mil",
        "veintitr s mil" = "veintitrés mil",
        "veintid s mil"  = "veintidós mil",
        "diecis is mil"  = "dieciséis mil",
        "veintis is mil" = "veintiséis mil",
        "tr e s m i l"   = "tres mil",
        
        # variantes sin tilde
        "veintidos mil"  = "veintidós mil",
        "veintitres mil" = "veintitrés mil",
        "veintiseis mil" = "veintiséis mil"
      ),
      categoria = factor(str_to_sentence(categoria))
    )
}


#MAIN:
library(purrr); library(tidyverse)

#Años
a <- 2015:2024
archivos <- paste0("sdf/", a, ".txt")
datos_loteria <- map_dfr(archivos, leer_loteria)
dades <- 