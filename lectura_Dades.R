library(dplyr)
library(stringr)
library(tibble)
library(stringi)

nateja <- function(dataset) {
  dataset %>%
    bind_rows(.) %>%
    mutate(
      premio_num = as.numeric(premio),
      premio_factor = factor(
        format(premio_num, scientific = FALSE, trim = TRUE)
      ),
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


leer_loteria <- function(archivo) {
  
  # Extraer año desde el nombre del archivo
  any <- as.integer(str_extract(basename(archivo), "\\d{4}"))
  
  # Moneda según año
  moneda <- ifelse(any >= 2002, "EUR", "PESETA")
  
  # Leer líneas y limpiar
  lineas <- readLines(archivo, warn = FALSE)
  lineas <- stri_enc_toutf8(lineas)
  lineas <- str_trim(lineas)
  lineas <- lineas[lineas != ""]
  
  categoria_actual <- NA
  res <- list()
  
  i <- 1
  while (i <= length(lineas)) {
    l <- lineas[i]
    
    # 1️⃣ Categoría (puede ser multilinea)
    if (str_detect(l, "^[A-Za-zÁÉÍÓÚáéíóúñÑ ]+$")) {
      cat_tmp <- l
      j <- i + 1
      
      while(j <= length(lineas) && !str_detect(lineas[j], "^\\d{5}")) {
        cat_tmp <- str_c(cat_tmp, str_trim(lineas[j]), sep = " ")
        j <- j + 1
      }
      
      categoria_actual <- str_squish(cat_tmp)
      i <- j
      next
    }
    
    # 2️⃣ Premio gordo
    if (str_detect(l, "^\\d{5}$")) {
      numero <- l
      letra <- NA
      premio <- NA
      
      # Comprobamos si las siguientes líneas son premio gordo
      if (i + 2 <= length(lineas) &&
          str_detect(lineas[i + 1], "^\\d") &&
          str_detect(lineas[i + 2], "EUROS")) {
        
        premio <- as.numeric(str_remove_all(lineas[i + 1], "\\.|,"))
        i <- i + 3
      } else {
        i <- i + 1
      }
      
      res <- append(res, list(
        tibble(
          numero = numero,
          letra = letra,
          premio = premio,
          categoria = categoria_actual,
          moneda = moneda,
          any = any
        )
      ))
      
      next
    }
    
    # 3️⃣ Premio normal (con letra y valor)
    if (str_detect(l, "^\\d{5}.*\\d+$")) {
      numero <- str_extract(l, "^\\d{5}")
      letra <- str_extract(l, "\\b[abct]\\b")
      premio <- as.numeric(str_extract(l, "\\d+$"))
      
      i <- i + 1
      
      res <- append(res, list(
        tibble(
          numero = numero,
          letra = letra,
          premio = premio,
          categoria = categoria_actual,
          moneda = moneda,
          any = any
        )
      ))
      
      next
    }
    
    # 4️⃣ Si no coincide con nada, seguimos
    i <- i + 1
  }
  
  bind_rows(res)
}
#MAIN:
library(purrr); library(tidyverse)

#Años
#a <- 2015:2024
#archivos <- paste0("dades/b", a, ".txt")

#datos_loteria <- map_dfr(archivos, leer_loteria)
