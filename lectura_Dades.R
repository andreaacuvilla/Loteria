library(dplyr); library(stringr); library(tibble); library(stringi); 
library(purrr); library(tidyverse);library(writexl)


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



anys <- 2011:2024
arxiu <- paste0("dades/b", anys, ".txt")

loteria <- map_dfr(arxiu, llegir)
write_xlsx(loteria, "loteria-11-25.xlsx")

message("El fitxer Excel s'ha creat correctament!")

