#Importem els paquets necessàris
library(dplyr)
library(stringr)
library(tibble)
library(stringi)
library(readr)
library(writexl)
library(purrr)

#Lectura de dades

llegir <- function(arxiu) {
  
  any_v <- as.integer(str_extract(basename(arxiu), "\\d{4}"))
  if (any_v <= 2001) {
    
    linies <- readLines(arxiu, encoding = "latin1", warn = FALSE)
    linies <- paste(linies, collapse = "\n")
    patro_num <- "(\\d{5})\\s+([\\.a-z]+)(\\d+)"
    seq <- str_match_all(linies, patro_num)[[1]]
    
    if (nrow(seq) == 0) return(tibble())
    
    premis <- tibble(
      numero = seq[, 2],
      lletres = seq[, 3],
      premi = as.numeric(seq[, 4])
    )
    
    categoria <- function(numero) {
      n <- as.integer(numero)
      if (n < 10) "unidad"
      else if (n < 100) "decena"
      else if (n < 1000) "centena"
      else if (n < 2000) "mil"
      else if (n < 3000) "dos mil"
      else if (n < 4000) "tres mil"
      else if (n < 5000) "cuatro mil"
      else if (n < 6000) "cinco mil"
      else if (n < 7000) "seis mil"
      else if (n < 8000) "siete mil"
      else if (n < 9000) "ocho mil"
      else if (n < 10000) "nueve mil"
      else if (n < 20000) "diez mil"
      else if (n < 30000) "veinte mil"
      else if (n < 40000) "treinta mil"
      else if (n < 50000) "cuarenta mil"
      else if (n < 60000) "cincuenta mil"
      else if (n < 70000) "sesenta mil"
      else if (n < 80000) "setenta mil"
      else if (n < 90000) "ochenta mil"
      else "noventa mil"
    }
    
    dades <- premis %>%
      mutate(
        lletra = "",
        categoria = sapply(numero, categoria),
        any = any_v
      )
    
    for (i in seq_len(nrow(dades))) {
      txt <- dades$lletres[i]
      
      if (str_detect(txt, "\\.t\\.\\.")) dades$lletra[i] <- "t"
      else if (str_detect(txt, "\\.c\\.\\.")) dades$lletra[i] <- "c"
      else if (str_detect(txt, "\\.a\\.\\.")) dades$lletra[i] <- "a"
      else {
        chars <- str_split(txt, "")[[1]]
        lletra <- chars[chars != "." & str_detect(chars, "[A-Za-z]")]
        if (length(lletra) > 0) dades$lletra[i] <- lletra[1]
      }
    }
    return(dades %>% select(numero, lletra, premi, categoria, any))
  }

  linies <- readLines(arxiu, warn = FALSE)
  linies <- stri_enc_toutf8(linies)
  linies <- str_trim(linies)
  linies <- linies[linies != ""]
  
  categoria_actual <- "General"
  res <- list()
  
  i <- 1
  while (i <= length(linies)) {
    
    l <- linies[i]
    
    # Categoria multilinea
    if (str_detect(l, "^[A-Za-zÁÉÍÓÚáéíóúñÑ ]+$") &&
        !str_detect(l, "^\\d{5}")) {
      
      cat_tmp <- l
      j <- i + 1
      while (j <= length(linies) &&
             !str_detect(linies[j], "^\\d{5}")) {
        cat_tmp <- str_c(cat_tmp, str_trim(linies[j]), sep = " ")
        j <- j + 1
      }
      categoria_actual <- str_squish(cat_tmp)
      i <- j
      next
    }
    
    # Numero / importe / EUROS
    if (str_detect(l, "^\\d{5}$")) {
      if (i + 2 <= length(linies) &&
          str_detect(linies[i + 1], "^[\\d\\.,]+$") &&
          str_detect(linies[i + 2], "EUROS")) {
        
        res[[length(res) + 1]] <- tibble(
          numero = l,
          lletra = NA_character_,
          premi = as.numeric(str_remove_all(linies[i + 1], "[\\.,]")),
          categoria = categoria_actual,
          any = any_v
        )
        i <- i + 3
        next
      }
    }
    
    # Todo en una línea
    if (str_detect(l, "^\\d{5}")) {
      res[[length(res) + 1]] <- tibble(
        numero = str_extract(l, "^\\d{5}"),
        lletra = str_extract(l, "\\b[abct]\\b"),
        premi = as.numeric(str_extract(l, "\\d+$")),
        categoria = categoria_actual,
        any = any_v
      )
      i <- i + 1
      next
    }
    
    i <- i + 1
  }
  
  bind_rows(res)
}

####################################
# LIMPIEZA
####################################
nateja <- function(dataset) {
  dataset %>%
    mutate(
      premi = as.numeric(premi),
      categoria = str_trim(categoria),
      categoria = str_to_lower(categoria),
      categoria = recode(
        categoria,
        "veinti n mil"   = "veintiún mil",
        "veintitr s mil" = "veintitrés mil",
        "veintid s mil"  = "veintidós mil",
        "diecis is mil"  = "dieciséis mil",
        "veintis is mil" = "veintiséis mil",
        "tr e s m i l"   = "tres mil",
        "veintidos mil"  = "veintidós mil",
        "veintitres mil" = "veintitrés mil",
        "veintiseis mil" = "veintiséis mil"
      ),
      categoria = factor(str_to_sentence(categoria))
    )
}

####################################
# MAIN
####################################
anys <- 2000:2025
arxius <- paste0("dades/b", anys, ".txt")

dades <- map_dfr(arxius, llegir)
dades <- nateja(dades)

write_xlsx(dades, "loteria-2000-2025.xlsx")
