# Librerías
library(tidyverse)
library(readxl)
library(readr)
library(kableExtra)
library(lme4)
library(lmerTest)
library(randomForest)
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(stringi)
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

anys <- 2000:2025
arxius <- paste0("Treball_Rmd/dades/b", anys, ".txt")

#dades <- map_dfr(arxius, llegir)
#dades <- nateja(dades)

#write_xlsx(dades, "loteria-2000-2025.xlsx")

# Lectura de les dades (desde .xlsx)
dades <- read_xlsx("loteria-2000-2025.xlsx")

# Pessetes a Euros
dades <- dades %>%
  mutate(premi = ifelse(any <= 2001, premi/166.386, premi))
dades$premi <- round(dades$premi, 2)

# Taula 1: Primers premis de l'any 2025
exemple <- head(dades[dades$any == 2025, ], 5)
taula1 <- exemple %>%
  kable(format = "latex", booktabs = TRUE, caption = "Primers registres del sorteig 2025") %>% 
  kable_styling(font_size = 9, latex_options = "HOLD_position")
print(taula1)

# Taula 2: Resum global del conjunt de dades
options(scipen = 999)
resum_lletres <- dades %>%
  count(lletra) %>%
  mutate(lletra_txt = paste0(ifelse(is.na(lletra), "Bombo", lletra), ": ", n)) %>%
  pull(lletra_txt) %>%
  paste(collapse = " | ")

resum_global <- tibble(
  Variable = c("Anys analitzats", "Total de números premiats", "Premi mínim (€)", 
               "Premi màxim (€)", "Categories (Lletra)"),
  Valor = c(n_distinct(dades$any), nrow(dades), min(dades$premi, na.rm = TRUE), 
            max(dades$premi, na.rm = TRUE), resum_lletres)
)

taula2 <- resum_global %>%
  kable(format = "latex", booktabs = TRUE, caption = "Resum global del conjunt de dades (2000-2025)") %>%
  kable_styling(font_size = 9, latex_options = "HOLD_position")
print(taula2)

# LEctura dades històric de la Grossa
historial <- read_table(
  "historial_gordo.txt",
  col_types = cols(
    Any = col_integer(),
    Numero = col_character(),
    Terminació = col_integer()
  )
)

# Taula 3: Dades històric
taula3 <- head(historial, 2) %>%
  kable(format = "latex", booktabs = TRUE, caption = "Conjunt de dades de la Grossa (1812-2025)") %>%
  kable_styling(font_size = 9, latex_options = "HOLD_position")
print(taula3)

# Gráfic 1: Distribució última xifra (1812-2025)
df_plot <- historial %>%
  count(Terminació) %>%
  mutate(Terminació = factor(Terminació, levels = 0:9))

ggplot(df_plot, aes(x = Terminació, y = n, fill = n)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = n), vjust = -0.4, size = 2.5, fontface = "bold") +
  scale_fill_gradient(low = "skyblue", high = "steelblue") +
  labs(
    title = "Distribució de l'última xifra",
    subtitle = "Grossa 1812-2025",
    x = "Última xifra",
    y = "Freqüència"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(face = "bold", size = 8),
    axis.title = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8)
  )

# Preparació de dades 
df_boles <- dades %>% 
  filter(is.na(lletra)) %>% 
  mutate(terminacio = as.numeric(numero) %% 10)

df_counts <- df_boles %>% count(terminacio)

# Gràfic 2: Terminacions de Boles
ggplot(df_counts, aes(x = factor(terminacio), y = n, fill = n)) +
  geom_col(show.legend = FALSE, width = 0.7, color = "white") +
  geom_hline(yintercept = nrow(df_boles)/10, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_fill_viridis_c(option = "mako", begin = 0.4, end = 0.8) +
  labs(
    title = "Verificació d'Atzar: Terminacions de Boles (2000-2025)",
    subtitle = "La línia vermella indica la freqüència teòrica esperada (distribució uniforme)",
    x = "Últim dígit",
    y = "Vegades premiat"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Preparació dades per període
df_net <- dades %>%
  mutate(
    numero = as.numeric(as.character(numero)),
    premi = as.numeric(as.character(premi)),
    lletra = ifelse(is.na(lletra) | lletra == "", "Bombo (Directe)", lletra),
    periodo = case_when(
      any <= 2004 ~ "P1 (fins 2004)",
      any <= 2010 ~ "P2 (2005-2010)",
      TRUE        ~ "P3 (des de 2011)"
    ),
    rang_num = case_when(
      numero < 66000 ~ "Baix (0-65999)",
      numero < 85000 ~ "Mig (66000-84999)",
      TRUE           ~ "Alt (85000-99999)"
    )
  )

# Resum Evolució
resum_evolucio <- df_net %>%
  group_by(periodo, rang_num) %>%
  summarise(total = n(), .groups = 'drop') %>%
  group_by(periodo) %>%
  mutate(percentatge = (total / sum(total)) * 100)

ggplot(resum_evolucio, aes(x = periodo, y = percentatge, fill = rang_num)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_text(aes(label = paste0(round(percentatge, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Baix (0-65999)" = "#2c3e50", 
                               "Mig (66000-84999)" = "#3498db", 
                               "Alt (85000-99999)" = "#95a5a6")) +
  labs(title = "Adaptació del sistema: Distribució de premis per franja", 
       subtitle = "El sistema integra les noves boles sense biaixos cap als números antics",
       x = "Període", y = "% del total de premis", fill = "Rang") +
  theme_minimal() + 
  coord_flip()

# Preparació de dades, model LMM
df_model <- df_net %>%
  filter(premi > 0, lletra == "Bombo (Directe)") %>%
  mutate(
    es_parell = factor(numero %% 2 == 0, labels = c("Senar", "Parell")),
    suma_digits = (numero %/% 10000) + (numero %/% 1000 %% 10) + 
      (numero %/% 100 %% 10) + (numero %/% 10 %% 10) + (numero %% 10),
    log_premi = log10(premi)
  )

# Model Lineal Mixte
model_mixt <- lmer(log_premi ~ es_parell + suma_digits + (1 | any), data = df_model)

# Taula 4: Coeficients del LMM
summary_model <- summary(model_mixt)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Predictor") %>%
  rename(Estimate = `Estimate`, Std_Error = `Std. Error`, t_value = `t value`)

summary_model %>%
  kable("latex", booktabs = TRUE, caption = "Coeficients fixos del LMM: Paritat i Suma de dígits") %>%
  kable_styling(font_size = 9, latex_options = c("HOLD_position"))

# Gràfic 4: Paritat
p1 <- ggplot(df_model, aes(x = es_parell, y = log_premi, fill = es_parell)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "black") +
  labs(title = "Mite: Paritat vs Valor del Premi", x = "Paritat", y = "Log10(Premi)") +
  theme_minimal() + 
  theme(legend.position = "none")
print(p1)

# Gràfic 5: Mite de la suma de dígits
p2 <- ggplot(df_model, aes(x = suma_digits, y = log_premi)) +
  geom_jitter(alpha = 0.1, color = "steelblue", size = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Mite: Suma de dígits", x = "Suma", y = "Log10(Premi)") +
  theme_minimal()
print(p2)

# Preparació de datos models
df_ml <- df_net %>%
  filter(!is.na(premi) & premi > 0)
df_ml$any_fact <- as.factor(df_ml$any)
df_ml$lletra <- as.factor(df_ml$lletra)

# 1. Model Mixt
model_mixt_final <- lmer(log(premi) ~ lletra + (1 | any_fact), data = df_ml)

# 2. GLM Gamma
model_glm <- glm(premi ~ lletra + any_fact, family = Gamma(link = "log"), data = df_ml)

# 3. Random Forest
set.seed(123)
data_mostra <- df_ml[sample(nrow(df_ml), 5000), ] 
model_rf <- randomForest(premi ~ lletra + numero, data = data_mostra, ntree = 100)

# Càlcul RMSE
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2, na.rm = TRUE))

pred_mixt <- exp(predict(model_mixt_final, data_mostra, re.form = NULL))
pred_glm  <- predict(model_glm, data_mostra, type = "response")
pred_rf   <- predict(model_rf, data_mostra)

error_mixt <- rmse(data_mostra$premi, pred_mixt)
error_glm <- rmse(data_mostra$premi, pred_glm)
error_rf <- rmse(data_mostra$premi, pred_rf)

# ERRORS
cat("RMSE Model Mixt:", round(error_mixt, 2), "\n")
cat("RMSE GLM Gamma:", round(error_glm, 2), "\n")
cat("RMSE Random Forest:", round(error_rf, 2), "\n")

# Test X^2
df_digitos <- df_net %>%
  filter(!is.na(numero), lletra == "Bombo (Directe)", any >= 2011) %>% 
  mutate(numero_str = sprintf("%05d", as.numeric(numero))) %>%
  mutate(
    d1 = substr(numero_str, 1, 1),
    d2 = substr(numero_str, 2, 2),
    d3 = substr(numero_str, 3, 3),
    d4 = substr(numero_str, 4, 4),
    d5 = substr(numero_str, 5, 5)
  ) %>%
  pivot_longer(
    cols = d1:d5,
    names_to = "posicio",
    values_to = "digit"
  )

df_counts_chi <- df_digitos %>%
  count(digit) %>%
  arrange(digit)

test_chi <- chisq.test(df_counts_chi$n, p = rep(0.1, 10))

# Taula 5: Resultat del test 
chi_resum <- data.frame(
  Estadístic = round(test_chi$statistic, 4),
  `Grau de llibertat` = test_chi$parameter,
  `p-valor` = signif(test_chi$p.value, 4)
)

chi_resum %>%
  kable("latex", booktabs = TRUE, caption = "Resultat del test Chi-quadrat per uniformitat") %>%
  kable_styling(font_size = 10, latex_options = c("HOLD_position"))