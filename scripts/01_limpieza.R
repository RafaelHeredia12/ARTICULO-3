# ============================================================
# Limpieza y feature engineering - Impacto de noticias financieras
# ============================================================
# Este script:
#   1. Carga financial_news.csv guiándose del diccionario de datos
#      para fijar el tipo correcto de cada columna desde la lectura
#   2. Explora la distribución cruda de Impact_Level (variable objetivo)
#   3. Recodifica Impact_Level a binaria (Alto vs Otro/Bajo)
#   4. Limpia Headline con stringr (minúsculas, sin puntuación/números,
#      espacios) antes de tokenizar
#   5. Tokeniza con tidytext::unnest_tokens() y remueve stop words en
#      inglés (siguiendo Text Mining with R, tidytextmining.com)
#   6. Construye la document-term matrix dispersa (Matrix)
#   7. Une la DTM con variables adicionales (sentimiento, sector, índice)
#   8. Separa train/test con semilla fija (set.seed(), no set_seed())
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(Matrix)

# ---- 1. Diccionario + carga tipada -----------------------------------
dict <- read_csv("data/raw/dict_financial_news.csv", show_col_types = FALSE)
print(dict[, c("Column Name", "Data Type", "Null Values")])

raw <- read_csv(
  "data/raw/financial_news.csv",
  col_types = cols(
    Date = col_date(format = "%d/%m/%Y"),
    Index_Change_Percent = col_double(),
    Trading_Volume = col_double(),
    .default = col_character()
  )
)

# ---- 2. Distribución cruda del target ---------------------------------
cat("Distribución original de Impact_Level:\n")
print(table(raw$Impact_Level, useNA = "ifany"))

# ---- 3. Target binario + id de documento -------------------------------
df <- raw %>%
  filter(!is.na(Headline)) %>%  # ~5% de titulares vienen vacíos (ver diccionario)
  mutate(
    doc_id = row_number(),
    Impact_Bin = case_when(
      Impact_Level == "High" ~ "Impacto Alto",
      Impact_Level %in% c("Medium", "Low") ~ "Impacto Otro/Bajo",
      TRUE ~ NA_character_
    ),
    Impact_Bin = factor(Impact_Bin, levels = c("Impacto Otro/Bajo", "Impacto Alto"))
  )

# ---- 4. Limpieza de texto con stringr ----------------------------------
df <- df %>%
  mutate(
    Headline_clean = Headline %>%
      str_to_lower() %>%
      str_replace_all("[^a-z\\s]", " ") %>%  # fuera números y puntuación
      str_squish()                            # colapsa espacios y trim
  )

# ---- 5. Tokenización + stop words --------------------------------------
data("stop_words")  # dataset integrado de tidytext (inglés)

tokens <- df %>%
  select(doc_id, Headline_clean) %>%
  unnest_tokens(word, Headline_clean) %>%  # una palabra por renglón
  anti_join(stop_words, by = "word") %>%
  filter(str_length(word) > 2)              # descarta ruido residual

# ---- 6. Document-term matrix dispersa ----------------------------------
dtm <- tokens %>%
  count(doc_id, word, name = "n") %>%
  cast_sparse(doc_id, word, n)  # filas = titulares, cols = vocabulario, val = frecuencia

# ---- 7. Features adicionales + unión con la DTM ------------------------
ids_con_texto <- as.integer(rownames(dtm))  # titulares que sobrevivieron la limpieza

extra <- df %>%
  filter(doc_id %in% ids_con_texto) %>%
  arrange(match(doc_id, ids_con_texto)) %>%  # mismo orden que dtm
  transmute(
    Sentiment = factor(if_else(is.na(Sentiment), "Desconocido", Sentiment)),
    Sector = factor(Sector),
    Market_Index = factor(Market_Index)
  )

extra_mat <- sparse.model.matrix(~ Sentiment + Sector + Market_Index - 1, data = extra)

features <- cbind(dtm, extra_mat)
target <- df$Impact_Bin[match(ids_con_texto, df$doc_id)]

# ---- 8. Train / test ----------------------------------------------------
set.seed(101)  # el enunciado dice set_seed(), la función real en R es set.seed()
n <- nrow(features)
idx_train <- sample(seq_len(n), size = floor(0.8 * n))

train_x <- features[idx_train, ]
test_x  <- features[-idx_train, ]
train_y <- target[idx_train]
test_y  <- target[-idx_train]

# ---- Guardar --------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(list(train_x = train_x, test_x = test_x, train_y = train_y, test_y = test_y),
        "data/processed/features_split.rds")

