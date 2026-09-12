# ============================================================
# Diagnóstico: ¿Impact_Level se relaciona con las variables
# estructuradas del dataset, o parece independiente de ellas?
# ============================================================
# Este bloque va bien en la sección "Datos" del .qmd, ANTES de
# justificar por qué el enfoque de texto (NLP) es necesario: si
# ni el cambio porcentual del índice predice Impact_Level, hace
# más sentido explorar el titular como fuente de señal.
#
# NOTA: si lo corres suelto en la consola (no dentro del .qmd,
# donde estas librerías ya están cargadas en el chunk de setup)
# necesitas cargar esto primero:
library(readr)
library(dplyr)
library(knitr)
library(tidyr)

# Ruta sin "../" porque, igual que 01_limpieza.R y 02_modelo.R, este
# script se corre desde la raíz del proyecto (vía el .Rproj). Si en
# cambio pegas este bloque DENTRO del .qmd (que vive en notebooks/),
# cambia la ruta a "../data/raw/financial_news.csv".
raw <- read_csv(
  "data/raw/financial_news.csv",
  col_types = cols(
    Date = col_date(format = "%d/%m/%Y"),
    Index_Change_Percent = col_double(),
    Trading_Volume = col_double(),
    .default = col_character()
  )
)

# ---- 1. Índice y volumen por nivel de impacto -----------------------------
raw |>
  mutate(abs_change = abs(Index_Change_Percent)) |>
  group_by(Impact_Level) |>
  summarise(
    media_cambio_pct = mean(abs_change, na.rm = TRUE),
    sd_cambio_pct    = sd(abs_change, na.rm = TRUE),
    media_volumen    = mean(Trading_Volume, na.rm = TRUE),
    .groups = "drop"
  ) |>
  kable(digits = 2, caption = "Cambio de índice y volumen por nivel de impacto")

# Prueba formal: ¿el cambio porcentual absoluto difiere por Impact_Level?
anova_cambio <- aov(abs(Index_Change_Percent) ~ Impact_Level, data = raw)
summary(anova_cambio)  # revisar el p-value de Impact_Level

# ---- 2. Sentimiento por nivel de impacto -----------------------------------
raw |>
  filter(!is.na(Sentiment)) |>
  count(Impact_Level, Sentiment) |>
  group_by(Impact_Level) |>
  mutate(prop = round(n / sum(n), 3)) |>
  select(-n) |>
  tidyr::pivot_wider(names_from = Sentiment, values_from = prop) |>
  kable(caption = "Proporción de sentimiento por nivel de impacto")

# ---- 3. Prueba chi-cuadrada: Impact_Level x Sentiment ----------------------
tabla_sent <- table(raw$Impact_Level, raw$Sentiment)
chisq.test(tabla_sent)  # revisar p-value