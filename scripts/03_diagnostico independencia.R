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
library(tidyr)
library(readr)
library(dplyr)
library(knitr)
library(ggplot2)
library(plotly)
#install.packages("plotly")

# Al inicio del script, junto a las demás librerías:
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE) 

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


# ---- 4. Visualización: cambio de índice por nivel de impacto --------------
p1 <- raw %>%
  filter(!is.na(Index_Change_Percent)) %>%
  mutate(Impact_Level = factor(Impact_Level, levels = c("Low", "Medium", "High"))) %>%
  ggplot(aes(x = Impact_Level, y = abs(Index_Change_Percent), fill = Impact_Level)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
  labs(title = "Cambio porcentual absoluto del índice por nivel de impacto",
       x = "Nivel de impacto", y = "Cambio porcentual absoluto (%)") +
  theme_minimal() + theme(legend.position = "none")

print(p1)
ggsave("outputs/figures/01_boxplot_cambio_indice.png", p1, width = 7, height = 5, dpi = 300)

# ---- 5. Visualización: sentimiento por nivel de impacto --------------------
p2 <- raw %>%
  filter(!is.na(Sentiment)) %>%
  count(Impact_Level, Sentiment) %>%
  group_by(Impact_Level) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Impact_Level, y = prop, fill = Sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Proporción de sentimiento por nivel de impacto",
       x = "Nivel de impacto", y = "Proporción") +
  theme_minimal()

print(p2)
ggsave("outputs/figures/02_barras_sentimiento.png", p2, width = 7, height = 5, dpi = 300)


# ---- 6. Visualización 3D: índice, volumen e impacto -----------------------
plot_ly(
  data = raw %>% filter(!is.na(Index_Change_Percent)),
  x = ~Index_Change_Percent, y = ~Trading_Volume,
  z = ~as.numeric(factor(Impact_Level, levels = c("Low", "Medium", "High"))),
  color = ~Impact_Level, type = "scatter3d", mode = "markers",
  marker = list(size = 3, opacity = 0.6)) %>%
  layout(scene = list(
    xaxis = list(title = "Cambio % del índice"),
    yaxis = list(title = "Volumen de operación"),
    zaxis = list(title = "Impact_Level")))
