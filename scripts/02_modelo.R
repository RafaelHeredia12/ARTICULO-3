# ============================================================
# Entrenamiento y evaluación - Clasificador naive Bayes
# Impacto de noticias financieras (Impacto Alto vs Otro/Bajo)
# ============================================================
# Continúa el pipeline de 01_limpieza.R. Este script:
#   1. Carga el split train/test (features_split.rds)
#   2. Entrena naive_bayes() del paquete naivebayes
#   3. Genera predicciones sobre el test set
#   4. Calcula accuracy, precision, recall, F1-score y matriz de
#      confusión
#   5. Guarda todo en resultados_modelo.rds para consumir en el .qmd
# ============================================================

library(naivebayes)
library(Matrix)
library(tibble)

features <- readRDS("data/processed/features_split.rds")
train_x <- features$train_x
test_x  <- features$test_x
train_y <- features$train_y
test_y  <- features$test_y

# Al inicio del script:
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)


# ---- 1. Entrenamiento ----------------------------------------------------
# naive_bayes() (a diferencia de las funciones especializadas como
# poisson_naive_bayes()) NO acepta matrices dispersas: internamente
# siempre corre `as.data.frame(x)`, lo cual falla con un dgCMatrix. Por
# eso convertimos a matriz densa con as.matrix() antes de entrenar. El
# vocabulario tras tokenizar ~3000 titulares es pequeño (unos cientos
# de palabras), así que la matriz densa resultante pesa solo unos MB —
# sin problema de memoria ni de tiempo.
train_x_mat <- as.matrix(train_x)
test_x_mat  <- as.matrix(test_x)

# usepoisson = TRUE sólo activa la distribución Poisson en columnas de
# clase "integer" (naive_bayes.default revisa is.integer(var), no sólo
# si los valores son enteros). as.matrix() sobre una dgCMatrix produce
# "double", así que sin este paso usepoisson=TRUE queda sin efecto y
# TODAS las columnas se modelan como Gaussianas -- un supuesto muy malo
# para conteos de palabras mayormente ceros, y probable causa de que la
# primera corrida haya salido tan floja. storage.mode<- fuerza la clase
# integer preservando dimnames.
storage.mode(train_x_mat) <- "integer"
storage.mode(test_x_mat)  <- "integer"

# laplace = 1: suaviza los conteos para evitar probabilidad cero cuando
# una palabra del vocabulario no aparece en alguna clase del training.
modelo_nb <- naive_bayes(
  x = train_x_mat,
  y = train_y,
  usepoisson = TRUE,
  laplace = 1
)

# Verificación: confirmar que sí se usó Poisson y no Gaussiana
cat("Distribuciones condicionales usadas (debe ser todo Poisson):\n")
print(table(get_cond_dist(modelo_nb)))

# ---- 2. Predicciones sobre el test set ------------------------------------
pred_clase <- predict(modelo_nb, newdata = test_x_mat, type = "class")

# ---- 3. Matriz de confusión -------------------------------------------------
# Filas = clase real, columnas = clase predicha
matriz_confusion <- table(Real = test_y, Predicho = pred_clase)
cat("Matriz de confusión:\n")
print(matriz_confusion)

p_confusion <- as.data.frame(matriz_confusion) %>%
  ggplot(aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c") +
  labs(title = "Matriz de confusión (test set, n = 576)") +
  theme_minimal()


print(p_confusion)
ggsave("outputs/figures/04_matriz_confusion.png", p_confusion, width = 6, height = 5, dpi = 300) 

# ---- 4. Métricas de desempeño -----------------------------------------------
# Clase positiva: "Impacto Alto" (la que interesa detectar a tiempo)
clase_pos <- "Impacto Alto"
clase_neg <- "Impacto Otro/Bajo"

vp <- matriz_confusion[clase_pos, clase_pos]  # verdaderos positivos
fp <- matriz_confusion[clase_neg, clase_pos]  # falsos positivos
fn <- matriz_confusion[clase_pos, clase_neg]  # falsos negativos
vn <- matriz_confusion[clase_neg, clase_neg]  # verdaderos negativos

accuracy  <- (vp + vn) / sum(matriz_confusion)
precision <- vp / (vp + fp)
recall    <- vp / (vp + fn)
f1        <- 2 * precision * recall / (precision + recall)

metricas <- tibble(
  Métrica = c("Accuracy", "Precision", "Recall", "F1-score"),
  Valor   = round(c(accuracy, precision, recall, f1), 4)
)
cat("\nMétricas:\n")
print(metricas)

# ---- 5. Términos/variables más discriminantes -----------------------------
# Para cada feature (palabra o dummy de Sentiment/Sector/Market_Index),
# modelo_nb$tables[[feature]] es una tabla 1xK con la lambda (tasa
# Poisson) estimada para cada clase. Comparamos lambda_Alto vs
# lambda_Otro/Bajo en escala log para ver qué términos/variables
# inclinan más la balanza hacia cada clase.
lambda_mat <- t(sapply(modelo_nb$tables, function(tab) tab[1, ]))
# lambda_mat: filas = features, columnas = c("Impacto Otro/Bajo","Impacto Alto")

log_ratio <- log(lambda_mat[, "Impacto Alto"]) - log(lambda_mat[, "Impacto Otro/Bajo"])

top_alto <- sort(log_ratio, decreasing = TRUE)[1:15]
top_bajo <- sort(log_ratio, decreasing = FALSE)[1:15]

top_terms_df <- bind_rows(
  tibble(termino = names(top_alto), log_ratio = as.numeric(top_alto), clase = "Impacto Alto"),
  tibble(termino = names(top_bajo), log_ratio = as.numeric(top_bajo), clase = "Impacto Otro/Bajo")
)

p_terminos <- top_terms_df %>%
  mutate(termino = forcats::fct_reorder(termino, log_ratio)) %>%
  ggplot(aes(x = termino, y = log_ratio, fill = clase)) +
  geom_col() + 
  coord_flip() +
  labs(title = "Términos más discriminantes por razón de tasas (log)", x = NULL, y = "Log-ratio") +
  theme_minimal()

print(p_terminos)
ggsave("outputs/figures/03_terminos_discriminantes.png", p_terminos, width = 7, height = 6, dpi = 300)

cat("\nTop 15 términos/variables asociados con Impacto Alto:\n")
print(round(top_alto, 3))

cat("\nTop 15 términos/variables asociados con Impacto Otro/Bajo:\n")
print(round(top_bajo, 3))

# ---- Guardar para el .qmd ----------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(
    modelo           = modelo_nb,
    matriz_confusion = matriz_confusion,
    metricas         = metricas,
    pred_clase       = pred_clase,
    top_alto         = top_alto,
    top_bajo         = top_bajo
  ),
  "data/processed/resultados_modelo.rds"
)


