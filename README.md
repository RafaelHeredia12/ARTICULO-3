# ARTICULO-3: Análisis de Sentimiento en Noticias Financieras y Modelado Predictivo

Este repositorio contiene el código, datos, scripts de procesamiento y reportes del proyecto de investigación **ARTICULO-3**. El objetivo principal es analizar el impacto del sentimiento presente en noticias financieras sobre la variación de índices bursátiles y construir modelos predictivos basados en procesamiento de lenguaje natural (NLP) y econometría/aprendizaje automático.

---

## 📁 Estructura del Proyecto

```text
ARTICULO-3/
├── data/
│   ├── processed/
│   │   ├── features_split.rds       # Conjuntos de datos procesados (Entrenamiento/Prueba)
│   │   └── resultados_modelo.rds    # Objeto R con métricas y predicciones del modelo
│   └── raw/
│       ├── dict_financial_news.csv  # Diccionario/léxico léxico financiero
│       └── financial_news.csv       # Dataset bruto de noticias financieras
├── notebooks/
│   ├── ARTICULO_3.html              # Reporte compilado en HTML
│   └── ARTICULO_3.qmd               # Documento dinámico en Quarto
├── outputs/figures/
│   ├── 01_boxplot_cambio_indice.png # Distribución de cambios en el índice
│   ├── 02_barras_sentimiento.png    # Distribución de categorías de sentimiento
│   ├── 03_terminos_discriminantes.png # Palabras/términos de mayor importancia
│   └── 04_matriz_confusion.png      # Matriz de confusión de la clasificación
├── scripts/
│   ├── 01_limpieza.R                # Preprocesamiento de texto y feature engineering
│   ├── 02_modelo.R                  # Entrenamiento y evaluación del modelo
│   └── 03_diagnostico independencia.R # Pruebas diagnósticas (independencia/residuos)
├── .gitignore                       # Archivos excluidos del control de versiones
├── ARTICULO-3.Rproj                 # Archivo de proyecto de RStudio
└── README.md                        # Descripción general del repositorio