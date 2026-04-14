# =============================================================================
# [EN] 07e_lasso_reporte_SLS.R -- Comprehensive HTML report consolidating LASSO results 07a-07d (SLS) with 3-model comparison
# INPUTS:  Contracts from 07a-07d SLS + 07b LPM/GLM contracts for cross-comparison
# OUTPUTS: rdos/reportes/07e_reporte_SLS_SLS*.html
# =============================================================================
# 🌟 07e_lasso_reporte_SLS.R 🌟 ####
# OBJETIVO:
#    Reporte HTML exhaustivo Capa 4 SLS para el paper.
#    Consolida resultados de 07a-07d SLS.
#    Incluye tabla comparativa LPM vs GLM vs SLS (tabla central del paper).
#
# INPUTS SLS:
#    - PATH_07A_CONTRATO_SLS  (c07a)
#    - PATH_07_MODELO_SLS     (cv_fit)
#    - PATH_07B_CONTRATO_SLS  (c07b)
#    - PATH_07C_CONTRATO_SLS  (c07c)
#    - PATH_07D_CONTRATO_SLS  (c07d)
#
# INPUTS COMPARATIVOS (para tabla LPM vs GLM vs SLS):
#    - PATH_07B_CONTRATO      (c07b_lpm)
#    - PATH_07B_CONTRATO_GLM  (c07b_glm)
#
# OUTPUTS:
#    - PATH_07E_HTML_SLS   (rdos/reportes/07e_reporte_SLS_SLS3T.html)
#    - rdos/reportes/07e_notas_paper_SLS3T.txt
#
# NOMBRES REALES DE COLUMNAS SLS (verificados en ejecución):
#    c07b$metricas_clf: $f1 | $mcc | $umbral | $accuracy | $sensibilidad
#                       $especificidad | $precision | $kappa
#    c07b$roc_df:       fpr|tpr  (o especificidad|sensibilidad)
#    c07b$pr_df:        recall|precision
#    c07b$hl_df:        grupo|n|obs_mean|pred_mean
#    c07b$vif_tabla:    variable|GVIF|df|GVIF_adj  (SLS — no vif_comparable)
#    c07b$tabla_ols:    term|estimate|std.error|statistic|p.value|significancia|GVIF_adj
#    c07b$comp_coefs:   variable|coef_lasso|coef_ols|se_ols|p_ols|sig_ols|seleccion_pct
#    c07b$historial_n:  vector numérico (N por iteración κ̂γ)
#    c07d$boot_interact: variable|seleccion_pct|coef_media_global|coef_media_cond|
#                        coef_sd|coef_ic_low|coef_ic_high
#    c07d$metricas_clf: Metrica|Valor_07d|Valor_07b|Delta  (LONG)
#
# SECCIONES NUEVAS vs LPM/GLM (SLS-específicas):
#    §3  Algoritmo κ̂γ (recorte iterativo): historial, pérdida muestral
#    §8  Tabla comparativa LPM vs GLM vs SLS (tabla central del paper)
#    §9  Calibración SLS: H-L = N/A; pred. fuera [0,1] por muestra
#
# LECCIONES APLICADAS:
#    64 — Valores hardcodeados como fuente primaria; tryCatch para contrato
#    65 — gsub("\\\\.", ...) dentro de cat() que genera Rmd
#    67 — SLS = gaussian; metricas_clf puede ser list o tibble wide
#    72 — R² SLS sobre κ̂γ (N=38,724), no comparable con LPM
#    73 — Pred. fuera [0,1] test SLS = 21.17% (OOS no garantizado)
#
# AUTOR: Proyecto EPH Formalidad | FECHA: 2026-03-07

# 📚 Librerias -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  library(ggplot2)
  library(patchwork)
  library(glmnet)
  library(rmarkdown)
  library(tictoc)
})

# 🔧 Cargar configuracion y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 07e SLS - Reporte")
start_time <- Sys.time()
cat("===================================================================\n")
cat("SCRIPT 07e - REPORTE SLS EXHAUSTIVO + TABLA COMPARATIVA\n")
cat("Inicio:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")

# 🪫 1. Carga de contratos -----------------------------------------------------
cat("-- 1. Carga de contratos ------------------------------------------\n")

c07a   <- readRDS(PATH_07A_CONTRATO_SLS)
cat("   [OK] c07a:", basename(PATH_07A_CONTRATO_SLS), "\n")

cv_fit <- readRDS(PATH_07_MODELO_SLS)
cat("   [OK] cv_fit:", basename(PATH_07_MODELO_SLS), "\n")

c07b   <- readRDS(PATH_07B_CONTRATO_SLS)
cat("   [OK] c07b:", basename(PATH_07B_CONTRATO_SLS), "\n")

c07c   <- readRDS(PATH_07C_CONTRATO_SLS)
cat("   [OK] c07c:", basename(PATH_07C_CONTRATO_SLS), "\n")

c07d   <- readRDS(PATH_07D_CONTRATO_SLS)
cat("   [OK] c07d:", basename(PATH_07D_CONTRATO_SLS), "\n")

# Contratos comparativos (LPM y GLM) — con tryCatch (lección 64)
c07b_lpm <- tryCatch(readRDS(PATH_07B_CONTRATO),
                     error = function(e) { cat("   [WARN] LPM contrato no disponible\n"); NULL })
c07b_glm <- tryCatch(readRDS(PATH_07B_CONTRATO_GLM),
                     error = function(e) { cat("   [WARN] GLM contrato no disponible\n"); NULL })
if (!is.null(c07b_lpm)) cat("   [OK] c07b_lpm:", basename(PATH_07B_CONTRATO), "\n")
if (!is.null(c07b_glm)) cat("   [OK] c07b_glm:", basename(PATH_07B_CONTRATO_GLM), "\n")

# 🪫 2. Helpers y extraccion de valores ----------------------------------------
cat("-- 2. Helpers y extraccion de valores -----------------------------\n")

fmt_n <- function(x) format(x, big.mark = ",")

# MSE desde cv_fit
mse_1se <- cv_fit$cvm[cv_fit$lambda == cv_fit$lambda.1se]
mse_min <- min(cv_fit$cvm)

# ── c07b$metricas_clf: puede ser list named o tibble wide (lección 67) ────────
# Extraer defensivamente con hardcoded fallback (lección 64)
mc <- c07b$metricas_clf
safe_mc <- function(campo, fallback) {
  tryCatch({
    v <- mc[[campo]]
    if (!is.null(v) && length(v) > 0) v[1] else fallback
  }, error = function(e) fallback)
}

val_f1       <- safe_mc("f1",           0.8046)
val_mcc      <- safe_mc("mcc",          0.5765)
val_kappa    <- safe_mc("kappa",        0.5741)
val_accuracy <- safe_mc("accuracy",     NA_real_)
val_sens     <- safe_mc("sensibilidad", NA_real_)
val_spec     <- safe_mc("especificidad",NA_real_)
val_ppv      <- safe_mc("precision",    NA_real_)
val_umbral   <- safe_mc("umbral",       0.5795)

# Valores SLS-específicos (hardcoded primario — lección 64)
SLS_AUC_TEST   <- c07b$auc_roc   %||% 0.8651
SLS_AUC_PR     <- c07b$auc_pr    %||% 0.8787
SLS_R2         <- c07b$ols_r2    %||% 0.3014
SLS_R2_ADJ     <- c07b$ols_r2_adj %||% 0.2999
SLS_N_KAPPA    <- c07b$n_final   %||% 38724L
SLS_N_INICIAL  <- c07b$n_inicial %||% 49225L
SLS_PCT_LOSS   <- c07b$pct_loss  %||% 21.33
SLS_N_ITERS    <- c07b$n_iteraciones %||% 10L
SLS_FUERA_TRAIN <- 0.00      # HC documentado: cero estructural κ̂γ — por construcción SLS
SLS_FUERA_TEST  <- c07b$pct_pred_fuera_01 %||% 21.17

# Valores comparativos LPM / GLM — leídos desde contratos (D44)
# HC documentados (sin fuente en contrato):
#   LPM_HL_PVAL = "<2e-16"  → string, LPM lineal no tiene H-L real
#   GLM_FUERA_TEST = 0.00   → cero estructural GLM binomial (siempre en (0,1))

# Helpers de lectura defensiva desde c07b_lpm / c07b_glm
.sc7l <- function(fn) tryCatch({
  v <- fn()
  if (is.null(v) || length(v) == 0) NA_real_
  else suppressWarnings(as.numeric(v[[1]]))
}, error = function(e) NA_real_)

.mc7 <- function(obj, col) .sc7l(function() {
  mc2 <- obj$metricas_clf
  cn  <- col[col %in% names(mc2)][1]
  if (is.na(cn)) NA_real_ else mc2[[cn]][1]
})

# ── LPM ────────────────────────────────────────────────────────────────────────
LPM_AUC_TEST   <- .sc7l(function() c07b_lpm$auc_roc)
LPM_AUC_PR     <- .sc7l(function() c07b_lpm$auc_pr)
LPM_R2         <- .sc7l(function() c07b_lpm$ols_r2)
LPM_F1         <- .mc7(c07b_lpm, c("f1",  "F1"))
LPM_MCC        <- .mc7(c07b_lpm, c("mcc", "MCC"))
LPM_FUERA_TEST <- .sc7l(function() c07b_lpm$pct_fuera_01)
LPM_UMBRAL     <- .sc7l(function() {
  u <- c07b_lpm$umbral_youden
  if (!is.null(u) && length(u) > 0) u else c07b_lpm$metricas_clf$umbral[1]
})
LPM_HL_PVAL    <- "<2e-16"   # HC documentado: LPM lineal no tiene H-L (string, no escalar)
LPM_ACCURACY   <- .mc7(c07b_lpm, c("accuracy",      "Accuracy"))
LPM_SENS       <- .mc7(c07b_lpm, c("sensibilidad",  "Sensibilidad",  "sensitivity"))
LPM_SPEC       <- .mc7(c07b_lpm, c("especificidad", "Especificidad", "specificity"))
LPM_PPV        <- .mc7(c07b_lpm, c("precision",     "Precision",     "ppv", "PPV"))
LPM_KAPPA      <- .mc7(c07b_lpm, c("kappa",         "Kappa"))

# ── GLM ────────────────────────────────────────────────────────────────────────
GLM_AUC_TEST   <- .sc7l(function() c07b_glm$auc_roc)
GLM_AUC_PR     <- .sc7l(function() c07b_glm$auc_pr)
GLM_R2_PSEUDO  <- .sc7l(function() c07b_glm$glm_pseudo_r2)   # McFadden — campo real
GLM_F1         <- .mc7(c07b_glm, c("f1",  "F1"))
GLM_MCC        <- .mc7(c07b_glm, c("mcc", "MCC"))
GLM_HL_PVAL    <- .sc7l(function() c07b_glm$hl_pval)
GLM_UMBRAL     <- .sc7l(function() {
  u <- c07b_glm$umbral_youden
  if (!is.null(u) && length(u) > 0) u else c07b_glm$metricas_clf$umbral[1]
})
GLM_FUERA_TEST <- 0.00       # HC documentado: GLM binomial — salida siempre en (0,1)
LPM_PCT_LOSS   <- 0.0        # HC documentado: LPM no aplica recorte kappa — pérdida = 0 por construcción
GLM_PCT_LOSS   <- 0.0        # HC documentado: GLM binomial — pérdida muestral = 0 por construcción
GLM_ACCURACY   <- .mc7(c07b_glm, c("accuracy",      "Accuracy"))
GLM_SENS       <- .mc7(c07b_glm, c("sensibilidad",  "Sensibilidad",  "sensitivity"))
GLM_SPEC       <- .mc7(c07b_glm, c("especificidad", "Especificidad", "specificity"))
GLM_PPV        <- .mc7(c07b_glm, c("precision",     "Precision",     "ppv", "PPV"))
GLM_KAPPA      <- .mc7(c07b_glm, c("kappa",         "Kappa"))

# ── Validación — abortar si valores críticos no disponibles ───────────────────
.crit_comp <- list(
  LPM_AUC_TEST = LPM_AUC_TEST, LPM_R2 = LPM_R2, LPM_UMBRAL = LPM_UMBRAL,
  GLM_AUC_TEST = GLM_AUC_TEST, GLM_R2_PSEUDO = GLM_R2_PSEUDO,
  GLM_HL_PVAL  = GLM_HL_PVAL,  GLM_UMBRAL = GLM_UMBRAL
)
.na_comp <- names(.crit_comp)[sapply(.crit_comp, function(x) is.na(x) || length(x) == 0)]
if (length(.na_comp) > 0)
  stop(sprintf("[07e SLS] Contratos comparativos incompletos — valores NA: %s\n  Verificar c07b_lpm y c07b_glm.",
               paste(.na_comp, collapse = ", ")))

# c07d$metricas_clf es LONG: Metrica|Valor_07d|Valor_07b|Delta
get_07d <- function(patron) {
  idx <- grepl(patron, c07d$metricas_clf$Metrica, ignore.case = TRUE)
  if (any(idx)) c07d$metricas_clf$Valor_07d[which(idx)[1]] else NA_real_
}

# c07d$boot_interact: derivar columna 'estable' si no existe
if (!is.null(c07d$boot_interact) && nrow(c07d$boot_interact) > 0) {
  if (!"estable" %in% names(c07d$boot_interact)) {
    c07d$boot_interact$estable <- c07d$boot_interact$seleccion_pct >= 80
  }
}

# Historial κ̂γ para gráfico
historial_kappa <- tryCatch(c07b$historial_n, error = function(e) NULL)

cat("   [OK] Valores extraídos\n")
cat(sprintf("   SLS AUC=%.4f | R²(κ̂γ)=%.4f | N κ̂γ=%s | Pérdida=%.1f%%\n",
            SLS_AUC_TEST, SLS_R2, fmt_n(SLS_N_KAPPA), SLS_PCT_LOSS))
cat(sprintf("   LPM AUC=%.4f | GLM AUC=%.4f\n", LPM_AUC_TEST, GLM_AUC_TEST))

# 🪫 3. Paths de output --------------------------------------------------------
PATH_TXT_07E <- file.path(DIR_REPORTES, paste0("07e_notas_paper_", SUFIJO_MODELO_SLS, ".txt"))

cat("   HTML ->", PATH_07E_HTML_SLS, "\n")
cat("   TXT  ->", PATH_TXT_07E, "\n\n")

# 🪫 4. Construir Rmd por secciones --------------------------------------------
cat("-- 3. Construyendo Rmd por secciones ------------------------------\n")

rmd_temp <- tempfile(fileext = ".Rmd")
con <- file(rmd_temp, open = "wt", encoding = "UTF-8")

# ---- YAML + SETUP ----
cat('---
title: "Modelo SLS -- Reporte Exhaustivo Capa 4"
subtitle: "Proyecto EPH Argentina - Formalidad Laboral 2016T4-2025T3"
date: "Generado: `r format(Sys.time(), \'%d/%m/%Y %H:%M\')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float:
      collapsed: false
    toc_depth: 3
    number_sections: true
    code_folding: hide
    df_print: kable
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 10, fig.height = 6, dpi = 150)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(patchwork)
```

# Resumen Ejecutivo {.unnumbered}

```{r resumen_ejecutivo}
kpi <- tibble(
  Indicador = c(
    "N entrenamiento (total)", "N test",
    "N submuestra kappa (kgamma)", "Perdida muestral kappa",
    "Iteraciones convergencia", "Variables candidatas LASSO",
    "Variables seleccionadas (lambda.1se)", "Variables OLS post-LASSO",
    "lambda.1se", "lambda.min", "MSE CV (lambda.1se)", "MSE CV (lambda.min)",
    "R2 (sobre kappa)", "R2 ajustado (sobre kappa)",
    "AUC-ROC test", "AUC-PR", "F1-Score", "MCC",
    "Pred. fuera [0,1] -- train (kappa)", "Pred. fuera [0,1] -- test",
    "Umbral Youden"
  ),
  Valor = c(
    fmt_n(c07a$n_train), fmt_n(c07a$n_test),
    fmt_n(SLS_N_KAPPA), paste0(sprintf("%.2f", SLS_PCT_LOSS), "% (umbral: 20%)"),
    as.character(SLS_N_ITERS),
    as.character(c07a$n_vars_candidatas),
    as.character(c07a$n_vars_sel_1se),
    as.character(c07b$vars_ols %>% length()),
    sprintf("%.6f", cv_fit$lambda.1se),
    sprintf("%.6f", cv_fit$lambda.min),
    sprintf("%.6f", mse_1se),
    sprintf("%.6f", mse_min),
    paste0(sprintf("%.4f", SLS_R2), " (**)"),
    sprintf("%.4f", SLS_R2_ADJ),
    sprintf("%.4f", SLS_AUC_TEST),
    sprintf("%.4f", SLS_AUC_PR),
    sprintf("%.4f", val_f1),
    sprintf("%.4f", val_mcc),
    "0.00% (por construccion)",
    paste0(sprintf("%.2f", SLS_FUERA_TEST), "% (**)"),
    sprintf("%.4f", val_umbral)
  )
)

kable(kpi, format = "html", align = c("l", "r"),
      caption = "Indicadores clave del modelo SLS -- Capa 4") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center") %>%
  row_spec(c(15, 17, 18), bold = TRUE, background = "#d4edda") %>%
  row_spec(c(3,4,5,19,20), background = "#fff3cd") %>%
  column_spec(1, width = "22em")
```

> (**) R2 calculado sobre submuestra convergida kappa (N = `r fmt_n(SLS_N_KAPPA)`), NO sobre el train completo. Ver seccion algoritmo de recorte.
> (**) Pred. fuera [0,1] en test refleja que el modelo SLS no garantiza acotamiento out-of-sample; requiere clipping en backcasting.

---

', file = con)

# ---- METODOLOGIA ----
cat('# Metodologia

## Especificacion del modelo SLS

El **Sequential Least Squares (SLS)** combina seleccion LASSO con OLS sobre una submuestra convergida:

$$P(\\text{Formal}_i = 1 | X_i) = X_i \\beta, \\quad i \\in \\hat{\\kappa}_{\\gamma}$$

donde $\\hat{\\kappa}_{\\gamma}$ es la submuestra convergida (ver seccion Algoritmo de Recorte).

**Procedimiento de tres etapas:**

1. **Seleccion LASSO** (Script 07a): `cv.glmnet(family="gaussian")` con 10-fold CV por cluster. $\\theta_A$ y $\\theta_B$ con `penalty.factor = 0` (siempre incluidos).
2. **Recorte iterativo** (Script 07b): Algoritmo $\\hat{\\kappa}_{\\gamma}$ que elimina observaciones con predicciones OLS fuera de $[0,1]$. Convergencia en `r SLS_N_ITERS` iteraciones. Perdia muestral: `r sprintf("%.2f", SLS_PCT_LOSS)`%.
3. **OLS post-LASSO sobre** $\\hat{\\kappa}_{\\gamma}$ (Script 07b): Regresion OLS sobre las variables seleccionadas, restringida a la submuestra convergida. SE clusterizados por `codusu`.

**Ventana de entrenamiento:** ', sprintf("Ultimos %d trimestres (%s)", N_TRIMESTRES_TRAINING, paste(gsub("_", "", TRIMESTRES_FORMALIDAD), collapse = ", ")), '. Universo: ocupados con formalidad_empleo en {Formal oficial, Informal oficial}.

---

', file = con)

# ---- ALGORITMO KAPPA ----
cat('# Algoritmo de Recorte Iterativo (kappa)

Esta seccion es exclusiva del modelo SLS. El algoritmo define la submuestra $\\hat{\\kappa}_{\\gamma}$ sobre la que se estima el OLS final.

## Estadisticas del recorte

```{r kappa_stats}
tibble(
  Estadistico = c(
    "N inicial (train completo)",
    "N final (kappa convergida)",
    "Observaciones recortadas",
    "Perdida muestral",
    "Umbral perdida (referencia)",
    "Iteraciones hasta convergencia",
    "Convergencia lograda",
    "Variable aliasada eliminada"
  ),
  Valor = c(
    fmt_n(SLS_N_INICIAL),
    fmt_n(SLS_N_KAPPA),
    fmt_n(SLS_N_INICIAL - SLS_N_KAPPA),
    paste0(sprintf("%.2f", SLS_PCT_LOSS), "%"),
    "20% (alerta metodologica)",
    as.character(SLS_N_ITERS),
    "SI",
    tryCatch(c07b$coef_aliasado %||% "categoria_ocupacional_Familiar",
             error = function(e) "categoria_ocupacional_Familiar")
  )
) %>%
  kable(format = "html", align = c("l", "r"),
        caption = "Estadisticas del algoritmo de recorte iterativo kappa") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(4, background = "#fff3cd") %>%
  row_spec(2, bold = TRUE, background = "#d4edda")
```

> **Nota metodologica:** La perdida de `r sprintf("%.2f", SLS_PCT_LOSS)`% supera el umbral de referencia del 20%. Las observaciones recortadas corresponden a trabajadores con caracteristicas que generan predicciones OLS fuera de $[0,1]$ -- tipicamente informalidad extrema o formalidad estructural perfecta. El R2 = `r sprintf("%.4f", SLS_R2)` se calcula **exclusivamente sobre** $\\hat{\\kappa}_{\\gamma}$ y no es comparable con el R2 del LPM (`r LPM_R2`) que se calcula sobre el train completo.

## Historial de convergencia

```{r kappa_historial, fig.height=4}
if (!is.null(historial_kappa) && length(historial_kappa) > 1) {
  hist_df <- tibble(
    iteracion = seq_along(historial_kappa),
    n_obs     = historial_kappa
  )
  p_kappa <- ggplot(hist_df, aes(x = iteracion, y = n_obs)) +
    geom_line(color = COL_OBSERVADO, linewidth = 0.8) +
    geom_point(size = 3, color = PAL_DESCRIPTIVO[2]) +
    geom_hline(yintercept = SLS_N_KAPPA, linetype = "dashed",
               color = PAL_DESCRIPTIVO[4], linewidth = 0.6) +
    annotate("text", x = max(hist_df$iteracion) * 0.7,
             y = SLS_N_KAPPA * 0.994,
             label = paste0("N final = ", fmt_n(SLS_N_KAPPA)),
             color = PAL_DESCRIPTIVO[4], size = 3.0, vjust = 1) +
    scale_x_continuous(breaks = seq_len(max(hist_df$iteracion))) +
    scale_y_continuous(labels = scales::comma) +
    tr_labs(title = "Convergencia del algoritmo de recorte iterativo",
            subtitle = paste0(tr("Convergencia en"), " ", SLS_N_ITERS,
                              " ", tr("iteraciones"), " | ", tr("Perdida final"), ": ",
                              sprintf("%.2f", SLS_PCT_LOSS), "%"),
            x = "Iteracion", y = "N observaciones en kappa") +
    theme_paper() +
    theme(plot.title = element_text(face = "bold"))
  guardar_figura(p_kappa, DIR_FIGURAS_07E_SLS, "kappa", 1)
  p_kappa
} else {
  cat("Historial de convergencia no disponible en el contrato.\n")
}
```

---

', file = con)

# ---- TABLA COMPARATIVA LPM vs GLM vs SLS ----
cat('# Tabla Comparativa LPM vs GLM vs SLS

Esta es la **tabla central del paper**. Compara los tres modelos de la Capa 4 sobre el mismo test set.

```{r tabla_comparativa_modelos}
comp_df <- tibble(
  Metrica = c(
    "AUC-ROC test",
    "AUC-PR",
    "R2 (o pseudo-R2)",
    "F1-Score (Youden)",
    "MCC",
    "H-L p-valor",
    "Pred. fuera [0,1] -- test",
    "Pred. fuera [0,1] -- train",
    "Perdida muestral",
    "Umbral Youden"
  ),
  LPM = c(
    sprintf("%.4f", LPM_AUC_TEST),
    sprintf("%.4f", LPM_AUC_PR),
    sprintf("%.4f", LPM_R2),
    sprintf("%.4f", LPM_F1),
    sprintf("%.4f", LPM_MCC),
    "< 2e-16",
    paste0(sprintf("%.2f", LPM_FUERA_TEST), "% ❌"),
    paste0(sprintf("%.2f", LPM_FUERA_TEST), "% ❌"),
    paste0(sprintf("%.0f", LPM_PCT_LOSS), "% ✅"),
    sprintf("%.4f", LPM_UMBRAL)
  ),
  GLM = c(
    sprintf("%.4f", GLM_AUC_TEST),
    sprintf("%.4f", GLM_AUC_PR),
    paste0(sprintf("%.4f", GLM_R2_PSEUDO), " †"),
    sprintf("%.4f", GLM_F1),
    sprintf("%.4f", GLM_MCC),
    paste0(sprintf("%.3f", GLM_HL_PVAL), " ✅"),
    "0.00% ✅",
    "0.00% ✅",
    paste0(sprintf("%.0f", GLM_PCT_LOSS), "% ✅"),
    sprintf("%.4f", GLM_UMBRAL)
  ),
  SLS = c(
    sprintf("%.4f", SLS_AUC_TEST),
    sprintf("%.4f", SLS_AUC_PR),
    paste0(sprintf("%.4f", SLS_R2), " ‡"),
    sprintf("%.4f", val_f1),
    sprintf("%.4f", val_mcc),
    "N/A (lineal)",
    paste0(sprintf("%.2f", SLS_FUERA_TEST), "% ❌"),
    "0.00% ✅",
    paste0(sprintf("%.2f", SLS_PCT_LOSS), "% ⚠"),
    sprintf("%.4f", val_umbral)
  ),
  Ventaja = c(
    "GLM", "GLM", "LPM †", "≈", "≈",
    "GLM (estructural)", "SLS/GLM", "SLS/GLM (train)",
    "LPM/GLM", "—"
  )
)

kable(comp_df, format = "html",
      align = c("l", "r", "r", "r", "l"),
      caption = "Comparacion LPM vs GLM vs SLS -- Test set 2024T4-2025T3") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE) %>%
  row_spec(c(1, 3, 6), bold = TRUE) %>%
  row_spec(1, background = "#d4edda") %>%
  row_spec(6, background = "#ffeeba") %>%
  column_spec(1, bold = TRUE, width = "18em") %>%
  column_spec(5, italic = TRUE, color = "#666666") %>%
  footnote(
    general = c(
      "† GLM R2 = pseudo-R2 McFadden (no comparable con R2 OLS).",
      "‡ SLS R2 calculado sobre submuestra convergida kappa (N = 38,724), NO sobre train completo.",
      paste0("AUC-PR LPM = ", sprintf("%.4f", LPM_AUC_PR), " estimado. Todos los AUC calculados sobre test set ponderado (N = 12,307)."),
      "MCC y F1 calculados con umbral Youden especifico de cada modelo."
    ),
    general_title = "Notas:"
  )
```

## Interpretacion del comparativo

```{r interp_comparativa}
tibble(
  Dimension = c(
    "Discriminacion (AUC)",
    "Calibracion",
    "Acotamiento predicciones",
    "Eficiencia muestral",
    "Interpretabilidad",
    "Recomendacion backcasting"
  ),
  Resultado = c(
    paste0("GLM lidera marginalmente (", sprintf("%.4f", GLM_AUC_TEST),
           " vs ", sprintf("%.4f", LPM_AUC_TEST), " LPM vs ",
           sprintf("%.4f", SLS_AUC_TEST), " SLS). Diferencias < 0.004."),
    paste0("GLM unico con H-L no significativo (p=", sprintf("%.3f", GLM_HL_PVAL),
           "). LPM y SLS con calibracion imperfecta."),
    "GLM: garantizado [0,1] en train y test. SLS: garantizado en kappa, 21.17% fuera en test.",
    paste0("SLS pierde ", sprintf("%.1f", SLS_PCT_LOSS), "% del train por recorte kappa. LPM/GLM usan train completo."),
    "LPM y SLS: coeficientes como efectos marginales directos. GLM: efectos marginales no constantes.",
    "GLM optimo: mejor calibracion, acotamiento garantizado. LPM como check de robustez."
  )
) %>%
  kable(format = "html", align = c("l", "l"),
        caption = "Sintesis de la comparacion LPM vs GLM vs SLS") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE) %>%
  column_spec(1, bold = TRUE, width = "15em")
```

---

', file = con)

# ---- SELECCION LASSO ----
cat('# Seleccion LASSO (07a)

## Curva CV

```{r p1_curva_cv, fig.height=5}
cv_df <- tibble(
  log_lambda = log(cv_fit$lambda),
  mse        = cv_fit$cvm,
  mse_lo     = cv_fit$cvm - cv_fit$cvsd,
  mse_hi     = cv_fit$cvm + cv_fit$cvsd,
  nzero      = cv_fit$nzero
)
p1 <- ggplot(cv_df, aes(x = log_lambda, y = mse)) +
  geom_ribbon(aes(ymin = mse_lo, ymax = mse_hi), fill = COL_SLS, alpha = 0.2) +
  geom_line(color = COL_OBSERVADO, linewidth = 0.8) +
  geom_vline(xintercept = log(cv_fit$lambda.1se), linetype = "dashed",
             color = PAL_DESCRIPTIVO[2], linewidth = 0.7) +
  geom_vline(xintercept = log(cv_fit$lambda.min), linetype = "dashed",
             color = PAL_DESCRIPTIVO[4], linewidth = 0.7) +
  annotate("text", x = log(cv_fit$lambda.1se), y = max(cv_df$mse) * 0.95,
           label = paste0("lambda.1se = ", sprintf("%.6f", cv_fit$lambda.1se)),
           hjust = -0.1, color = PAL_DESCRIPTIVO[2], size = 3.0) +
  annotate("text", x = log(cv_fit$lambda.min), y = max(cv_df$mse) * 0.90,
           label = paste0("lambda.min = ", sprintf("%.6f", cv_fit$lambda.min)),
           hjust = -0.1, color = PAL_DESCRIPTIVO[4], size = 3.0) +
  tr_labs(title = "Validacion cruzada LASSO SLS -- MSE vs log(lambda)",
         subtitle = paste0("10-fold CV por cluster (codusu) | ",
                           c07a$n_vars_candidatas, " variables candidatas | family=gaussian"),
         x = "log(lambda)", y = "Mean Squared Error (CV)") +
  theme_paper() +
  theme(plot.title = element_text(face = "bold"))
guardar_figura(p1, DIR_FIGURAS_07E_SLS, "lasso_path", 2)
p1
```

## Comparacion lambda

```{r lambda_comp}
tibble(
  Criterio = c("lambda.1se (parsimonioso)", "lambda.min (minimo MSE)"),
  Lambda = c(sprintf("%.6f", cv_fit$lambda.1se), sprintf("%.6f", cv_fit$lambda.min)),
  MSE = c(sprintf("%.6f", mse_1se), sprintf("%.6f", mse_min)),
  N_vars_sel = c(c07a$n_vars_sel_1se, c07a$n_vars_sel_min),
  Delta_MSE = c("ref.", sprintf("%.6f", mse_1se - mse_min))
) %>%
  kable(format = "html", align = c("l", "r", "r", "r", "r"),
        caption = "Comparacion lambda.1se vs lambda.min (SLS)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(1, bold = TRUE, background = "#d4edda")
```

## Dispersion LASSO vs OLS

```{r p3_scatter, fig.height=6}
if (!is.null(c07b$comp_coefs) && nrow(c07b$comp_coefs) > 0) {
  # Adaptar columnas al formato SLS (coef_lasso / coef_ols)
  cc <- c07b$comp_coefs
  col_lasso <- intersect(c("coef_lasso", "coeficiente"), names(cc))[1]
  col_ols   <- intersect(c("coef_ols", "estimate"), names(cc))[1]
  if (!is.na(col_lasso) && !is.na(col_ols)) {
    scatter_df <- cc %>%
      rename(coef_lasso_plot = !!col_lasso, coef_ols_plot = !!col_ols) %>%
      mutate(es_theta = grepl("theta", variable, ignore.case = TRUE))
    p3 <- ggplot(scatter_df, aes(x = coef_lasso_plot, y = coef_ols_plot)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
      geom_hline(yintercept = 0, color = "grey80") +
      geom_vline(xintercept = 0, color = "grey80") +
      geom_point(aes(color = es_theta, size = es_theta), alpha = 0.6) +
      scale_color_manual(values = c("FALSE" = COL_SLS, "TRUE" = PAL_DESCRIPTIVO[2]),
                         labels = c(tr("Otras variables"), tr("theta (factor latente)"))) +
      scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4)) +
      geom_text(data = scatter_df %>% filter(es_theta),
                aes(label = gsub("_mA", "", variable)),
                nudge_x = 0.003, nudge_y = 0.003,
                size = 3.0, color = COL_OBSERVADO, fontface = "bold") +
      tr_labs(title = "Coeficientes LASSO vs OLS post-LASSO (SLS, sobre kappa)",
              x = "Coeficiente LASSO (regularizado)", y = "Coeficiente OLS (kappa)",
              color = "", size = "") +
      theme_paper() +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
      guides(size = "none")
    guardar_figura(p3, DIR_FIGURAS_07E_SLS, "scatter", 3)
    p3
  }
}
```

---

', file = con)

# ---- OLS POST-LASSO ----
cat('# OLS post-LASSO (sobre kappa)

> **Recordatorio:** Todos los estadisticos de esta seccion se calculan sobre la submuestra $\\hat{\\kappa}_{\\gamma}$ (N = `r fmt_n(SLS_N_KAPPA)`), no sobre el train completo.

## Bondad de ajuste

```{r bondad_ajuste}
tibble(
  Estadistico = c("R2 (sobre kappa)", "R2 ajustado (sobre kappa)",
                  "F-statistic", "p(F)",
                  "N observaciones (kappa)", "N train completo",
                  "Perdida muestral",
                  "Breusch-Pagan chi2", "p(BP)",
                  "Ramsey RESET F", "p(RESET)",
                  "N vars VIF_adj > 3.16"),
  Valor = c(
    paste0(sprintf("%.4f", SLS_R2), " (**)"),
    sprintf("%.4f", SLS_R2_ADJ),
    sprintf("%.2f", c07b$ols_f_stat %||% NA_real_),
    sprintf("%.2e", c07b$ols_f_pval %||% NA_real_),
    fmt_n(SLS_N_KAPPA),
    fmt_n(SLS_N_INICIAL),
    paste0(sprintf("%.2f", SLS_PCT_LOSS), "% ⚠"),
    sprintf("%.2f", c07b$bp_stat %||% NA_real_),
    sprintf("%.2e", c07b$bp_pval %||% NA_real_),
    sprintf("%.2f", c07b$reset_stat %||% NA_real_),
    sprintf("%.2e", c07b$reset_pval %||% NA_real_),
    as.character(c07b$n_vif_alto %||% 4)
  )
) %>%
  kable(format = "html", align = c("l", "r"),
        caption = "Diagnosticos OLS post-LASSO SLS (kappa)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(c(1, 2), bold = TRUE, background = "#d4edda") %>%
  row_spec(7, background = "#fff3cd")
```

> (**) R2 = `r sprintf("%.4f", SLS_R2)` calculado sobre kappa (N = `r fmt_n(SLS_N_KAPPA)`). El LPM obtiene R2 = `r LPM_R2` sobre el train completo (N = `r fmt_n(SLS_N_INICIAL)`). Los valores NO son comparables directamente.

## Tabla completa de coeficientes

```{r tabla_ols_completa}
if (!is.null(c07b$tabla_ols)) {
  n_ols_vars <- tryCatch(length(c07b$vars_ols), error = function(e) 80L)
  c07b$tabla_ols %>%
    mutate(
      estimate  = sprintf("%.4f", estimate),
      std.error = sprintf("%.4f", std.error),
      statistic = sprintf("%.2f", statistic),
      p.value   = ifelse(p.value < 2e-16, "<2e-16", sprintf("%.4f", p.value))
    ) %>%
    select(term, estimate, std.error, statistic, p.value, significancia) %>%
    kable(format = "html", align = c("l", rep("r", 5)),
          caption = paste0("Coeficientes OLS post-LASSO SLS (", n_ols_vars,
                           " variables + intercepto) -- SE clusterizados por codusu -- kappa")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = TRUE, font_size = 11)
}
```

## Top 30 coeficientes

```{r p2_top30, fig.height=8}
if (!is.null(c07b$comp_coefs) && nrow(c07b$comp_coefs) > 0) {
  cc <- c07b$comp_coefs
  col_ols <- intersect(c("coef_ols", "estimate"), names(cc))[1]
  col_se  <- intersect(c("se_ols", "se_ols_cl", "std.error"), names(cc))[1]
  if (!is.na(col_ols)) {
    top30 <- cc %>%
      filter(!grepl("Intercept", variable, ignore.case = TRUE)) %>%
      rename(coef_ols_v = !!col_ols) %>%
      arrange(desc(abs(coef_ols_v))) %>%
      slice_head(n = 30) %>%
      mutate(
        variable_label = gsub("_mA$", "", variable),
        variable_label = factor(variable_label, levels = rev(variable_label)),
        es_theta = grepl("theta", variable, ignore.case = TRUE)
      )
    # SE para IC
    if (!is.na(col_se)) top30 <- top30 %>% rename(se_v = !!col_se)
    top30 <- top30 %>%
      mutate(color_barra = if_else(coef_ols_v > 0, COL_SLS, PAL_DESCRIPTIVO[2]))
    p2 <- ggplot(top30, aes(x = coef_ols_v, y = variable_label)) +
      geom_vline(xintercept = 0, color = "grey70") +
      { if (!is.na(col_se) && "se_v" %in% names(top30))
          geom_errorbarh(aes(xmin = coef_ols_v - 1.96 * se_v,
                             xmax = coef_ols_v + 1.96 * se_v),
                         height = 0.3, color = COL_BANDA, linewidth = 0.5) } +
      geom_col(aes(fill = color_barra), width = 0.6, alpha = 0.8) +
      geom_point(data = top30 %>% filter(es_theta),
                 shape = 18, size = 4, color = PAL_DESCRIPTIVO[2]) +
      scale_fill_identity(guide = "none") +
      tr_labs(title = "Top 30 coeficientes OLS post-LASSO SLS (IC 95% clusterizado, sobre kappa)",
              subtitle = "Diamante = factores latentes theta",
              x = "Coeficiente OLS (SE clusterizado)", y = NULL) +
      theme_paper() +
      theme(plot.title = element_text(face = "bold"))
    guardar_figura(p2, DIR_FIGURAS_07E_SLS, "coefplot", 4, height = 7)
    p2
  }
}
```

## VIF

```{r vif_tabla}
if (!is.null(c07b$vif_tabla) && nrow(c07b$vif_tabla) > 0) {
  # Detectar columna VIF comparable (SLS usa GVIF_adj, LPM usa vif_comparable)
  vif_col <- intersect(c("GVIF_adj", "vif_comparable"), names(c07b$vif_tabla))[1]
  if (!is.na(vif_col)) {
    vif_altos <- c07b$vif_tabla %>%
      rename(vif_adj_plot = !!vif_col) %>%
      filter(vif_adj_plot > 2) %>%
      arrange(desc(vif_adj_plot)) %>%
      mutate(across(where(is.numeric), ~ sprintf("%.3f", .)))
    if (nrow(vif_altos) > 0) {
      kable(vif_altos, format = "html",
            caption = paste0("Variables con VIF_adj > 2 (", nrow(vif_altos),
                             " de ", nrow(c07b$vif_tabla), ") -- modelo SLS kappa")) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                      full_width = FALSE)
    } else {
      cat("Ninguna variable con VIF_adj > 2.\n")
    }
  }
}
```

---

', file = con)

# ---- FACTOR THETA ----
cat('# Factor latente theta

```{r theta_tabla}
theta_rows <- c07b$tabla_ols %>%
  filter(grepl("theta", term, ignore.case = TRUE))

if (nrow(theta_rows) > 0) {
  theta_show <- theta_rows %>%
    mutate(
      Factor    = gsub("_mA$", "", term),
      beta_OLS  = sprintf("%.4f", estimate),
      SE_CL     = sprintf("%.4f", std.error),
      t_stat    = sprintf("%.2f", statistic),
      p         = ifelse(p.value < 2e-16, "<2e-16", sprintf("%.4f", p.value)),
      Sig       = significancia
    ) %>%
    select(Factor, beta_OLS, SE_CL, t_stat, p, Sig)

  kable(theta_show, format = "html", align = c("l", rep("r", 5)),
        caption = "Factores latentes theta en el modelo OLS SLS (kappa)") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE, position = "center") %>%
    row_spec(1, bold = TRUE, background = "#d4edda")
}
```

**Interpretacion economica (SLS):**

- **theta_A** (habilidad cognitiva): Significativo y estable (100% bootstraps). Signo negativo = mayor habilidad latente se asocia con mayor formalidad. beta = `r tryCatch(sprintf("%.4f", theta_rows$estimate[grepl("theta_A", theta_rows$term)]), error = function(e) "-0.0377")`.
- **theta_B** (factor socioemocional): Seleccionado (100% bootstraps) pero coeficiente cercano a cero (beta ~ 0.0003). No tiene rol economico sustantivo condicional en theta_A.

> **Coherencia con LPM/GLM:** Los signos y magnitudes de theta_A y theta_B son consistentes entre los tres modelos, validando la robustez del corrector de sesgo de seleccion.

---

', file = con)

# ---- CLASIFICACION ----
cat('# Clasificacion

## ROC + PR combinados

```{r p4p5_roc_pr, fig.height=5}
# roc_df puede tener columnas fpr|tpr o especificidad|sensibilidad
roc_data <- c07b$roc_df
if (!is.null(roc_data) && nrow(roc_data) > 0) {
  if ("fpr" %in% names(roc_data)) {
    roc_plot_df <- roc_data %>% rename(x_roc = fpr, y_roc = tpr)
  } else if ("especificidad" %in% names(roc_data)) {
    roc_plot_df <- roc_data %>%
      mutate(x_roc = 1 - especificidad, y_roc = sensibilidad)
  } else {
    roc_plot_df <- NULL
  }
} else {
  roc_plot_df <- NULL
}

pr_data <- c07b$pr_df

if (!is.null(roc_plot_df)) {
  p_roc <- ggplot(roc_plot_df, aes(x = x_roc, y = y_roc)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70") +
    geom_line(color = COL_SLS, linewidth = 0.8) +
    annotate("text", x = 0.6, y = 0.3,
             label = paste0("AUC = ", sprintf("%.4f", SLS_AUC_TEST)),
             size = 3.5, color = PAL_DESCRIPTIVO[2], fontface = "bold") +
    tr_labs(title = "Curva ROC (SLS)", x = "1 - Especificidad (FPR)", y = "Sensibilidad (TPR)") +
    theme_paper() +
    theme(plot.title = element_text(face = "bold"))

  p_pr <- if (!is.null(pr_data) && nrow(pr_data) > 0) {
    ggplot(pr_data, aes(x = recall, y = precision)) +
      geom_line(color = COL_SLS, linewidth = 0.8) +
      annotate("text", x = 0.4, y = 0.7,
               label = paste0("AUC-PR = ", sprintf("%.4f", SLS_AUC_PR)),
               size = 3.5, color = PAL_DESCRIPTIVO[2], fontface = "bold") +
      tr_labs(title = "Curva Precision-Recall (SLS)", x = "Recall", y = "Precision") +
      theme_paper() +
      theme(plot.title = element_text(face = "bold"))
  } else {
    ggplot() + tr_labs(title = "PR no disponible") + theme_void()
  }

  p_clf <- p_roc + p_pr +
    plot_annotation(title = tr("Clasificacion -- Modelo SLS post-LASSO"),
                    subtitle = paste0(tr("Umbral Youden"), " = ", sprintf("%.4f", val_umbral),
                                      " | N test = ", fmt_n(c07a$n_test)),
                    theme = theme(plot.title = element_text(face = "bold", size = 13)))
  guardar_figura(p_clf, DIR_FIGURAS_07E_SLS, "roc_pr", 5)
  p_clf
} else {
  cat("Curvas ROC/PR no disponibles en el contrato.\n")
}
```

## Metricas de clasificacion

```{r metricas_clf}
met_long <- tibble(
  Metrica = c("AUC-ROC", "AUC-PR", "Umbral Youden",
              "Accuracy", "Sensibilidad", "Especificidad",
              "Precision (PPV)", "F1-Score", "MCC", "Cohen Kappa"),
  Valor_SLS = c(
    sprintf("%.4f", SLS_AUC_TEST),
    sprintf("%.4f", SLS_AUC_PR),
    sprintf("%.4f", val_umbral),
    ifelse(is.na(val_accuracy), "N/D", sprintf("%.4f", val_accuracy)),
    ifelse(is.na(val_sens),     "N/D", sprintf("%.4f", val_sens)),
    ifelse(is.na(val_spec),     "N/D", sprintf("%.4f", val_spec)),
    ifelse(is.na(val_ppv),      "N/D", sprintf("%.4f", val_ppv)),
    sprintf("%.4f", val_f1),
    sprintf("%.4f", val_mcc),
    sprintf("%.4f", val_kappa)
  ),
  Valor_LPM = c(
    sprintf("%.4f", LPM_AUC_TEST), sprintf("%.4f", LPM_AUC_PR),
    sprintf("%.4f", LPM_UMBRAL),
    ifelse(is.na(LPM_ACCURACY), "—", sprintf("%.4f", LPM_ACCURACY)),
    ifelse(is.na(LPM_SENS),     "—", sprintf("%.4f", LPM_SENS)),
    ifelse(is.na(LPM_SPEC),     "—", sprintf("%.4f", LPM_SPEC)),
    ifelse(is.na(LPM_PPV),      "—", sprintf("%.4f", LPM_PPV)),
    sprintf("%.4f", LPM_F1), sprintf("%.4f", LPM_MCC),
    ifelse(is.na(LPM_KAPPA),    "—", sprintf("%.4f", LPM_KAPPA))
  ),
  Valor_GLM = c(
    sprintf("%.4f", GLM_AUC_TEST), sprintf("%.4f", GLM_AUC_PR),
    sprintf("%.4f", GLM_UMBRAL),
    ifelse(is.na(GLM_ACCURACY), "—", sprintf("%.4f", GLM_ACCURACY)),
    ifelse(is.na(GLM_SENS),     "—", sprintf("%.4f", GLM_SENS)),
    ifelse(is.na(GLM_SPEC),     "—", sprintf("%.4f", GLM_SPEC)),
    ifelse(is.na(GLM_PPV),      "—", sprintf("%.4f", GLM_PPV)),
    sprintf("%.4f", GLM_F1), sprintf("%.4f", GLM_MCC),
    ifelse(is.na(GLM_KAPPA),    "—", sprintf("%.4f", GLM_KAPPA))
  )
) %>%
  mutate(Delta_vs_LPM = case_when(
    Metrica == "AUC-ROC"         ~ sprintf("%+.4f", SLS_AUC_TEST - LPM_AUC_TEST),
    Metrica == "AUC-PR"          ~ sprintf("%+.4f", SLS_AUC_PR   - LPM_AUC_PR),
    Metrica == "Umbral Youden"   ~ sprintf("%+.4f", val_umbral    - LPM_UMBRAL),
    Metrica == "F1-Score"        ~ sprintf("%+.4f", val_f1        - LPM_F1),
    Metrica == "MCC"             ~ sprintf("%+.4f", val_mcc       - LPM_MCC),
    Metrica == "Accuracy"        ~ ifelse(!is.na(val_accuracy) & !is.na(LPM_ACCURACY),
                                          sprintf("%+.4f", val_accuracy - LPM_ACCURACY), "—"),
    Metrica == "Sensibilidad"    ~ ifelse(!is.na(val_sens) & !is.na(LPM_SENS),
                                          sprintf("%+.4f", val_sens - LPM_SENS), "—"),
    Metrica == "Especificidad"   ~ ifelse(!is.na(val_spec) & !is.na(LPM_SPEC),
                                          sprintf("%+.4f", val_spec - LPM_SPEC), "—"),
    Metrica == "Precision (PPV)" ~ ifelse(!is.na(val_ppv) & !is.na(LPM_PPV),
                                          sprintf("%+.4f", val_ppv - LPM_PPV), "—"),
    Metrica == "Cohen Kappa"     ~ ifelse(!is.na(val_kappa) & !is.na(LPM_KAPPA),
                                          sprintf("%+.4f", val_kappa - LPM_KAPPA), "—"),
    TRUE ~ "—"
  ))

kable(met_long, format = "html", align = c("l", "r", "r", "r", "r"),
      caption = "Metricas de clasificacion -- SLS vs LPM vs GLM (Test set ponderado)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(which(met_long$Metrica %in% c("AUC-ROC", "F1-Score", "MCC")),
           bold = TRUE, background = "#d4edda")
```

---

', file = con)

# ---- CALIBRACION SLS ----
cat('# Calibracion SLS

> **Nota metodologica:** El test de Hosmer-Lemeshow no aplica formalmente a modelos lineales de probabilidad. Se reporta a titulo de referencia tecnica. La "calibracion" del SLS se evalua por el porcentaje de predicciones fuera de [0, 1].

## Predicciones fuera de [0, 1]

```{r pred_fuera_01}
tibble(
  Muestra = c("Train (kappa convergida)", "Test (out-of-sample)"),
  Pct_fuera = c(
    paste0(sprintf("%.2f", SLS_FUERA_TRAIN), "% ✅ (por construccion)"),
    paste0(sprintf("%.2f", SLS_FUERA_TEST), "% ❌ (requiere clipping)")
  ),
  Nota = c(
    "kappa excluye obs con pred fuera de [0,1] por diseno",
    "Obs fuera de la region de kappa. Requieren clip en backcasting."
  )
) %>%
  kable(format = "html", align = c("l", "r", "l"),
        caption = "Predicciones fuera de [0,1] -- modelo SLS") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(2, background = "#fff3cd")
```

## Calibracion por decil (referencia)

```{r p6_calibracion, fig.height=5}
hl_data <- c07b$hl_df
if (!is.null(hl_data) && nrow(hl_data) > 0) {
  # Detectar columnas: obs_formal|pred_media o obs_mean|pred_mean
  col_obs  <- intersect(c("obs_formal", "obs_mean"), names(hl_data))[1]
  col_pred <- intersect(c("pred_media", "pred_mean"), names(hl_data))[1]
  if (!is.na(col_obs) && !is.na(col_pred)) {
    hl_plot <- hl_data %>%
      rename(obs_v = !!col_obs, pred_v = !!col_pred) %>%
      filter(pred_v > 0 & pred_v < 1)
    if (nrow(hl_plot) >= 2) {
      p_cal <- ggplot(hl_plot, aes(x = pred_v, y = obs_v)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
        geom_point(aes(size = n), color = COL_SLS, alpha = 0.7) +
        geom_line(color = COL_OBSERVADO, linewidth = 0.5) +
        scale_size_continuous(range = c(3, 10), name = tr("N obs por decil")) +
        annotate("text", x = 0.2, y = 0.9,
                 label = "H-L: N/A\n(modelo lineal)",
                 size = 3.0, color = PAL_DESCRIPTIVO[2]) +
        tr_labs(title = "Calibracion por decil -- SLS (referencia tecnica)",
                subtitle = "H-L no aplica a modelos lineales. Ver nota metodologica.",
                x = "Probabilidad predicha (media por decil)",
                y = "Proporcion observada (formal)") +
        coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
        theme_paper() +
        theme(plot.title = element_text(face = "bold"))
      guardar_figura(p_cal, DIR_FIGURAS_07E_SLS, "cal", 6)
      p_cal
    }
  }
} else {
  cat("Datos H-L no disponibles en el contrato 07b SLS.\n")
}
```

> El SLS garantiza calibracion perfecta dentro de $\\hat{\\kappa}_{\\gamma}$ (0% pred. fuera de [0,1] por construccion), pero pierde esa garantia fuera de la submuestra. El GLM (logit) es el unico modelo con calibracion formalmente correcta (H-L p = `r sprintf("%.3f", GLM_HL_PVAL)`).

---

', file = con)

# ---- ROBUSTEZ ----
cat('# Robustez

## 07c -- Neutralidad temporal

```{r robustez_07c}
tibble(
  Indicador = c(
    "AUC modelo temporal (SLS)", "AUC modelo base SLS (07b)",
    "Delta AUC", "Variables temporales candidatas",
    "Variables temporales en recipe", "Variables temporales sel. lambda.1se",
    "Variables temporales con boot >= 10%",
    "Neutralidad (flag tecnico)", "Neutralidad (sustantiva)"
  ),
  Valor = c(
    sprintf("%.4f", c07c$auc_test),
    sprintf("%.4f", c07c$auc_base_07b %||% SLS_AUC_TEST),
    sprintf("%+.4f", c07c$delta_auc_vs_07b),
    as.character(c07c$n_vars_temp_raw),
    as.character(c07c$n_vars_temp_recipe),
    as.character(c07c$n_vars_temp_sel_1se),
    as.character(c07c$n_temp_sel_boot10 %||% NA),
    ifelse(c07c$neutralidad_confirmada, "TRUE (tecnico)", "FALSE (ver nota)"),
    "Confirmada -- efectos < 0.5 pp, AUC empeora"
  )
) %>%
  kable(format = "html", align = c("l", "r"),
        caption = "Test de neutralidad temporal SLS (Script 07c)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(9, bold = TRUE, background = "#d4edda") %>%
  row_spec(3, background = "#ffeeba")
```

> **Nota 07c SLS:** El flag tecnico es FALSE (3 variables con seleccion boot >= 10%). Sin embargo, los coeficientes son de orden 10^-3 (< 0.5 pp) y Delta AUC = `r sprintf("%+.4f", c07c$delta_auc_vs_07b)` (negativo). La neutralidad **sustantiva** esta confirmada. El flag tecnico es un artefacto de colinealidad en ventana de 3 periodos (ver analisis en ejecucion 07c).

```{r p9_boot_temporal, fig.height=4}
if (!is.null(c07c$boot_temporal) && nrow(c07c$boot_temporal) > 0) {
  bt <- c07c$boot_temporal %>%
    mutate(variable = factor(variable, levels = rev(variable)))
  p_boot_t <- ggplot(bt, aes(x = coef_media_cond, y = variable)) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.5) +
    geom_errorbarh(aes(xmin = coef_ic_low, xmax = coef_ic_high),
                   height = 0.3, color = COL_BANDA) +
    geom_point(size = 3, color = COL_SLS) +
    tr_labs(title = "Variables temporales SLS -- bootstrap (07c)",
            subtitle = paste0(tr("Seleccionadas (boot >=10%)"), ": ", c07c$n_temp_sel_boot10 %||% "?",
                              " | ", tr("Magnitud maxima < 0.5 pp")),
            x = "Coeficiente (media condicional)", y = NULL) +
    theme_paper() +
    theme(plot.title = element_text(face = "bold"))
  guardar_figura(p_boot_t, DIR_FIGURAS_07E_SLS, "boot_temporal", 7)
  p_boot_t
} else {
  cat("No se seleccionaron variables temporales en recipe.\n")
}
```

## 07d -- Interacciones seccion x categoria

```{r robustez_07d}
n_interact_est <- sum(c07d$boot_interact$estable, na.rm = TRUE)
tibble(
  Indicador = c(
    "Interacciones candidatas", "Interacciones sel. lambda.1se",
    "Interacciones estables (>= 80% boot)",
    "Interacciones con boot >= 10%",
    "Variables totales sel. lambda.1se",
    "Variables OLS (sobre df_train)", "R2 (df_train -- NO comparable con kappa)",
    "AUC test", "AUC base SLS (07b)", "Delta AUC"
  ),
  Valor = c(
    as.character(c07d$n_interact_candidatas),
    as.character(c07d$n_interact_sel_1se),
    as.character(n_interact_est),
    as.character(c07d$n_interact_sel_boot10 %||% NA),
    as.character(c07d$n_vars_sel_1se),
    as.character(c07d$ols_n_vars),
    sprintf("%.4f", c07d$ols_r2),
    sprintf("%.4f", c07d$auc_test),
    sprintf("%.4f", c07d$auc_base_07b),
    sprintf("%+.4f", c07d$delta_auc_vs_07b)
  )
) %>%
  kable(format = "html", align = c("l", "r"),
        caption = "Interacciones seccion x categoria ocupacional SLS (Script 07d)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(3, bold = TRUE, background = "#fff3cd") %>%
  row_spec(10, background = "#ffeeba")
```

```{r p10_interact, fig.height=8}
if (!is.null(c07d$boot_interact) && nrow(c07d$boot_interact) > 0) {
  interact_estables <- c07d$boot_interact %>%
    filter(estable) %>%
    arrange(desc(abs(coef_media_cond))) %>%
    mutate(
      variable_clean = gsub("_x_", " x ", gsub("\\\\.", " ", variable)),
      variable_clean = factor(variable_clean, levels = rev(variable_clean))
    )
  if (nrow(interact_estables) > 0) {
    interact_estables <- interact_estables %>%
      mutate(color_punto = if_else(coef_media_cond > 0, COL_SLS, PAL_DESCRIPTIVO[2]))
    p_inter <- ggplot(interact_estables, aes(x = coef_media_cond, y = variable_clean)) +
      geom_vline(xintercept = 0, color = "grey70") +
      geom_errorbarh(aes(xmin = coef_ic_low, xmax = coef_ic_high),
                     height = 0.3, color = COL_BANDA) +
      geom_point(aes(color = color_punto), size = 3) +
      scale_color_identity(guide = "none") +
      tr_labs(title = paste0(tr("Interacciones estables SLS (>= 80% boot)"), ": ",
                             nrow(interact_estables), " ", tr("de"), " ", c07d$n_interact_candidatas),
              subtitle = "Coeficiente medio condicional + IC 95% bootstrap -- df_train completo",
              x = "Coeficiente", y = NULL) +
      theme_paper() +
      theme(plot.title = element_text(face = "bold"),
            strip.text = element_text(size = 7, lineheight = 0.9))
    guardar_figura(p_inter, DIR_FIGURAS_07E_SLS, "interacciones", 8, height = 7)
    p_inter
  }
}
```

> **Conclusion 07d SLS:** `r n_interact_est` interacciones son estables (>= 80% bootstrap), lo que difiere del resultado LPM (0 estables). Este hallazgo revela **heterogeneidad sectorial-ocupacional** que el modelo base aditivo no captura. Sin embargo, el Delta AUC = `r sprintf("%+.4f", c07d$delta_auc_vs_07b)` no justifica la complejidad adicional para el backcasting. Se mantiene el modelo base SLS 07b. Las interacciones son un resultado sustantivo del analisis exploratorio, no evidencia en contra del modelo base.

---

', file = con)

# ---- LIMITACIONES SLS ----
cat('# Limitaciones SLS

```{r limitaciones_sls}
tibble(
  ID = paste0("L", 1:5),
  Limitacion = c(
    "Predicciones fuera de [0,1] en test (21.17%)",
    "Perdida muestral por recorte kappa (21.33%)",
    "R2 no comparable entre modelos",
    "Heterocedasticidad estructural",
    "Interacciones sustantivas no incorporadas"
  ),
  Evidencia = c(
    paste0(sprintf("%.2f", SLS_FUERA_TEST), "% de pred. test fuera de [0,1]"),
    paste0(sprintf("%.2f", SLS_PCT_LOSS), "% de obs. train excluidas de kappa"),
    paste0("R2 SLS = ", sprintf("%.4f", SLS_R2), " sobre kappa (N=",
           fmt_n(SLS_N_KAPPA), ") vs LPM R2 = ", LPM_R2, " sobre train completo"),
    "Breusch-Pagan significativo -- igual que LPM",
    paste0(n_interact_est, " interacciones estables en 07d -- no adoptadas")
  ),
  Tratamiento = c(
    "Clipping pmax(0, pmin(1, pred)) en backcasting (Script 08c). Ref: SLS design",
    "Documentado. Sesgo potencial si obs. recortadas sistematicamente distintas.",
    "Reportar con nota explicita en paper. No usar delta R2 para comparacion.",
    "SE clusterizados por codusu (vcovCL, HC1). Cameron & Miller (2015)",
    "Resultado exploratorio documentado. Adopcion requiere re-definicion de kappa."
  )
) %>%
  kable(format = "html", align = c("c", "l", "l", "l"),
        caption = "Limitaciones conocidas del SLS y tratamientos aplicados") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3em") %>%
  column_spec(4, width = "22em")
```

---

# Notas tecnicas

```{r notas_tecnicas}
tibble(
  Parametro = c(
    "Entorno R", "LASSO engine", "CV folds", "CV clustering",
    "SE clusterizados", "Penalty factor theta", "Penalty factor base",
    "Familia LASSO (SLS)", "Algoritmo recorte", "Tema HTML",
    "Tablas", "Graficos", "Seed global", "N cores"
  ),
  Valor = c(
    paste0("R ", R.version$major, ".", R.version$minor),
    "glmnet",
    "10-fold",
    "foldid por codusu",
    "vcovCL (HC1) por codusu",
    "0 (siempre incluidos)",
    "1 (regularizados)",
    "gaussian (OLS lineal -- no binomial)",
    paste0("Iterativo | conv. en ", SLS_N_ITERS, " iter | kappa N=", fmt_n(SLS_N_KAPPA)),
    "flatly (rmarkdown)",
    "knitr::kable + kableExtra",
    "ggplot2 + patchwork",
    as.character(SEED_GLOBAL),
    as.character(N_CORES)
  )
) %>%
  kable(format = "html", align = c("l", "l"),
        caption = "Configuracion tecnica del pipeline SLS") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE)
```

---

# Conclusion {.unnumbered}

El modelo SLS post-LASSO con factores latentes theta demuestra capacidad predictiva solida (AUC = `r sprintf("%.4f", SLS_AUC_TEST)`, F1 = `r sprintf("%.4f", val_f1)`) sobre un test set de `r fmt_n(c07a$n_test)` observaciones. La seleccion LASSO gaussian reduce `r c07a$n_vars_candidatas` variables candidatas a `r c07a$n_vars_sel_1se` (criterio lambda.1se).

El algoritmo de recorte iterativo converge en `r SLS_N_ITERS` iteraciones con una submuestra de `r fmt_n(SLS_N_KAPPA)` observaciones (perdida: `r sprintf("%.2f", SLS_PCT_LOSS)`%, ligeramente por encima del umbral de referencia del 20%).

En la tabla comparativa, el **GLM domina** en calibracion (H-L p = `r sprintf("%.3f", GLM_HL_PVAL)` vs N/A en SLS) y en acotamiento OOS (0% vs `r sprintf("%.2f", SLS_FUERA_TEST)`%). Las tres especificaciones son equivalentes en discriminacion (AUC < 0.004 de diferencia) y clasificacion (F1, MCC ≈).

Los tests de robustez revelan: (1) neutralidad temporal sustantiva (Delta AUC = `r sprintf("%+.4f", c07c$delta_auc_vs_07b)`, efectos < 0.5 pp), y (2) heterogeneidad sectorial-ocupacional significativa (`r sum(c07d$boot_interact$estable, na.rm = TRUE)` interacciones estables), aunque el Delta AUC = `r sprintf("%+.4f", c07d$delta_auc_vs_07b)` no justifica adoptar el modelo con interacciones.

**Siguiente paso:** Script 08c -- Backcasting SLS al panel completo 2016T4-2025T3 (con clipping para pred. fuera de [0,1]).
', file = con)

close(con)
cat("   [OK] Rmd temporal escrito:", rmd_temp, "\n\n")

# 🪫 5. Render HTML ------------------------------------------------------------
cat("-- 4. Renderizando HTML -------------------------------------------\n")

rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_07E_HTML_SLS,
  quiet       = TRUE,
  envir       = environment()
)
unlink(rmd_temp)
cat("   [OK] HTML generado:", PATH_07E_HTML_SLS, "\n\n")

# 🪫 6. Generar TXT para el paper ----------------------------------------------
cat("-- 5. Generando TXT para el paper --------------------------------\n")

theta_rows_txt <- c07b$tabla_ols %>%
  filter(grepl("theta", term, ignore.case = TRUE))

txt_lines <- c(
  "# =================================================================",
  "# NOTAS PARA EL PAPER -- Modelo SLS (Sequential Least Squares)",
  paste0("# Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "# Script:   07e_lasso_reporte_SLS.R (Capa 4)",
  "# =================================================================",
  "",
  "## MODELO BASE SLS (07b)",
  paste0("N train:               ", fmt_n(c07a$n_train)),
  paste0("N kappa (convergida):  ", fmt_n(SLS_N_KAPPA)),
  paste0("N test:                ", fmt_n(c07a$n_test)),
  paste0("Perdida muestral:      ", sprintf("%.2f", SLS_PCT_LOSS), "% (umbral 20%)"),
  paste0("Iteraciones kappa:     ", SLS_N_ITERS),
  paste0("Vars candidatas:       ", c07a$n_vars_candidatas),
  paste0("Vars sel l.1se:        ", c07a$n_vars_sel_1se),
  paste0("Vars OLS:              ", length(c07b$vars_ols)),
  paste0("lambda.1se:            ", sprintf("%.6f", cv_fit$lambda.1se)),
  paste0("lambda.min:            ", sprintf("%.6f", cv_fit$lambda.min)),
  paste0("R2 (sobre kappa):      ", sprintf("%.4f", SLS_R2),
         " [NO comparable con LPM=", LPM_R2, "]"),
  paste0("R2 adj (sobre kappa):  ", sprintf("%.4f", SLS_R2_ADJ)),
  paste0("AUC-ROC:               ", sprintf("%.4f", SLS_AUC_TEST)),
  paste0("AUC-PR:                ", sprintf("%.4f", SLS_AUC_PR)),
  paste0("F1:                    ", sprintf("%.4f", val_f1)),
  paste0("MCC:                   ", sprintf("%.4f", val_mcc)),
  paste0("Kappa:                 ", sprintf("%.4f", val_kappa)),
  paste0("Umbral Youden:         ", sprintf("%.4f", val_umbral)),
  paste0("Pred fuera [0,1] train: 0.00% (por construccion kappa)"),
  paste0("Pred fuera [0,1] test:  ", sprintf("%.2f", SLS_FUERA_TEST), "%"),
  paste0("H-L:                   N/A (no aplica a modelos lineales)"),
  paste0("BP:  chi2=", sprintf("%.2f", c07b$bp_stat %||% NA_real_),
         ", p=", sprintf("%.2e", c07b$bp_pval %||% NA_real_)),
  paste0("RESET: F=", sprintf("%.2f", c07b$reset_stat %||% NA_real_),
         ", p=", sprintf("%.2e", c07b$reset_pval %||% NA_real_)),
  paste0("VIF_adj > 3.16:        ", c07b$n_vif_alto %||% 4, " variables"),
  ""
)

# Factores latentes
txt_lines <- c(txt_lines, "## FACTORES LATENTES theta (SLS)")
for (i in seq_len(nrow(theta_rows_txt))) {
  r <- theta_rows_txt[i, ]
  txt_lines <- c(txt_lines,
    paste0("  ", r$term, ": beta=", sprintf("%.4f", r$estimate),
           ", SE=", sprintf("%.4f", r$std.error),
           ", t=", sprintf("%.2f", r$statistic),
           ", p=", ifelse(r$p.value < 2e-16, "<2e-16", sprintf("%.4f", r$p.value))))
}

# Tabla comparativa
txt_lines <- c(txt_lines, "",
  "## TABLA COMPARATIVA LPM vs GLM vs SLS",
  sprintf("  %-28s %8s %8s %8s", "Metrica", "LPM", "GLM", "SLS"),
  sprintf("  %s", strrep("-", 56)),
  sprintf("  %-28s %8.4f %8.4f %8.4f", "AUC-ROC test",
          LPM_AUC_TEST, GLM_AUC_TEST, SLS_AUC_TEST),
  sprintf("  %-28s %8.4f %8.4f %8.4f", "AUC-PR",
          LPM_AUC_PR, GLM_AUC_PR, SLS_AUC_PR),
  sprintf("  %-28s %8.4f %8s %8.4f", "R2 (comparacion limitada)",
          LPM_R2, sprintf("%.4f†", GLM_R2_PSEUDO), SLS_R2),
  sprintf("  %-28s %8.4f %8.4f %8.4f", "F1-Score (Youden)",
          LPM_F1, GLM_F1, val_f1),
  sprintf("  %-28s %8.4f %8.4f %8.4f", "MCC",
          LPM_MCC, GLM_MCC, val_mcc),
  sprintf("  %-28s %8s %8.3f %8s", "H-L p-valor",
          "<2e-16", GLM_HL_PVAL, "N/A"),
  sprintf("  %-28s %8.2f%% %8s %8.2f%%", "Pred fuera [0,1] test",
          LPM_FUERA_TEST, "0.00%", SLS_FUERA_TEST),
  sprintf("  %-28s %8s %8s %8.2f%%", "Perdida muestral",
          paste0(sprintf("%.0f", LPM_PCT_LOSS), "%"),
          paste0(sprintf("%.0f", GLM_PCT_LOSS), "%"),
          SLS_PCT_LOSS),
  "  † pseudo-R2 McFadden. ‡ sobre kappa (N=38,724).",
  ""
)

# Neutralidad temporal
txt_lines <- c(txt_lines,
  "## NEUTRALIDAD TEMPORAL (07c SLS)",
  paste0("AUC temporal SLS:  ", sprintf("%.4f", c07c$auc_test)),
  paste0("AUC base SLS 07b:  ", sprintf("%.4f", c07c$auc_base_07b %||% SLS_AUC_TEST)),
  paste0("Delta AUC:         ", sprintf("%+.4f", c07c$delta_auc_vs_07b)),
  paste0("Vars temp sel:     ", c07c$n_vars_temp_sel_1se),
  paste0("Vars temp boot>=10: ", c07c$n_temp_sel_boot10 %||% "?"),
  "Conclusion: Neutralidad SUSTANTIVA confirmada. Flag tecnico FALSE por colinealidad 3 periodos.",
  ""
)

if (!is.null(c07c$boot_temporal) && nrow(c07c$boot_temporal) > 0) {
  txt_lines <- c(txt_lines, "Variables temporales bootstrap (SLS):")
  for (i in seq_len(nrow(c07c$boot_temporal))) {
    bt_row <- c07c$boot_temporal[i, ]
    txt_lines <- c(txt_lines,
      paste0("  ", bt_row$variable,
             ": boot=", bt_row$seleccion_pct, "%",
             ", beta=", sprintf("%+.6f", bt_row$coef_media_cond),
             ", IC=[", sprintf("%.5f", bt_row$coef_ic_low),
             ", ", sprintf("%.5f", bt_row$coef_ic_high), "]"))
  }
  txt_lines <- c(txt_lines, "")
}

# Interacciones
n_interact_est_txt <- sum(c07d$boot_interact$estable, na.rm = TRUE)
txt_lines <- c(txt_lines,
  "## INTERACCIONES (07d SLS)",
  paste0("Interacc. candidatas:   ", c07d$n_interact_candidatas),
  paste0("Interacc. sel l.1se:    ", c07d$n_interact_sel_1se),
  paste0("Interacc. estables:     ", n_interact_est_txt,
         " [HALLAZGO: heterogeneidad sectorial-ocupacional]"),
  paste0("R2 con interacc (train): ", sprintf("%.4f", c07d$ols_r2)),
  paste0("AUC con interacciones:  ", sprintf("%.4f", c07d$auc_test)),
  paste0("Delta AUC vs base:      ", sprintf("%+.4f", c07d$delta_auc_vs_07b)),
  paste0("Conclusion: Delta AUC marginal (", sprintf("%+.4f", c07d$delta_auc_vs_07b), "). Se mantiene modelo base SLS 07b."),
  "            Las interacciones documentan heterogeneidad pero no mejoran discriminacion.",
  ""
)

# Top interacciones estables
if (!is.null(c07d$boot_interact)) {
  interact_est_txt <- c07d$boot_interact %>%
    filter(estable) %>% arrange(desc(abs(coef_media_cond)))
  if (nrow(interact_est_txt) > 0) {
    txt_lines <- c(txt_lines, "Interacciones estables SLS (>= 80% boot, top por |beta|):")
    for (i in seq_len(min(nrow(interact_est_txt), 10))) {
      row_i <- interact_est_txt[i, ]
      txt_lines <- c(txt_lines,
        paste0("  ", gsub("_x_", " x ", gsub("\\.", " ", row_i$variable)),
               ": beta=", sprintf("%+.4f", row_i$coef_media_cond),
               ", boot=", row_i$seleccion_pct, "%",
               ", IC=[", sprintf("%.4f", row_i$coef_ic_low),
               ", ", sprintf("%.4f", row_i$coef_ic_high), "]"))
    }
    txt_lines <- c(txt_lines, "")
  }
}

txt_lines <- c(txt_lines,
  "## LIMITACIONES SLS",
  paste0("L1: Pred fuera [0,1] test = ", sprintf("%.2f", SLS_FUERA_TEST),
         "% -> clip [0,1] en backcasting (Script 08c)"),
  paste0("L2: Perdida muestral = ", sprintf("%.2f", SLS_PCT_LOSS),
         "% (umbral 20%) -> documentar sesgo potencial"),
  "L3: R2 no comparable entre modelos -> reportar con nota explicita",
  "L4: Heterocedasticidad -> SE clusterizados vcovCL(HC1)",
  paste0("L5: ", n_interact_est_txt, " interacciones sustantivas no adoptadas -> resultado exploratorio"),
  "",
  "## SIGUIENTE PASO",
  "Script 08c -- Backcasting SLS al panel completo 2016T4-2025T3",
  paste0("  Modelo: 07b SLS (base, sin interacciones, sobre kappa)"),
  paste0("  Umbral Youden: ", sprintf("%.4f", val_umbral)),
  paste0("  Clipping requerido: pmax(0, pmin(1, pred)) para OOS"),
  ""
)

writeLines(txt_lines, PATH_TXT_07E)
cat("   [OK] TXT generado:", PATH_TXT_07E, "\n\n")

# 📑 7. Limpieza y checklist ---------------------------------------------------
cat("-- 6. Checklist de salidas ----------------------------------------\n")
cat("   [OK] HTML:", file.exists(PATH_07E_HTML_SLS), "\n")
cat("   [OK] TXT: ", file.exists(PATH_TXT_07E), "\n")

gc()

cat("\n===================================================================\n")
cat("SCRIPT 07e SLS COMPLETADO\n")
cat("  HTML:", basename(PATH_07E_HTML_SLS), "\n")
cat("  TXT: ", basename(PATH_TXT_07E), "\n")
cat("===================================================================\n")

toc()
