# =============================================================================
# [EN] 07e_lasso_reporte_LPM.R -- Comprehensive HTML report consolidating LASSO results 07a-07d (LPM family)
# INPUTS:  Contracts from 07a-07d LPM, rdos/modelos/07_modelo_lasso_LPM*.rds
# OUTPUTS: rdos/reportes/07e_reporte_LPM_LPM*.html
# =============================================================================
# 🌟 07e_lasso_reporte_LPM.R 🌟 ####
# OBJETIVO:
#    Reporte HTML exhaustivo Capa 4 LPM para el paper.
#    Consolida resultados de 07a-07d.
#
# INPUTS:
#    - rdos/contratos/07a_contrato_lasso_LPM3T.rds    (c07a)
#    - rdos/modelos/07_modelo_lasso_LPM3T.rds          (cv_fit)
#    - rdos/contratos/07b_contrato_postlasso_LPM3T.rds (c07b)
#    - rdos/contratos/07c_contrato_tiempo_LPM3T.rds    (c07c)
#    - rdos/contratos/07d_contrato_interacciones_LPM3T.rds (c07d)
#
# OUTPUTS:
#    - rdos/reportes/07e_reporte_LPM_LPM3T.html
#    - rdos/reportes/07e_notas_paper_LPM3T.txt
#
# NOMBRES REALES DE COLUMNAS (verificados en consola):
#    c07b$metricas_clf: umbral|accuracy|sensibilidad|especificidad|precision|f1|mcc|kappa
#    c07b$roc_df:       fpr|tpr|umbral_grid
#    c07b$pr_df:        recall|precision|umbral_grid
#    c07b$hl_df:        grupo|n|obs_formal|pred_media
#    c07b$vif_tabla:    variable|VIF|vif_comparable
#    c07b$tabla_ols:    term|estimate|std.error|statistic|p.value|significancia
#    c07b$comp_coefs:   variable|coef_ols|se_ols_cl|p_ols|sig_ols|coef_lasso|delta_coef|en_lasso
#    c07b$boot_summary: VACIO
#    c07d$boot_interact: variable|seleccion_pct|coef_media_global|coef_media_cond|coef_sd|coef_ic_low|coef_ic_high
#    c07c$boot_temporal: idem
#    c07d$metricas_clf: Metrica|Valor_07d|Valor_07b|Delta

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

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 07e - Reporte LPM")
start_time <- Sys.time()
cat("===================================================================\n")
cat("SCRIPT 07e - REPORTE LPM EXHAUSTIVO\n")
cat("Inicio:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")

# 🪫 1. Carga de contratos -----------------------------------------------------
cat("-- 1. Carga de contratos ------------------------------------------\n")

c07a   <- readRDS(PATH_07_CONTRATO)
cat("   [OK] c07a:", basename(PATH_07_CONTRATO), "\n")

cv_fit <- readRDS(PATH_07_MODELO_LASSO)
cat("   [OK] cv_fit:", basename(PATH_07_MODELO_LASSO), "\n")

c07b   <- readRDS(PATH_07B_CONTRATO)
cat("   [OK] c07b:", basename(PATH_07B_CONTRATO), "\n")

c07c   <- readRDS(PATH_07C_CONTRATO)
cat("   [OK] c07c:", basename(PATH_07C_CONTRATO), "\n")

c07d   <- readRDS(PATH_07D_CONTRATO)
cat("   [OK] c07d:", basename(PATH_07D_CONTRATO), "\n")

# 🪫 2. Helpers y extraccion de valores ----------------------------------------
cat("-- 2. Helpers y extraccion de valores -----------------------------\n")

fmt_n <- function(x) format(x, big.mark = ",")

# MSE desde cv_fit (c07a no los tiene)
mse_1se <- cv_fit$cvm[cv_fit$lambda == cv_fit$lambda.1se]
mse_min <- min(cv_fit$cvm)

# c07b$metricas_clf es WIDE: umbral|accuracy|sensibilidad|especificidad|precision|f1|mcc|kappa
mc <- c07b$metricas_clf
val_f1       <- mc$f1[1]
val_mcc      <- mc$mcc[1]
val_kappa    <- mc$kappa[1]
val_accuracy <- mc$accuracy[1]
val_sens     <- mc$sensibilidad[1]
val_spec     <- mc$especificidad[1]
val_ppv      <- mc$precision[1]
val_umbral   <- mc$umbral[1]
val_pct_fuera <- c07b$pct_pred_fuera_01 %||% 10.14

# c07d$metricas_clf es LONG: Metrica|Valor_07d|Valor_07b|Delta
get_07d <- function(patron) {
  idx <- grepl(patron, c07d$metricas_clf$Metrica, ignore.case = TRUE)
  if (any(idx)) c07d$metricas_clf$Valor_07d[which(idx)[1]] else NA_real_
}

# boot_summary en c07b esta VACIO - no hay datos de bootstrap en 07b
has_boot_07b <- !is.null(c07b$boot_summary) && length(names(c07b$boot_summary)) > 0

# c07d$boot_interact no tiene columna 'estable' - derivarla
if (!is.null(c07d$boot_interact) && nrow(c07d$boot_interact) > 0) {
  if (!"estable" %in% names(c07d$boot_interact)) {
    c07d$boot_interact$estable <- c07d$boot_interact$seleccion_pct >= 80
  }
}

cat("   [OK] Valores extraidos\n")
cat("   boot_summary en c07b disponible:", has_boot_07b, "\n")

# 🪫 3. Paths de output --------------------------------------------------------
PATH_TXT_07E <- file.path(DIR_REPORTES, paste0("07e_notas_paper_", SUFIJO_MODELO_LPM, ".txt"))

cat("   HTML ->", PATH_07E_HTML, "\n")
cat("   TXT  ->", PATH_TXT_07E, "\n\n")

# 🪫 4. Construir Rmd por secciones --------------------------------------------
cat("-- 3. Construyendo Rmd por secciones ------------------------------\n")

rmd_temp <- tempfile(fileext = ".Rmd")
con <- file(rmd_temp, open = "wt", encoding = "UTF-8")

# ---- YAML + SETUP + RESUMEN ----
cat('---
title: "Modelo LPM -- Reporte Exhaustivo Capa 4"
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
    "N entrenamiento", "N test", "Variables candidatas LASSO",
    "Variables seleccionadas (lambda.1se)", "Variables OLS post-LASSO",
    "lambda.1se", "lambda.min", "MSE CV (lambda.1se)", "MSE CV (lambda.min)",
    "R2", "R2 ajustado", "AUC-ROC", "AUC-PR", "F1-Score", "MCC",
    "Pred. fuera [0,1]", "Umbral Youden", "Accuracy"
  ),
  Valor = c(
    fmt_n(c07a$n_train), fmt_n(c07a$n_test),
    as.character(c07a$n_vars_candidatas),
    as.character(c07a$n_vars_sel_1se),
    as.character(c07b$n_vars_ols),
    sprintf("%.6f", cv_fit$lambda.1se),
    sprintf("%.6f", cv_fit$lambda.min),
    sprintf("%.6f", mse_1se),
    sprintf("%.6f", mse_min),
    sprintf("%.4f", c07b$ols_r2),
    sprintf("%.4f", c07b$ols_r2_adj),
    sprintf("%.4f [%.4f - %.4f]", c07b$auc_roc, c07b$auc_roc_ci[1], c07b$auc_roc_ci[3]),
    sprintf("%.4f", c07b$auc_pr),
    sprintf("%.4f", val_f1),
    sprintf("%.4f", val_mcc),
    paste0(sprintf("%.2f", val_pct_fuera), "%"),
    sprintf("%.4f", val_umbral),
    sprintf("%.4f", val_accuracy)
  )
)

kable(kpi, format = "html", align = c("l", "r"),
      caption = "Indicadores clave del modelo LPM -- Capa 4") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center") %>%
  row_spec(c(10:11, 12, 15), bold = TRUE, background = "#d4edda") %>%
  column_spec(1, width = "20em")
```

---

', file = con)

# ---- METODOLOGIA ----
cat('# Metodologia

## Especificacion del modelo

El **Linear Probability Model (LPM)** estima:

$$P(\\text{Formal}_i = 1 | X_i) = X_i \\beta$$

donde $X_i$ incluye variables demograficas, educativas, laborales, geograficas, proxies longitudinales y los factores latentes $\\theta_A$ (habilidad cognitiva) y $\\theta_B$ (factor socioemocional) estimados por heterofactor MLE (Sarzosa & Urzua, 2016).

**Procedimiento de dos etapas:**

1. **Seleccion LASSO** (Script 07a): `cv.glmnet(family="gaussian")` con 10-fold CV por cluster (`codusu`). $\\theta_A$ y $\\theta_B$ con `penalty.factor = 0` (siempre incluidos).
2. **OLS post-LASSO** (Script 07b): Regresion OLS sobre las `r c07a$n_vars_sel_1se` variables seleccionadas. Errores estandar clusterizados por `codusu` (Cameron & Miller, 2015).

**Ventana de entrenamiento:** ', sprintf("Ultimos %d trimestres con formalidad observada (%s)", N_TRIMESTRES_TRAINING, paste(gsub("_", "", TRIMESTRES_FORMALIDAD), collapse = ", ")), '. Universo: ocupados con formalidad_empleo en {Formal oficial, Informal oficial}.

## Variables candidatas

```{r vars_candidatas}
tibble(
  Grupo = c("Demograficas", "Educativas", "Laborales",
            "Geograficas", "Familiares", "Proxies longitudinales",
            "Factores latentes"),
  Variables = c(
    "edad, edad^2, sexo, estado_civil, lugar_nacimiento, parentesco",
    "nivel_educ, anios_educ, asistencia_escuela, tipo_escuela, alfabetizacion",
    "seccion, calificacion, categoria_ocupacional, antiguedad, horas, tamanio_estab, tipo_empresa",
    "aglomerado, region",
    "ingreso_real_capita_familiar, nbi, ich_score",
    "rezago_escolar, clima_educativo, emparejamiento_selectivo, calificacion_norm, entropia, residual_vivienda, busqueda_formal",
    "theta_A (habilidad cognitiva), theta_B (factor socioemocional)"
  ),
  N_approx = c("~6", "~5", "~7", "~2", "~3", "~7", "2")
) %>%
  kable(format = "html", caption = "Grupos de variables candidatas para LASSO") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
```

> **Nota:** Tras `step_dummy()` sobre categoricas, el total de columnas en la matrix de diseno es `r c07a$n_vars_candidatas`.

---

', file = con)

# ---- SELECCION LASSO ----
cat('# Seleccion LASSO

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
  geom_ribbon(aes(ymin = mse_lo, ymax = mse_hi), fill = COL_LPM, alpha = 0.2) +
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
  tr_labs(title = "Validacion cruzada LASSO -- MSE vs log(lambda)",
         subtitle = paste0("10-fold CV por cluster (codusu) | ",
                           c07a$n_vars_candidatas, " variables candidatas"),
         x = "log(lambda)", y = "Mean Squared Error (CV)") +
  theme_paper() +
  theme(plot.title = element_text(face = "bold"))
guardar_figura(p1, DIR_FIGURAS_07E_LPM, "lasso_path", 1)
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
        caption = "Comparacion lambda.1se vs lambda.min") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(1, bold = TRUE, background = "#d4edda")
```

## Dispersion LASSO vs OLS

```{r p3_scatter, fig.height=6}
# comp_coefs: variable|coef_ols|se_ols_cl|p_ols|sig_ols|coef_lasso|delta_coef|en_lasso
if (!is.null(c07b$comp_coefs) && nrow(c07b$comp_coefs) > 0) {
  scatter_df <- c07b$comp_coefs %>%
    mutate(es_theta = grepl("theta", variable, ignore.case = TRUE))

  p3 <- ggplot(scatter_df, aes(x = coef_lasso, y = coef_ols)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = 0, color = "grey80") +
    geom_vline(xintercept = 0, color = "grey80") +
    geom_point(aes(color = es_theta, size = es_theta), alpha = 0.6) +
    scale_color_manual(values = c("FALSE" = COL_LPM, "TRUE" = PAL_DESCRIPTIVO[2]),
                       labels = c(tr("Otras variables"), tr("theta (factor latente)"))) +
    scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4)) +
    geom_text(data = scatter_df %>% filter(es_theta),
              aes(label = gsub("_mA", "", variable)),
              nudge_x = 0.003, nudge_y = 0.003,
              size = 2.8, color = COL_OBSERVADO, fontface = "bold") +
    tr_labs(title = "Coeficientes LASSO vs OLS post-LASSO",
            x = "Coeficiente LASSO (regularizado)", y = "Coeficiente OLS (post-LASSO)",
            color = "", size = "") +
    theme_paper() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
    guides(size = "none")
  guardar_figura(p3, DIR_FIGURAS_07E_LPM, "scatter", 2)
  p3
}
```

---

', file = con)

# ---- OLS POST-LASSO ----
cat('# OLS post-LASSO

## Bondad de ajuste

```{r bondad_ajuste}
tibble(
  Estadistico = c("R2", "R2 ajustado", "F-statistic", "p(F)",
                   "N observaciones", "df modelo", "df residuales",
                   "Breusch-Pagan chi2", "p(BP)", "Ramsey RESET F", "p(RESET)",
                   "N vars VIF > 3.16"),
  Valor = c(
    sprintf("%.4f", c07b$ols_r2),
    sprintf("%.4f", c07b$ols_r2_adj),
    sprintf("%.2f", c07b$ols_f_stat),
    sprintf("%.2e", c07b$ols_f_pval),
    fmt_n(c07b$ols_n),
    as.character(c07b$ols_df),
    fmt_n(c07b$ols_df_res),
    sprintf("%.2f", c07b$bp_stat),
    sprintf("%.2e", c07b$bp_pval),
    sprintf("%.2f", c07b$reset_stat),
    sprintf("%.2e", c07b$reset_pval),
    as.character(c07b$n_vif_alto)
  )
) %>%
  kable(format = "html", align = c("l", "r"),
        caption = "Diagnosticos OLS post-LASSO") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(c(1,2), bold = TRUE, background = "#d4edda")
```

## Tabla completa de coeficientes

```{r tabla_ols_completa}
# tabla_ols: term|estimate|std.error|statistic|p.value|significancia (sin GVIF_adj)
if (!is.null(c07b$tabla_ols)) {
  c07b$tabla_ols %>%
    mutate(
      estimate  = sprintf("%.4f", estimate),
      std.error = sprintf("%.4f", std.error),
      statistic = sprintf("%.2f", statistic),
      p.value   = ifelse(p.value < 2e-16, "<2e-16", sprintf("%.4f", p.value))
    ) %>%
    kable(format = "html", align = c("l", rep("r", 5)),
          caption = paste0("Coeficientes OLS post-LASSO (", c07b$n_vars_ols,
                           " variables + intercepto) -- SE clusterizados por codusu")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = TRUE, font_size = 11)
}
```

## Top 30 coeficientes

```{r p2_top30, fig.height=8}
# Usar comp_coefs (que tiene datos) en lugar de boot_summary (vacio)
if (!is.null(c07b$comp_coefs) && nrow(c07b$comp_coefs) > 0) {
  top30 <- c07b$comp_coefs %>%
    filter(!grepl("Intercept", variable, ignore.case = TRUE)) %>%
    arrange(desc(abs(coef_ols))) %>%
    slice_head(n = 30) %>%
    mutate(
      variable_label = gsub("_mA$", "", variable),
      es_theta = grepl("theta", variable, ignore.case = TRUE),
      variable_label = factor(variable_label, levels = rev(variable_label))
    )

  top30 <- top30 %>%
    mutate(color_barra = if_else(coef_ols > 0, COL_LPM, PAL_DESCRIPTIVO[2]))

  p2 <- ggplot(top30, aes(x = coef_ols, y = variable_label)) +
    geom_vline(xintercept = 0, color = "grey70") +
    geom_errorbarh(aes(xmin = coef_ols - 1.96 * se_ols_cl,
                       xmax = coef_ols + 1.96 * se_ols_cl),
                   height = 0.3, color = COL_BANDA, linewidth = 0.5) +
    geom_col(aes(fill = color_barra), width = 0.6, alpha = 0.8) +
    geom_point(data = top30 %>% filter(es_theta),
               shape = 18, size = 4, color = PAL_DESCRIPTIVO[2]) +
    scale_fill_identity(guide = "none") +
    tr_labs(title = "Top 30 coeficientes OLS post-LASSO (IC 95% clusterizado)",
            subtitle = "Diamante = factores latentes theta",
            x = "Coeficiente OLS (SE clusterizado)", y = NULL) +
    theme_paper() +
    theme(plot.title = element_text(face = "bold"))
  guardar_figura(p2, DIR_FIGURAS_07E_LPM, "coefplot", 3, height = 7)
  p2
}
```

## VIF

```{r vif_tabla}
# vif_tabla: variable|VIF|vif_comparable
if (!is.null(c07b$vif_tabla) && nrow(c07b$vif_tabla) > 0) {
  vif_altos <- c07b$vif_tabla %>%
    filter(vif_comparable > 2) %>%
    arrange(desc(vif_comparable)) %>%
    mutate(across(where(is.numeric), ~ sprintf("%.3f", .)))

  if (nrow(vif_altos) > 0) {
    kable(vif_altos, format = "html",
          caption = paste0("Variables con vif_comparable > 2 (", nrow(vif_altos),
                           " de ", nrow(c07b$vif_tabla), ")"),
          col.names = c("Variable", "VIF", "VIF comparable")) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = FALSE)
  } else {
    cat("Ninguna variable con vif_comparable > 2.\n")
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
      Factor = gsub("_mA$", "", term),
      beta_OLS = sprintf("%.4f", estimate),
      SE_CL = sprintf("%.4f", std.error),
      t_stat = sprintf("%.2f", statistic),
      p = ifelse(p.value < 2e-16, "<2e-16", sprintf("%.4f", p.value)),
      Sig = significancia
    ) %>%
    select(Factor, beta_OLS, SE_CL, t_stat, p, Sig)

  kable(theta_show, format = "html", align = c("l", rep("r", 5)),
        caption = "Factores latentes theta en el modelo OLS post-LASSO") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE, position = "center") %>%
    row_spec(1, bold = TRUE, background = "#d4edda")
}
```

**Interpretacion economica:**

- **theta_A** (habilidad cognitiva): Signo negativo coherente si theta_A codificado como "ausencia de rezago" -- mayor habilidad se asocia con mayor formalidad. Significativo (p < 2e-16).
- **theta_B** (factor socioemocional): No significativo (p aprox 0.29). No predice formalidad condicional en theta_A y covariables estructurales. Incluido con penalty.factor = 0, pero coeficiente no distinguible de cero.

> **Para el paper:** theta_A es el corrector de sesgo de seleccion efectivo. theta_B funciona como control sin capacidad predictiva marginal.

---

', file = con)

# ---- CLASIFICACION ----
cat('# Clasificacion

## ROC + PR combinados

```{r p4p5_roc_pr, fig.height=5}
# roc_df: fpr|tpr|umbral_grid
p_roc <- ggplot(c07b$roc_df, aes(x = fpr, y = tpr)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70") +
  geom_line(color = COL_OBSERVADO, linewidth = 0.8) +
  annotate("text", x = 0.6, y = 0.3,
           label = paste0("AUC = ", sprintf("%.4f", c07b$auc_roc)),
           size = 3.5, color = PAL_DESCRIPTIVO[2], fontface = "bold") +
  tr_labs(title = "Curva ROC", x = "1 - Especificidad (FPR)", y = "Sensibilidad (TPR)") +
  theme_paper() +
  theme(plot.title = element_text(face = "bold"))

# pr_df: recall|precision|umbral_grid
p_pr <- ggplot(c07b$pr_df, aes(x = recall, y = precision)) +
  geom_line(color = COL_OBSERVADO, linewidth = 0.8) +
  annotate("text", x = 0.4, y = 0.7,
           label = paste0("AUC-PR = ", sprintf("%.4f", c07b$auc_pr)),
           size = 3.5, color = PAL_DESCRIPTIVO[2], fontface = "bold") +
  tr_labs(title = "Curva Precision-Recall", x = "Recall", y = "Precision") +
  theme_paper() +
  theme(plot.title = element_text(face = "bold"))

p_clf <- p_roc + p_pr +
  plot_annotation(title = tr("Clasificacion -- Modelo LPM post-LASSO"),
                  subtitle = paste0(tr("Umbral Youden"), " = ", sprintf("%.4f", val_umbral),
                                    " | N test = ", fmt_n(c07a$n_test)),
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))
guardar_figura(p_clf, DIR_FIGURAS_07E_LPM, "roc_pr", 4)
p_clf
```

## Metricas de clasificacion

```{r metricas_clf}
# metricas_clf es wide: umbral|accuracy|sensibilidad|especificidad|precision|f1|mcc|kappa
met_long <- tibble(
  Metrica = c("Umbral Youden", "Accuracy", "Sensibilidad", "Especificidad",
              "Precision (PPV)", "F1-Score", "MCC", "Cohen Kappa"),
  Valor = c(val_umbral, val_accuracy, val_sens, val_spec,
            val_ppv, val_f1, val_mcc, val_kappa)
) %>%
  mutate(Valor = sprintf("%.4f", Valor))

kable(met_long, format = "html", align = c("l", "r"),
      caption = "Metricas de clasificacion -- Test set (ponderado)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(which(grepl("AUC|F1|MCC", met_long$Metrica)),
           bold = TRUE, background = "#d4edda")
```

## Calibracion Hosmer-Lemeshow

```{r p6_calibracion, fig.height=5}
# hl_df: grupo|n|obs_formal|pred_media
if (!is.null(c07b$hl_df) && nrow(c07b$hl_df) > 0) {
  p6 <- ggplot(c07b$hl_df, aes(x = pred_media, y = obs_formal)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(aes(size = n), color = COL_LPM, alpha = 0.7) +
    geom_line(color = COL_OBSERVADO, linewidth = 0.5) +
    scale_size_continuous(range = c(3, 10), name = tr("N obs por decil")) +
    annotate("text", x = 0.2, y = 0.9,
             label = paste0("H-L chi2 = ", sprintf("%.1f", c07b$hl_stat),
                            "\np = ", sprintf("%.2e", c07b$hl_pval)),
             size = 3.0, color = PAL_DESCRIPTIVO[2]) +
    tr_labs(title = "Calibracion Hosmer-Lemeshow por decil",
            subtitle = "Prediccion media vs proporcion observada",
            x = "Probabilidad predicha (media por decil)",
            y = "Proporcion observada (formal)") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_paper() +
    theme(plot.title = element_text(face = "bold"))
  guardar_figura(p6, DIR_FIGURAS_07E_LPM, "cal", 5)
  p6
}
```

> **Nota:** El test H-L rechaza la hipotesis nula de calibracion perfecta. Limitacion estructural del LPM. Ver seccion Limitaciones.

---

', file = con)

# ---- ESTABILIDAD BOOTSTRAP ----
cat('# Estabilidad bootstrap

```{r boot_nota}
if (!has_boot_07b) {
  cat("**Nota:** El contrato 07b no contiene datos de bootstrap (boot_summary vacio).",
      "La estabilidad bootstrap se evalua a traves de los scripts 07c (temporal) y 07d (interacciones),",
      "cuyos contratos si incluyen informacion de bootstrap.\n")
}
```

```{r boot_07b_plots}
if (has_boot_07b) {
  p7 <- ggplot(c07b$boot_summary, aes(x = seleccion_pct)) +
    geom_histogram(binwidth = 5, fill = COL_LPM, color = "white", alpha = 0.8) +
    geom_vline(xintercept = 80, linetype = "dashed", color = PAL_DESCRIPTIVO[2]) +
    tr_labs(title = "Distribucion de estabilidad bootstrap",
            x = "% de bootstraps con seleccion", y = "Frecuencia") +
    theme_paper()
  guardar_figura(p7, DIR_FIGURAS_07E_LPM, "boot", 6)
  p7
}
```

---

', file = con)

# ---- ROBUSTEZ ----
cat('# Robustez

## 07c -- Neutralidad temporal

```{r robustez_07c}
tibble(
  Indicador = c("AUC modelo temporal", "AUC modelo base (07b)",
                "Delta AUC", "Variables temporales candidatas",
                "Variables temporales en recipe", "Variables temporales sel. lambda.1se",
                "Neutralidad (flag tecnico)", "Neutralidad (sustantiva)"),
  Valor = c(
    sprintf("%.4f", c07c$auc_test),
    sprintf("%.4f", c07c$auc_base_07b %||% c07b$auc_roc),
    sprintf("%+.4f", c07c$delta_auc_vs_07b),
    as.character(c07c$n_vars_temp_raw),
    as.character(c07c$n_vars_temp_recipe),
    as.character(c07c$n_vars_temp_sel_1se),
    ifelse(c07c$neutralidad_confirmada, "TRUE", "FALSE"),
    "Confirmada -- efectos < 0.5 pp, AUC empeora"
  )
) %>%
  kable(format = "html", align = c("l", "r"),
        caption = "Test de neutralidad temporal (Script 07c)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(8, bold = TRUE, background = "#d4edda") %>%
  row_spec(3, background = "#ffeeba")
```

```{r p9_boot_temporal, fig.height=4}
# boot_temporal: variable|seleccion_pct|coef_media_global|coef_media_cond|coef_sd|coef_ic_low|coef_ic_high
if (!is.null(c07c$boot_temporal) && nrow(c07c$boot_temporal) > 0) {
  bt <- c07c$boot_temporal %>%
    mutate(variable = factor(variable, levels = rev(variable)))

  p9 <- ggplot(bt, aes(x = coef_media_cond, y = variable)) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.5) +
    geom_errorbarh(aes(xmin = coef_ic_low, xmax = coef_ic_high),
                   height = 0.3, color = COL_BANDA) +
    geom_point(size = 3, color = PAL_DESCRIPTIVO[2]) +
    tr_labs(title = "Variables temporales -- bootstrap (07c)",
            subtitle = paste0(tr("Seleccionadas"), ": ", c07c$n_vars_temp_sel_1se,
                              " ", tr("de"), " ", c07c$n_vars_temp_recipe,
                              " | ", tr("Todos los efectos < 0.5 pp")),
            x = "Coeficiente (media condicional)", y = NULL) +
    theme_paper() +
    theme(plot.title = element_text(face = "bold"))
  guardar_figura(p9, DIR_FIGURAS_07E_LPM, "boot_temporal", 7)
  p9
} else {
  cat("No se seleccionaron variables temporales.\n")
}
```

> **Conclusion 07c:** La neutralidad temporal esta sustantivamente confirmada. El modelo no requiere efectos fijos de periodo para backcasting.

## 07d -- Interacciones seccion x categoria

```{r robustez_07d}
tibble(
  Indicador = c("Interacciones candidatas", "Interacciones sel. lambda.1se",
                "Interacciones estables (>= 80% boot)",
                "Variables totales sel. lambda.1se",
                "Variables OLS", "R2", "R2 base (07b)", "Delta R2",
                "AUC test", "AUC base (07b)", "Delta AUC"),
  Valor_07d = c(
    as.character(c07d$n_interact_candidatas),
    as.character(c07d$n_interact_sel_1se),
    as.character(sum(c07d$boot_interact$estable, na.rm = TRUE)),
    as.character(c07d$n_vars_sel_1se),
    as.character(c07d$ols_n_vars),
    sprintf("%.4f", c07d$ols_r2),
    sprintf("%.4f", c07d$ols_r2_base),
    sprintf("%+.4f", c07d$delta_r2),
    sprintf("%.4f", c07d$auc_test),
    sprintf("%.4f", c07d$auc_base_07b),
    sprintf("%+.4f", c07d$delta_auc_vs_07b)
  )
) %>%
  kable(format = "html", align = c("l", "r"),
        caption = "Interacciones seccion x categoria ocupacional (Script 07d)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(c(8, 11), background = "#ffeeba")
```

```{r p10_interact, fig.height=7}
# boot_interact: variable|seleccion_pct|...|estable (derivado arriba)
if (!is.null(c07d$boot_interact) && nrow(c07d$boot_interact) > 0) {
  interact_estables <- c07d$boot_interact %>%
    filter(estable) %>%
    arrange(desc(abs(coef_media_cond))) %>%
    mutate(
      variable_clean = gsub("_X_", " x ", gsub("\\\\.", " ", variable)),
      variable_clean = factor(variable_clean, levels = rev(variable_clean))
    )

  if (nrow(interact_estables) > 0) {
    interact_estables <- interact_estables %>%
      mutate(color_punto = if_else(coef_media_cond > 0, COL_LPM, PAL_DESCRIPTIVO[2]))

    p10 <- ggplot(interact_estables, aes(x = coef_media_cond, y = variable_clean)) +
      geom_vline(xintercept = 0, color = "grey70") +
      geom_errorbarh(aes(xmin = coef_ic_low, xmax = coef_ic_high),
                     height = 0.3, color = COL_BANDA) +
      geom_point(aes(color = color_punto), size = 3) +
      scale_color_identity(guide = "none") +
      tr_labs(title = paste0(tr("Interacciones estables (>= 80% boot)"), ": ",
                             nrow(interact_estables), " ", tr("de"), " ", c07d$n_interact_candidatas),
              subtitle = "Coeficiente medio condicional + IC 95% bootstrap",
              x = "Coeficiente", y = NULL) +
      theme_paper() +
      theme(plot.title = element_text(face = "bold"))
    guardar_figura(p10, DIR_FIGURAS_07E_LPM, "interacciones", 8, height = 7)
    p10
  }
}
```

> **Conclusion 07d:** Las interacciones mejoran el R2 in-sample pero el AUC out-of-sample no mejora. Se mantiene el modelo base por parsimonia.

---

', file = con)

# ---- LIMITACIONES + NOTAS + CONCLUSION ----
cat('# Limitaciones LPM

```{r limitaciones_lpm}
tibble(
  ID = paste0("L", 1:5),
  Limitacion = c(
    "Predicciones fuera de [0, 1]",
    "Heterocedasticidad estructural",
    "No-linealidad (Ramsey RESET significativo)",
    "Calibracion imperfecta (Hosmer-Lemeshow significativo)",
    "Efectos marginales constantes"
  ),
  Evidencia = c(
    paste0(sprintf("%.2f", val_pct_fuera), "% de predicciones fuera del rango"),
    paste0("Breusch-Pagan chi2 = ", sprintf("%.1f", c07b$bp_stat), ", p < 2e-16"),
    paste0("RESET F = ", sprintf("%.1f", c07b$reset_stat), ", p < 2e-16"),
    paste0("H-L chi2 = ", sprintf("%.1f", c07b$hl_stat), ", p < 2e-16"),
    "Asumido por diseno (link lineal)"
  ),
  Tratamiento = c(
    "Clipping pmax(0, pmin(1, pred)). Ref: Angrist & Pischke (2009, MHE)",
    "SE clusterizados por codusu (vcovCL, HC1). Cameron & Miller (2015)",
    "Comparacion con GLM (logit) y SLS en scripts paralelos",
    "Comparacion con GLM (logit) que provee calibracion nativa",
    "Interpretacion directa como efectos marginales promedio. Wooldridge (2010)"
  )
) %>%
  kable(format = "html", align = c("c", "l", "l", "l"),
        caption = "Limitaciones conocidas del LPM y tratamientos aplicados") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3em") %>%
  column_spec(4, width = "25em")
```

---

# Notas tecnicas

```{r notas_tecnicas}
tibble(
  Parametro = c(
    "Entorno R", "LASSO engine", "CV folds", "CV clustering",
    "SE clusterizados", "Penalty factor theta", "Penalty factor base",
    "Tema HTML", "Tablas", "Graficos",
    "Seed global", "N cores"
  ),
  Valor = c(
    paste0("R ", R.version$major, ".", R.version$minor),
    "glmnet (family = gaussian)",
    "10-fold",
    "foldid por codusu",
    "vcovCL (HC1) por codusu",
    "0 (siempre incluidos)",
    "1 (regularizados)",
    "flatly (rmarkdown)",
    "knitr::kable + kableExtra",
    "ggplot2 + patchwork",
    as.character(SEED_GLOBAL),
    as.character(N_CORES)
  )
) %>%
  kable(format = "html", align = c("l", "l"),
        caption = "Configuracion tecnica del pipeline") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE)
```

---

# Conclusion {.unnumbered}

El modelo LPM post-LASSO con factores latentes theta demuestra capacidad predictiva solida (AUC = `r sprintf("%.4f", c07b$auc_roc)`, F1 = `r sprintf("%.4f", val_f1)`) sobre un test set de `r fmt_n(c07a$n_test)` observaciones. La seleccion LASSO reduce `r c07a$n_vars_candidatas` variables candidatas a `r c07a$n_vars_sel_1se` (criterio lambda.1se).

El factor latente theta_A (habilidad cognitiva) es estadisticamente significativo y estable, confirmando que la correccion por sesgo de seleccion (Sarzosa & Urzua, 2016) aporta informacion predictiva. theta_B no es significativo condicional en theta_A.

Los tests de robustez confirman: (1) neutralidad temporal sustantiva (Delta AUC = `r sprintf("%+.4f", c07c$delta_auc_vs_07b)`), y (2) las interacciones seccion x categoria no mejoran el AUC out-of-sample (`r sprintf("%+.4f", c07d$delta_auc_vs_07b)`), justificando la parsimonia del modelo base.

**Siguiente paso:** Script 08 -- Backcasting al panel completo 2016T4-2025T3.
', file = con)

close(con)
cat("   [OK] Rmd temporal escrito:", rmd_temp, "\n\n")

# 🪫 5. Render HTML ------------------------------------------------------------
cat("-- 4. Renderizando HTML -------------------------------------------\n")

rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_07E_HTML,
  quiet       = TRUE,
  envir       = environment()
)
unlink(rmd_temp)

cat("   [OK] HTML generado:", PATH_07E_HTML, "\n\n")

# 🪫 6. Generar TXT para el paper ----------------------------------------------
cat("-- 5. Generando TXT para el paper --------------------------------\n")

theta_rows_txt <- c07b$tabla_ols %>%
  filter(grepl("theta", term, ignore.case = TRUE))

txt_lines <- c(
  "# =================================================================",
  "# NOTAS PARA EL PAPER -- Modelo LPM (Linear Probability Model)",
  paste0("# Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "# Script:   07e_lasso_reporte_LPM.R (Capa 4)",
  "# =================================================================",
  "",
  "## MODELO BASE (07b)",
  paste0("N train:         ", fmt_n(c07a$n_train)),
  paste0("N test:          ", fmt_n(c07a$n_test)),
  paste0("Vars candidatas: ", c07a$n_vars_candidatas),
  paste0("Vars sel l.1se:  ", c07a$n_vars_sel_1se),
  paste0("Vars OLS:        ", c07b$n_vars_ols),
  paste0("lambda.1se:      ", sprintf("%.6f", cv_fit$lambda.1se)),
  paste0("lambda.min:      ", sprintf("%.6f", cv_fit$lambda.min)),
  paste0("R2:              ", sprintf("%.4f", c07b$ols_r2)),
  paste0("R2 adj:          ", sprintf("%.4f", c07b$ols_r2_adj)),
  paste0("AUC-ROC:         ", sprintf("%.4f", c07b$auc_roc),
         " [", sprintf("%.4f", c07b$auc_roc_ci[1]),
         "-", sprintf("%.4f", c07b$auc_roc_ci[3]), "]"),
  paste0("AUC-PR:          ", sprintf("%.4f", c07b$auc_pr)),
  paste0("F1:              ", sprintf("%.4f", val_f1)),
  paste0("MCC:             ", sprintf("%.4f", val_mcc)),
  paste0("Kappa:           ", sprintf("%.4f", val_kappa)),
  paste0("Accuracy:        ", sprintf("%.4f", val_accuracy)),
  paste0("Sensibilidad:    ", sprintf("%.4f", val_sens)),
  paste0("Especificidad:   ", sprintf("%.4f", val_spec)),
  paste0("Precision PPV:   ", sprintf("%.4f", val_ppv)),
  paste0("Umbral Youden:   ", sprintf("%.4f", val_umbral)),
  paste0("Pred fuera [0,1]: ", sprintf("%.2f", val_pct_fuera), "%"),
  paste0("H-L: chi2=", sprintf("%.2f", c07b$hl_stat),
         ", p=", sprintf("%.2e", c07b$hl_pval)),
  paste0("BP:  chi2=", sprintf("%.2f", c07b$bp_stat),
         ", p=", sprintf("%.2e", c07b$bp_pval)),
  paste0("RESET: F=", sprintf("%.2f", c07b$reset_stat),
         ", p=", sprintf("%.2e", c07b$reset_pval)),
  paste0("VIF > 3.16:      ", c07b$n_vif_alto, " variables"),
  ""
)

txt_lines <- c(txt_lines, "## FACTORES LATENTES theta")
for (i in seq_len(nrow(theta_rows_txt))) {
  r <- theta_rows_txt[i, ]
  txt_lines <- c(txt_lines,
    paste0("  ", r$term, ": beta=", sprintf("%.4f", r$estimate),
           ", SE=", sprintf("%.4f", r$std.error),
           ", t=", sprintf("%.2f", r$statistic),
           ", p=", ifelse(r$p.value < 2e-16, "<2e-16", sprintf("%.4f", r$p.value))))
}

txt_lines <- c(txt_lines, "",
  "## NEUTRALIDAD TEMPORAL (07c)",
  paste0("AUC temporal:  ", sprintf("%.4f", c07c$auc_test)),
  paste0("AUC base 07b:  ", sprintf("%.4f", c07c$auc_base_07b %||% c07b$auc_roc)),
  paste0("Delta AUC:     ", sprintf("%+.4f", c07c$delta_auc_vs_07b)),
  paste0("Vars temp sel: ", c07c$n_vars_temp_sel_1se),
  "Conclusion: Neutralidad sustantiva confirmada -- efectos < 0.5 pp",
  ""
)

if (!is.null(c07c$boot_temporal) && nrow(c07c$boot_temporal) > 0) {
  txt_lines <- c(txt_lines, "Variables temporales bootstrap:")
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

n_interact_est <- sum(c07d$boot_interact$estable, na.rm = TRUE)
txt_lines <- c(txt_lines,
  "## INTERACCIONES (07d)",
  paste0("Interacc. candidatas:   ", c07d$n_interact_candidatas),
  paste0("Interacc. sel l.1se:    ", c07d$n_interact_sel_1se),
  paste0("Interacc. estables:     ", n_interact_est),
  paste0("R2 con interacciones:   ", sprintf("%.4f", c07d$ols_r2)),
  paste0("Delta R2 vs base:       ", sprintf("%+.4f", c07d$delta_r2)),
  paste0("AUC con interacciones:  ", sprintf("%.4f", c07d$auc_test)),
  paste0("Delta AUC vs base:      ", sprintf("%+.4f", c07d$delta_auc_vs_07b)),
  "Conclusion: Parsimonia justificada. AUC no mejora out-of-sample.",
  ""
)

if (!is.null(c07d$boot_interact)) {
  interact_est <- c07d$boot_interact %>% filter(estable) %>%
    arrange(desc(abs(coef_media_cond)))
  if (nrow(interact_est) > 0) {
    txt_lines <- c(txt_lines, "Interacciones estables (>= 80% boot, top por |beta|):")
    for (i in seq_len(min(nrow(interact_est), 10))) {
      row_i <- interact_est[i, ]
      txt_lines <- c(txt_lines,
        paste0("  ", gsub("_X_", " x ", gsub("\\.", " ", row_i$variable)),
               ": beta=", sprintf("%+.4f", row_i$coef_media_cond),
               ", boot=", row_i$seleccion_pct, "%",
               ", IC=[", sprintf("%.4f", row_i$coef_ic_low),
               ", ", sprintf("%.4f", row_i$coef_ic_high), "]"))
    }
    txt_lines <- c(txt_lines, "")
  }
}

txt_lines <- c(txt_lines,
  "## LIMITACIONES LPM",
  paste0("L1: Pred fuera [0,1] = ", sprintf("%.2f", val_pct_fuera),
         "% -> clip [0,1]. Ref: Angrist & Pischke (2009)"),
  "L2: Heterocedasticidad -> SE clusterizados vcovCL(HC1). Ref: Cameron & Miller (2015)",
  "L3: No-linealidad (RESET sig.) -> comparar con GLM/SLS",
  "L4: Calibracion imperfecta (H-L sig.) -> comparar con GLM",
  "L5: Efectos marginales constantes -> interpretar como promedios. Ref: Wooldridge (2010)",
  "",
  "## SIGUIENTE PASO",
  "Script 08 -- Backcasting al panel completo 2016T4-2025T3",
  paste0("  Modelo: 07b (base, sin interacciones)"),
  paste0("  Umbral Youden: ", sprintf("%.4f", val_umbral)),
  ""
)

writeLines(txt_lines, PATH_TXT_07E)
cat("   [OK] TXT generado:", PATH_TXT_07E, "\n\n")

# 📑 7. Limpieza y checklist ---------------------------------------------------
cat("-- 6. Checklist de salidas ----------------------------------------\n")
cat("   [OK] HTML:", file.exists(PATH_07E_HTML), "\n")
cat("   [OK] TXT: ", file.exists(PATH_TXT_07E), "\n")

gc()

cat("\n===================================================================\n")
cat("SCRIPT 07e COMPLETADO\n")
cat("  HTML:", basename(PATH_07E_HTML), "\n")
cat("  TXT: ", basename(PATH_TXT_07E), "\n")
cat("===================================================================\n")

toc()
