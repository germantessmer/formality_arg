# =============================================================================
# [EN] 10f_loco_quarter_GLM.R -- Leave-One-Quarter-Out cross-validation for GLM: AUC, F1, calibration by fold
# INPUTS:  rdos/datos/04_panel_con_proxies.rds, rdos/datos/06_theta_predichos.rds
# OUTPUTS: rdos/reportes/00_loco_quarter_GLM.html, rdos/reportes/00_loco_quarter_GLM.csv
# =============================================================================
# 🌟 10f_loco_quarter_GLM.R 🌟 ####
# OBJETIVO: Leave-One-Quarter-Out (LOCO) cross-validation for GLM
#   Entrena en N-1 trimestres del overlap, testea en el restante.
#   Reporta AUC-ROC, AUC-PR, F1, calibration delta por fold.
# INPUTS:  rdos/datos/04_panel_con_proxies.rds (panel con proxies + theta)
#          rdos/datos/06_theta_predichos.rds
# OUTPUTS: rdos/reportes/00_loco_quarter_GLM.csv
#          rdos/reportes/00_loco_quarter_GLM.html (reporte diagnostico)
# TIEMPO ESTIMADO: ~5-10 minutos (N folds x LASSO + CV)

# ⌛ Inicio contador de tiempo -------------------------------------------------

t_inicio <- proc.time()

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(glmnet)
  library(pROC)
  library(recipes)
  library(doParallel)
  library(foreach)
})

# 🔧 Cargar configuración y funciones ------------------------------------------

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# Funciones defensivas (no exportadas por funciones_comunes.R)
safe_load <- function(path) {
  if (!file.exists(path)) stop("Archivo no encontrado: ", path)
  readRDS(path)
}
safe_get <- function(obj, ...) {
  keys <- c(...)
  result <- obj
  for (k in keys) {
    if (is.null(result) || !k %in% names(result)) return(NA_real_)
    result <- result[[k]]
  }
  result
}

cat("=" , rep("=", 69), "\n", sep = "")
cat("  LOCO Quarter Validation — GLM (binomial + LASSO)\n")
cat("=" , rep("=", 69), "\n", sep = "")

# 🪫 1. Cargar panel (08 ya tiene proxies + theta + predicciones) ------------

path_panel_08 <- file.path(DIR_DATOS, paste0("08_panel_formalidad_", SUFIJO_MODELO_GLM, ".rds"))
panel <- safe_load(path_panel_08)

cat("Panel cargado:", nrow(panel), "obs x", ncol(panel), "cols\n")

# 🪫 2. Definir variables del modelo (replica 07a_lasso_GLM.R) ---------------

# Renombrar theta si es necesario (panel 08 usa theta_A, recipe usa theta_A_mA)
if ("theta_A" %in% names(panel) && !"theta_A_mA" %in% names(panel)) {
  panel <- panel %>% rename(theta_A_mA = theta_A, theta_B_mA = theta_B)
  cat("Renombrado theta_A -> theta_A_mA\n")
}

VARS_MODELO <- c(
  "formalidad_empleo", "pondera", "codusu", "periodo_id",
  "theta_A_mA", "theta_B_mA",
  "edad", "edad_cuadrado", "sexo", "estado_civil",
  "lugar_nacimiento", "parentesco",
  "nivel_educ_obtenido2", "asistencia_escuela", "tipo_escuela", "alfabetizacion",
  "aglomerado", "mas_500",
  "seccion", "calificacion", "antiguedad", "categoria_ocupacional",
  "nbi", "miembros_hogar", "menores10", "mayores10",
  "principal_tareas_hogar", "otros_tareas_hogar",
  "ich_score", "residual_vivienda",
  "rezago_escolar_cohorte", "clima_educativo_hogar",
  "emparejamiento_selectivo", "calificacion_norm", "entropia_estabilidad",
  "busqueda_formal",
  "vive_alquiler", "vive_ganancias_negocio", "vive_renta_financiera",
  "vive_beca", "vive_cuota_alimenticia", "vive_ahorros",
  "vive_prestamos_personas", "vive_prestamos_financieros",
  "vive_financiamiento", "vive_venta_bienes", "vive_otro_ingreso"
)
# Agregar horas_trabajadas si existe
if ("horas_trabajadas" %in% names(panel)) VARS_MODELO <- c(VARS_MODELO, "horas_trabajadas")

VARS_MODELO <- intersect(VARS_MODELO, names(panel))
cat("Variables del modelo disponibles:", length(VARS_MODELO), "\n")

# 🪫 3. Filtrar overlap ------------------------------------------------------

train_raw <- panel %>%
  filter(
    condicion_actividad == "Ocupado",
    formalidad_empleo %in% c("Formal oficial", "Informal oficial"),
    periodo_id %in% TRIMESTRES_FORMALIDAD
  ) %>%
  select(all_of(VARS_MODELO)) %>%
  mutate(
    formalidad_bin = as.integer(formalidad_empleo == "Formal oficial"),
    lugar_nacimiento = case_when(
      lugar_nacimiento %in% c("Localidad", "Provincia", "Otra provincia") ~ "Argentina",
      lugar_nacimiento == "Pais limitrofe" ~ "Pais_Limitrofe",
      lugar_nacimiento == "Otro pais"      ~ "Otro_Pais",
      TRUE ~ as.character(lugar_nacimiento)
    )
  )

rm(panel); gc(verbose = FALSE)

cat("Overlap total:", nrow(train_raw), "obs en", length(TRIMESTRES_FORMALIDAD), "trimestres\n")
cat("Distribucion por trimestre:\n")
print(table(train_raw$periodo_id))

theta_cols <- c("theta_A_mA", "theta_B_mA")

# 🪫 4. LOCO loop ------------------------------------------------------------

quarters <- TRIMESTRES_FORMALIDAD
resultados <- list()

# Configurar paralelismo para cv.glmnet interno
cl <- makeCluster(min(N_CORES, 7))
registerDoParallel(cl)

for (q_idx in seq_along(quarters)) {

  q_test  <- quarters[q_idx]
  q_train <- setdiff(quarters, q_test)

  cat("\n", rep("-", 60), "\n")
  cat("  Fold", q_idx, ": test =", q_test, "| train =", paste(q_train, collapse = " + "), "\n")
  cat(rep("-", 60), "\n")

  df_train <- train_raw %>% filter(periodo_id %in% q_train)
  df_test  <- train_raw %>% filter(periodo_id == q_test)

  cat("  n_train =", nrow(df_train), "| n_test =", nrow(df_test), "\n")

  # Preparar recipe (replica 07a_lasso_GLM.R)
  df_rec_train <- df_train %>% select(-pondera, -formalidad_empleo, -codusu, -periodo_id)
  df_rec_test  <- df_test  %>% select(-pondera, -formalidad_empleo, -codusu, -periodo_id)

  rec <- recipe(formalidad_bin ~ ., data = df_rec_train) %>%
    step_mutate(across(all_nominal_predictors(), ~ na_if(as.character(.), "Ns/Nr"))) %>%
    step_mutate(across(all_nominal_predictors(), ~ na_if(as.character(.), "Ns/Nc"))) %>%
    step_novel(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
    step_zv(all_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    prep(training = df_rec_train)

  X_train <- bake(rec, new_data = df_rec_train, composition = "matrix")
  X_test  <- bake(rec, new_data = df_rec_test,  composition = "matrix")
  # Remover columna target si quedó
  X_train <- X_train[, colnames(X_train) != "formalidad_bin", drop = FALSE]
  X_test  <- X_test[, colnames(X_test) != "formalidad_bin", drop = FALSE]

  y_train <- df_train$formalidad_bin
  y_test  <- df_test$formalidad_bin

  # Ponderadores normalizados
  w_train <- df_train$pondera / mean(df_train$pondera)
  w_test  <- df_test$pondera / mean(df_test$pondera)

  # Penalty factors (theta sin regularizar)
  col_names_X <- colnames(X_train)
  pf <- rep(1, ncol(X_train))
  theta_idx <- which(col_names_X %in% theta_cols)
  if (length(theta_idx) > 0) pf[theta_idx] <- 0

  # Folds por hogar
  cluster_ids     <- unique(df_train$codusu)
  n_clusters      <- length(cluster_ids)
  set.seed(SEED_GLOBAL + q_idx)
  fold_asignacion <- tibble(
    codusu = cluster_ids,
    fold   = sample(rep(1:10, length.out = n_clusters))
  )
  foldid_vec <- tibble(codusu = df_train$codusu) %>%
    left_join(fold_asignacion, by = "codusu") %>%
    pull(fold)

  # LASSO + CV
  cv_fit <- cv.glmnet(
    x              = X_train,
    y              = y_train,
    weights        = w_train,
    alpha          = 1,
    family         = "binomial",
    penalty.factor = pf,
    type.measure   = "auc",
    foldid         = foldid_vec,
    parallel       = TRUE
  )

  lambda_1se <- cv_fit$lambda.1se
  n_vars_sel <- sum(as.vector(coef(cv_fit, s = "lambda.1se")) != 0) - 1

  cat("  lambda.1se =", round(lambda_1se, 6), "| vars seleccionadas =", n_vars_sel, "\n")

  # Predicciones en test
  pred_prob <- as.vector(predict(cv_fit, newx = X_test, s = "lambda.1se", type = "response"))
  pred_clip <- pmax(0, pmin(1, pred_prob))

  # AUC-ROC
  roc_obj <- roc(response = y_test, predictor = pred_clip,
                 weights = w_test, levels = c(0, 1), quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))
  auc_ci  <- as.numeric(ci.auc(roc_obj, method = "delong"))

  # AUC-PR
  pr_obj <- PRROC::pr.curve(
    scores.class0 = pred_clip[y_test == 1],
    scores.class1 = pred_clip[y_test == 0],
    curve = FALSE
  )
  auc_pr <- pr_obj$auc.integral

  # Umbral Youden
  coords_best <- coords(roc_obj, x = "best", best.method = "youden",
                        ret = c("threshold", "sensitivity", "specificity"))
  umbral <- coords_best$threshold[1]

  # Clasificacion
  pred_clase <- as.integer(pred_clip >= umbral)
  tp <- sum(pred_clase == 1 & y_test == 1)
  tn <- sum(pred_clase == 0 & y_test == 0)
  fp <- sum(pred_clase == 1 & y_test == 0)
  fn <- sum(pred_clase == 0 & y_test == 1)

  prec_val   <- tp / max(tp + fp, 1)
  recall_val <- tp / max(tp + fn, 1)
  f1_val     <- 2 * prec_val * recall_val / max(prec_val + recall_val, 1e-10)
  mcc_num    <- (tp * tn - fp * fn)
  mcc_den    <- sqrt(as.numeric(tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc_val    <- ifelse(mcc_den > 0, mcc_num / mcc_den, 0)

  # Calibracion: diferencia entre tasa observada y predicha ponderadas
  tasa_obs  <- weighted.mean(y_test, w_test)
  tasa_pred <- weighted.mean(pred_clip, w_test)
  cal_delta <- abs(tasa_obs - tasa_pred)

  # Fuera de [0,1]
  pct_fuera <- mean(pred_prob < 0 | pred_prob > 1) * 100

  cat(sprintf("  AUC-ROC = %.4f [%.4f, %.4f]\n", auc_val, auc_ci[1], auc_ci[3]))
  cat(sprintf("  AUC-PR  = %.4f | F1 = %.4f | MCC = %.4f\n", auc_pr, f1_val, mcc_val))
  cat(sprintf("  Cal delta = %.4f pp | Fuera [0,1] = %.2f%%\n", cal_delta * 100, pct_fuera))

  resultados[[q_idx]] <- tibble(
    fold          = q_idx,
    test_quarter  = q_test,
    train_quarters = paste(q_train, collapse = "+"),
    n_train       = nrow(df_train),
    n_test        = nrow(df_test),
    n_vars_sel    = n_vars_sel,
    auc_roc       = round(auc_val, 4),
    auc_roc_ci_lo = round(auc_ci[1], 4),
    auc_roc_ci_hi = round(auc_ci[3], 4),
    auc_pr        = round(auc_pr, 4),
    f1            = round(f1_val, 4),
    mcc           = round(mcc_val, 4),
    umbral_youden = round(umbral, 4),
    cal_delta_pp  = round(cal_delta * 100, 2),
    pct_fuera_01  = round(pct_fuera, 2)
  )
}

stopCluster(cl)

# 🪫 5. Consolidar resultados ------------------------------------------------

df_loco <- bind_rows(resultados)

cat("\n", rep("=", 60), "\n")
cat("  RESUMEN LOCO — GLM (binomial + LASSO)\n")
cat(rep("=", 60), "\n\n")
print(df_loco %>% select(test_quarter, n_train, n_test, auc_roc, auc_pr, f1, mcc, cal_delta_pp))

# Referencia: metricas del modelo completo para comparacion
c07a_ref <- safe_load(PATH_07_CONTRATO_GLM)
auc_3t   <- safe_get(c07a_ref, "auc_test")

# 🪫 6. Guardar CSV ----------------------------------------------------------

path_csv  <- file.path(DIR_REPORTES, "00_loco_quarter_GLM.csv")
path_html <- file.path(DIR_REPORTES, "00_loco_quarter_GLM.html")

write_csv(df_loco, path_csv)
cat("CSV guardado:", path_csv, "\n")

# 🪫 7. Generar reporte HTML -------------------------------------------------

cat("-- Construyendo reporte HTML ------------------------------------------\n")

rmd_temp <- tempfile(fileext = ".Rmd")
con      <- file(rmd_temp, open = "wt", encoding = "UTF-8")

writeLines('---
title: "LOCO Quarter Validation -- GLM (binomial + LASSO)"
subtitle: "Diagnostico auxiliar -- Review 1, Observacion A5"
date: "Generado: `r format(Sys.time(), \'%d/%m/%Y %H:%M\')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float:
      collapsed: false
    toc_depth: 3
    number_sections: true
    df_print: kable
---', con)

writeLines('
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 9, fig.height = 5, dpi = 150)
library(tidyverse); library(knitr); library(kableExtra)
```
', con)

# ── Seccion 1: Proposito ──
writeLines('
# Proposito y contexto

Este reporte responde a la observacion **A5** del Review 1:

> *"Consider stability of coefficients/marginal effects across 2024Q4, 2025Q1, 2025Q2, 2025Q3
> (train on one quarter, test on others)."*

El ejercicio **Leave-One-Quarter-Out (LOCO)** entrena el modelo GLM (binomial + LASSO)
en 3 de los 4 trimestres del overlap (2024T4, 2025T1, 2025T2, 2025T3) y testea en el cuarto.
Se repite para cada combinacion, resultando en 4 folds.

**Objetivo:** verificar que el poder predictivo del modelo no depende de un trimestre
especifico del overlap, reforzando la evidencia de transportabilidad temporal.

## Donde incorporar estos resultados en el paper

| Resultado | Seccion LaTeX | Uso |
|-----------|---------------|-----|
| Tabla LOCO (metricas por fold) | Appendix G (Predictive Models), nueva subseccion | Tabla complementaria de transportabilidad |
| AUC-ROC medio y estabilidad | Seccion 4.4 (Models performance), parrafo temporal-neutrality | Oracion adicional citando LOCO |
| Calibration delta por fold | Appendix G o H (Backcasting) | Evidencia de estabilidad del umbral |

', con)

# ── Seccion 2: Resultados ──
writeLines('
# Resultados por fold
', con)

writeLines(sprintf('
```{r tabla-loco}
df_loco <- read.csv("%s")

df_loco %%>%%
  select(test_quarter, n_train, n_test, auc_roc, auc_roc_ci_lo, auc_roc_ci_hi,
         auc_pr, f1, mcc, cal_delta_pp) %%>%%
  kable(col.names = c("Test Quarter", "n train", "n test",
                       "AUC-ROC", "CI low", "CI high",
                       "AUC-PR", "F1", "MCC", "Cal. delta (pp)"),
        digits = 4, align = "lccccccccc",
        caption = "Metricas de clasificacion por fold LOCO") %%>%%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %%>%%
  row_spec(0, bold = TRUE)
```
', gsub("\\\\", "/", path_csv)), con)

writeLines(sprintf('
## Comparacion con modelo completo (%dT)

El modelo completo (entrenado en %d trimestres) tiene AUC-ROC de test = **%%.4f**.

```{r comparacion}
cat(sprintf("AUC-ROC medio LOCO: %%.4f (sd = %%.4f)\\n",
            mean(df_loco$auc_roc), sd(df_loco$auc_roc)))
cat(sprintf("AUC-ROC modelo %dT:  %%.4f\\n", %.4f))
cat(sprintf("Diferencia media:   %%.4f\\n",
            %.4f - mean(df_loco$auc_roc)))
```

', N_TRIMESTRES_TRAINING, N_TRIMESTRES_TRAINING, N_TRIMESTRES_TRAINING, auc_3t, auc_3t), con)

# ── Seccion 3: Interpretacion ──
writeLines('
# Interpretacion

## Estabilidad inter-trimestral

Si las metricas son estables entre folds (variacion < 0.02 en AUC, < 2 pp en calibracion),
el resultado indica que:

1. El modelo no depende de un trimestre especifico del overlap
2. La regla de prediccion es transportable entre periodos del redesign
3. La evidencia de neutralidad temporal (ya documentada via 07c) se confirma
   con un test mas exigente (out-of-sample por periodo, no solo por hogar)

## Limitaciones

- Folds limitados por el overlap disponible
- El training con N-1 trimestres reduce la muestra respecto al modelo completo
- Las metricas esperables son ligeramente inferiores al modelo completo por menor n

', con)

writeLines('
```{r diagnostico-final}
rango_auc <- max(df_loco$auc_roc) - min(df_loco$auc_roc)
rango_cal <- max(df_loco$cal_delta_pp) - min(df_loco$cal_delta_pp)

cat("--- DIAGNOSTICO ---\\n")
cat(sprintf("Rango AUC-ROC entre folds: %.4f\\n", rango_auc))
cat(sprintf("Rango Cal delta entre folds: %.2f pp\\n", rango_cal))

if (rango_auc < 0.02 & rango_cal < 2) {
  cat("CONCLUSION: Estabilidad inter-trimestral CONFIRMADA\\n")
} else {
  cat("ATENCION: Variabilidad inter-trimestral detectada — revisar\\n")
}
```
', con)

close(con)
cat("   [OK] Rmd escrito:", rmd_temp, "\n")

# Render
rmarkdown::render(
  input       = rmd_temp,
  output_file = path_html,
  quiet       = TRUE,
  envir       = new.env(parent = globalenv())
)
unlink(rmd_temp)
cat("   [OK] HTML generado:", path_html, "\n")

# 📑 8. Checklist y timer ----------------------------------------------------

cat("\nChecklist de outputs:\n")
cat("  CSV: ", ifelse(file.exists(path_csv),  "OK", "FALTA"), "\n")
cat("  HTML:", ifelse(file.exists(path_html), "OK", "FALTA"), "\n")

# 📦 CONTRATO ----------------------------------------------------------------
cat("\n── Generando contrato 10f ─────────────────────────────────────────\n")

contrato_10f <- list(
  script     = "10f_loco_quarter_GLM.R",
  fecha      = format(Sys.time(), "%Y-%m-%d %H:%M"),
  n_folds    = nrow(df_loco),
  auc_min    = round(min(df_loco$auc_roc), 4),
  auc_max    = round(max(df_loco$auc_roc), 4),
  auc_mean   = round(mean(df_loco$auc_roc), 4),
  auc_sd     = round(sd(df_loco$auc_roc), 4),
  auc_by_fold = setNames(df_loco$auc_roc, df_loco$test_quarter)
)

path_contrato_10f <- file.path(DIR_CONTRATOS, "10f_contrato_loco_quarter.rds")
saveRDS(contrato_10f, path_contrato_10f)
cat(sprintf("  [✅] Contrato guardado: %s\n", path_contrato_10f))

rm(panel, train_raw, df_loco, c07a_ref)
gc(verbose = FALSE)

cat("\nTiempo:", round((proc.time() - t_inicio)["elapsed"] / 60, 1), "minutos\n")
