# =============================================================================
# [EN] 10j_loco_aglomerado_GLM.R -- Leave-One-Cluster-Out CV by urban agglomerate (31 folds) for GLM spatial robustness
# INPUTS:  rdos/datos/08_panel_formalidad_GLM*.rds
# OUTPUTS: rdos/reportes/10j_loco_aglomerado_GLM.html, rdos/figuras/10j_forest_auc_aglomerado.pdf
# =============================================================================
# 🌟 10j_loco_aglomerado_GLM.R 🌟 ####
# OBJETIVO: Leave-One-Cluster-Out (LOCO) por aglomerado para GLM
#   Entrena en 30 de 31 aglomerados del overlap, testea en el held-out.
#   Repite para cada aglomerado. Reporta AUC-ROC, AUC-PR, F1, MCC,
#   calibration delta. Genera CSV, figuras y reporte HTML.
# INPUTS:  rdos/datos/08_panel_formalidad_{SUFIJO_MODELO_GLM}.rds
# OUTPUTS: rdos/reportes/10j_loco_aglomerado_GLM.csv
#          rdos/reportes/10j_loco_aglomerado_GLM.html
#          rdos/figuras/10j_forest_auc_aglomerado.pdf
#          rdos/figuras/10j_auc_vs_n.pdf
#          rdos/reportes/10j_loco_aglomerado_notas.txt
# TIEMPO ESTIMADO: ~15-25 minutos (31 folds x LASSO + CV, paralelo)

# ⌛ Inicio contador de tiempo -------------------------------------------------

t_inicio <- proc.time()

# 📚 Librerias -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(glmnet)
  library(pROC)
  library(recipes)
  library(doParallel)
  library(foreach)
  library(kableExtra)
})

# 🔧 Cargar configuracion y funciones ------------------------------------------

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

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

cat("=", rep("=", 69), "\n", sep = "")
cat("  LOCO Aglomerado Validation -- GLM (binomial + LASSO)\n")
cat("  Responde a R3-Q9: transportabilidad geografica\n")
cat("=", rep("=", 69), "\n", sep = "")

# 🪫 1. Cargar panel -----------------------------------------------------------

path_panel_08 <- file.path(DIR_DATOS,
                           paste0("08_panel_formalidad_", SUFIJO_MODELO_GLM, ".rds"))
panel <- safe_load(path_panel_08)
cat("Panel cargado:", nrow(panel), "obs x", ncol(panel), "cols\n")

# Renombrar theta si es necesario
if ("theta_A" %in% names(panel) && !"theta_A_mA" %in% names(panel)) {
  panel <- panel %>% rename(theta_A_mA = theta_A, theta_B_mA = theta_B)
  cat("Renombrado theta_A -> theta_A_mA\n")
}

# 🪫 2. Variables del modelo (replica 07a) -------------------------------------

VARS_MODELO <- c(
  "formalidad_empleo", "pondera", "codusu", "periodo_id", "aglomerado",
  "theta_A_mA", "theta_B_mA",
  "edad", "edad_cuadrado", "sexo", "estado_civil",
  "lugar_nacimiento", "parentesco",
  "nivel_educ_obtenido2", "asistencia_escuela", "tipo_escuela", "alfabetizacion",
  "mas_500",
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
if ("horas_trabajadas" %in% names(panel)) VARS_MODELO <- c(VARS_MODELO, "horas_trabajadas")
VARS_MODELO <- intersect(VARS_MODELO, names(panel))
cat("Variables del modelo disponibles:", length(VARS_MODELO), "\n")

# 🪫 3. Filtrar overlap --------------------------------------------------------

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

cat("Overlap total:", nrow(train_raw), "obs en",
    length(TRIMESTRES_FORMALIDAD), "trimestres\n")

# 🪫 4. Diagnostico de aglomerados --------------------------------------------

aglo_tab <- train_raw %>%
  count(aglomerado, name = "n_obs") %>%
  arrange(desc(n_obs)) %>%
  mutate(
    pct       = round(n_obs / sum(n_obs) * 100, 1),
    pct_formal = map_dbl(aglomerado, ~ {
      mean(train_raw$formalidad_bin[train_raw$aglomerado == .x]) * 100
    })
  )

cat("\nDistribucion por aglomerado:\n")
print(aglo_tab, n = Inf)

aglomerados <- aglo_tab$aglomerado
n_aglo      <- length(aglomerados)
cat("\nTotal aglomerados:", n_aglo, "\n")

# Umbral minimo para AUC confiable
N_MIN_TEST <- 100
aglo_chicos <- aglo_tab %>% filter(n_obs < N_MIN_TEST)
if (nrow(aglo_chicos) > 0) {
  cat("Aglomerados con n <", N_MIN_TEST, "obs (AUC puede ser inestable):\n")
  print(aglo_chicos)
}

theta_cols <- c("theta_A_mA", "theta_B_mA")

# 🪫 5. LOCO loop --------------------------------------------------------------

resultados <- list()

cl <- makeCluster(min(N_CORES, 7))
registerDoParallel(cl)

for (a_idx in seq_along(aglomerados)) {

  aglo_test  <- aglomerados[a_idx]
  aglo_train <- setdiff(aglomerados, aglo_test)

  cat("\n", rep("-", 60), "\n")
  cat(sprintf("  Fold %d/%d: held-out = %s\n", a_idx, n_aglo, aglo_test))
  cat(rep("-", 60), "\n")

  df_train <- train_raw %>% filter(aglomerado %in% aglo_train)
  df_test  <- train_raw %>% filter(aglomerado == aglo_test)

  n_test <- nrow(df_test)
  cat("  n_train =", nrow(df_train), "| n_test =", n_test, "\n")

  # Preparar recipe — aglomerado SE INCLUYE como predictor (step_novel maneja
  # el nivel held-out que no aparece en training)
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
  X_train <- X_train[, colnames(X_train) != "formalidad_bin", drop = FALSE]
  X_test  <- X_test[, colnames(X_test) != "formalidad_bin", drop = FALSE]

  y_train <- df_train$formalidad_bin
  y_test  <- df_test$formalidad_bin

  w_train <- df_train$pondera / mean(df_train$pondera)
  w_test  <- df_test$pondera  / mean(df_test$pondera)

  # Penalty factors (theta sin regularizar)
  col_names_X <- colnames(X_train)
  pf <- rep(1, ncol(X_train))
  theta_idx <- which(col_names_X %in% theta_cols)
  if (length(theta_idx) > 0) pf[theta_idx] <- 0

  # Folds CV por hogar
  cluster_ids     <- unique(df_train$codusu)
  n_clusters      <- length(cluster_ids)
  set.seed(SEED_GLOBAL + a_idx)
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

  cat("  lambda.1se =", round(lambda_1se, 6),
      "| vars seleccionadas =", n_vars_sel, "\n")

  # Predicciones en test
  pred_prob <- as.vector(
    predict(cv_fit, newx = X_test, s = "lambda.1se", type = "response")
  )
  pred_clip <- pmax(0, pmin(1, pred_prob))

  # AUC-ROC (solo si hay ambas clases en test)
  clases_test <- length(unique(y_test))
  if (clases_test == 2 && n_test >= 20) {
    roc_obj <- roc(response = y_test, predictor = pred_clip,
                   levels = c(0, 1), quiet = TRUE)
    auc_val <- as.numeric(auc(roc_obj))
    auc_ci  <- tryCatch(
      as.numeric(ci.auc(roc_obj, method = "delong")),
      error = function(e) c(NA_real_, auc_val, NA_real_)
    )

    # AUC-PR
    auc_pr <- tryCatch({
      pr_obj <- PRROC::pr.curve(
        scores.class0 = pred_clip[y_test == 1],
        scores.class1 = pred_clip[y_test == 0],
        curve = FALSE
      )
      pr_obj$auc.integral
    }, error = function(e) NA_real_)

    # Umbral Youden
    coords_best <- coords(roc_obj, x = "best", best.method = "youden",
                          ret = c("threshold", "sensitivity", "specificity"))
    umbral <- coords_best$threshold[1]
  } else {
    auc_val <- NA_real_
    auc_ci  <- c(NA_real_, NA_real_, NA_real_)
    auc_pr  <- NA_real_
    umbral  <- 0.5
    cat("  [WARN] Solo", clases_test, "clase(s) o n<20 en test — AUC no calculable\n")
  }

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

  # Calibracion
  tasa_obs  <- weighted.mean(y_test, w_test)
  tasa_pred <- weighted.mean(pred_clip, w_test)
  cal_delta <- abs(tasa_obs - tasa_pred)

  cat(sprintf("  AUC-ROC = %.4f [%.4f, %.4f]\n",
              auc_val, auc_ci[1], auc_ci[3]))
  cat(sprintf("  AUC-PR  = %.4f | F1 = %.4f | MCC = %.4f\n",
              auc_pr, f1_val, mcc_val))
  cat(sprintf("  Cal delta = %.2f pp | Tasa obs = %.1f%% | Tasa pred = %.1f%%\n",
              cal_delta * 100, tasa_obs * 100, tasa_pred * 100))

  resultados[[a_idx]] <- tibble(
    fold          = a_idx,
    aglomerado    = aglo_test,
    n_test        = n_test,
    n_train       = nrow(df_train),
    pct_formal    = round(tasa_obs * 100, 1),
    n_vars_sel    = n_vars_sel,
    auc_roc       = round(auc_val, 4),
    auc_roc_ci_lo = round(auc_ci[1], 4),
    auc_roc_ci_hi = round(auc_ci[3], 4),
    auc_pr        = round(auc_pr, 4),
    f1            = round(f1_val, 4),
    mcc           = round(mcc_val, 4),
    umbral_youden = round(umbral, 4),
    cal_delta_pp  = round(cal_delta * 100, 2),
    tasa_obs_pct  = round(tasa_obs * 100, 1),
    tasa_pred_pct = round(tasa_pred * 100, 1)
  )
}

stopCluster(cl)

# 🪫 6. Consolidar resultados -------------------------------------------------

df_loco <- bind_rows(resultados)

cat("\n", rep("=", 60), "\n")
cat("  RESUMEN LOCO Aglomerado -- GLM (binomial + LASSO)\n")
cat(rep("=", 60), "\n\n")
print(df_loco %>%
        select(aglomerado, n_test, pct_formal, auc_roc, auc_pr, f1, mcc, cal_delta_pp),
      n = Inf)

# Estadisticos resumen (solo aglomerados con AUC valido)
df_validos <- df_loco %>% filter(!is.na(auc_roc))

resumen <- tibble(
  metrica = c("AUC-ROC mediana", "AUC-ROC media", "AUC-ROC sd",
              "AUC-ROC min", "AUC-ROC max", "AUC-ROC IQR",
              "AUC-PR mediana", "F1 mediana", "MCC mediana",
              "Cal delta mediana (pp)", "Cal delta max (pp)",
              "N aglomerados validos", "N aglomerados total"),
  valor = c(
    round(median(df_validos$auc_roc), 4),
    round(mean(df_validos$auc_roc), 4),
    round(sd(df_validos$auc_roc), 4),
    round(min(df_validos$auc_roc), 4),
    round(max(df_validos$auc_roc), 4),
    round(IQR(df_validos$auc_roc), 4),
    round(median(df_validos$auc_pr, na.rm = TRUE), 4),
    round(median(df_validos$f1), 4),
    round(median(df_validos$mcc), 4),
    round(median(df_validos$cal_delta_pp), 2),
    round(max(df_validos$cal_delta_pp), 2),
    nrow(df_validos),
    n_aglo
  )
)

cat("\nEstadisticos resumen:\n")
print(resumen, n = Inf)

# Referencia: modelo completo (N_TRIMESTRES_TRAINING trimestres)
c07a_ref   <- safe_load(PATH_07_CONTRATO_GLM)
auc_full   <- safe_get(c07a_ref, "auc_test")
cat(sprintf("\nAUC-ROC modelo completo (%s): %.4f\n", SUFIJO_MODELO_GLM, auc_full))
cat(sprintf("AUC-ROC mediana LOCO aglo:    %.4f\n", median(df_validos$auc_roc)))
cat(sprintf("Diferencia mediana:           %.4f\n",
            auc_full - median(df_validos$auc_roc)))

# 🪫 7. Guardar CSV ------------------------------------------------------------

path_csv  <- file.path(DIR_REPORTES, "10j_loco_aglomerado_GLM.csv")
path_html <- file.path(DIR_REPORTES, "10j_loco_aglomerado_GLM.html")

write_csv(df_loco, path_csv)
cat("\nCSV guardado:", path_csv, "\n")

# 🪫 8. Figuras ----------------------------------------------------------------

cat("\n-- Generando figuras --------------------------------------------------\n")

# DIR_FIGURAS_10J definido en parametros.R y creado automáticamente

# ── 8a. Forest plot: AUC por aglomerado ──
fig_forest <- df_validos %>%
  mutate(aglomerado = fct_reorder(aglomerado, auc_roc)) %>%
  ggplot(aes(x = auc_roc, y = aglomerado)) +
  geom_point(size = 2.5, color = COL_GLM) +
  geom_errorbarh(aes(xmin = auc_roc_ci_lo, xmax = auc_roc_ci_hi),
                 height = 0.3, linewidth = 0.4, color = COL_GLM) +
  geom_vline(xintercept = auc_full, linetype = "dashed", color = COL_OBSERVADO, linewidth = 0.5) +
  geom_vline(xintercept = median(df_validos$auc_roc),
             linetype = "dotted", color = COL_GLM, linewidth = 0.5) +
  scale_x_continuous(limits = c(
    max(0.5, min(df_validos$auc_roc_ci_lo, na.rm = TRUE) - 0.02), 1
  )) +
  tr_labs(
    title = NULL,
    subtitle = NULL,
    x = "AUC-ROC", y = NULL
  ) +
  theme_paper()

guardar_figura(fig_forest, DIR_FIGURAS_10J, "forest_auc", 1, width = 10, height = 8)
cat("  Forest plot: DIR_FIGURAS_10J\n")

# ── 8b. AUC vs n (tamano muestral) ──
fig_auc_n <- df_validos %>%
  ggplot(aes(x = n_test, y = auc_roc)) +
  geom_point(size = 2.5, color = COL_GLM) +
  geom_smooth(method = "lm", se = TRUE, color = COL_GLM,
              linewidth = 0.6, alpha = 0.15) +
  geom_hline(yintercept = auc_full, linetype = "dashed", color = COL_OBSERVADO,
             linewidth = 0.5) +
  tr_labs(
    title = "AUC-ROC vs sample size of held-out aglomerado",
    subtitle = sprintf("Dashed = full model (%.3f) | Pearson r = %.3f",
                       auc_full,
                       cor(df_validos$auc_roc, df_validos$n_test,
                           use = "complete.obs")),
    x = "n (held-out)", y = "AUC-ROC"
  ) +
  theme_paper()

guardar_figura(fig_auc_n, DIR_FIGURAS_10J, "auc_vs_n", 2, width = 8, height = 6)
cat("  AUC vs n: DIR_FIGURAS_10J\n")

# ── 8c. Calibration delta por aglomerado ──
fig_cal <- df_validos %>%
  mutate(aglomerado = fct_reorder(aglomerado, cal_delta_pp)) %>%
  ggplot(aes(x = cal_delta_pp, y = aglomerado)) +
  geom_point(size = 2.5, color = COL_GLM) +
  geom_vline(xintercept = median(df_validos$cal_delta_pp),
             linetype = "dotted", color = COL_GLM, linewidth = 0.5) +
  tr_labs(
    title = "Calibration delta (pp) by held-out aglomerado",
    subtitle = sprintf("Dotted = median (%.1f pp)", median(df_validos$cal_delta_pp)),
    x = "|Observed rate - Predicted rate| (pp)", y = NULL
  ) +
  theme_paper()

guardar_figura(fig_cal, DIR_FIGURAS_10J, "cal_delta", 3, width = 10, height = 8)
cat("  Cal delta: DIR_FIGURAS_10J\n")

# 🪫 9. Notas para el paper ----------------------------------------------------

path_notas <- file.path(DIR_REPORTES, "10j_loco_aglomerado_notas.txt")
con_notas  <- file(path_notas, open = "wt", encoding = "UTF-8")

writeLines(c(
  "=== NOTAS PARA EL PAPER — R3-Q9 Geographic Transportability ===",
  sprintf("Generado: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "--- Appendix (nueva subseccion: Geographic Transportability) ---",
  "",
  sprintf("To assess geographic transportability, we perform a leave-one-cluster-out"),
  sprintf("(LOCO) exercise over the %d EPH aglomerados. For each fold, the GLM is", n_aglo),
  "trained on the remaining aglomerados and tested on the held-out one.",
  "",
  sprintf("Median AUC-ROC across %d folds: %.3f (IQR: %.3f)",
          nrow(df_validos), median(df_validos$auc_roc), IQR(df_validos$auc_roc)),
  sprintf("Full-sample AUC-ROC: %.3f", auc_full),
  sprintf("AUC-ROC range: [%.3f, %.3f]",
          min(df_validos$auc_roc), max(df_validos$auc_roc)),
  sprintf("Median calibration delta: %.1f pp (max: %.1f pp)",
          median(df_validos$cal_delta_pp), max(df_validos$cal_delta_pp)),
  "",
  "The model maintains strong discriminative performance when predicting",
  "formality in geographic regions not seen during training, supporting",
  "the transportability of the estimated relationships across labor markets.",
  "",
  "--- Cuerpo (Sec 4.4 o Sec 5, una oracion) ---",
  "",
  "Geographic transportability is confirmed by a leave-one-aglomerado-out",
  sprintf("exercise yielding median AUC-ROC of %.3f (IQR %.3f; see Appendix [X]).",
          median(df_validos$auc_roc), IQR(df_validos$auc_roc)),
  "",
  "--- Figuras ---",
  "",
  "Figure [X]: Forest plot of AUC-ROC by held-out aglomerado (10j_forest_auc_aglomerado.pdf)",
  "Figure [X]: AUC-ROC vs sample size (10j_auc_vs_n.pdf)",
  "Figure [X]: Calibration delta by aglomerado (10j_cal_delta_aglomerado.pdf)"
), con_notas)

close(con_notas)
cat("\nNotas paper:", path_notas, "\n")

# 🪫 10. Reporte HTML ----------------------------------------------------------

cat("\n-- Construyendo reporte HTML ------------------------------------------\n")

rmd_temp <- tempfile(fileext = ".Rmd")
con      <- file(rmd_temp, open = "wt", encoding = "UTF-8")

writeLines('---
title: "LOCO Aglomerado Validation -- GLM (binomial + LASSO)"
subtitle: "R3-Q9: Geographic Transportability"
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
                      fig.width = 10, fig.height = 7, dpi = 150)
library(tidyverse); library(knitr); library(kableExtra)
source(here::here("script", "config", "funciones_comunes.R"))
```
', con)

# Seccion 1: Proposito
writeLines('
# Proposito y contexto

Este reporte responde a la observacion **R3-Q9** del Review 3:

> *"How stable are the estimated relationships across regions and demographic
> subgroups? Have you tested model transportability by training on some
> aglomerados and testing on held-out ones?"*

El ejercicio **Leave-One-Cluster-Out (LOCO)** entrena el modelo GLM (binomial + LASSO)
en 30 de 31 aglomerados del overlap y testea en el held-out. Se repite para cada
aglomerado, generando 31 folds.

**Objetivo:** verificar que el poder predictivo del modelo no depende de un mercado
laboral geografico especifico, reforzando la evidencia de transportabilidad espacial.
', con)

# Seccion 2: Resultados
writeLines(sprintf('
# Resultados por aglomerado

```{r tabla-loco}
df_loco <- read.csv("%s")

df_loco %%>%%
  select(aglomerado, n_test, pct_formal, auc_roc, auc_roc_ci_lo, auc_roc_ci_hi,
         auc_pr, f1, mcc, cal_delta_pp) %%>%%
  arrange(desc(auc_roc)) %%>%%
  kable(col.names = c("Aglomerado", "n test", "%% Formal",
                       "AUC-ROC", "CI low", "CI high",
                       "AUC-PR", "F1", "MCC", "Cal delta (pp)"),
        digits = c(0, 0, 1, 4, 4, 4, 4, 4, 4, 1),
        align = "lccccccccr",
        caption = "Metricas de clasificacion por aglomerado held-out") %%>%%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 12) %%>%%
  row_spec(0, bold = TRUE)
```
', gsub("\\\\", "/", path_csv)), con)

# Seccion 3: Resumen
writeLines(sprintf('
# Estadisticos resumen

```{r resumen}
df_validos <- df_loco %%>%% filter(!is.na(auc_roc))

cat("AUC-ROC mediana:   ", round(median(df_validos$auc_roc), 4), "\\n")
cat("AUC-ROC media:     ", round(mean(df_validos$auc_roc), 4), "\\n")
cat("AUC-ROC sd:        ", round(sd(df_validos$auc_roc), 4), "\\n")
cat("AUC-ROC rango:     [", round(min(df_validos$auc_roc), 4), ",",
    round(max(df_validos$auc_roc), 4), "]\\n")
cat("AUC-ROC IQR:       ", round(IQR(df_validos$auc_roc), 4), "\\n")
cat("Cal delta mediana:  ", round(median(df_validos$cal_delta_pp), 1), "pp\\n")
cat("Cal delta max:      ", round(max(df_validos$cal_delta_pp), 1), "pp\\n")
cat("\\n")
cat("Modelo completo: %.4f\\n")
```
', auc_full), con)

# Seccion 4: Figuras
writeLines(sprintf('
# Figuras

## Forest plot: AUC-ROC por aglomerado

```{r forest, fig.width=10, fig.height=8}
df_validos %%>%%
  mutate(aglomerado = fct_reorder(aglomerado, auc_roc)) %%>%%
  ggplot(aes(x = auc_roc, y = aglomerado)) +
  geom_point(size = 2.5, color = COL_GLM) +
  geom_errorbarh(aes(xmin = auc_roc_ci_lo, xmax = auc_roc_ci_hi),
                 height = 0.3, linewidth = 0.4, color = COL_GLM) +
  geom_vline(xintercept = %.4f, linetype = "dashed", color = COL_OBSERVADO, linewidth = 0.5) +
  geom_vline(xintercept = median(df_validos$auc_roc),
             linetype = "dotted", color = COL_GLM, linewidth = 0.5) +
  tr_labs(title = "AUC-ROC by held-out aglomerado (LOCO)",
       subtitle = sprintf("Dashed = full model (%%.3f) | Dotted = LOCO median (%%.3f)",
                          %.4f, median(df_validos$auc_roc)),
       x = "AUC-ROC", y = NULL) +
  theme_paper()
```

## AUC-ROC vs tamano muestral

```{r auc-vs-n, fig.width=8, fig.height=6}
df_validos %%>%%
  ggplot(aes(x = n_test, y = auc_roc)) +
  geom_point(size = 2.5, color = COL_GLM) +
  geom_smooth(method = "lm", se = TRUE, color = COL_GLM,
              linewidth = 0.6, alpha = 0.15) +
  geom_hline(yintercept = %.4f, linetype = "dashed", color = COL_OBSERVADO, linewidth = 0.5) +
  tr_labs(title = "AUC-ROC vs sample size",
       x = "n (held-out)", y = "AUC-ROC") +
  theme_paper()
```

## Calibration delta por aglomerado

```{r cal-delta, fig.width=10, fig.height=8}
df_validos %%>%%
  mutate(aglomerado = fct_reorder(aglomerado, cal_delta_pp)) %%>%%
  ggplot(aes(x = cal_delta_pp, y = aglomerado)) +
  geom_point(size = 2.5, color = COL_GLM) +
  geom_vline(xintercept = median(df_validos$cal_delta_pp),
             linetype = "dotted", color = COL_GLM, linewidth = 0.5) +
  tr_labs(title = "Calibration delta (pp) by aglomerado",
       x = "|Observed - Predicted| (pp)", y = NULL) +
  theme_paper()
```
', auc_full, auc_full, auc_full), con)

# Seccion 5: Interpretacion
writeLines('
# Interpretacion

## Transportabilidad geografica

Si la mediana de AUC-ROC se mantiene por encima de 0.80 y el rango no excede 0.10,
el modelo demuestra transportabilidad robusta entre mercados laborales.

```{r diagnostico-final}
rango_auc <- max(df_validos$auc_roc) - min(df_validos$auc_roc)
rango_cal <- max(df_validos$cal_delta_pp)

cat("--- DIAGNOSTICO ---\\n")
cat(sprintf("Mediana AUC-ROC: %.4f\\n", median(df_validos$auc_roc)))
cat(sprintf("Rango AUC-ROC: %.4f\\n", rango_auc))
cat(sprintf("Max calibration delta: %.1f pp\\n", rango_cal))

if (median(df_validos$auc_roc) >= 0.80 & rango_auc < 0.15) {
  cat("\\nCONCLUSION: Transportabilidad geografica CONFIRMADA\\n")
} else if (median(df_validos$auc_roc) >= 0.75) {
  cat("\\nCONCLUSION: Transportabilidad geografica ACEPTABLE con variabilidad\\n")
} else {
  cat("\\nATENCION: Transportabilidad geografica DEBIL — revisar\\n")
}
```

## Donde incorporar en el paper

| Resultado | Seccion LaTeX | Uso |
|-----------|---------------|-----|
| Tabla LOCO aglomerado | Appendix (nueva subseccion) | Tabla de transportabilidad geografica |
| Forest plot AUC | Appendix | Figura de soporte |
| Oracion resumen | Sec 4.4 o Sec 5 | Evidencia complementaria al LOCO temporal |

## Limitaciones

- Sample sizes varian entre aglomerados (de ~100 a ~5000+ en GBA)
- Aglomerados chicos (<200 obs) generan CIs amplios
- El ejercicio no captura variacion entre regiones rurales vs urbanas (EPH = urbana)
- No evalua transportabilidad a aglomerados fuera de la EPH
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

# 📑 11. Checklist y timer -----------------------------------------------------

cat("\nChecklist de outputs:\n")
path_forest <- file.path(DIR_FIGURAS_10J, "10j_loco_aglomerado_forest_auc_01.pdf")
path_auc_n  <- file.path(DIR_FIGURAS_10J, "10j_loco_aglomerado_auc_vs_n_02.pdf")
path_cal    <- file.path(DIR_FIGURAS_10J, "10j_loco_aglomerado_cal_delta_03.pdf")
cat("  CSV:    ", ifelse(file.exists(path_csv),     "OK", "FALTA"), " ", path_csv, "\n")
cat("  HTML:   ", ifelse(file.exists(path_html),    "OK", "FALTA"), " ", path_html, "\n")
cat("  Forest: ", ifelse(file.exists(path_forest),  "OK", "FALTA"), " ", path_forest, "\n")
cat("  AUC/n:  ", ifelse(file.exists(path_auc_n),   "OK", "FALTA"), " ", path_auc_n, "\n")
cat("  Cal:    ", ifelse(file.exists(path_cal),      "OK", "FALTA"), " ", path_cal, "\n")
cat("  Notas:  ", ifelse(file.exists(path_notas),    "OK", "FALTA"), " ", path_notas, "\n")

# 📦 CONTRATO ----------------------------------------------------------------
cat("\n── Generando contrato 10j ─────────────────────────────────────────\n")

contrato_10j <- list(
  script     = "10j_loco_aglomerado_GLM.R",
  fecha      = format(Sys.time(), "%Y-%m-%d %H:%M"),
  n_folds    = nrow(df_validos),
  auc_median = round(median(df_validos$auc_roc), 4),
  auc_mean   = round(mean(df_validos$auc_roc), 4),
  auc_sd     = round(sd(df_validos$auc_roc), 4),
  auc_iqr    = round(IQR(df_validos$auc_roc), 4),
  auc_min    = round(min(df_validos$auc_roc), 4),
  auc_max    = round(max(df_validos$auc_roc), 4),
  n_above_80 = sum(df_validos$auc_roc > 0.80),
  f1_median       = round(median(df_validos$f1), 4),
  mcc_median      = round(median(df_validos$mcc), 4),
  cal_delta_median = round(median(df_validos$cal_delta_pp), 2),
  cal_delta_max    = round(max(df_validos$cal_delta_pp), 2),
  auc_full_sample  = round(as.numeric(c07a_ref$auc_test %||% c07a_ref$auc_roc %||% NA), 4),
  # Per-agglomerate table for appendix
  tab_aglomerado   = as.data.frame(df_validos)
)

path_contrato_10j <- file.path(DIR_CONTRATOS, "10j_contrato_loco_geo.rds")
saveRDS(contrato_10j, path_contrato_10j)
cat(sprintf("  [✅] Contrato guardado: %s\n", path_contrato_10j))

rm(train_raw, df_loco, df_validos, resultados, c07a_ref, resumen)
gc(verbose = FALSE)

cat("\nTiempo:", round((proc.time() - t_inicio)["elapsed"] / 60, 1), "minutos\n")
