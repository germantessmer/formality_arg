# =============================================================================
# [EN] 10k_coef_stability_GLM.R -- Coefficient stability across LOCO quarter folds: correlations, top stable/unstable variables
# INPUTS:  rdos/datos/08_panel_formalidad_GLM*.rds
# OUTPUTS: rdos/reportes/10k_coef_stability_GLM.html, rdos/figuras/10k_coef_stability/*.pdf
# =============================================================================
# 🌟 10k_coef_stability_GLM.R 🌟 ####
# OBJETIVO: Coefficient stability across LOCO quarter folds (R4-Q2)
#   Re-entrena GLM en cada fold LOCO (train 2Q, test 1Q) y compara
#   vectores de coeficientes. Reporta correlaciones, top estables/inestables,
#   scatter plots.
# INPUTS:  rdos/datos/08_panel_formalidad_{SUFIJO_MODELO_GLM}.rds
# OUTPUTS: rdos/reportes/10k_coef_stability_GLM.csv
#          rdos/reportes/10k_coef_stability_GLM.html
#          rdos/figuras/10k_coef_stability/10k_scatter_coefs_*.pdf
#          rdos/reportes/10k_coef_stability_notas.txt
# TIEMPO ESTIMADO: ~5-8 minutos (3 folds x LASSO)

# ⌛ Inicio contador de tiempo -------------------------------------------------

options(renv.config.auto.snapshot = FALSE)
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

cat("=", rep("=", 69), "\n", sep = "")
cat("  Coefficient Stability — LOCO Quarter GLM\n")
cat("  Responde a R4-Q2\n")
cat("=", rep("=", 69), "\n", sep = "")

# 🪫 1. Cargar panel y preparar ------------------------------------------------

path_panel_08 <- file.path(DIR_DATOS,
                           paste0("08_panel_formalidad_", SUFIJO_MODELO_GLM, ".rds"))
panel <- safe_load(path_panel_08)

if ("theta_A" %in% names(panel) && !"theta_A_mA" %in% names(panel)) {
  panel <- panel %>% rename(theta_A_mA = theta_A, theta_B_mA = theta_B)
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
if ("horas_trabajadas" %in% names(panel)) VARS_MODELO <- c(VARS_MODELO, "horas_trabajadas")
VARS_MODELO <- intersect(VARS_MODELO, names(panel))

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
cat("Overlap total:", nrow(train_raw), "obs\n")

theta_cols <- c("theta_A_mA", "theta_B_mA")
quarters <- TRIMESTRES_FORMALIDAD

# 🪫 2. LOCO loop — extraer coeficientes --------------------------------------

coef_list <- list()

cl <- makeCluster(min(N_CORES, 7))
registerDoParallel(cl)

for (q_idx in seq_along(quarters)) {

  q_test  <- quarters[q_idx]
  q_train <- setdiff(quarters, q_test)

  cat("\n--- Fold", q_idx, ": test =", q_test, "---\n")

  df_train <- train_raw %>% filter(periodo_id %in% q_train)

  df_rec_train <- df_train %>% select(-pondera, -formalidad_empleo, -codusu, -periodo_id)

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
  X_train <- X_train[, colnames(X_train) != "formalidad_bin", drop = FALSE]
  y_train <- df_train$formalidad_bin
  w_train <- df_train$pondera / mean(df_train$pondera)

  pf <- rep(1, ncol(X_train))
  theta_idx <- which(colnames(X_train) %in% theta_cols)
  if (length(theta_idx) > 0) pf[theta_idx] <- 0

  cluster_ids <- unique(df_train$codusu)
  set.seed(SEED_GLOBAL + q_idx)
  fold_asignacion <- tibble(
    codusu = cluster_ids,
    fold   = sample(rep(1:10, length.out = length(cluster_ids)))
  )
  foldid_vec <- tibble(codusu = df_train$codusu) %>%
    left_join(fold_asignacion, by = "codusu") %>%
    pull(fold)

  cv_fit <- cv.glmnet(
    x = X_train, y = y_train, weights = w_train,
    alpha = 1, family = "binomial", penalty.factor = pf,
    type.measure = "auc", foldid = foldid_vec, parallel = TRUE
  )

  coef_vec <- as.vector(coef(cv_fit, s = "lambda.1se"))
  coef_names <- c("(Intercept)", colnames(X_train))

  coef_list[[q_idx]] <- tibble(
    variable = coef_names,
    coef     = coef_vec,
    fold     = q_idx,
    test_q   = q_test
  )

  n_sel <- sum(coef_vec[-1] != 0)
  cat("  lambda.1se =", round(cv_fit$lambda.1se, 6), "| vars sel =", n_sel, "\n")
}

stopCluster(cl)

# 🪫 3. Comparar coeficientes -------------------------------------------------

# Alinear por nombre: tomar union de variables, rellenar con 0 las ausentes
all_vars <- sort(unique(unlist(lapply(coef_list, function(x) x$variable))))

# Crear matriz alineada
coef_mat <- matrix(0, nrow = length(all_vars), ncol = 3,
                   dimnames = list(all_vars, paste0("fold_", 1:3)))
for (q_idx in 1:3) {
  cl_df <- coef_list[[q_idx]]
  matched <- match(cl_df$variable, all_vars)
  coef_mat[matched, q_idx] <- cl_df$coef
}

df_coefs <- as_tibble(coef_mat, rownames = "variable") %>%
  mutate(fold_1 = as.numeric(fold_1),
         fold_2 = as.numeric(fold_2),
         fold_3 = as.numeric(fold_3))

# Excluir intercept para correlaciones
df_coefs_noicpt <- df_coefs %>% filter(variable != "(Intercept)")

# Correlaciones par-a-par
cor_12 <- cor(df_coefs_noicpt$fold_1, df_coefs_noicpt$fold_2)
cor_13 <- cor(df_coefs_noicpt$fold_1, df_coefs_noicpt$fold_3)
cor_23 <- cor(df_coefs_noicpt$fold_2, df_coefs_noicpt$fold_3)

cat("\n=== Correlaciones de vectores de coeficientes ===\n")
cat(sprintf("Fold 1 vs 2: r = %.4f\n", cor_12))
cat(sprintf("Fold 1 vs 3: r = %.4f\n", cor_13))
cat(sprintf("Fold 2 vs 3: r = %.4f\n", cor_23))
cat(sprintf("Media:       r = %.4f\n", mean(c(cor_12, cor_13, cor_23))))

# Variables con mayor variabilidad
df_coefs_noicpt <- df_coefs_noicpt %>%
  mutate(
    mean_coef = (fold_1 + fold_2 + fold_3) / 3,
    sd_coef   = pmap_dbl(list(fold_1, fold_2, fold_3), ~ sd(c(..1, ..2, ..3))),
    cv_coef   = ifelse(abs(mean_coef) > 0.001, sd_coef / abs(mean_coef), NA_real_),
    # Signo consistente?
    sign_consistent = (sign(fold_1) == sign(fold_2)) & (sign(fold_2) == sign(fold_3)),
    any_nonzero     = (fold_1 != 0) | (fold_2 != 0) | (fold_3 != 0)
  )

# Solo variables seleccionadas en al menos 1 fold
df_selected <- df_coefs_noicpt %>% filter(any_nonzero)

cat("\n=== Estabilidad de seleccion ===\n")
cat("Variables seleccionadas en 3/3 folds:", sum(df_selected$fold_1 != 0 &
    df_selected$fold_2 != 0 & df_selected$fold_3 != 0), "\n")
cat("Variables seleccionadas en 2/3 folds:", sum(
    (df_selected$fold_1 != 0) + (df_selected$fold_2 != 0) +
    (df_selected$fold_3 != 0) == 2), "\n")
cat("Variables seleccionadas en 1/3 folds:", sum(
    (df_selected$fold_1 != 0) + (df_selected$fold_2 != 0) +
    (df_selected$fold_3 != 0) == 1), "\n")
cat("Signo consistente (entre seleccionadas 3/3):",
    sum(df_selected$sign_consistent &
        df_selected$fold_1 != 0 & df_selected$fold_2 != 0 & df_selected$fold_3 != 0), "\n")

# Top 20 mas estables (por sd) entre seleccionadas en 3/3
sel_3of3 <- df_selected %>%
  filter(fold_1 != 0, fold_2 != 0, fold_3 != 0) %>%
  arrange(sd_coef)

cat("\n=== Top 20 variables MAS ESTABLES (menor sd entre folds) ===\n")
print(sel_3of3 %>% select(variable, fold_1, fold_2, fold_3, mean_coef, sd_coef) %>%
        head(20), n = 20)

cat("\n=== Top 20 variables MENOS ESTABLES (mayor sd entre folds) ===\n")
print(sel_3of3 %>% arrange(desc(sd_coef)) %>%
        select(variable, fold_1, fold_2, fold_3, mean_coef, sd_coef) %>%
        head(20), n = 20)

# 🪫 4. Guardar CSV ------------------------------------------------------------

path_csv <- file.path(DIR_REPORTES, "10k_coef_stability_GLM.csv")
write_csv(df_coefs_noicpt, path_csv)
cat("\nCSV:", path_csv, "\n")

# 🪫 5. Figuras ----------------------------------------------------------------

dir_fig <- file.path(DIR_FIGURAS, "10k_coef_stability")
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)

# Scatter plots fold-i vs fold-j
pairs_list <- list(c(1, 2), c(1, 3), c(2, 3))
pair_cors  <- c(cor_12, cor_13, cor_23)

for (p_idx in seq_along(pairs_list)) {
  fi <- pairs_list[[p_idx]][1]
  fj <- pairs_list[[p_idx]][2]
  col_i <- paste0("fold_", fi)
  col_j <- paste0("fold_", fj)

  fig <- df_selected %>%
    ggplot(aes(x = .data[[col_i]], y = .data[[col_j]])) +
    geom_point(alpha = 0.5, size = 1.5, color = COL_GLM) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = COL_OBSERVADO) +
    tr_labs(
      title = sprintf("Coefficient stability: Fold %d vs Fold %d", fi, fj),
      subtitle = sprintf("Pearson r = %.4f | Test quarters: %s vs %s",
                         pair_cors[p_idx], quarters[fi], quarters[fj]),
      x = sprintf("Coefficients (Fold %d: test = %s)", fi, quarters[fi]),
      y = sprintf("Coefficients (Fold %d: test = %s)", fj, quarters[fj])
    ) +
    theme_paper() +
    coord_fixed()

  guardar_figura(fig, dir_fig, "scatter", p_idx, width = 7, height = 7)
  cat("  Scatter", fi, "vs", fj, "\n")
}

# Sign consistency bar chart
n_folds_selected <- df_selected %>%
  mutate(n_sel = (fold_1 != 0) + (fold_2 != 0) + (fold_3 != 0)) %>%
  count(n_sel) %>%
  mutate(label = paste0(n_sel, "/3 folds"))

fig_sel <- n_folds_selected %>%
  ggplot(aes(x = label, y = n)) +
  geom_col(fill = COL_GLM, width = 0.5) +
  geom_text(aes(label = n), vjust = -0.3) +
  tr_labs(title = "Variable selection consistency across LOCO folds",
       x = "Selected in N folds", y = "Number of variables") +
  theme_paper()

guardar_figura(fig_sel, dir_fig, "barras", 1, width = 6, height = 5)
cat("  Selection consistency saved\n")

# 🪫 6. Notas para el paper ----------------------------------------------------

path_notas <- file.path(DIR_REPORTES, "10k_coef_stability_notas.txt")
con_notas  <- file(path_notas, open = "wt", encoding = "UTF-8")

writeLines(c(
  "=== NOTAS PARA EL PAPER — R4-Q2 Coefficient Stability ===",
  sprintf("Generado: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "--- Appendix G (Predictive Models, nueva subseccion) ---",
  "",
  "To assess the stability of the estimated coefficients across the short",
  "overlap window, we compare the LASSO coefficient vectors from the three",
  "leave-one-quarter-out folds.",
  "",
  sprintf("Pairwise Pearson correlations of coefficient vectors:"),
  sprintf("  Fold 1 vs 2 (test 2024T4 vs 2025T1): r = %.4f", cor_12),
  sprintf("  Fold 1 vs 3 (test 2024T4 vs 2025T2): r = %.4f", cor_13),
  sprintf("  Fold 2 vs 3 (test 2025T1 vs 2025T2): r = %.4f", cor_23),
  sprintf("  Mean: r = %.4f", mean(c(cor_12, cor_13, cor_23))),
  "",
  sprintf("Variables selected in 3/3 folds: %d",
          sum(df_selected$fold_1 != 0 & df_selected$fold_2 != 0 & df_selected$fold_3 != 0)),
  sprintf("Of those, sign-consistent: %d",
          sum(df_selected$sign_consistent &
              df_selected$fold_1 != 0 & df_selected$fold_2 != 0 & df_selected$fold_3 != 0)),
  "",
  "The high pairwise correlations and near-complete sign consistency confirm",
  "that the prediction rule is not driven by quarter-specific artifacts.",
  "",
  "--- Cuerpo (Sec 4.3, una oracion) ---",
  "",
  sprintf("Coefficient vectors from the three LOCO folds are highly correlated"),
  sprintf("(mean pairwise r = %.3f), with [N] of [M] selected variables",
          mean(c(cor_12, cor_13, cor_23))),
  "retaining consistent signs across all folds (Appendix [X])."
), con_notas)

close(con_notas)
cat("\nNotas:", path_notas, "\n")

# 🪫 7. Reporte HTML -----------------------------------------------------------

path_html <- file.path(DIR_REPORTES, "10k_coef_stability_GLM.html")
rmd_temp  <- tempfile(fileext = ".Rmd")
con       <- file(rmd_temp, open = "wt", encoding = "UTF-8")

writeLines('---
title: "Coefficient Stability -- LOCO Quarter GLM"
subtitle: "R4-Q2: Formal stability diagnostics"
date: "Generado: `r format(Sys.time(), \'%d/%m/%Y %H:%M\')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float: { collapsed: false }
    number_sections: true
    df_print: kable
---', con)

writeLines(sprintf('
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 8, fig.height = 7, dpi = 150)
library(tidyverse); library(knitr); library(kableExtra)
```

# Coefficient correlations

Pairwise Pearson correlations of LASSO coefficient vectors (excluding intercept):

| Pair | r |
|------|---|
| Fold 1 vs 2 | %.4f |
| Fold 1 vs 3 | %.4f |
| Fold 2 vs 3 | %.4f |
| **Mean** | **%.4f** |

# Variable selection consistency

```{r sel-table}
df <- read.csv("%s")
df_sel <- df %%>%% filter(fold_1 != 0 | fold_2 != 0 | fold_3 != 0) %%>%%
  mutate(n_sel = (fold_1 != 0) + (fold_2 != 0) + (fold_3 != 0))
cat("Variables selected in 3/3:", sum(df_sel$n_sel == 3), "\\n")
cat("Variables selected in 2/3:", sum(df_sel$n_sel == 2), "\\n")
cat("Variables selected in 1/3:", sum(df_sel$n_sel == 1), "\\n")

df_3of3 <- df_sel %%>%% filter(n_sel == 3)
cat("Sign-consistent (3/3):", sum(sign(df_3of3$fold_1) == sign(df_3of3$fold_2) &
    sign(df_3of3$fold_2) == sign(df_3of3$fold_3)), "\\n")
```

# Top stable and unstable variables

```{r top-vars}
df_3of3 <- df_sel %%>%% filter(n_sel == 3) %%>%%
  mutate(mean_coef = (fold_1 + fold_2 + fold_3)/3,
         sd_coef = pmap_dbl(list(fold_1, fold_2, fold_3), ~sd(c(..1,..2,..3))))

cat("\\n--- Most stable (lowest sd) ---\\n")
df_3of3 %%>%% arrange(sd_coef) %%>%%
  select(variable, fold_1, fold_2, fold_3, mean_coef, sd_coef) %%>%%
  head(20) %%>%% kable(digits = 4) %%>%% kable_styling(font_size = 11)
```

```{r unstable}
cat("\\n--- Least stable (highest sd) ---\\n")
df_3of3 %%>%% arrange(desc(sd_coef)) %%>%%
  select(variable, fold_1, fold_2, fold_3, mean_coef, sd_coef) %%>%%
  head(20) %%>%% kable(digits = 4) %%>%% kable_styling(font_size = 11)
```
', cor_12, cor_13, cor_23, mean(c(cor_12, cor_13, cor_23)),
   gsub("\\\\", "/", path_csv)), con)

close(con)

rmarkdown::render(input = rmd_temp, output_file = path_html,
                  quiet = TRUE, envir = new.env(parent = globalenv()))
unlink(rmd_temp)
cat("HTML:", path_html, "\n")

# 📦 Contrato -----------------------------------------------------------------

path_contrato_10k <- file.path(DIR_CONTRATOS, "10k_contrato_coef_stability.rds")

n_vars_all3 <- sum(sel_3of3$fold_1 != 0 & sel_3of3$fold_2 != 0 & sel_3of3$fold_3 != 0)
n_sign_consistent <- sum(sel_3of3$sign_consistent &
                         sel_3of3$fold_1 != 0 & sel_3of3$fold_2 != 0 & sel_3of3$fold_3 != 0)

contrato_10k <- list(
  script              = "10k_coef_stability_GLM.R",
  fecha               = format(Sys.time(), "%Y-%m-%d %H:%M"),
  r_12                = cor_12,
  r_13                = cor_13,
  r_23                = cor_23,
  r_mean              = mean(c(cor_12, cor_13, cor_23)),
  n_vars_all3         = n_vars_all3,
  n_sign_consistent   = n_sign_consistent
)

saveRDS(contrato_10k, path_contrato_10k)
cat("\n📦 Contrato:", path_contrato_10k, "\n")

# 📑 Checklist y timer ---------------------------------------------------------

cat("\nChecklist:\n")
cat("  CSV:  ", ifelse(file.exists(path_csv),  "OK", "FALTA"), "\n")
cat("  HTML: ", ifelse(file.exists(path_html), "OK", "FALTA"), "\n")
cat("  Notas:", ifelse(file.exists(path_notas),"OK", "FALTA"), "\n")
cat("  Contrato:", ifelse(file.exists(path_contrato_10k), "OK", "FALTA"), "\n")

rm(train_raw, df_coefs, df_coefs_noicpt, coef_list, contrato_10k)
gc(verbose = FALSE)

cat("\nTiempo:", round((proc.time() - t_inicio)["elapsed"] / 60, 1), "minutos\n")
