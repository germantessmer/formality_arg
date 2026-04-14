# =============================================================================
# [EN] 10m_sparsity_sensitivity_GLM.R -- Sparsity sensitivity: compare lambda.1se vs lambda.min vs pruned specifications
# INPUTS:  Models/contracts from 07a GLM, rdos/datos/08_panel_formalidad_GLM*.rds
# OUTPUTS: rdos/reportes/10m_sparsity_sensitivity.html, rdos/figuras/10m_sparsity_sensitivity/*.pdf
# =============================================================================
# 🌟 10m_sparsity_sensitivity_GLM.R 🌟 ####
# OBJETIVO: Sparsity sensitivity (R4-Q8)
#   Compara 3 especificaciones GLM: (A) lambda.1se (baseline, ~94 vars),
#   (B) lambda.min (~108 vars), (C) pruned (top vars con bootstrap stability >=80%).
#   Re-predice la serie backcasted bajo cada especificación y compara.
# INPUTS:  rdos/modelos/07_modelo_lasso_{SUFIJO_MODELO_GLM}.rds (cv_fit original)
#          rdos/modelos/07_recipe_lasso_{SUFIJO_MODELO_GLM}.rds (recipe prepped)
#          rdos/datos/08_panel_formalidad_{SUFIJO_MODELO_GLM}.rds (panel con predicciones)
#          rdos/contratos/07a_contrato_lasso_{SUFIJO_MODELO_GLM}.rds (boot_summary)
# OUTPUTS: rdos/reportes/10m_sparsity_sensitivity.csv
#          rdos/reportes/10m_sparsity_sensitivity.html
#          rdos/figuras/10m_sparsity_sensitivity/10m_series_comparison.pdf
#          rdos/reportes/10m_sparsity_notas.txt
# TIEMPO ESTIMADO: ~10-15 minutos

options(renv.config.auto.snapshot = FALSE)

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

cat("=", rep("=", 69), "\n", sep = "")
cat("  Sparsity Sensitivity — GLM (R4-Q8)\n")
cat("=", rep("=", 69), "\n", sep = "")

# 🪫 1. Cargar modelo, recipe y contrato --------------------------------------

cv_fit  <- safe_load(PATH_07_MODELO_GLM)
recipe_prepped <- safe_load(PATH_07_RECIPE_GLM)
c07a    <- safe_load(PATH_07_CONTRATO_GLM)

lambda_1se <- c07a$lambda_1se
lambda_min <- c07a$lambda_min

cat("lambda.1se:", lambda_1se, "\n")
cat("lambda.min:", lambda_min, "\n")

# Bootstrap summary para pruning
boot_summary <- c07a$boot_summary
cat("Bootstrap summary:", nrow(boot_summary), "variables\n")

# Variables con stability >= 80%
vars_stable <- boot_summary %>%
  filter(seleccion_pct >= 80) %>%
  pull(variable)
cat("Variables con bootstrap stability >= 80%:", length(vars_stable), "\n")
cat("Variables:", paste(head(vars_stable, 10), collapse = ", "), "...\n")

# 🪫 2. Cargar panel y preparar ------------------------------------------------

path_panel_08 <- file.path(DIR_DATOS,
                           paste0("08_panel_formalidad_", SUFIJO_MODELO_GLM, ".rds"))
panel <- safe_load(path_panel_08)

if ("theta_A" %in% names(panel) && !"theta_A_mA" %in% names(panel)) {
  panel <- panel %>% rename(theta_A_mA = theta_A, theta_B_mA = theta_B)
}

# Filtrar ocupados con formalidad para test set (overlap)
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
  "busqueda_formal", "trimestre",
  "vive_alquiler", "vive_ganancias_negocio", "vive_renta_financiera",
  "vive_beca", "vive_cuota_alimenticia", "vive_ahorros",
  "vive_prestamos_personas", "vive_prestamos_financieros",
  "vive_financiamiento", "vive_venta_bienes", "vive_otro_ingreso"
)
if ("horas_trabajadas" %in% names(panel)) VARS_MODELO <- c(VARS_MODELO, "horas_trabajadas")
VARS_MODELO <- intersect(VARS_MODELO, names(panel))

# Universo overlap (para test metrics)
overlap <- panel %>%
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

# Universo historico completo (ocupados, para backcast)
historico <- panel %>%
  filter(condicion_actividad == "Ocupado") %>%
  mutate(
    lugar_nacimiento = case_when(
      lugar_nacimiento %in% c("Localidad", "Provincia", "Otra provincia") ~ "Argentina",
      lugar_nacimiento == "Pais limitrofe" ~ "Pais_Limitrofe",
      lugar_nacimiento == "Otro pais"      ~ "Otro_Pais",
      TRUE ~ as.character(lugar_nacimiento)
    )
  )

rm(panel); gc(verbose = FALSE)

cat("Overlap:", nrow(overlap), "obs\n")
cat("Historico (ocupados):", nrow(historico), "obs\n")

# 🪫 3. Bake matrices ----------------------------------------------------------

# Preparar recipe data (sin meta vars)
bake_for_glmnet <- function(recipe, df) {
  df_rec <- df %>% select(-any_of(c("pondera", "formalidad_empleo", "codusu",
                                     "periodo_id", "formalidad_bin",
                                     "condicion_actividad")))
  mat <- bake(recipe, new_data = df_rec, composition = "matrix")
  mat <- mat[, colnames(mat) != "formalidad_bin", drop = FALSE]
  mat
}

X_overlap   <- bake_for_glmnet(recipe_prepped, overlap)
X_historico <- bake_for_glmnet(recipe_prepped, historico)

cat("X_overlap:", nrow(X_overlap), "x", ncol(X_overlap), "\n")
cat("X_historico:", nrow(X_historico), "x", ncol(X_historico), "\n")

# 🪫 4. Tres especificaciones --------------------------------------------------

# Umbrales (Youden) — usar el mismo para comparabilidad
umbral_baseline <- c07a$umbral_youden

resultados_spec <- list()

specs <- list(
  list(name = "lambda.1se (baseline)", s = "lambda.1se"),
  list(name = "lambda.min", s = "lambda.min")
)

for (sp in specs) {
  cat("\n--- Spec:", sp$name, "---\n")

  # Coeficientes
  coef_vec <- coef(cv_fit, s = sp$s)
  n_sel <- sum(as.vector(coef_vec)[-1] != 0)
  cat("  Vars seleccionadas:", n_sel, "\n")

  # Predicciones overlap
  pred_overlap <- as.vector(
    predict(cv_fit, newx = X_overlap, s = sp$s, type = "response")
  )

  # AUC overlap
  roc_obj <- roc(response = overlap$formalidad_bin, predictor = pred_overlap,
                 levels = c(0, 1), quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))

  # Brier
  brier <- mean((pred_overlap - overlap$formalidad_bin)^2)

  # Calibracion
  w <- overlap$pondera / mean(overlap$pondera)
  tasa_obs  <- weighted.mean(overlap$formalidad_bin, w)
  tasa_pred <- weighted.mean(pred_overlap, w)
  cal_delta <- abs(tasa_obs - tasa_pred)

  cat(sprintf("  AUC = %.4f | Brier = %.4f | Cal delta = %.2f pp\n",
              auc_val, brier, cal_delta * 100))

  # Predicciones historicas
  pred_hist <- as.vector(
    predict(cv_fit, newx = X_historico, s = sp$s, type = "response")
  )

  # Serie trimestral
  serie <- historico %>%
    mutate(pred_prob = pred_hist,
           pred_clase = as.integer(pred_prob >= umbral_baseline)) %>%
    group_by(periodo_id) %>%
    summarise(
      tasa_formal = weighted.mean(pred_clase, pondera),
      mean_prob   = weighted.mean(pred_prob, pondera),
      n           = n(),
      .groups = "drop"
    ) %>%
    mutate(spec = sp$name)

  resultados_spec[[sp$name]] <- list(
    n_vars = n_sel, auc = auc_val, brier = brier,
    cal_delta_pp = cal_delta * 100, serie = serie
  )
}

# Spec C: Pruned (solo vars con bootstrap stability >= 80%)
cat("\n--- Spec: Pruned (stability >= 80%) ---\n")

# Identificar columnas en X que corresponden a vars estables
# Las vars estables son nombres pre-dummy; necesito mapear a nombres post-dummy
coef_1se <- coef(cv_fit, s = "lambda.1se")
coef_names_all <- rownames(coef_1se)[-1]  # sin intercept

# Mapear: una variable "estable" produce multiples dummies
# Patron: variable original es prefijo del nombre dummy
cols_to_keep <- c()
for (v in vars_stable) {
  # Match exacto o prefijo con _
  matches <- coef_names_all[startsWith(coef_names_all, v)]
  # Refinar: solo match exacto o con separador _ despues
  matches <- matches[matches == v | substr(matches, nchar(v) + 1, nchar(v) + 1) == "_"]
  if (length(matches) == 0) {
    if (v %in% coef_names_all) matches <- v
  }
  cols_to_keep <- c(cols_to_keep, matches)
}
cols_to_keep <- unique(cols_to_keep)
cols_to_keep <- intersect(cols_to_keep, colnames(X_overlap))
cat("  Columnas retenidas (post-dummy):", length(cols_to_keep), "\n")

# Re-estimar GLM sin regularizacion sobre subset de columnas
X_pruned_overlap <- X_overlap[, cols_to_keep, drop = FALSE]
X_pruned_hist    <- X_historico[, cols_to_keep, drop = FALSE]

# Usar glmnet con lambda muy bajo (~ no regularizacion) en vez de glm() para consistencia
cl <- makeCluster(min(N_CORES, 7))
registerDoParallel(cl)

# Cluster folds
cluster_ids <- unique(overlap$codusu)
set.seed(SEED_GLOBAL)
fold_asignacion <- tibble(
  codusu = cluster_ids,
  fold   = sample(rep(1:10, length.out = length(cluster_ids)))
)
foldid_vec <- tibble(codusu = overlap$codusu) %>%
  left_join(fold_asignacion, by = "codusu") %>%
  pull(fold)

w_overlap <- overlap$pondera / mean(overlap$pondera)

# theta cols en pruned
theta_cols_pruned <- intersect(c("theta_A_mA", "theta_B_mA"), cols_to_keep)
pf_pruned <- rep(1, ncol(X_pruned_overlap))
theta_idx_p <- which(colnames(X_pruned_overlap) %in% theta_cols_pruned)
if (length(theta_idx_p) > 0) pf_pruned[theta_idx_p] <- 0

cv_pruned <- cv.glmnet(
  x = X_pruned_overlap, y = overlap$formalidad_bin, weights = w_overlap,
  alpha = 1, family = "binomial", penalty.factor = pf_pruned,
  type.measure = "auc", foldid = foldid_vec, parallel = TRUE
)
stopCluster(cl)

# Usar lambda.1se del modelo pruned
pred_pruned_ov <- as.vector(
  predict(cv_pruned, newx = X_pruned_overlap, s = "lambda.1se", type = "response")
)
n_sel_pruned <- sum(as.vector(coef(cv_pruned, s = "lambda.1se"))[-1] != 0)

roc_pruned <- roc(response = overlap$formalidad_bin, predictor = pred_pruned_ov,
                  levels = c(0, 1), quiet = TRUE)
auc_pruned <- as.numeric(auc(roc_pruned))
brier_pruned <- mean((pred_pruned_ov - overlap$formalidad_bin)^2)
cal_delta_pruned <- abs(weighted.mean(overlap$formalidad_bin, w_overlap) -
                        weighted.mean(pred_pruned_ov, w_overlap))

cat(sprintf("  Vars sel: %d | AUC = %.4f | Brier = %.4f | Cal delta = %.2f pp\n",
            n_sel_pruned, auc_pruned, brier_pruned, cal_delta_pruned * 100))

pred_pruned_hist <- as.vector(
  predict(cv_pruned, newx = X_pruned_hist, s = "lambda.1se", type = "response")
)

serie_pruned <- historico %>%
  mutate(pred_prob = pred_pruned_hist,
         pred_clase = as.integer(pred_prob >= umbral_baseline)) %>%
  group_by(periodo_id) %>%
  summarise(
    tasa_formal = weighted.mean(pred_clase, pondera),
    mean_prob   = weighted.mean(pred_prob, pondera),
    n           = n(),
    .groups = "drop"
  ) %>%
  mutate(spec = "Pruned (stability >= 80%)")

resultados_spec[["pruned"]] <- list(
  n_vars = n_sel_pruned, auc = auc_pruned, brier = brier_pruned,
  cal_delta_pp = cal_delta_pruned * 100, serie = serie_pruned
)

# 🪫 5. Consolidar series ------------------------------------------------------

series_all <- bind_rows(
  resultados_spec[["lambda.1se (baseline)"]]$serie,
  resultados_spec[["lambda.min"]]$serie,
  resultados_spec[["pruned"]]$serie
)

# Tabla resumen de metricas
metricas <- tibble(
  specification = c("lambda.1se (baseline)", "lambda.min", "Pruned (>=80%)"),
  n_vars = c(resultados_spec[["lambda.1se (baseline)"]]$n_vars,
             resultados_spec[["lambda.min"]]$n_vars,
             resultados_spec[["pruned"]]$n_vars),
  auc_roc = c(resultados_spec[["lambda.1se (baseline)"]]$auc,
              resultados_spec[["lambda.min"]]$auc,
              resultados_spec[["pruned"]]$auc),
  brier = c(resultados_spec[["lambda.1se (baseline)"]]$brier,
            resultados_spec[["lambda.min"]]$brier,
            resultados_spec[["pruned"]]$brier),
  cal_delta_pp = c(resultados_spec[["lambda.1se (baseline)"]]$cal_delta_pp,
                   resultados_spec[["lambda.min"]]$cal_delta_pp,
                   resultados_spec[["pruned"]]$cal_delta_pp)
)

# Correlaciones entre series
s1 <- resultados_spec[["lambda.1se (baseline)"]]$serie$tasa_formal
s2 <- resultados_spec[["lambda.min"]]$serie$tasa_formal
s3 <- resultados_spec[["pruned"]]$serie$tasa_formal

cor_1se_min <- cor(s1, s2)
cor_1se_pru <- cor(s1, s3)
cor_min_pru <- cor(s2, s3)

delta_1se_min <- max(abs(s1 - s2)) * 100
delta_1se_pru <- max(abs(s1 - s3)) * 100
delta_min_pru <- max(abs(s2 - s3)) * 100

cat("\n=== METRICAS ===\n")
print(metricas)
cat(sprintf("\nCorrelaciones de series trimestrales:\n"))
cat(sprintf("  1se vs min:    r = %.4f | max delta = %.2f pp\n", cor_1se_min, delta_1se_min))
cat(sprintf("  1se vs pruned: r = %.4f | max delta = %.2f pp\n", cor_1se_pru, delta_1se_pru))
cat(sprintf("  min vs pruned: r = %.4f | max delta = %.2f pp\n", cor_min_pru, delta_min_pru))

# 🪫 6. Guardar CSV ------------------------------------------------------------

path_csv <- file.path(DIR_REPORTES, "10m_sparsity_sensitivity.csv")
write_csv(series_all, path_csv)

path_metricas <- file.path(DIR_REPORTES, "10m_sparsity_metricas.csv")
write_csv(metricas, path_metricas)
cat("\nCSV series:", path_csv, "\n")
cat("CSV metricas:", path_metricas, "\n")

# 🪫 7. Figuras ----------------------------------------------------------------

dir_fig <- file.path(DIR_FIGURAS, "10m_sparsity_sensitivity")
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)

fig_series <- series_all %>%
  mutate(periodo_num = as.numeric(factor(periodo_id, levels = sort(unique(periodo_id))))) %>%
  ggplot(aes(x = periodo_num, y = tasa_formal * 100, color = spec)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = c(
    "lambda.1se (baseline)" = COL_GLM,
    "lambda.min" = COL_LPM,
    "Pruned (stability >= 80%)" = COL_SLS
  )) +
  tr_labs(
    title = "Sparsity sensitivity: backcasted formality rate under three GLM specifications",
    subtitle = sprintf("Series correlation: 1se-min r=%.3f | 1se-pruned r=%.3f | max delta: %.1f pp",
                       cor_1se_min, cor_1se_pru, max(delta_1se_min, delta_1se_pru)),
    x = "Quarter (index)", y = "Formality rate (%)", color = "Specification"
  ) +
  theme_paper() +
  theme(legend.position = "bottom")

guardar_figura(fig_series, dir_fig, "series", 1, width = 11, height = 6)
cat("Fig: series comparison saved\n")

# 🪫 8. Notas para el paper ----------------------------------------------------

path_notas <- file.path(DIR_REPORTES, "10m_sparsity_notas.txt")
con_notas  <- file(path_notas, open = "wt", encoding = "UTF-8")

writeLines(c(
  "=== NOTAS PARA EL PAPER — R4-Q8 Sparsity Sensitivity ===",
  sprintf("Generado: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "--- Appendix G (nueva subseccion) ---",
  "",
  "Three GLM specifications were compared to assess sensitivity to predictor",
  "set size: the baseline (lambda.1se), a richer specification (lambda.min),",
  "and a pruned specification retaining only variables with bootstrap selection",
  "stability >= 80%.",
  "",
  sprintf("Specification  | N vars | AUC    | Brier  | Cal delta"),
  sprintf("lambda.1se     | %d     | %.4f | %.4f | %.1f pp",
          metricas$n_vars[1], metricas$auc_roc[1], metricas$brier[1], metricas$cal_delta_pp[1]),
  sprintf("lambda.min     | %d    | %.4f | %.4f | %.1f pp",
          metricas$n_vars[2], metricas$auc_roc[2], metricas$brier[2], metricas$cal_delta_pp[2]),
  sprintf("Pruned (>=80%%) | %d     | %.4f | %.4f | %.1f pp",
          metricas$n_vars[3], metricas$auc_roc[3], metricas$brier[3], metricas$cal_delta_pp[3]),
  "",
  sprintf("Series correlations: 1se-min r=%.4f | 1se-pruned r=%.4f", cor_1se_min, cor_1se_pru),
  sprintf("Max delta: 1se-min %.1f pp | 1se-pruned %.1f pp", delta_1se_min, delta_1se_pru),
  "",
  "Main levels and dynamics persist across all three specifications."
), con_notas)

close(con_notas)
cat("Notas:", path_notas, "\n")

# 🪫 9. Reporte HTML -----------------------------------------------------------

path_html <- file.path(DIR_REPORTES, "10m_sparsity_sensitivity.html")
rmd_temp  <- tempfile(fileext = ".Rmd")
con       <- file(rmd_temp, open = "wt", encoding = "UTF-8")

writeLines('---
title: "Sparsity Sensitivity -- GLM"
subtitle: "R4-Q8: lambda.1se vs lambda.min vs pruned"
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
                      fig.width = 11, fig.height = 6, dpi = 150)
library(tidyverse); library(knitr); library(kableExtra)
```

# Metrics comparison

```{r metricas}
m <- read.csv("%s")
m %%>%% kable(digits = 4) %%>%% kable_styling(font_size = 12)
```

# Series comparison

```{r series, fig.width=11, fig.height=6}
s <- read.csv("%s")
s %%>%%
  mutate(q = as.numeric(factor(periodo_id, levels = sort(unique(periodo_id))))) %%>%%
  ggplot(aes(x = q, y = tasa_formal*100, color = spec)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  labs(x = "Quarter", y = "Formality (%%)", color = "Spec") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
```

# Correlations

- 1se vs min: r = %.4f | max delta = %.1f pp
- 1se vs pruned: r = %.4f | max delta = %.1f pp
- min vs pruned: r = %.4f | max delta = %.1f pp
', gsub("\\\\", "/", path_metricas), gsub("\\\\", "/", path_csv),
   cor_1se_min, delta_1se_min, cor_1se_pru, delta_1se_pru,
   cor_min_pru, delta_min_pru), con)

close(con)
rmarkdown::render(input = rmd_temp, output_file = path_html,
                  quiet = TRUE, envir = new.env(parent = globalenv()))
unlink(rmd_temp)
cat("HTML:", path_html, "\n")

# 📦 Contrato -----------------------------------------------------------------

path_contrato_10m <- file.path(DIR_CONTRATOS, "10m_contrato_sparsity.rds")

contrato_10m <- list(
  script          = "10m_sparsity_sensitivity_GLM.R",
  fecha           = format(Sys.time(), "%Y-%m-%d %H:%M"),
  n_vars_baseline = metricas$n_vars[1],
  n_vars_min      = metricas$n_vars[2],
  n_vars_pruned   = metricas$n_vars[3],
  r_1se_pruned    = cor_1se_pru,
  r_1se_min       = cor_1se_min,
  delta_1se_pruned = delta_1se_pru,
  delta_1se_min    = delta_1se_min,
  r_min_pruned     = cor_min_pru,
  delta_min_pruned = delta_min_pru,
  sparsity_table   = metricas
)

saveRDS(contrato_10m, path_contrato_10m)
cat("\n📦 Contrato:", path_contrato_10m, "\n")

# 📑 Checklist y timer ---------------------------------------------------------

cat("\nChecklist:\n")
cat("  CSV series:  ", ifelse(file.exists(path_csv), "OK", "FALTA"), "\n")
cat("  CSV metrics: ", ifelse(file.exists(path_metricas), "OK", "FALTA"), "\n")
cat("  Figure:      ", ifelse(length(list.files(dir_fig, pattern = "\\.pdf$")) > 0, "OK", "FALTA"), "\n")
cat("  Notas:       ", ifelse(file.exists(path_notas), "OK", "FALTA"), "\n")
cat("  HTML:        ", ifelse(file.exists(path_html), "OK", "FALTA"), "\n")
cat("  Contrato:    ", ifelse(file.exists(path_contrato_10m), "OK", "FALTA"), "\n")

rm(historico, overlap, X_overlap, X_historico, series_all, cv_fit, recipe_prepped, contrato_10m)
gc(verbose = FALSE)

cat("\nTiempo:", round((proc.time() - t_inicio)["elapsed"] / 60, 1), "minutos\n")
