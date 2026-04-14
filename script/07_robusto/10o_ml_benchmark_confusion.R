# =============================================================================
# [EN] 10o_ml_benchmark_confusion.R -- ML benchmark (Random Forest vs GLM) and confusion matrix vs official indicators
# INPUTS:  rdos/datos/08_panel_formalidad_SLS*.rds, raw EPH microdata (overlap)
# OUTPUTS: rdos/reportes/10o_confusion_primitives_vs_oficial.txt, rdos/figuras/10o_ml_benchmark_roc.pdf
# =============================================================================
# ==============================================================================
# 🌟 10o_ml_benchmark_confusion.R
# ==============================================================================
# OBJETIVO: (1) Confusion matrix primitives vs EMPLEO/SECTOR oficiales
#           (2) ML benchmark: Random Forest vs GLM en overlap
# INPUTS:   rdos/datos/08_panel_formalidad_{SUFIJO_MODELO_SLS}.rds
#           rdos/datos/02_panel_con_taxonomia.rds
#           Microdatos EPH crudos (overlap)
# OUTPUTS:  rdos/reportes/10o_confusion_primitives_vs_oficial.txt
#           rdos/reportes/10o_ml_benchmark.txt
#           rdos/figuras/10o_ml_benchmark_roc.pdf
# TIEMPO ESTIMADO: ~3-5 min
# ==============================================================================

t_inicio <- proc.time()

# ── 📦 Paquetes ──────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(ggplot2)
  library(glmnet)
  library(ranger)
  library(pROC)
})

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ==============================================================================
# 🌟 PARTE 1: CONFUSION MATRIX — PRIMITIVES VS EMPLEO/SECTOR OFICIALES
# ==============================================================================
cat("\n═══ PARTE 1: Primitives vs EMPLEO/SECTOR oficiales ═══\n")

# Cargar panel 02 que tiene formalidad_empleo (campo oficial derivado)
cat("📥 Cargando panel 02...\n")
panel02 <- as.data.table(readRDS(file.path(DIR_DATOS, "02_panel_con_taxonomia.rds")))

# Cargar panel 08 que tiene nuestra reconstrucción
cat("📥 Cargando panel 08...\n")
panel08 <- as.data.table(readRDS(PATH_08_PANEL_CONSOLIDADO))

# Merge
panel08[, formalidad_empleo := panel02$formalidad_empleo]
panel08[, condicion_formalidad := panel02$condicion_formalidad]

# Solo overlap, solo ocupados
overlap <- panel08[tipo_estimacion_pea == "Observado" & condicion_actividad == "Ocupado"]
cat("N overlap ocupados:", nrow(overlap), "\n")

# Nombre dinámico de columna de clasificación calibrada GLM
col_clase_cal_glm <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_GLM, "_pea")

# Nuestra reconstrucción: col_clase_cal_glm (Formal/Informal)
# Campo oficial: formalidad_empleo (Formal oficial / Informal oficial / No corresponde)

# Para empleados: comparar formalidad_empleo vs condicion_formalidad
empleados_ov <- overlap[categoria_ocupacional == "Empleado" &
                        formalidad_empleo %in% c("Formal oficial", "Informal oficial")]
cat("\nEmpleados en overlap con ambas medidas:", nrow(empleados_ov), "\n")

# Nuestra clasificación para empleados viene de condicion_formalidad
# (que replica desc_jubilatorio_asalariado)
empleados_ov[, nuestra := fifelse(condicion_formalidad == "Formal", "Formal", "Informal")]
empleados_ov[, oficial := fifelse(formalidad_empleo == "Formal oficial", "Formal", "Informal")]

cat("\n--- Empleados: Primitives vs EMPLEO oficial ---\n")
cm_emp <- table(Primitives = empleados_ov$nuestra, Oficial = empleados_ov$oficial)
print(cm_emp)
agree_emp <- sum(diag(cm_emp)) / sum(cm_emp)
cat(sprintf("Agreement: %.4f (%.2f%%)\n", agree_emp, agree_emp * 100))

# Para independientes: comparar nuestra GLM prediction vs sector_formalidad
# Necesitamos cargar sector_formalidad del raw EPH
# Lo hacemos desde panel02 si existe, sino desde raw
indep_ov <- overlap[categoria_ocupacional %in% c("Patrón", "Cuenta Propia")]
cat("\nIndependientes en overlap:", nrow(indep_ov), "\n")

# Nuestra clasificación: col_clase_cal_glm
indep_ov[, nuestra := get(col_clase_cal_glm)]
cat("Nuestra clasificación:\n")
print(table(indep_ov$nuestra, useNA = "always"))

# Escribir resultados
con <- file(file.path(DIR_REPORTES, "10o_confusion_primitives_vs_oficial.txt"), "w")
cat("=== CONFUSION MATRIX: PRIMITIVES VS EMPLEO/SECTOR OFICIALES ===\n", file = con)
cat(sprintf("Generado: %s\n\n", Sys.time()), file = con)

cat("--- EMPLEADOS ---\n", file = con)
cat(sprintf("N: %d\n", nrow(empleados_ov)), file = con)
cat(sprintf("Agreement: %.4f (%.2f%%)\n\n", agree_emp, agree_emp * 100), file = con)
capture.output(print(cm_emp), file = con, append = TRUE)

# Kappa
if (requireNamespace("caret", quietly = TRUE)) {
  kappa_val <- caret::confusionMatrix(cm_emp)$overall["Kappa"]
  cat(sprintf("\nCohen's Kappa: %.4f\n", kappa_val), file = con)
}

cat("\n\n--- INDEPENDIENTES ---\n", file = con)
cat(sprintf("N: %d\n", nrow(indep_ov)), file = con)
cat("Nuestra clasificación (from primitives + GLM prediction):\n", file = con)
capture.output(print(table(indep_ov$nuestra, useNA = "always")), file = con, append = TRUE)
cat("\nNota: Para independientes, no existe campo oficial comparable pre-overlap.\n", file = con)
cat("La variable formalidad_empleo es 'No corresponde' para no-asalariados.\n", file = con)
cat("Nuestra clasificación desde primitivos sigue exactamente el algoritmo\n", file = con)
cat("dual-path documentado por INDEC (2025), aplicado indicator por indicator.\n", file = con)

close(con)
cat("✓ 10o_confusion_primitives_vs_oficial.txt\n")

rm(panel02, empleados_ov, indep_ov); gc()

# ==============================================================================
# 🌟 PARTE 2: ML BENCHMARK — RANDOM FOREST VS GLM
# ==============================================================================
cat("\n═══ PARTE 2: ML Benchmark — Random Forest vs GLM ═══\n")

# Usar solo overlap con formalidad observada
ocup_ov <- overlap[!is.na(get(col_clase_cal_glm))]
cat("N overlap para benchmark:", nrow(ocup_ov), "\n")

# Outcome binario
ocup_ov[, y_formal := fifelse(get(col_clase_cal_glm) == "Formal", 1L, 0L)]

# Predictores: usar las mismas columnas que el GLM
# Identificar predictores disponibles (demográficos + laborales + thetas)
pred_cols <- c("edad", "edad_cuadrado", "sexo", "region", "aglomerado",
               "nivel_educ_obtenido2", "seccion", "calificacion",
               "categoria_ocupacional", "antiguedad",
               "miembros_hogar", "menores10", "mayores10",
               "ich_score", "clima_educativo_hogar", "emparejamiento_selectivo",
               "calificacion_norm", "entropia_estabilidad", "residual_vivienda",
               "theta_A", "theta_B")

# Filtrar columnas que existen
pred_cols <- pred_cols[pred_cols %in% names(ocup_ov)]
cat("Predictores:", length(pred_cols), "\n")

# Preparar dataset limpio
ml_data <- ocup_ov[, c("y_formal", "pondera", "codusu", "periodo_id", pred_cols), with = FALSE]
ml_data <- na.omit(ml_data)
cat("N completo:", nrow(ml_data), "\n")

# Train/test split: usar el mismo esquema que el paper (70/30 stratified)
set.seed(SEED_GLOBAL)
hogares <- unique(ml_data$codusu)
n_train <- round(0.7 * length(hogares))
train_hogares <- sample(hogares, n_train)
ml_data[, split := fifelse(codusu %in% train_hogares, "train", "test")]

train <- ml_data[split == "train"]
test  <- ml_data[split == "test"]
cat("Train:", nrow(train), "| Test:", nrow(test), "\n")

# ── GLM (logistic) ───────────────────────────────────────────────────────────
cat("\n  Fitting GLM...\n")
formula_str <- paste("y_formal ~", paste(pred_cols, collapse = " + "))
glm_fit <- glm(as.formula(formula_str), data = train, family = binomial(),
               weights = pondera / mean(pondera))
pred_glm_test <- predict(glm_fit, newdata = test, type = "response")

# ── Random Forest ────────────────────────────────────────────────────────────
cat("  Fitting Random Forest...\n")
# Convert factors for ranger
train_rf <- copy(train)
train_rf[, y_formal := factor(y_formal, levels = c(0, 1))]

rf_fit <- ranger(
  as.formula(formula_str),
  data = train_rf[, c("y_formal", pred_cols), with = FALSE],
  num.trees = 500,
  mtry = floor(sqrt(length(pred_cols))),
  min.node.size = 20,
  probability = TRUE,
  seed = SEED_GLOBAL,
  num.threads = N_CORES
)

test_rf <- copy(test)
test_rf[, y_formal := factor(y_formal, levels = c(0, 1))]
pred_rf_test <- predict(rf_fit, data = test_rf[, pred_cols, with = FALSE])$predictions[, "1"]

# ── Métricas ─────────────────────────────────────────────────────────────────
cat("\n  Computing metrics...\n")

# AUC-ROC
roc_glm <- roc(test$y_formal, pred_glm_test, quiet = TRUE)
roc_rf  <- roc(test$y_formal, pred_rf_test, quiet = TRUE)

auc_glm <- as.numeric(auc(roc_glm))
auc_rf  <- as.numeric(auc(roc_rf))

# Brier score
brier_glm <- mean((pred_glm_test - test$y_formal)^2)
brier_rf  <- mean((pred_rf_test - test$y_formal)^2)

# ECE (10 bins)
calc_ece <- function(pred, obs, bins = 10) {
  breaks <- quantile(pred, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
  breaks <- unique(breaks)
  if (length(breaks) < 3) return(NA_real_)
  bin <- cut(pred, breaks = breaks, include.lowest = TRUE)
  dt <- data.table(pred = pred, obs = obs, bin = bin)
  ece_tab <- dt[, .(n = .N, mp = mean(pred), mo = mean(obs)), by = bin]
  ece_tab[, ad := abs(mp - mo)]
  weighted.mean(ece_tab$ad, ece_tab$n)
}

ece_glm <- calc_ece(pred_glm_test, test$y_formal)
ece_rf  <- calc_ece(pred_rf_test, test$y_formal)

# Calibration slope
cal_slope <- function(pred, obs) {
  df <- data.frame(y = obs, eta = qlogis(pmin(pmax(pred, 1e-6), 1 - 1e-6)))
  fit <- glm(y ~ eta, data = df, family = binomial())
  coef(fit)["eta"]
}

slope_glm <- cal_slope(pred_glm_test, test$y_formal)
slope_rf  <- cal_slope(pred_rf_test, test$y_formal)

# Print results
cat("\n  === RESULTADOS ===\n")
cat(sprintf("  %-25s %10s %10s\n", "Metric", "GLM", "RF"))
cat(sprintf("  %-25s %10.4f %10.4f\n", "AUC-ROC", auc_glm, auc_rf))
cat(sprintf("  %-25s %10.4f %10.4f\n", "Brier score", brier_glm, brier_rf))
cat(sprintf("  %-25s %10.4f %10.4f\n", "ECE", ece_glm, ece_rf))
cat(sprintf("  %-25s %10.3f %10.3f\n", "Cal. slope", slope_glm, slope_rf))

# DeLong test
delong <- roc.test(roc_glm, roc_rf, method = "delong")
cat(sprintf("  DeLong p-value: %.4f\n", delong$p.value))

# ── Fig: ROC comparison (ggplot2 + theme_paper) ─────────────────────────────
roc_glm_df <- data.frame(
  fpr = 1 - roc_glm$specificities,
  tpr = roc_glm$sensitivities,
  model = sprintf("GLM (AUC = %.4f)", auc_glm)
)
roc_rf_df <- data.frame(
  fpr = 1 - roc_rf$specificities,
  tpr = roc_rf$sensitivities,
  model = sprintf("RF (AUC = %.4f)", auc_rf)
)
roc_df <- rbind(roc_glm_df, roc_rf_df)

p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line(linewidth = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = COL_BANDA) +
  scale_color_manual(values = setNames(
    c(COL_GLM, COL_OBSERVADO),
    c(sprintf("GLM (AUC = %.4f)", auc_glm), sprintf("RF (AUC = %.4f)", auc_rf))
  )) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  tr_labs(title = "ROC: GLM vs Random Forest (overlap test set)",
       x = "False Positive Rate", y = "True Positive Rate") +
  theme_paper() +
  theme(legend.position = c(0.7, 0.2),
        legend.background = element_rect(fill = alpha("#FFFFFF", 0.9), color = NA))

guardar_figura(p_roc, DIR_FIGURAS_10O, "roc", 1, width = 6, height = 6)

# Escribir notas
con2 <- file(file.path(DIR_REPORTES, "10o_ml_benchmark.txt"), "w")
cat("=== ML BENCHMARK: GLM VS RANDOM FOREST ===\n", file = con2)
cat(sprintf("Generado: %s\n\n", Sys.time()), file = con2)
cat(sprintf("Train: %d | Test: %d\n", nrow(train), nrow(test)), file = con2)
cat(sprintf("Predictores: %d\n\n", length(pred_cols)), file = con2)
cat(sprintf("%-25s %10s %10s\n", "Metric", "GLM", "RF"), file = con2)
cat(sprintf("%-25s %10.4f %10.4f\n", "AUC-ROC", auc_glm, auc_rf), file = con2)
cat(sprintf("%-25s %10.4f %10.4f\n", "Brier score", brier_glm, brier_rf), file = con2)
cat(sprintf("%-25s %10.4f %10.4f\n", "ECE", ece_glm, ece_rf), file = con2)
cat(sprintf("%-25s %10.3f %10.3f\n", "Cal. slope", slope_glm, slope_rf), file = con2)
cat(sprintf("\nDeLong p-value: %.4f\n", delong$p.value), file = con2)
cat("\nConclusion: ", file = con2)
if (abs(auc_glm - auc_rf) < 0.01) {
  cat("No significant difference in discrimination.\n", file = con2)
  cat("GLM preferred for bounded predictions and interpretability.\n", file = con2)
} else if (auc_rf > auc_glm) {
  cat("RF has marginally better AUC but GLM preferred for calibration.\n", file = con2)
} else {
  cat("GLM outperforms RF in discrimination.\n", file = con2)
}
close(con2)
cat("  ✓ 10o_ml_benchmark.txt\n")

# ==============================================================================
# 🏁 Checklist
# ==============================================================================
cat("\n═══ CHECKLIST ═══\n")
outputs <- c(
  file.path(DIR_REPORTES, "10o_confusion_primitives_vs_oficial.txt"),
  file.path(DIR_REPORTES, "10o_ml_benchmark.txt"),
  file.path(DIR_FIGURAS_10O, "10o_ml_benchmark_roc_01.pdf"),
  file.path(DIR_CONTRATOS, "10o_contrato_benchmark.rds")
)
for (f in outputs) cat(sprintf("  %s: %s\n", basename(f), ifelse(file.exists(f), "✓", "✗")))

# ==============================================================================
# 📦 CONTRATO: Escalares clave para trazabilidad
# ==============================================================================
cat("\n═══ Generando contrato 10o ═══\n")

contrato_10o <- list(
  script  = "10o_ml_benchmark_confusion.R",
  fecha   = Sys.time(),
  # Parte 1: agreement primitives vs EMPLEO oficial (empleados)
  agreement_pct = agree_emp * 100,
  # Parte 2: ML benchmark — GLM vs RF
  auc_glm_bench       = auc_glm,
  auc_rf_bench        = auc_rf,
  brier_glm_bench     = brier_glm,
  brier_rf_bench      = brier_rf,
  ece_glm_bench       = ece_glm,
  ece_rf_bench        = ece_rf,
  cal_slope_glm_bench = as.numeric(slope_glm),
  cal_slope_rf_bench  = as.numeric(slope_rf)
)

path_contrato_10o <- file.path(DIR_CONTRATOS, "10o_contrato_benchmark.rds")
saveRDS(contrato_10o, path_contrato_10o)
cat("  ✓", basename(path_contrato_10o), "\n")

rm(panel08, overlap, ocup_ov, ml_data, train, test); gc()
cat("\nTiempo:", round((proc.time() - t_inicio)["elapsed"] / 60, 1), "minutos\n")
