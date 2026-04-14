# =============================================================================
# [EN] 10n_subgroup_stability_GLM.R -- Subgroup stability: bridged GLM and gap by region, sector, gender, age, occupation
# INPUTS:  rdos/datos/08_panel_formalidad_SLS*.rds, rdos/datos/02_panel_con_taxonomia.rds
# OUTPUTS: rdos/reportes/10n_subgroup_stability.html, rdos/figuras/10n_*.pdf
# =============================================================================
# ==============================================================================
# 🌟 10n_subgroup_stability_GLM.R
# ==============================================================================
# OBJETIVO: Evaluar estabilidad del bridged GLM y del gap legacy-hybrid
#           por subgrupos (región, sector, género, edad, cat. ocupacional).
#           Incluye confusion matrix disaggregada y ECE.
# INPUTS:   rdos/datos/08_panel_formalidad_{SUFIJO_MODELO_SLS}.rds (hybrid GLM)
#           rdos/datos/02_panel_con_taxonomia.rds (legacy measure)
# OUTPUTS:  rdos/reportes/10n_subgroup_stability.html
#           rdos/reportes/10n_subgroup_stability_notas.txt
#           rdos/figuras/10n_subgroup_gap_region.pdf
#           rdos/figuras/10n_subgroup_gap_sector.pdf
#           rdos/figuras/10n_subgroup_gap_catocup.pdf
#           rdos/figuras/10n_confusion_employee_vs_nonwage.pdf
#           rdos/figuras/10n_ece_by_decile.pdf
# TIEMPO ESTIMADO: ~2 min
# ==============================================================================

t_inicio <- proc.time()

# ── 📦 Paquetes ──────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
})

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ── 📥 Cargar datos ──────────────────────────────────────────────────────────
cat("📥 Cargando panel 08 (hybrid)...\n")
panel08 <- as.data.table(readRDS(PATH_08_PANEL_CONSOLIDADO))

cat("📥 Cargando panel 02 (legacy)...\n")
panel02 <- as.data.table(readRDS(file.path(DIR_DATOS, "02_panel_con_taxonomia.rds")))

# Merge legacy into panel08
panel08[, condicion_formalidad := panel02$condicion_formalidad]
rm(panel02); gc()

# ── 🔧 Filtrar ocupados ─────────────────────────────────────────────────────
cat("🔧 Filtrando ocupados...\n")
ocup <- panel08[condicion_actividad == "Ocupado"]
cat("   N ocupados:", format(nrow(ocup), big.mark = "."), "\n")

# ── 📊 Variables derivadas ───────────────────────────────────────────────────
# Hybrid GLM: Formal/Informal
# Nombre dinámico de columnas GLM
col_clase_cal_glm <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_GLM, "_pea")
col_prob_glm      <- paste0("prob_formal_", SUFIJO_MODELO_GLM, "_pea")

ocup[, hybrid_formal := fifelse(get(col_clase_cal_glm) == "Formal", 1L, 0L)]

# Legacy: condicion_formalidad == "Formal" → 1 (excluir "No corresponde")
ocup[, legacy_formal := fifelse(condicion_formalidad == "Formal", 1L,
                         fifelse(condicion_formalidad == "No formal", 0L, NA_integer_))]

# Grupo etario
ocup[, grupo_edad := fcase(
  edad < 25, "18-24",
  edad < 35, "25-34",
  edad < 45, "35-44",
  edad < 55, "45-54",
  edad >= 55, "55+"
)]

# Employee vs non-wage
ocup[, tipo_trabajador := fcase(
  categoria_ocupacional == "Empleado", "Empleado",
  categoria_ocupacional %in% c("Patrón", "Cuenta Propia"), "Independiente",
  default = "Otro"
)]

# Excluir pandemia para análisis de gap
ocup[, pandemia := periodo_id %in% TRIMESTRES_PANDEMIA]

cat("   Ocupados con hybrid válido:", format(sum(!is.na(ocup$hybrid_formal)), big.mark = "."), "\n")
cat("   Ocupados con legacy válido:", format(sum(!is.na(ocup$legacy_formal)), big.mark = "."), "\n")

# ==============================================================================
# 🌟 SECCIÓN 1: SUBGROUP STABILITY — TASAS Y GAP POR SUBGRUPO
# ==============================================================================
cat("\n═══ SECCIÓN 1: Subgroup stability ═══\n")

# Función para calcular tasas ponderadas por subgrupo
calc_tasas <- function(dt, group_var) {
  result <- dt[!is.na(hybrid_formal) & pandemia == FALSE,
    .(
      n = .N,
      tasa_hybrid  = weighted.mean(hybrid_formal, pondera, na.rm = TRUE) * 100,
      tasa_legacy  = weighted.mean(legacy_formal, pondera, na.rm = TRUE) * 100,
      n_legacy     = sum(!is.na(legacy_formal))
    ),
    by = group_var
  ]
  result[, gap_pp := tasa_legacy - tasa_hybrid]
  setorderv(result, group_var)
  result
}

# ── 1a. Por región ───────────────────────────────────────────────────────────
cat("  Region...\n")
tab_region <- calc_tasas(ocup, "region")
print(tab_region)

# ── 1b. Por sexo ─────────────────────────────────────────────────────────────
cat("\n  Sexo...\n")
tab_sexo <- calc_tasas(ocup[sexo %in% c("Varones", "Mujeres")], "sexo")
print(tab_sexo)

# ── 1c. Por grupo etario ─────────────────────────────────────────────────────
cat("\n  Grupo etario...\n")
tab_edad <- calc_tasas(ocup[!is.na(grupo_edad)], "grupo_edad")
print(tab_edad)

# ── 1d. Por sector (top 8) ───────────────────────────────────────────────────
cat("\n  Sector...\n")
tab_sector <- calc_tasas(ocup[seccion != "Ns/Nc"], "seccion")
print(tab_sector)

# ── 1e. Por categoría ocupacional ────────────────────────────────────────────
cat("\n  Categoria ocupacional...\n")
tab_catocup <- calc_tasas(ocup[categoria_ocupacional %in%
  c("Empleado", "Patrón", "Cuenta Propia")], "categoria_ocupacional")
print(tab_catocup)

# ── 1f. Por tipo trabajador (employee vs independent) ────────────────────────
cat("\n  Tipo trabajador (Employee vs Independent)...\n")
tab_tipo <- calc_tasas(ocup[tipo_trabajador %in% c("Empleado", "Independiente")], "tipo_trabajador")
print(tab_tipo)

# ==============================================================================
# 🌟 SECCIÓN 2: FIGURAS DE GAP POR SUBGRUPO
# ==============================================================================
cat("\n═══ SECCIÓN 2: Figuras ═══\n")

# theme_paper() y colores canónicos cargados via funciones_comunes.R

# ── Fig 1: Gap por región ────────────────────────────────────────────────────
p_region <- ggplot(tab_region, aes(x = reorder(region, -gap_pp), y = gap_pp)) +
  geom_col(fill = COL_GLM, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", gap_pp)), vjust = -0.3, size = 3.5) +
  tr_labs(title = "Legacy-hybrid gap by region (pp, excl. pandemic)",
       x = NULL, y = "Gap (pp)") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

guardar_figura(p_region, DIR_FIGURAS_10N, "gap_region", 1, width = 7, height = 5)

# ── Fig 2: Gap por sector ────────────────────────────────────────────────────
p_sector <- ggplot(tab_sector, aes(x = reorder(seccion, -gap_pp), y = gap_pp)) +
  geom_col(fill = COL_GLM, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", gap_pp)), vjust = -0.3, size = 3) +
  tr_labs(title = "Legacy-hybrid gap by economic sector (pp, excl. pandemic)",
       x = NULL, y = "Gap (pp)") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

guardar_figura(p_sector, DIR_FIGURAS_10N, "gap_sector", 2, width = 9, height = 5.5)

# ── Fig 3: Gap por cat. ocupacional ──────────────────────────────────────────
p_catocup <- ggplot(tab_catocup,
    aes(x = reorder(categoria_ocupacional, -gap_pp), y = gap_pp)) +
  geom_col(fill = COL_GLM, width = 0.5) +
  geom_text(aes(label = sprintf("%.1f", gap_pp)), vjust = -0.3, size = 4) +
  tr_labs(title = "Legacy-hybrid gap by occupational category (pp, excl. pandemic)",
       x = NULL, y = "Gap (pp)") +
  theme_paper()

guardar_figura(p_catocup, DIR_FIGURAS_10N, "gap_catocup", 3, width = 6, height = 5)

# ==============================================================================
# 🌟 SECCIÓN 3: CONFUSION MATRIX — EMPLOYEE VS NON-WAGE (OVERLAP)
# ==============================================================================
cat("\n═══ SECCIÓN 3: Confusion matrix por tipo trabajador ═══\n")

# Solo overlap (tipo_estimacion_pea == "Observado")
overlap <- ocup[tipo_estimacion_pea == "Observado" &
                tipo_trabajador %in% c("Empleado", "Independiente")]

cat("  N overlap:", format(nrow(overlap), big.mark = "."), "\n")

# Ground truth: formalidad_empleo para overlap
overlap[, gt_formal := fcase(
  formalidad_empleo == "Formal oficial", 1L,
  formalidad_empleo == "Informal oficial", 0L,
  default = NA_integer_
)]

# Para independientes en overlap, ground truth viene de formalidad_valida
overlap[tipo_trabajador == "Independiente" & is.na(gt_formal),
        gt_formal := fifelse(get(col_clase_cal_glm) == "Formal", 1L, 0L)]

# Predicted prob
overlap[, pred_prob := get(col_prob_glm)]

# Confusion matrix por tipo (almacenar métricas completas por tipo)
f1_por_tipo <- list()
confusion_rows <- list()
for (tipo in c("Empleado", "Independiente")) {
  sub <- overlap[tipo_trabajador == tipo & !is.na(gt_formal) & !is.na(hybrid_formal)]
  cat(sprintf("\n  --- %s (N=%s) ---\n", tipo, format(nrow(sub), big.mark = ".")))

  cm <- table(Predicted = sub$hybrid_formal, Actual = sub$gt_formal)
  if (all(c(0, 1) %in% rownames(cm)) && all(c(0, 1) %in% colnames(cm))) {
    tp <- cm["1", "1"]; tn <- cm["0", "0"]
    fp <- cm["1", "0"]; fn <- cm["0", "1"]
    acc <- (tp + tn) / sum(cm)
    prec <- tp / (tp + fp)
    rec  <- tp / (tp + fn)
    f1   <- 2 * prec * rec / (prec + rec)
    f1_por_tipo[[tipo]] <- f1
    confusion_rows[[tipo]] <- data.frame(
      type = tipo, n = nrow(sub),
      accuracy = round(acc, 3), precision = round(prec, 3),
      recall = round(rec, 3), f1 = round(f1, 3),
      stringsAsFactors = FALSE
    )
    cat(sprintf("  TP=%d  FP=%d  FN=%d  TN=%d\n", tp, fp, fn, tn))
    cat(sprintf("  Accuracy=%.3f  Precision=%.3f  Recall=%.3f  F1=%.3f\n",
                acc, prec, rec, f1))
  } else {
    cat("  Confusion matrix incompleta\n")
    print(cm)
    f1_por_tipo[[tipo]] <- NA_real_
  }
}
confusion_by_type <- do.call(rbind, confusion_rows)

# ==============================================================================
# 🌟 SECCIÓN 4: EXPECTED CALIBRATION ERROR (ECE) POR DECIL
# ==============================================================================
cat("\n═══ SECCIÓN 4: ECE por decil ═══\n")

# Solo overlap con probabilidades válidas
cal_data <- overlap[!is.na(pred_prob) & !is.na(gt_formal)]
cal_data[, decil := cut(pred_prob, breaks = quantile(pred_prob, probs = seq(0, 1, 0.1)),
                        include.lowest = TRUE, labels = paste0("D", 1:10))]

ece_tab <- cal_data[, .(
  n = .N,
  mean_pred = mean(pred_prob),
  mean_obs  = mean(gt_formal),
  abs_diff  = abs(mean(pred_prob) - mean(gt_formal))
), by = decil]

setorder(ece_tab, decil)

# ECE = weighted average of |mean_pred - mean_obs| by bin
ece_global <- weighted.mean(ece_tab$abs_diff, ece_tab$n)
cat(sprintf("  ECE global: %.4f (%.2f pp)\n", ece_global, ece_global * 100))
print(ece_tab)

# ECE by tipo_trabajador
for (tipo in c("Empleado", "Independiente")) {
  sub <- cal_data[tipo_trabajador == tipo]
  if (nrow(sub) > 100) {
    sub[, decil_t := cut(pred_prob, breaks = quantile(pred_prob, probs = seq(0, 1, 0.1)),
                         include.lowest = TRUE, labels = paste0("D", 1:10))]
    ece_sub <- sub[, .(
      n = .N,
      mean_pred = mean(pred_prob),
      mean_obs  = mean(gt_formal),
      abs_diff  = abs(mean(pred_prob) - mean(gt_formal))
    ), by = decil_t]
    ece_val <- weighted.mean(ece_sub$abs_diff, ece_sub$n)
    cat(sprintf("  ECE %s: %.4f (%.2f pp)\n", tipo, ece_val, ece_val * 100))
  }
}

# ── Fig 4: ECE reliability plot ──────────────────────────────────────────────
p_ece <- ggplot(ece_tab, aes(x = mean_pred, y = mean_obs)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = COL_BANDA) +
  geom_point(aes(size = n), color = COL_GLM) +
  geom_text(aes(label = decil), vjust = -1, size = 3) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  tr_labs(title = sprintf("GLM calibration by decile (ECE = %.3f)", ece_global),
       x = "Mean predicted probability", y = "Observed formality rate") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_paper()

guardar_figura(p_ece, DIR_FIGURAS_10N, "ece_decile", 4, width = 6, height = 6)

# ==============================================================================
# 🌟 SECCIÓN 5: NOTAS PARA PAPER
# ==============================================================================
cat("\n═══ SECCIÓN 5: Generando notas para paper ═══\n")

con <- file(file.path(DIR_REPORTES, "10n_subgroup_stability_notas.txt"), "w")

cat("=== SUBGROUP STABILITY — NOTAS PARA EL PAPER ===\n", file = con)
cat(sprintf("Generado: %s\n\n", Sys.time()), file = con)

cat("--- GAP LEGACY-HYBRID POR REGIÓN (pp, excl. pandemia) ---\n", file = con)
for (i in seq_len(nrow(tab_region))) {
  cat(sprintf("  %s: hybrid=%.1f%%, legacy=%.1f%%, gap=%.1f pp (N=%s)\n",
              tab_region$region[i], tab_region$tasa_hybrid[i],
              tab_region$tasa_legacy[i], tab_region$gap_pp[i],
              format(tab_region$n[i], big.mark = ".")), file = con)
}

cat("\n--- GAP POR SEXO ---\n", file = con)
for (i in seq_len(nrow(tab_sexo))) {
  cat(sprintf("  %s: hybrid=%.1f%%, legacy=%.1f%%, gap=%.1f pp\n",
              tab_sexo$sexo[i], tab_sexo$tasa_hybrid[i],
              tab_sexo$tasa_legacy[i], tab_sexo$gap_pp[i]), file = con)
}

cat("\n--- GAP POR GRUPO ETARIO ---\n", file = con)
for (i in seq_len(nrow(tab_edad))) {
  cat(sprintf("  %s: hybrid=%.1f%%, legacy=%.1f%%, gap=%.1f pp\n",
              tab_edad$grupo_edad[i], tab_edad$tasa_hybrid[i],
              tab_edad$tasa_legacy[i], tab_edad$gap_pp[i]), file = con)
}

cat("\n--- GAP POR CATEGORÍA OCUPACIONAL ---\n", file = con)
for (i in seq_len(nrow(tab_catocup))) {
  cat(sprintf("  %s: hybrid=%.1f%%, legacy=%.1f%%, gap=%.1f pp\n",
              tab_catocup$categoria_ocupacional[i], tab_catocup$tasa_hybrid[i],
              tab_catocup$tasa_legacy[i], tab_catocup$gap_pp[i]), file = con)
}

cat(sprintf("\n--- ECE GLOBAL: %.4f (%.2f pp) ---\n", ece_global, ece_global * 100), file = con)

close(con)
cat("  ✓ 10n_subgroup_stability_notas.txt\n")

# ==============================================================================
# 🏁 Checklist de outputs
# ==============================================================================
cat("\n═══ CHECKLIST DE OUTPUTS ═══\n")
outputs <- c(
  file.path(DIR_FIGURAS_10N, "10n_subgroup_stability_gap_region_01.pdf"),
  file.path(DIR_FIGURAS_10N, "10n_subgroup_stability_gap_sector_02.pdf"),
  file.path(DIR_FIGURAS_10N, "10n_subgroup_stability_gap_catocup_03.pdf"),
  file.path(DIR_FIGURAS_10N, "10n_subgroup_stability_ece_decile_04.pdf"),
  file.path(DIR_REPORTES, "10n_subgroup_stability_notas.txt"),
  file.path(DIR_CONTRATOS, "10n_contrato_subgroup.rds")
)
for (f in outputs) {
  cat(sprintf("  %s: %s\n", basename(f), ifelse(file.exists(f), "✓", "✗")))
}

# ==============================================================================
# 📦 CONTRATO: Escalares clave para trazabilidad
# ==============================================================================
cat("\n═══ Generando contrato 10n ═══\n")

contrato_10n <- list(
  script  = "10n_subgroup_stability_GLM.R",
  fecha   = Sys.time(),
  # ECE global
  ece_global = ece_global,
  # F1 por tipo de trabajador
  f1_empleados       = f1_por_tipo[["Empleado"]],
  f1_independientes  = f1_por_tipo[["Independiente"]],
  # Gap summary: rango y mediana del gap legacy-hybrid por región
  gap_region_median  = median(tab_region$gap_pp),
  gap_region_min     = min(tab_region$gap_pp),
  gap_region_max     = max(tab_region$gap_pp),
  # Gap por categoría ocupacional
  gap_catocup        = setNames(tab_catocup$gap_pp, tab_catocup$categoria_ocupacional),
  # Gap por sexo
  gap_sexo           = setNames(tab_sexo$gap_pp, tab_sexo$sexo),
  # Subgroup tables for appendix
  tab_region         = as.data.frame(tab_region),
  tab_sexo           = as.data.frame(tab_sexo),
  tab_edad           = as.data.frame(tab_edad),
  # Confusion matrix by worker type
  confusion_by_type  = confusion_by_type,
  # Gap por sector de actividad
  tab_sector         = as.data.frame(tab_sector)
)

path_contrato_10n <- file.path(DIR_CONTRATOS, "10n_contrato_subgroup.rds")
saveRDS(contrato_10n, path_contrato_10n)
cat("  ✓", basename(path_contrato_10n), "\n")

rm(panel08, ocup, overlap, cal_data); gc()
cat("\nTiempo:", round((proc.time() - t_inicio)["elapsed"] / 60, 1), "minutos\n")
