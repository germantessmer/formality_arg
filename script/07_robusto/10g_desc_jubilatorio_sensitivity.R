# =============================================================================
# [EN] 10g_desc_jubilatorio_sensitivity.R -- PP07H (pension contribution) missingness quantification for wage earners
# INPUTS:  C:/oes/eph_rdos/capa2/EPH*.RData (overlap quarters)
# OUTPUTS: Console diagnostics for B2-Q6 response
# =============================================================================
# 🌟 10g_desc_jubilatorio_sensitivity.R 🌟 ####
# OBJETIVO: PP07H missingness -- cuantificación definitiva para B2-Q6

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages(library(here))

# 🔧 Cargar configuración y funciones ------------------------------------------

source(here::here("script/config/parametros.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------

cat("=== PP07H definitivo para B2-Q6 ===\n\n")

archivos <- setNames(
  file.path(RUTA_BASES, paste0("EPH", TRIMESTRES_FORMALIDAD, ".RData")),
  TRIMESTRES_FORMALIDAD
)

resultados <- list()

for (trim_id in names(archivos)) {
  cat("── Trimestre:", trim_id, "──\n")
  env <- new.env()
  load(archivos[trim_id], envir=env)
  df <- env$datos

  # Filtrar ocupados
  ocup <- df[!is.na(df$condicion_actividad) &
             df$condicion_actividad == "Ocupado", ]
  n_ocup   <- nrow(ocup)
  w_ocup   <- sum(ocup$pondera, na.rm=TRUE)

  # Empleados (asalariados)
  empl <- ocup[!is.na(ocup$categoria_ocupacional) &
               ocup$categoria_ocupacional == "Empleado", ]
  n_empl <- nrow(empl)
  w_empl <- sum(empl$pondera, na.rm=TRUE)

  # formalidad_empleo para empleados
  n_formal_empl   <- sum(empl$formalidad_empleo == "Formal oficial",   na.rm=TRUE)
  n_informal_empl <- sum(empl$formalidad_empleo == "Informal oficial", na.rm=TRUE)
  n_nsnc_empl     <- sum(empl$formalidad_empleo == "Ns/Nr",            na.rm=TRUE)
  n_nocorresp     <- sum(empl$formalidad_empleo == "No corresponde",   na.rm=TRUE)
  cat(sprintf("  Empleados: N=%s | Formal=%d | Informal=%d | Ns/Nr=%d | NoCorresp=%d\n",
              format(n_empl, big.mark=","),
              n_formal_empl, n_informal_empl, n_nsnc_empl, n_nocorresp))

  # ¿Columna informal_nsnc (sin sufijo)?
  col_nsnc <- "informal_nsnc"
  if (col_nsnc %in% names(ocup)) {
    nsnc_all <- ocup[[col_nsnc]]
    cat("  informal_nsnc class:", class(nsnc_all), "\n")
    cat("  informal_nsnc unique (5):", paste(unique(nsnc_all)[1:5], collapse=", "), "\n")
    n_nsnc_all <- sum(!is.na(nsnc_all) & nsnc_all > 0)
    cat("  informal_nsnc (n>0 en ocupados):", n_nsnc_all, "\n")
  }

  # informal_nsnc_CatEmpl: ocupados con informal+nsnc en CATEGORÍA EMPLEADO
  col_empl_nsnc <- "informal_nsnc_CatEmpl"
  if (col_empl_nsnc %in% names(ocup)) {
    nsnc_empl_vals <- ocup[[col_empl_nsnc]]
    n_nsnc_empl_col <- sum(!is.na(nsnc_empl_vals) & nsnc_empl_vals > 0)
    cat("  informal_nsnc_CatEmpl (n>0 en ocupados):", n_nsnc_empl_col, "\n")
    # Estos son empleados con sector NS/NC — DISTINTO de PP07H NS/NC
  }

  # La verdad es: después de la imputación RF, formalidad_empleo tiene 0 NS/NR para empleados
  # El <2% viene del raw EPH antes de imputar.
  # Estimación worst-case analítica:
  # Asumimos que el 2% upper bound de PP07H NS/NC fue imputable
  # Con tasa formal = n_formal_empl / (n_formal_empl + n_informal_empl)
  tasa_formal_empl  <- n_formal_empl / (n_formal_empl + n_informal_empl)
  # Peor caso: 2% de empleados tenían PP07H NS/NC, todos fueron imputados como formal
  pct_nsnc_ub <- 0.018  # upper bound documentado en R1 (<2%)
  n_imputed_ub <- round(n_empl * pct_nsnc_ub)
  # Worst-case: todos los imputados habían sido formal → si se tratan como informal
  n_reclasif_worstcase <- round(n_imputed_ub * tasa_formal_empl)
  delta_empl_pp <- n_reclasif_worstcase / n_empl * 100
  delta_total_pp <- n_reclasif_worstcase / n_ocup * 100

  cat(sprintf("  Tasa formal empleados: %.1f%%\n", tasa_formal_empl*100))
  cat(sprintf("  Upper bound PP07H NS/NC (1.8%% de empl): ~%d obs\n", n_imputed_ub))
  cat(sprintf("  Worst-case reclasificados (todos imputados → formal → informal): %d\n", n_reclasif_worstcase))
  cat(sprintf("  Delta tasa asalariados: %.3f pp\n", delta_empl_pp))
  cat(sprintf("  Delta tasa total ocupados: %.3f pp\n", delta_total_pp))

  resultados[[trim_id]] <- data.frame(
    trim_id              = trim_id,
    n_ocupados_trimestre = n_ocup,
    n_empleados          = n_empl,
    n_formal_empl        = n_formal_empl,
    n_informal_empl      = n_informal_empl,
    tasa_formal_empl_pct = round(tasa_formal_empl * 100, 2),
    nsnc_ub_pct          = pct_nsnc_ub * 100,
    n_nsnc_ub            = n_imputed_ub,
    n_worstcase_formal   = n_reclasif_worstcase,
    delta_tasa_empl_pp   = round(delta_empl_pp, 3),
    delta_tasa_total_pp  = round(delta_total_pp, 3)
  )
  rm(df, ocup, empl, env); gc(verbose=FALSE)
  cat("\n")
}

res <- do.call(rbind, resultados)
cat("=== TABLA CONSOLIDADA ===\n")
print(res)

out <- file.path(DIR_REPORTES, "00_fase2_pp07h_missingness.csv")
write.csv(res, out, row.names=FALSE)
cat("\n[✅] Guardado:", out, "\n")
cat("\nPeor caso TOTAL en cualquier trimestre:",
    round(max(res$delta_tasa_total_pp), 3), "pp\n")
