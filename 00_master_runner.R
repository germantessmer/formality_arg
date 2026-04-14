# =============================================================================
# [EN] 00_master_runner.R -- Master runner: executes all pipeline layers (0-7) sequentially
# INPUTS:  All scripts in script/ (layers 0-7); pre-computed heterofactor assumed
# OUTPUTS: rdos/logs/pipeline_status.txt (real-time progress log)
# =============================================================================
# ==============================================================================
# 00_master_runner.R  — Pipeline completo formalidad_rev (capas 0-7)
# ==============================================================================
#
# OBJETIVO:
#    Ejecuta secuencialmente todos los scripts del pipeline, desde la carga de
#    datos (Capa 0) hasta robustez (Capa 7), EXCLUYENDO el heterofactor
#    (06a_heterofactor_estimacion.R) que insume ~6 horas y se asume ya ejecutado.
#
# PREREQUISITOS:
#    - .Rproj activo (here::here() debe resolver a C:/formalidad_rev)
#    - Bases EPH disponibles en C:/oes/eph_rdos/capa2/
#    - Heterofactor ya ejecutado (rdos/datos/06_theta_predichos.rds + contratos)
#
# ESTADO EN TIEMPO REAL:
#    rdos/logs/pipeline_status.txt  -- se actualiza tras cada script
#    Abrir en cualquier momento para ver progreso, tiempos y errores.
#
# MANEJO DE ERRORES:
#    Ante error en un script, lo loguea y CONTINUA con el siguiente.
#    Los scripts que dependen de uno fallido probablemente fallen tambien,
#    pero el pipeline sigue para maximizar lo que se completa overnight.
#
# USO:
#    Rscript 00_master_runner.R
# ==============================================================================

suppressPackageStartupMessages(library(here))

# -- Configuracion del runner --------------------------------------------------
TIMESTAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE <- file.path(here::here(), "rdos", "logs",
                      paste0("master_runner_", TIMESTAMP, ".txt"))
STATUS_FILE <- file.path(here::here(), "rdos", "logs", "pipeline_status.txt")
dir.create(dirname(LOG_FILE), showWarnings = FALSE, recursive = TRUE)

t_global <- proc.time()
INICIO_GLOBAL <- Sys.time()

# -- Estado global (se actualiza tras cada script) -----------------------------
estado <- data.frame(
  idx       = integer(0),
  script    = character(0),
  desc      = character(0),
  status    = character(0),
  elapsed   = numeric(0),
  hora_fin  = character(0),
  error_msg = character(0),
  stringsAsFactors = FALSE
)

escribir_status <- function(pipeline, estado, idx_actual = NULL) {
  lineas <- character(0)
  lineas <- c(lineas,
    strrep("=", 70),
    sprintf("  PIPELINE STATUS  |  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("  Inicio: %s  |  Elapsed: %.1f min",
            format(INICIO_GLOBAL, "%H:%M:%S"),
            as.numeric(difftime(Sys.time(), INICIO_GLOBAL, units = "mins"))),
    strrep("=", 70), "")

  n_ok   <- sum(estado$status == "OK",    na.rm = TRUE)
  n_fail <- sum(estado$status == "ERROR", na.rm = TRUE)
  n_pend <- length(pipeline) - nrow(estado)
  lineas <- c(lineas,
    sprintf("  Completados: %d  |  Errores: %d  |  Pendientes: %d  |  Total: %d",
            n_ok, n_fail, n_pend, length(pipeline)), "")

  lineas <- c(lineas, strrep("-", 70))
  lineas <- c(lineas, sprintf("  %-4s  %-6s  %-6s  %s", "#", "STATUS", "MIN", "DESCRIPCION"))
  lineas <- c(lineas, strrep("-", 70))

  for (i in seq_along(pipeline)) {
    p <- pipeline[[i]]
    if (i <= nrow(estado)) {
      e <- estado[i, ]
      marca <- if (e$status == "OK") "[OK]" else "[FAIL]"
      tiempo <- sprintf("%5.1f", e$elapsed)
    } else if (!is.null(idx_actual) && i == idx_actual) {
      marca <- "[>>>]"
      tiempo <- "  ..."
    } else {
      marca <- "[  ]"
      tiempo <- "     "
    }
    lineas <- c(lineas, sprintf("  %-4d  %-6s  %s  %s", i, marca, tiempo, p$desc))
  }

  # Errores detallados
  errores <- estado[estado$status == "ERROR", ]
  if (nrow(errores) > 0) {
    lineas <- c(lineas, "", strrep("-", 70), "  ERRORES DETALLADOS:", strrep("-", 70))
    for (j in seq_len(nrow(errores))) {
      lineas <- c(lineas,
        sprintf("  [%d] %s", errores$idx[j], errores$script[j]),
        sprintf("       %s", errores$error_msg[j]), "")
    }
  }

  lineas <- c(lineas, "", strrep("=", 70))
  writeLines(lineas, STATUS_FILE)
}

# -- Funcion auxiliar ----------------------------------------------------------
run_script <- function(ruta_script, descripcion = NULL, idx = NULL, pipeline = NULL) {
  nombre <- basename(ruta_script)
  desc   <- if (!is.null(descripcion)) descripcion else nombre

  cat(sprintf("\n%s\n", strrep("=", 65)))
  cat(sprintf(">  [%d/%d] %s\n", idx, length(pipeline), desc))
  cat(sprintf("   Inicio: %s\n", format(Sys.time(), "%H:%M:%S")))
  cat(sprintf("%s\n", strrep("-", 65)))

  # Actualizar status: en progreso
  escribir_status(pipeline, estado, idx_actual = idx)

  t0 <- proc.time()

  tryCatch({
    source(ruta_script, local = new.env(parent = globalenv()), echo = FALSE)
    elapsed <- round((proc.time() - t0)["elapsed"] / 60, 1)
    cat(sprintf("\n   [OK] COMPLETADO en %.1f min\n", elapsed))
    list(ok = TRUE, elapsed = elapsed, script = nombre, error_msg = "")

  }, error = function(e) {
    elapsed <- round((proc.time() - t0)["elapsed"] / 60, 1)
    msg <- conditionMessage(e)
    cat(sprintf("\n   [ERROR] FALLO tras %.1f min\n", elapsed))
    cat(sprintf("   Mensaje: %s\n", msg))
    list(ok = FALSE, elapsed = elapsed, script = nombre, error_msg = msg)
  })
}

# Registrar inicio en log
sink(LOG_FILE, append = FALSE, split = TRUE)

cat(strrep("=", 65), "\n")
cat("  MASTER RUNNER  --  formalidad_rev  (pipeline 00 -> 10o)\n")
cat(sprintf("  Inicio: %s\n", format(INICIO_GLOBAL, "%Y-%m-%d %H:%M:%S")))
cat("  Heterofactor (06a): INCLUIDO (~6h estimadas)\n")
cat("  Capas 0-6: DETIENE ante error (encadenadas)\n")
cat("  Capa 7:    CONTINUA ante error (independientes)\n")
cat(sprintf("  Status en vivo: %s\n", STATUS_FILE))
cat(strrep("=", 65), "\n\n")


# -- Definicion de rutas por capa ----------------------------------------------
DIR_SCRIPTS <- file.path(here::here(), "script")
DIR_00 <- file.path(DIR_SCRIPTS, "00_diccionarios")
DIR_01 <- file.path(DIR_SCRIPTS, "01_datos_base")
DIR_02 <- file.path(DIR_SCRIPTS, "02_proxies")
DIR_03 <- file.path(DIR_SCRIPTS, "03_heterofactor")
DIR_04 <- file.path(DIR_SCRIPTS, "04_modelado")
DIR_05 <- file.path(DIR_SCRIPTS, "05_backcasting")
DIR_06 <- file.path(DIR_SCRIPTS, "06_comparativo")
DIR_07 <- file.path(DIR_SCRIPTS, "07_robusto")


# -- Secuencia completa --------------------------------------------------------
pipeline <- list(

  # -- CAPA 0: Diccionarios (critical) ------------------------------------------
  list(path = file.path(DIR_00, "00_diccionarios.R"),
       desc = "00  | DICC        | Lookup tables EPH", critical = TRUE),

  # -- CAPA 1: Datos base (critical) -------------------------------------------
  list(path = file.path(DIR_01, "01_carga_panel.R"),
       desc = "01  | DATOS BASE  | Carga y panel historico", critical = TRUE),
  list(path = file.path(DIR_01, "02_taxonomia.R"),
       desc = "02  | DATOS BASE  | Taxonomia unica", critical = TRUE),
  list(path = file.path(DIR_01, "03a_ingresos_limpieza.R"),
       desc = "03a | DATOS BASE  | Limpieza de ingresos", critical = TRUE),
  list(path = file.path(DIR_01, "03b_ich.R"),
       desc = "03b | DATOS BASE  | Indice Calidad Habitacional (MCA)", critical = TRUE),
  list(path = file.path(DIR_01, "03c_reporte_base.R"),
       desc = "03c | DATOS BASE  | Reporte validacion Capa 1", critical = TRUE),

  # -- CAPA 2: Proxies (critical) ----------------------------------------------
  list(path = file.path(DIR_02, "04_proxies.R"),
       desc = "04  | PROXIES     | 7 proxies (cog + socioemocional)", critical = TRUE),
  list(path = file.path(DIR_02, "05_reporte_proxies.R"),
       desc = "05  | PROXIES     | Validacion Kotlarski + reporte", critical = TRUE),

  # -- CAPA 3: Heterofactor (critical) ------------------------------------------
  list(path = file.path(DIR_03, "06a_heterofactor_estimacion.R"),
       desc = "06a | HETEROFACT  | Estimacion heterofactor (~6h)", critical = TRUE),
  list(path = file.path(DIR_03, "06b_diagnostico_theta.R"),
       desc = "06b | HETEROFACT  | Diagnostico theta", critical = TRUE),
  list(path = file.path(DIR_03, "06c_diagnostico_comparativo.R"),
       desc = "06c | HETEROFACT  | Comparativo Modelo A vs B", critical = TRUE),
  list(path = file.path(DIR_03, "06d_reporte_heterofactor.R"),
       desc = "06d | HETEROFACT  | Reporte HTML heterofactor", critical = TRUE),

  # -- CAPA 4: Modelado LPM (critical) -----------------------------------------
  list(path = file.path(DIR_04, "LPM", "07a_lasso_LPM.R"),
       desc = "07a | LPM         | LASSO + CV + Bootstrap", critical = TRUE),
  list(path = file.path(DIR_04, "LPM", "07b_postlasso_LPM.R"),
       desc = "07b | LPM         | Post-LASSO OLS robusto", critical = TRUE),
  list(path = file.path(DIR_04, "LPM", "07c_lasso_tiempo_LPM.R"),
       desc = "07c | LPM         | Estabilidad temporal", critical = TRUE),
  list(path = file.path(DIR_04, "LPM", "07d_lasso_interacciones_LPM.R"),
       desc = "07d | LPM         | LASSO interacciones", critical = TRUE),
  list(path = file.path(DIR_04, "LPM", "07e_lasso_reporte_LPM.R"),
       desc = "07e | LPM         | Reporte HTML", critical = TRUE),

  # -- CAPA 4: Modelado GLM (critical) -----------------------------------------
  list(path = file.path(DIR_04, "GLM", "07a_lasso_GLM.R"),
       desc = "07a | GLM         | LASSO binomial + CV + Bootstrap", critical = TRUE),
  list(path = file.path(DIR_04, "GLM", "07b_postlasso_GLM.R"),
       desc = "07b | GLM         | Post-LASSO GLM + AMEs", critical = TRUE),
  list(path = file.path(DIR_04, "GLM", "07c_lasso_tiempo_GLM.R"),
       desc = "07c | GLM         | Estabilidad temporal", critical = TRUE),
  list(path = file.path(DIR_04, "GLM", "07d_lasso_interacciones_GLM.R"),
       desc = "07d | GLM         | LASSO interacciones", critical = TRUE),
  list(path = file.path(DIR_04, "GLM", "07e_lasso_reporte_GLM.R"),
       desc = "07e | GLM         | Reporte HTML", critical = TRUE),

  # -- CAPA 4: Modelado SLS (critical) -----------------------------------------
  list(path = file.path(DIR_04, "SLS", "07a_lasso_SLS.R"),
       desc = "07a | SLS         | LASSO + recorte iterativo + Bootstrap", critical = TRUE),
  list(path = file.path(DIR_04, "SLS", "07b_postlasso_SLS.R"),
       desc = "07b | SLS         | Post-LASSO OLS iterativo", critical = TRUE),
  list(path = file.path(DIR_04, "SLS", "07c_lasso_tiempo_SLS.R"),
       desc = "07c | SLS         | Estabilidad temporal", critical = TRUE),
  list(path = file.path(DIR_04, "SLS", "07d_lasso_interacciones_SLS.R"),
       desc = "07d | SLS         | LASSO interacciones", critical = TRUE),
  list(path = file.path(DIR_04, "SLS", "07e_lasso_reporte_SLS.R"),
       desc = "07e | SLS         | Reporte HTML", critical = TRUE),

  # -- CAPA 5: Backcasting LPM (critical) --------------------------------------
  list(path = file.path(DIR_05, "LPM", "08a_backcasting_LPM.R"),
       desc = "08a | LPM         | Backcasting panel completo", critical = TRUE),
  list(path = file.path(DIR_05, "LPM", "08b_reporte_backcasting_LPM.R"),
       desc = "08b | LPM         | Reporte backcasting", critical = TRUE),
  list(path = file.path(DIR_05, "LPM", "08c_reporte_hibrido_LPM.R"),
       desc = "08c | LPM         | Reporte variable hibrida", critical = TRUE),

  # -- CAPA 5: Backcasting GLM (critical) --------------------------------------
  list(path = file.path(DIR_05, "GLM", "08_backcasting_GLM.R"),
       desc = "08  | GLM         | Backcasting panel completo", critical = TRUE),
  list(path = file.path(DIR_05, "GLM", "08b_reporte_backcasting_GLM.R"),
       desc = "08b | GLM         | Reporte backcasting", critical = TRUE),
  list(path = file.path(DIR_05, "GLM", "08c_reporte_hibrido_GLM.R"),
       desc = "08c | GLM         | Reporte variable hibrida", critical = TRUE),

  # -- CAPA 5: Backcasting SLS (critical) --------------------------------------
  list(path = file.path(DIR_05, "SLS", "08_backcasting_SLS.R"),
       desc = "08  | SLS         | Backcasting panel completo", critical = TRUE),
  list(path = file.path(DIR_05, "SLS", "08b_reporte_backcasting_SLS.R"),
       desc = "08b | SLS         | Reporte backcasting", critical = TRUE),
  list(path = file.path(DIR_05, "SLS", "08c_reporte_hibrido_SLS.R"),
       desc = "08c | SLS         | Reporte variable hibrida", critical = TRUE),

  # -- CAPA 6: Comparativos (critical) -----------------------------------------
  list(path = file.path(DIR_06, "09a_comp_modelado.R"),
       desc = "09a | COMP        | Comparativo de modelos", critical = TRUE),
  list(path = file.path(DIR_06, "09b_comp_retropredictivo.R"),
       desc = "09b | COMP        | Comparativo backcasting puro", critical = TRUE),
  list(path = file.path(DIR_06, "09c_comp_hibrido.R"),
       desc = "09c | COMP        | Comparativo variable hibrida", critical = TRUE),

  # -- CAPA 7: Robustez (NO critical -- scripts independientes) ----------------
  list(path = file.path(DIR_07, "10a_robustness_heterofactor.R"),
       desc = "10a | ROBUSTO     | Robustez heterofactor", critical = FALSE),
  list(path = file.path(DIR_07, "10b_robustness_comparacion_modeloA.R"),
       desc = "10b | ROBUSTO     | Comparacion modelo A", critical = FALSE),
  list(path = file.path(DIR_07, "10c_bandas_incertidumbre_GLM.R"),
       desc = "10c | ROBUSTO     | Bandas de incertidumbre GLM", critical = FALSE),
  list(path = file.path(DIR_07, "10d_calibracion_bootstrap.R"),
       desc = "10d | ROBUSTO     | Calibracion bootstrap", critical = FALSE),
  list(path = file.path(DIR_07, "10e_validacion_sipa.R"),
       desc = "10e | ROBUSTO     | Validacion SIPA", critical = FALSE),
  list(path = file.path(DIR_07, "10f_loco_quarter_GLM.R"),
       desc = "10f | ROBUSTO     | LOCO quarter GLM", critical = FALSE),
  list(path = file.path(DIR_07, "10g_desc_jubilatorio_sensitivity.R"),
       desc = "10g | ROBUSTO     | Sensibilidad desc jubilatorio", critical = FALSE),
  list(path = file.path(DIR_07, "10h_sensibilidad_reglas_formalidad.R"),
       desc = "10h | ROBUSTO     | Sensibilidad reglas formalidad", critical = FALSE),
  list(path = file.path(DIR_07, "10i_backcast_factores_restringidos.R"),
       desc = "10i | ROBUSTO     | Backcast factores restringidos", critical = FALSE),
  list(path = file.path(DIR_07, "10j_descomposicion_gap.R"),
       desc = "10j | ROBUSTO     | Descomposicion gap", critical = FALSE),
  list(path = file.path(DIR_07, "10j_loco_aglomerado_GLM.R"),
       desc = "10j | ROBUSTO     | LOCO aglomerado GLM", critical = FALSE),
  list(path = file.path(DIR_07, "10k_coef_stability_GLM.R"),
       desc = "10k | ROBUSTO     | Estabilidad coeficientes GLM", critical = FALSE),
  list(path = file.path(DIR_07, "10l_measurement_invariance_proxies.R"),
       desc = "10l | ROBUSTO     | Invarianza de medida proxies", critical = FALSE),
  list(path = file.path(DIR_07, "10m_sparsity_sensitivity_GLM.R"),
       desc = "10m | ROBUSTO     | Sensibilidad sparsity GLM", critical = FALSE),
  list(path = file.path(DIR_07, "10n_subgroup_stability_GLM.R"),
       desc = "10n | ROBUSTO     | Estabilidad subgrupos GLM", critical = FALSE),
  list(path = file.path(DIR_07, "10o_ml_benchmark_confusion.R"),
       desc = "10o | ROBUSTO     | ML benchmark confusion", critical = FALSE)
)


# -- Verificar existencia de scripts -------------------------------------------
cat("-- Verificando existencia de scripts --\n")
faltantes <- character(0)
for (item in pipeline) {
  existe <- file.exists(item$path)
  cat(sprintf("  [%s] %s\n", if (existe) "OK" else "XX", item$desc))
  if (!existe) faltantes <- c(faltantes, item$path)
}

if (length(faltantes) > 0) {
  cat(sprintf("\n!! %d script(s) no encontrados:\n", length(faltantes)))
  for (f in faltantes) cat(sprintf("     %s\n", f))
  sink()
  stop("Pipeline abortado: scripts faltantes (ver arriba).")
}

# Cargar parametros para verificaciones
source(here::here("script", "config", "parametros.R"), local = TRUE)

cat(sprintf("\n[OK] %d scripts verificados. Iniciando pipeline...\n\n", length(pipeline)))

# Status inicial
escribir_status(pipeline, estado)


# -- Ejecucion secuencial -----------------------------------------------------
for (i in seq_along(pipeline)) {
  item <- pipeline[[i]]
  is_critical <- isTRUE(item$critical)
  res  <- run_script(item$path, item$desc, idx = i, pipeline = pipeline)

  # Registrar resultado
  estado <- rbind(estado, data.frame(
    idx       = i,
    script    = res$script,
    desc      = item$desc,
    status    = if (res$ok) "OK" else "ERROR",
    elapsed   = res$elapsed,
    hora_fin  = format(Sys.time(), "%H:%M:%S"),
    error_msg = res$error_msg,
    stringsAsFactors = FALSE
  ))

  # Actualizar status en disco
  escribir_status(pipeline, estado)

  # Si fallo un script critico (capas 0-6): detener pipeline
  if (!res$ok && is_critical) {
    cat(sprintf("\n!! ERROR CRITICO en script encadenado: %s\n", res$script))
    cat("!! Pipeline detenido. Los scripts de capa 7 NO se ejecutaron.\n")
    break
  }

  # Limpiar memoria entre scripts
  gc(verbose = FALSE)
}


# -- Resumen final -------------------------------------------------------------
t_total <- round((proc.time() - t_global)["elapsed"] / 60, 1)
n_ok    <- sum(estado$status == "OK")
n_fail  <- sum(estado$status == "ERROR")

cat(sprintf("\n%s\n", strrep("=", 65)))
cat("  RESUMEN FINAL -- MASTER RUNNER\n")
cat(sprintf("%s\n", strrep("=", 65)))
cat(sprintf("  Completados: %d / %d\n", n_ok, length(pipeline)))
cat(sprintf("  Errores:     %d\n", n_fail))
cat(sprintf("  Tiempo total: %.1f min (%.1f horas)\n", t_total, t_total / 60))
cat(sprintf("  Log:   %s\n", LOG_FILE))
cat(sprintf("  Status: %s\n", STATUS_FILE))

cat("\n  Tiempos por script:\n")
for (j in seq_len(nrow(estado))) {
  e <- estado[j, ]
  marca <- if (e$status == "OK") "OK  " else "FAIL"
  cat(sprintf("    [%s]  %5.1f min  --  %s\n", marca, e$elapsed, e$script))
}

if (n_fail > 0) {
  cat(sprintf("\n  !! %d script(s) con errores:\n", n_fail))
  errores <- estado[estado$status == "ERROR", ]
  for (j in seq_len(nrow(errores))) {
    cat(sprintf("     %s: %s\n", errores$script[j], errores$error_msg[j]))
  }
}

cat(sprintf("%s\n", strrep("=", 65)))
cat(sprintf("\nPipeline finalizado: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

sink()

# Escribir status final
escribir_status(pipeline, estado)
