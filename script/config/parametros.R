# =============================================================================
# [EN] parametros.R -- Global configuration: paths, seeds, temporal parameters, model suffixes, core counts
# INPUTS:  None (self-contained configuration)
# OUTPUTS: Environment variables: DIR_*, PATH_*, SEED_GLOBAL, N_CORES, SUFIJO_MODELO_*
# =============================================================================
# ==============================================================================
# parametros.R
# Configuración global del proyecto EPH - Formalidad con Backcasting
# ==============================================================================

suppressPackageStartupMessages({
  library(here)  # Para paths relativos robustos
})

# ── Rutas del proyecto ────────────────────────────────────────────────────────
RUTA_PROYECTO <- here::here()  # C:/formalidad_back (detecta .Rproj automáticamente)
RUTA_SCRIPTS  <- file.path(RUTA_PROYECTO, "script")

# Datos crudos EPH (solo lectura, externos al proyecto)
RUTA_BASES    <- "C:/oes/eph_rdos/capa2/"

# Outputs DENTRO del proyecto
RUTA_OUTPUTS  <- file.path(RUTA_PROYECTO, "rdos")

# Subdirectorios de outputs (se crean automáticamente si no existen)
DIR_DATOS     <- file.path(RUTA_OUTPUTS, "datos")
DIR_MODELOS   <- file.path(RUTA_OUTPUTS, "modelos")
DIR_CONTRATOS <- file.path(RUTA_OUTPUTS, "contratos")
DIR_REPORTES  <- file.path(RUTA_OUTPUTS, "reportes")
DIR_CACHE     <- file.path(RUTA_OUTPUTS, "cache")
DIR_LOGS      <- file.path(RUTA_OUTPUTS, "logs")
DIR_INPUTS    <- file.path(RUTA_OUTPUTS, "inputs")

# Subdirectorios de diccionarios (definidos aquí para estar disponibles en el loop)
DIR_DICC     <- file.path(DIR_INPUTS, "diccionarios")            # CSVs de mapeo + .rds final
DIR_DICC_OBS <- file.path(DIR_INPUTS, "diccionarios", "observed") # valores observados crudos

# Crear estructura de directorios
for (dir in c(DIR_DATOS, DIR_MODELOS, DIR_CONTRATOS, DIR_REPORTES,
              DIR_CACHE, DIR_LOGS, DIR_INPUTS,
              DIR_DICC, DIR_DICC_OBS)) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# ── Configuración temporal ────────────────────────────────────────────────────
ANIO_INI <- 2016
TRIM_INI <- 4
ANIO_FIN <- 2025
TRIM_FIN <- 3

PERIODO_INI <- ANIO_INI * 10L + TRIM_INI  # 20164
PERIODO_FIN <- ANIO_FIN * 10L + TRIM_FIN  # 20252

# Trimestres con formalidad observada (para filtro LASSO entrenamiento)
TRIMESTRES_FORMALIDAD <- c("2024_T4", "2025_T1", "2025_T2", "2025_T3")

# Trimestres pandemia (para exclusión en métricas de coherencia histórica)
TRIMESTRES_PANDEMIA <- c("2020_T1", "2020_T2", "2020_T3", "2020_T4", "2021_T1", "2021_T2")

# ── Seeds y performance ───────────────────────────────────────────────────────
SEED_GLOBAL <- 123
set.seed(SEED_GLOBAL)

N_CORES <- max(1, parallel::detectCores(logical = FALSE) - 1)

# ── 5. SUFIJOS DE MODELO ──────────────────────────────────────────────────────
# CRÍTICO: precede a todos los paths que usan paste0(sufijo) — lección 20
N_TRIMESTRES_TRAINING <- 4L
SUFIJO_MODELO_LPM <- paste0("LPM", N_TRIMESTRES_TRAINING, "T")  # "LPM4T"
SUFIJO_MODELO_GLM <- paste0("GLM", N_TRIMESTRES_TRAINING, "T")  # "GLM4T"
SUFIJO_MODELO_SLS <- paste0("SLS", N_TRIMESTRES_TRAINING, "T")  # "SLS4T"

# ── 6. PATHS CAPA 0-2 (sin sufijo) ───────────────────────────────────────────
PATH_DICCIONARIOS     <- file.path(DIR_INPUTS, "diccionarios", "00_diccionarios.rds")
PATH_00_DICCIONARIOS  <- PATH_DICCIONARIOS   # alias explícito — usado en 00_diccionarios.R
PATH_01_PANEL_RAW     <- file.path(DIR_DATOS, "01_panel_historico_raw.rds")
PATH_02_PANEL_TAX      <- file.path(DIR_DATOS, "02_panel_con_taxonomia.rds")
PATH_02_PANEL_TAXONOMIA <- PATH_02_PANEL_TAX   # alias usado en 02_taxonomia.R
PATH_03A_PANEL_ING      <- file.path(DIR_DATOS, "03a_panel_ingresos_limpio.rds")
PATH_03A_PANEL_INGRESOS <- PATH_03A_PANEL_ING    # alias usado en 03a_ingresos_limpieza.R y 03b_ich.R
PATH_03B_PANEL_BASE     <- file.path(DIR_DATOS, "03b_panel_base_completo.rds")
PATH_03B_PANEL_ICH      <- PATH_03B_PANEL_BASE   # alias usado en 03b_ich.R y 03c_reporte_base.R
PATH_CONTRATO_03B       <- file.path(DIR_CONTRATOS, "03b_contrato_ich.rds")  # usado en 03b/03c
PATH_03B_MCA_FIT     <- file.path(DIR_MODELOS,  "03b_mca_fit.rds")
PATH_03C_HTML        <- file.path(DIR_REPORTES, "03c_reporte_ich.html")
PATH_03C_DIAG_RDS    <- file.path(DIR_REPORTES, "03c_diagnostico_ich.rds")
PATH_03C_CONTRIB_CSV <- file.path(DIR_REPORTES, "03c_tabla_contribuciones.csv")
PATH_03C_ESTAB_CSV   <- file.path(DIR_REPORTES, "03c_estabilidad_temporal.csv")
PATH_04_PANEL_PROXIES <- file.path(DIR_DATOS, "04_panel_con_proxies.rds")
PATH_CONTRATO_05      <- file.path(DIR_CONTRATOS, "05_contrato_proxies.rds")
PATH_05_HTML          <- file.path(DIR_REPORTES, "05_reporte_proxies.html")
PATH_05_CSV_DESC      <- file.path(DIR_REPORTES, "05_tabla_descriptiva_proxies.csv")
PATH_05_CSV_COR       <- file.path(DIR_REPORTES, "05_matriz_correlaciones.csv")
PATH_05_CSV_TESTS     <- file.path(DIR_REPORTES, "05_tests_kotlarski.csv")
PATH_05_CSV_TEMPORAL  <- file.path(DIR_REPORTES, "05_evolucion_temporal_proxies.csv")
PATH_05_DIAG_RDS      <- file.path(DIR_REPORTES, "05_diagnostico_proxies.rds")
PATH_05_RMD           <- here::here("script", "02_proxies", "05_reporte_proxies.Rmd")

# UMBRALES FRAMEWORK KOTLARSKI (usados en 05_reporte_proxies.R)
UMBRAL_INTRA_COR <- 0.85   # máx correlación intra-factor (T3, T4)
UMBRAL_CROSS_COR <- 0.50   # máx correlación cross-factor (T5)
UMBRAL_SD_MIN    <- 0.10   # mín desviación estándar (detección proxies degenerados)

# ── 7. PATHS CAPA 3 — HETEROFACTOR ───────────────────────────────────────────
PATH_06_THETA         <- file.path(DIR_DATOS,     "06_theta_predichos.rds")
PATH_06_MODELO_HETERO <- file.path(DIR_MODELOS,   "06_modelo_heterofactor.rds")
PATH_CONTRATO_06      <- file.path(DIR_CONTRATOS, "06_contrato_heterofactor.rds")
PATH_CONTRATO_06B     <- file.path(DIR_CONTRATOS, "06b_contrato_diagnostico_theta.rds")
PATH_CONTRATO_06C     <- file.path(DIR_CONTRATOS, "06c_contrato_comparativo.rds")
PATH_HTML_06D         <- file.path(DIR_REPORTES,  "06d_reporte_heterofactor.html")
PATH_TXT_06D          <- file.path(DIR_REPORTES,  "06d_notas_paper.txt")

# ── 8. PATHS CAPA 4 — MODELADO (con sufijo dinámico) ─────────────────────────

# — LPM —
PATH_07_MODELO_LASSO   <- file.path(DIR_MODELOS,   paste0("07_modelo_lasso_",            SUFIJO_MODELO_LPM, ".rds"))
PATH_07_RECIPE_LASSO   <- file.path(DIR_MODELOS,   paste0("07_recipe_lasso_",            SUFIJO_MODELO_LPM, ".rds"))
PATH_07_CONTRATO       <- file.path(DIR_CONTRATOS, paste0("07a_contrato_lasso_",         SUFIJO_MODELO_LPM, ".rds"))
PATH_07B_CONTRATO      <- file.path(DIR_CONTRATOS, paste0("07b_contrato_postlasso_",     SUFIJO_MODELO_LPM, ".rds"))
PATH_07C_CONTRATO      <- file.path(DIR_CONTRATOS, paste0("07c_contrato_tiempo_",        SUFIJO_MODELO_LPM, ".rds"))
PATH_07D_CONTRATO      <- file.path(DIR_CONTRATOS, paste0("07d_contrato_interacciones_", SUFIJO_MODELO_LPM, ".rds"))
PATH_07E_HTML          <- file.path(DIR_REPORTES,  paste0("07e_reporte_LPM_",            SUFIJO_MODELO_LPM, ".html"))
PATH_07E_TXT_LPM       <- file.path(DIR_REPORTES,  paste0("07e_notas_paper_",            SUFIJO_MODELO_LPM, ".txt"))
# Aliases _LPM explícitos — para uso simétrico en scripts comparativos (sección 11)
PATH_07_MODELO_LPM     <- PATH_07_MODELO_LASSO
PATH_07_RECIPE_LPM     <- PATH_07_RECIPE_LASSO
PATH_07_POSTLASSO_LPM  <- file.path(DIR_MODELOS,   paste0("07b_postlasso_",              SUFIJO_MODELO_LPM, ".rds"))
PATH_07A_CONTRATO_LPM  <- PATH_07_CONTRATO
PATH_07B_CONTRATO_LPM  <- PATH_07B_CONTRATO
PATH_07C_CONTRATO_LPM  <- PATH_07C_CONTRATO
PATH_07D_CONTRATO_LPM  <- PATH_07D_CONTRATO

# — GLM —
PATH_07_MODELO_GLM     <- file.path(DIR_MODELOS,   paste0("07_modelo_lasso_",            SUFIJO_MODELO_GLM, ".rds"))
PATH_07_RECIPE_GLM     <- file.path(DIR_MODELOS,   paste0("07_recipe_lasso_",            SUFIJO_MODELO_GLM, ".rds"))
PATH_07_CONTRATO_GLM   <- file.path(DIR_CONTRATOS, paste0("07a_contrato_lasso_",         SUFIJO_MODELO_GLM, ".rds"))
PATH_07B_CONTRATO_GLM  <- file.path(DIR_CONTRATOS, paste0("07b_contrato_postlasso_",     SUFIJO_MODELO_GLM, ".rds"))
PATH_07C_CONTRATO_GLM  <- file.path(DIR_CONTRATOS, paste0("07c_contrato_tiempo_",        SUFIJO_MODELO_GLM, ".rds"))
PATH_07D_CONTRATO_GLM  <- file.path(DIR_CONTRATOS, paste0("07d_contrato_interacciones_", SUFIJO_MODELO_GLM, ".rds"))
PATH_07E_HTML_GLM      <- file.path(DIR_REPORTES,  paste0("07e_reporte_GLM_",            SUFIJO_MODELO_GLM, ".html"))
PATH_07E_TXT_GLM       <- file.path(DIR_REPORTES,  paste0("07e_notas_paper_",            SUFIJO_MODELO_GLM, ".txt"))
# Aliases _GLM simétricos con patrón A/B — para uso en scripts comparativos
PATH_07A_CONTRATO_GLM  <- PATH_07_CONTRATO_GLM
PATH_07_POSTLASSO_GLM  <- file.path(DIR_MODELOS,   paste0("07b_postlasso_",              SUFIJO_MODELO_GLM, ".rds"))

# — SLS —
PATH_07_MODELO_SLS     <- file.path(DIR_MODELOS,   paste0("07_modelo_lasso_",            SUFIJO_MODELO_SLS, ".rds"))
PATH_07_RECIPE_SLS     <- file.path(DIR_MODELOS,   paste0("07_recipe_lasso_",            SUFIJO_MODELO_SLS, ".rds"))
PATH_07_CONTRATO_SLS   <- file.path(DIR_CONTRATOS, paste0("07a_contrato_lasso_",         SUFIJO_MODELO_SLS, ".rds"))
PATH_07A_CONTRATO_SLS  <- PATH_07_CONTRATO_SLS     # alias explícito usado en 07a_lasso_SLS.R
PATH_07_POSTLASSO_SLS  <- file.path(DIR_MODELOS,   paste0("07b_postlasso_",              SUFIJO_MODELO_SLS, ".rds"))
PATH_07B_CONTRATO_SLS  <- file.path(DIR_CONTRATOS, paste0("07b_contrato_postlasso_",     SUFIJO_MODELO_SLS, ".rds"))
PATH_07C_CONTRATO_SLS  <- file.path(DIR_CONTRATOS, paste0("07c_contrato_tiempo_",        SUFIJO_MODELO_SLS, ".rds"))
PATH_07D_CONTRATO_SLS  <- file.path(DIR_CONTRATOS, paste0("07d_contrato_interacciones_", SUFIJO_MODELO_SLS, ".rds"))
PATH_07E_HTML_SLS      <- file.path(DIR_REPORTES,  paste0("07e_reporte_SLS_",            SUFIJO_MODELO_SLS, ".html"))
PATH_07E_TXT_SLS       <- file.path(DIR_REPORTES,  paste0("07e_notas_paper_",            SUFIJO_MODELO_SLS, ".txt"))

# ── 9. PATHS CAPA 5 — BACKCASTING ────────────────────────────────────────────
#
# Convención:
#   PATH_08_PANEL_*      → panel acumulado con predicciones del modelo *
#   PATH_08_CONTRATO_*   → contrato del backcasting (metadatos + métricas)
#   PATH_08B_HTML_*      → reporte HTML diagnóstico del backcasting
#   PATH_08C_HTML_*      → reporte HTML variable híbrida
#
# Cadena de inputs/outputs:
#   08_panel_formalidad_LPM3T.rds  (90 cols)   ← output 08_backcasting_LPM.R
#   08_panel_formalidad_GLM3T.rds  (100 cols)  ← output 08_backcasting_GLM.R  [input = panel LPM]
#   08_panel_formalidad_SLS3T.rds  (~110 cols) ← output 08_backcasting_SLS.R  [input = panel GLM]

# — LPM —
PATH_08_PANEL_LPM      <- file.path(DIR_DATOS,     paste0("08_panel_formalidad_",        SUFIJO_MODELO_LPM, ".rds"))
PATH_08_PANEL_BACKCAST <- PATH_08_PANEL_LPM        # alias legacy — preservado para scripts ya ejecutados
PATH_08_CONTRATO_LPM   <- file.path(DIR_CONTRATOS, paste0("08_contrato_backcasting_",    SUFIJO_MODELO_LPM, ".rds"))
PATH_08B_HTML_LPM      <- file.path(DIR_REPORTES,  paste0("08b_reporte_backcasting_",    SUFIJO_MODELO_LPM, ".html"))
PATH_08C_HTML_LPM      <- file.path(DIR_REPORTES,  paste0("08c_reporte_hibrido_",        SUFIJO_MODELO_LPM, ".html"))

# — GLM —
PATH_08_PANEL_GLM      <- file.path(DIR_DATOS,     paste0("08_panel_formalidad_",        SUFIJO_MODELO_GLM, ".rds"))
PATH_08_CONTRATO_GLM   <- file.path(DIR_CONTRATOS, paste0("08_contrato_backcasting_",    SUFIJO_MODELO_GLM, ".rds"))
PATH_08B_HTML_GLM      <- file.path(DIR_REPORTES,  paste0("08b_reporte_backcasting_",    SUFIJO_MODELO_GLM, ".html"))
PATH_08C_HTML_GLM      <- file.path(DIR_REPORTES,  paste0("08c_reporte_hibrido_",        SUFIJO_MODELO_GLM, ".html"))

# — SLS — (panel SLS es el más completo: contiene columnas LPM + GLM + SLS)
PATH_08_PANEL_SLS      <- file.path(DIR_DATOS,     paste0("08_panel_formalidad_",        SUFIJO_MODELO_SLS, ".rds"))
PATH_08_CONTRATO_SLS   <- file.path(DIR_CONTRATOS, paste0("08_contrato_backcasting_",    SUFIJO_MODELO_SLS, ".rds"))
PATH_08B_HTML_SLS      <- file.path(DIR_REPORTES,  paste0("08b_reporte_backcasting_",    SUFIJO_MODELO_SLS, ".html"))
PATH_08C_HTML_SLS      <- file.path(DIR_REPORTES,  paste0("08c_reporte_hibrido_",        SUFIJO_MODELO_SLS, ".html"))

# Panel consolidado — alias para scripts que necesitan los tres modelos a la vez
# El panel SLS hereda columnas LPM y GLM → es la fuente canónica para comparativos
PATH_08_PANEL_CONSOLIDADO <- PATH_08_PANEL_SLS

# ── 10. PATHS CAPA 6 — REPORTE FINAL (por modelo) ────────────────────────────
# (usados por scripts 09a/09b individuales de cada modelo, si se generan)

# — LPM —
PATH_09A_CONTRATO      <- file.path(DIR_CONTRATOS, paste0("09a_contrato_calculos_",      SUFIJO_MODELO_LPM, ".rds"))
PATH_09B_HTML          <- file.path(DIR_REPORTES,  paste0("09b_reporte_",                SUFIJO_MODELO_LPM, ".html"))

# — GLM —
PATH_09A_CONTRATO_GLM  <- file.path(DIR_CONTRATOS, paste0("09a_contrato_calculos_",      SUFIJO_MODELO_GLM, ".rds"))
PATH_09B_HTML_GLM      <- file.path(DIR_REPORTES,  paste0("09b_reporte_",                SUFIJO_MODELO_GLM, ".html"))

# — SLS —
PATH_09A_CONTRATO_SLS  <- file.path(DIR_CONTRATOS, paste0("09a_contrato_calculos_",      SUFIJO_MODELO_SLS, ".rds"))
PATH_09B_HTML_SLS      <- file.path(DIR_REPORTES,  paste0("09b_reporte_",                SUFIJO_MODELO_SLS, ".html"))

# ── 11. PATHS CAPA 6 — REPORTES COMPARATIVOS ─────────────────────────────────
#
# Scripts en: script/06_comparativo/
#   09a_comp_modelado.R        → compara modelos post-LASSO (coefs, métricas, LASSO selección)
#   09b_comp_retropredictivo.R → compara series de backcasting puro (sin imputación)
#   09c_comp_hibrido.R         → compara variable híbrida (GT + predicción) de los tres modelos
#
# Inputs de 09a: contratos 07a/07b/07c/07d × LPM/GLM/SLS (ya definidos en sección 8)
# Inputs de 09b: PATH_08_PANEL_CONSOLIDADO + contratos 08 × LPM/GLM/SLS (sección 9)
# Inputs de 09c: PATH_08_PANEL_CONSOLIDADO + PATH_02_PANEL_TAX + contratos 08 × LPM/GLM/SLS

DIR_COMPARATIVO     <- file.path(RUTA_SCRIPTS, "06_comparativo")
dir.create(DIR_COMPARATIVO, showWarnings = FALSE, recursive = TRUE)

PATH_09A_COMP_HTML  <- file.path(DIR_REPORTES, "09a_comp_modelado.html")
PATH_09A_COMP_NOTAS <- file.path(DIR_REPORTES, "09a_comp_modelado_notas.txt")

PATH_09B_COMP_HTML     <- file.path(DIR_REPORTES,  "09b_comp_retropredictivo.html")
PATH_09B_COMP_NOTAS    <- file.path(DIR_REPORTES,  "09b_comp_retropredictivo_notas.txt")
PATH_09B_COMP_CONTRATO <- file.path(DIR_CONTRATOS, "09b_contrato_comp_retropredictivo.rds")

PATH_09C_COMP_HTML     <- file.path(DIR_REPORTES,  "09c_comp_hibrido.html")
PATH_09C_COMP_NOTAS    <- file.path(DIR_REPORTES,  "09c_comp_hibrido_notas.txt")
PATH_09C_COMP_CONTRATO <- file.path(DIR_CONTRATOS, "09c_contrato_comp_hibrido.rds")

# Aliases cortos — usados directamente en los scripts 09a/09b/09c
PATH_09A_HTML       <- PATH_09A_COMP_HTML
PATH_09A_NOTAS      <- PATH_09A_COMP_NOTAS
PATH_09B_HTML       <- PATH_09B_COMP_HTML
PATH_09B_NOTAS      <- PATH_09B_COMP_NOTAS
PATH_09C_HTML       <- PATH_09C_COMP_HTML
PATH_09C_NOTAS      <- PATH_09C_COMP_NOTAS

# ── Niveles esperados de variables categóricas ────────────────────────────────
# (usados para validaciones en múltiples scripts)

SECCION_LEVELS <- c(
  "Admin Publica y Defensa",
  "Agro y Mineria",
  "Comercio",
  "Construccion",
  "Enseñanza",
  "Financieras e Inmobiliarias",
  "Hoteleria y Gastronomia",
  "Industria",
  "Informatica y Comunicaciones",
  "Organismos Extraterritoriales",
  "Otros/Desconocido",
  "Profesionales y Administrativas",
  "Salud",
  "Servicio Domestico",
  "Servicios Personales y Comunitarios",
  "Transporte",
  "No aplica (No PEA)",        # Para no-PEA
  "Sin Experiencia Previa"     # Para imputación
)

CALIFICACION_LEVELS <- c(
  "Anómalo",
  "No calificado",
  "Ns/Nc",
  "Operativo",
  "Profesional",
  "Técnico",
  "No aplica (No PEA)",        # Para no-PEA
  "Sin Calificación"           # Para imputación
)

# ── 12. FIGURAS ───────────────────────────────────────────────────────────────
# Directorio raíz para figuras exportadas al paper (LaTeX / \includegraphics{})
# Subdirectorios nombrados según el script que genera cada reporte.
# La función guardar_figura() en theme_paper.R usa basename(dir_destino) como
# prefijo del nombre de archivo — mantener consistencia con estos nombres.

DIR_FIGURAS         <- file.path(RUTA_OUTPUTS, "figuras")

# Scripts sin modelos (03c, 05, 06d) — usan PAL_DESCRIPTIVO
DIR_FIGURAS_03C     <- file.path(DIR_FIGURAS, "03c_reporte_base")
DIR_FIGURAS_05      <- file.path(DIR_FIGURAS, "05_reporte_proxies")
DIR_FIGURAS_06D     <- file.path(DIR_FIGURAS, "06d_reporte_heterofactor")

# Scripts de modelado — por modelo (LPM / GLM / SLS)
DIR_FIGURAS_07E_LPM <- file.path(DIR_FIGURAS, "07e_lasso_reporte_LPM")
DIR_FIGURAS_07E_GLM <- file.path(DIR_FIGURAS, "07e_lasso_reporte_GLM")
DIR_FIGURAS_07E_SLS <- file.path(DIR_FIGURAS, "07e_lasso_reporte_SLS")

# Scripts de backcasting — diagnóstico y variable híbrida
DIR_FIGURAS_08B_LPM <- file.path(DIR_FIGURAS, "08b_reporte_backcasting_LPM")
DIR_FIGURAS_08B_GLM <- file.path(DIR_FIGURAS, "08b_reporte_backcasting_GLM")
DIR_FIGURAS_08B_SLS <- file.path(DIR_FIGURAS, "08b_reporte_backcasting_SLS")
DIR_FIGURAS_08C_LPM <- file.path(DIR_FIGURAS, "08c_reporte_hibrido_LPM")
DIR_FIGURAS_08C_GLM <- file.path(DIR_FIGURAS, "08c_reporte_hibrido_GLM")
DIR_FIGURAS_08C_SLS <- file.path(DIR_FIGURAS, "08c_reporte_hibrido_SLS")

# Scripts comparativos
DIR_FIGURAS_09A     <- file.path(DIR_FIGURAS, "09a_comp_modelado")
DIR_FIGURAS_09B     <- file.path(DIR_FIGURAS, "09b_comp_retropredictivo")
DIR_FIGURAS_09C     <- file.path(DIR_FIGURAS, "09c_comp_hibrido")

# Scripts de robustez (capa 7)
DIR_FIGURAS_10A     <- file.path(DIR_FIGURAS, "10a_robustness_heterofactor")
DIR_FIGURAS_10B     <- file.path(DIR_FIGURAS, "10b_comparacion_modeloA")
DIR_FIGURAS_10J     <- file.path(DIR_FIGURAS, "10j_loco_aglomerado")
DIR_FIGURAS_10N     <- file.path(DIR_FIGURAS, "10n_subgroup_stability")
DIR_FIGURAS_10O     <- file.path(DIR_FIGURAS, "10o_ml_benchmark")

# Crear estructura de directorios de figuras
for (dir in c(
  DIR_FIGURAS,
  DIR_FIGURAS_03C, DIR_FIGURAS_05, DIR_FIGURAS_06D,
  DIR_FIGURAS_07E_LPM, DIR_FIGURAS_07E_GLM, DIR_FIGURAS_07E_SLS,
  DIR_FIGURAS_08B_LPM, DIR_FIGURAS_08B_GLM, DIR_FIGURAS_08B_SLS,
  DIR_FIGURAS_08C_LPM, DIR_FIGURAS_08C_GLM, DIR_FIGURAS_08C_SLS,
  DIR_FIGURAS_09A, DIR_FIGURAS_09B, DIR_FIGURAS_09C,
  DIR_FIGURAS_10A, DIR_FIGURAS_10B, DIR_FIGURAS_10J, DIR_FIGURAS_10N, DIR_FIGURAS_10O
)) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# ── 13. MENSAJES INFORMATIVOS ─────────────────────────────────────────────────
cat("═══════════════════════════════════════════════════\n")
cat("📂 CONFIGURACIÓN GLOBAL CARGADA\n")
cat("═══════════════════════════════════════════════════\n")
cat("Proyecto:  ", RUTA_PROYECTO, "\n")
cat("rdos/:     ", RUTA_OUTPUTS,  "\n")
cat("Bases EPH: ", RUTA_BASES,    "\n")
cat("Rango:     ", ANIO_INI, "T", TRIM_INI, " -> ", ANIO_FIN, "T", TRIM_FIN, "\n", sep = "")
cat("Modelos:   ", SUFIJO_MODELO_LPM, "/", SUFIJO_MODELO_GLM, "/", SUFIJO_MODELO_SLS, "\n")
cat("Cores:     ", N_CORES,  "\n")
cat("Seed:      ", SEED_GLOBAL, "\n")
cat("═══════════════════════════════════════════════════\n\n")
