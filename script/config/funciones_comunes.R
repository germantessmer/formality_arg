# =============================================================================
# [EN] funciones_comunes.R -- Central function loader: sources all 5 function modules in correct dependency order
# INPUTS:  script/funciones/{helpers,validaciones,limpieza,taxonomia}.R, script/config/theme_paper.R
# OUTPUTS: All project functions available in the global environment
# =============================================================================
# ==============================================================================
# funciones_comunes.R
# Carga centralizada de todas las funciones modulares del proyecto
# ==============================================================================
# 📌 OBJETIVO:
#    Hacer source() de los 4 módulos de funciones en el orden correcto.
#    Se carga UNA VEZ al inicio de cada script, DESPUÉS de parametros.R.
#
# 📥 REQUISITO PREVIO:
#    source("C:/formalidad_back/script/config/parametros.R")
#
# 📤 RESULTADO:
#    Objetos disponibles en el environment global:
#    · helpers.R      → reportar_perdida, safe_scale, pct_na, n_unique, share_zeros
#    · validaciones.R → hard_stop, soft_warn, validate_ids_unique, validate_completitud
#    · limpieza.R     → limpiar_edad, limpiar_ingresos, armar_antiguedad, zap_num,
#                       zap_chr, periodo_num, parse_anio_trim_from_filename
#    · taxonomia.R    → mapear_seccion, normalizar_calificacion,
#                       get_seccion_levels, get_calificacion_levels
#    · theme_paper.R  → theme_paper, scale_color_modelos, scale_fill_modelos,
#                       scale_linetype_modelos, scale_shape_modelos,
#                       scale_linewidth_poblacion, guardar_figura,
#                       COL_GLM, COL_LPM, COL_SLS, COL_OBSERVADO, COL_BANDA,
#                       PAL_DESCRIPTIVO, ANCHO_FIG, ALTO_FIG
# ==============================================================================

# ── 1. VERIFICAR DEPENDENCIA ──────────────────────────────────────────────────

if (!requireNamespace("here", quietly = TRUE)) {
  stop("📦 Paquete 'here' no instalado. Instalar con: install.packages('here')")
}


# ── 2. RUTA DE FUNCIONES ──────────────────────────────────────────────────────

RUTA_FUNCIONES <- file.path(here::here(), "script", "funciones")

if (!dir.exists(RUTA_FUNCIONES)) {
  stop(paste0(
    "❌ Carpeta de funciones no encontrada: ", RUTA_FUNCIONES, "\n",
    "   Verificá que el .Rproj esté en C:/formalidad_back/ y que exista la carpeta script/funciones/"
  ))
}


# ── 3. CARGA DE MÓDULOS EN ORDEN ─────────────────────────────────────────────
# Orden obligatorio: helpers primero (base), luego los que dependen de él

cat("📚 Cargando funciones comunes...\n")

archivos_funciones <- c(
  "helpers.R",        # 1° — utilidades base (las demás dependen de este)
  "validaciones.R",   # 2° — usa helpers
  "limpieza.R",       # 3° — usa helpers
  "taxonomia.R",      # 4° — usa limpieza
  "derivar_capa2.R",  # 5° — réplica capa1→capa2 (nivel_educ_obtenido2, condicion_formalidad)
  "theme_paper.R"     # 6° — sistema gráfico unificado (requiere ggplot2 + parametros.R)
)

# Diccionario de presentacion — cargado desde config/ (no funciones/)
RUTA_CONFIG <- file.path(here::here(), "script", "config")
ruta_dicc   <- file.path(RUTA_CONFIG, "diccionario_presentacion.R")
if (file.exists(ruta_dicc)) {
  source(ruta_dicc, encoding = "UTF-8")
  cat("   \u2705 diccionario_presentacion.R\n", sep = "")
}

for (archivo in archivos_funciones) {
  ruta_completa <- file.path(RUTA_FUNCIONES, archivo)

  if (!file.exists(ruta_completa)) {
    stop(paste0("❌ Módulo no encontrado: ", ruta_completa))
  }

  source(ruta_completa, encoding = "UTF-8")
  cat("   ✅ ", archivo, "\n", sep = "")
}

cat("📚 Funciones comunes cargadas correctamente\n\n")


# ── 4. VERIFICACIÓN POST-CARGA ───────────────────────────────────────────────
# Confirma que todas las funciones críticas quedaron disponibles en el environment

funciones_esperadas <- c(
  # helpers.R
  "reportar_perdida", "safe_scale", "pct_na", "n_unique", "share_zeros",

  # validaciones.R
  "hard_stop", "soft_warn", "validate_ids_unique", "validate_completitud",

  # limpieza.R
  "limpiar_edad", "limpiar_ingresos", "armar_antiguedad", "zap_num", "zap_chr",
  "periodo_num", "parse_anio_trim_from_filename",

  # taxonomia.R
  "mapear_seccion", "normalizar_calificacion", "get_seccion_levels", "get_calificacion_levels",

  # derivar_capa2.R
  "derivar_vars_capa2",

  # theme_paper.R — funciones
  "theme_paper", "scale_color_modelos", "scale_fill_modelos",
  "scale_linetype_modelos", "scale_shape_modelos",
  "scale_linewidth_poblacion", "guardar_figura",

  # theme_paper.R — constantes
  "COL_GLM", "COL_LPM", "COL_SLS", "COL_OBSERVADO", "COL_BANDA",
  "PAL_DESCRIPTIVO", "ANCHO_FIG", "ALTO_FIG",

  # diccionario_presentacion.R
  "tr", "tr_labs", "tr_levels", "tr_df", "IDIOMA"
)

funciones_faltantes <- funciones_esperadas[!sapply(funciones_esperadas, exists)]

if (length(funciones_faltantes) > 0) {
  warning(paste0(
    "⚠️ Funciones esperadas no encontradas: ",
    paste(funciones_faltantes, collapse = ", ")
  ))
} else {
  cat("✅ Todas las funciones críticas verificadas (", length(funciones_esperadas), ")\n\n", sep = "")
}

# ── Fin ───────────────────────────────────────────────────────────────────────
