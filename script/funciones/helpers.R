# =============================================================================
# [EN] helpers.R -- Utility functions: safe_scale, pct_na, safe_load, safe_get, contract helpers
# INPUTS:  None (function definitions only)
# OUTPUTS: Functions: reportar_perdida(), safe_scale(), safe_load(), safe_get(), etc.
# =============================================================================
# ==============================================================================
# helpers.R
# Funciones auxiliares generales
# ==============================================================================
# 📌 OBJETIVO:
#    Proveer utilidades base usadas transversalmente en todo el pipeline.
#    Este módulo se carga PRIMERO dentro de funciones_comunes.R porque
#    los demás módulos (validaciones, limpieza, taxonomia) dependen de él.
#
# 📦 DEPENDENCIAS (deben estar cargadas en el script principal):
#    haven, data.table, dplyr
#
# 📤 FUNCIONES EXPORTADAS:
#    · reportar_perdida()       — auditoría de pérdida de observaciones en filtros
#    · safe_scale()             — escalado con protección contra sd = 0
#    · safe_log1p_nonneg()      — log(x+1) con protección contra negativos
#    · pct_na()                 — porcentaje de NAs
#    · n_unique()               — cantidad de valores únicos (sin NA)
#    · share_zeros()            — proporción de ceros en un vector
#    · key_trim()               — normalización de keys para joins con diccionarios
#    · `%||%`                   — operador coalesce (primer no-NULL)
#    · detect_var_name()        — detecta nombre real de variable (cambios EPH entre trimestres)
#    · safe_extract_var()       — extrae columna probando múltiples aliases
# ==============================================================================


#' Reportar pérdida de observaciones tras un filtro
#'
#' Auditoría estándar de pérdidas. Imprime en consola el antes/después
#' y el porcentaje excluido.
#'
#' @param original  Data frame original (antes del filtro)
#' @param filtrado  Data frame resultante (después del filtro)
#' @param etapa     Nombre descriptivo del filtro (para logging)
#'
#' @examples
#' reportar_perdida(datos_inicial, datos_filtrado, "Exclusión inactivos")
reportar_perdida <- function(original, filtrado, etapa = "Filtro") {
  n_orig    <- nrow(original)
  n_final   <- nrow(filtrado)
  perdidos  <- n_orig - n_final
  pct       <- round((perdidos / n_orig) * 100, 2)

  cat(paste0("\n📋 Auditoría: ", etapa, "\n"))
  cat("   Iniciales:  ", format(n_orig,   big.mark = ".", decimal.mark = ","), "\n")
  cat("   Finales:    ", format(n_final,  big.mark = ".", decimal.mark = ","), "\n")
  cat("   Excluidos:  ", format(perdidos, big.mark = ".", decimal.mark = ","),
      " (", pct, "%)\n", sep = "")
  cat("--------------------------------------------------\n")
}


#' Escalado seguro (protege contra sd = 0)
#'
#' Versión de scale() que maneja casos degenerados. Si sd = 0 o no finito,
#' devuelve un vector de NA con advertencia.
#'
#' @param x Vector numérico
#' @return  Vector escalado, o NA si sd = 0
#'
#' @examples
#' x_scaled <- safe_scale(datos$ingreso)
safe_scale <- function(x) {
  mu  <- mean(x, na.rm = TRUE)
  sdv <- sd(x,   na.rm = TRUE)

  if (!is.finite(sdv) || sdv == 0) {
    warning("safe_scale: sd = 0 o no finito. Devolviendo NA.")
    return(rep(NA_real_, length(x)))
  }

  (x - mu) / sdv
}


#' Logaritmo seguro para valores no negativos
#'
#' Aplica log(x + 1) reemplazando valores negativos por NA antes de transformar.
#'
#' @param x Vector numérico
#' @return  log(x + 1) con NA donde x < 0
safe_log1p_nonneg <- function(x) {
  x2 <- data.table::fifelse(!is.na(x) & x < 0, NA_real_, x)
  log(x2 + 1)
}


#' Calcular porcentaje de NAs
#'
#' @param x Vector (cualquier tipo)
#' @return  Porcentaje de NAs redondeado a 3 decimales (0–100)
pct_na <- function(x) {
  round(100 * mean(is.na(x)), 3)
}


#' Calcular número de valores únicos (excluyendo NA)
#'
#' @param x Vector
#' @return  Entero con cantidad de valores distintos (sin NA)
n_unique <- function(x) {
  dplyr::n_distinct(x, na.rm = TRUE)
}


#' Calcular proporción de ceros
#'
#' Maneja vectores con etiquetas haven antes de evaluar.
#'
#' @param x Vector numérico (puede tener etiquetas haven)
#' @return  Proporción de valores == 0, o NA si todo es NA
share_zeros <- function(x) {
  x2 <- suppressWarnings(as.numeric(as.character(haven::zap_labels(x))))
  if (all(is.na(x2))) return(NA_real_)
  round(mean(x2 == 0, na.rm = TRUE), 5)
}


#' Key trimming para joins con diccionarios
#'
#' Normaliza keys removiendo espacios en los extremos del string.
#'
#' @param x Vector character (o coercible a character)
#' @return  Vector con trimws aplicado
key_trim <- function(x) {
  trimws(as.character(x))
}


#' Operador coalesce: devuelve el primer argumento no-NULL
#'
#' @param a Primer valor
#' @param b Valor de fallback
#' @return  a si no es NULL, sino b
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}


#' Detectar nombre real de variable ante cambios entre trimestres EPH
#'
#' La EPH cambió nombres de variables entre trimestres. Esta función
#' detecta cuál nombre existe realmente en el data frame.
#'
#' @param df          Data frame
#' @param var_aliases Vector de nombres posibles, ordenados por preferencia
#' @return            Nombre de columna que existe, o NA_character_ si ninguno
#'
#' @examples
#' nombre_real <- detect_var_name(datos, c("regtenencia", "ii7", "ii_7"))
detect_var_name <- function(df, var_aliases) {
  for (alias in var_aliases) {
    if (alias %in% names(df)) return(alias)
  }
  return(NA_character_)
}


#' Extraer columna probando múltiples aliases (safe)
#'
#' Wrapper sobre detect_var_name() que devuelve directamente el vector.
#'
#' @param df          Data frame
#' @param var_aliases Vector de nombres posibles
#' @return            Vector de la columna encontrada, o NULL si no existe
safe_extract_var <- function(df, var_aliases) {
  nombre_real <- detect_var_name(df, var_aliases)
  if (is.na(nombre_real)) return(NULL)
  return(df[[nombre_real]])
}

# ── Fin ───────────────────────────────────────────────────────────────────────
