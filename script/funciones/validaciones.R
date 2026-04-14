# =============================================================================
# [EN] validaciones.R -- Validation functions: hard_stop, soft_warn, contract checks, completeness audits
# INPUTS:  None (function definitions only)
# OUTPUTS: Functions: hard_stop(), validate_ids_unique(), contract_check(), etc.
# =============================================================================
# ==============================================================================
# validaciones.R
# Funciones de validación, hard stops y contract checks
# ==============================================================================
# 📌 OBJETIVO:
#    Proveer funciones de validación reutilizables para asegurar integridad
#    de datos a lo largo del pipeline. Se carga DESPUÉS de helpers.R.
#
# 📦 DEPENDENCIAS (deben estar cargadas en el script principal):
#    dplyr
#
# 📤 FUNCIONES EXPORTADAS:
#    · hard_stop()                   — stop condicional con mensaje formateado
#    · soft_warn()                   — warning condicional con mensaje formateado
#    · validate_ids_unique()         — hard stop si hay IDs duplicados
#    · validate_completitud()        — reporte de NA% con warning si supera umbral
#    · contract_check()              — valida campos requeridos en objeto de contrato
#    · validate_valores_esperados()  — hard stop si hay valores fuera del set esperado
#    · validate_cobertura_temporal() — verifica rango de periodo_num
# ==============================================================================


#' Hard stop condicional
#'
#' Stop con mensaje formateado si la condición no se cumple.
#'
#' @param cond Condición lógica (debe ser TRUE para continuar)
#' @param msg  Mensaje de error
#'
#' @examples
#' hard_stop(nrow(datos) > 0, "Dataset vacío")
hard_stop <- function(cond, msg) {
  if (!isTRUE(cond)) {
    stop(paste0("❌ ", msg), call. = FALSE)
  }
}


#' Soft warning condicional
#'
#' Warning con mensaje formateado si la condición no se cumple.
#'
#' @param cond Condición lógica
#' @param msg  Mensaje de warning
soft_warn <- function(cond, msg) {
  if (!isTRUE(cond)) {
    warning(paste0("⚠️ ", msg), call. = FALSE)
  }
}


#' Validar unicidad de IDs
#'
#' Hard stop si hay IDs duplicados en la columna especificada.
#'
#' @param df     Data frame
#' @param id_col Nombre de columna de ID (character)
#'
#' @examples
#' validate_ids_unique(datos, "id_individuo")
validate_ids_unique <- function(df, id_col) {
  if (!id_col %in% names(df)) {
    hard_stop(FALSE, paste0("Columna '", id_col, "' no existe en el dataset."))
  }

  n_dup <- sum(duplicated(df[[id_col]]))

  hard_stop(
    n_dup == 0,
    paste0("IDs duplicados en '", id_col, "': ", n_dup, " duplicados encontrados.")
  )

  cat("✅ IDs únicos en '", id_col, "': OK\n", sep = "")
}


#' Validar completitud de variables críticas
#'
#' Reporta NA% de cada variable y genera warning si alguna supera el umbral.
#'
#' @param df            Data frame
#' @param vars_criticas Vector character con nombres de columnas a chequear
#' @param umbral_warn   Umbral de NA% para warning (default: 10)
#'
#' @examples
#' validate_completitud(datos, c("seccion", "calificacion", "ingreso_real_final"))
validate_completitud <- function(df, vars_criticas, umbral_warn = 10) {
  cat("\n📊 Completitud de variables críticas:\n")

  for (v in vars_criticas) {
    if (!v %in% names(df)) {
      cat(sprintf("   %-35s: 🛑 FALTA EN DATASET\n", v))
      next
    }

    n_nas   <- sum(is.na(df[[v]]))
    pct_ok  <- round(100 * (1 - n_nas / nrow(df)), 1)
    pct_na  <- 100 - pct_ok
    simbolo <- if (pct_na > umbral_warn) "⚠️" else "✅"

    cat(sprintf("   %s %-35s: %5.1f%% completo (%s NAs)\n",
                simbolo, v, pct_ok, format(n_nas, big.mark = ".", decimal.mark = ",")))

    if (pct_na > umbral_warn) {
      warning(paste0("Variable '", v, "' tiene ", pct_na,
                     "% NAs (umbral: ", umbral_warn, "%)"),
              call. = FALSE)
    }
  }
  cat("\n")
}


#' Validar estructura de un objeto de contrato
#'
#' Hard stop si el contrato (lista) no tiene todos los campos requeridos.
#'
#' @param contrato        Lista (objeto de contrato)
#' @param required_fields Vector character con campos obligatorios
#' @param contract_name   Nombre del contrato (para mensajes)
#'
#' @examples
#' contract_check(contrato_03b, c("timestamp", "N_total", "vars_M"), "contrato_03b")
contract_check <- function(contrato, required_fields, contract_name = "contrato") {
  missing <- setdiff(required_fields, names(contrato))

  if (length(missing) > 0) {
    hard_stop(
      FALSE,
      paste0("Contrato '", contract_name, "' incompleto. Faltan campos: ",
             paste(missing, collapse = ", "))
    )
  }

  cat("✅ Contrato '", contract_name, "': campos requeridos OK\n", sep = "")
}


#' Validar que una variable no tiene valores fuera del set esperado
#'
#' Hard stop si aparecen valores inesperados (máx. 10 mostrados en el mensaje).
#'
#' @param x                Vector a validar
#' @param valores_esperados Vector con valores permitidos
#' @param var_name          Nombre de la variable (para mensajes)
#' @param allow_na          Permitir NAs sin error (default: TRUE)
#'
#' @examples
#' validate_valores_esperados(datos$sexo, c("Varones", "Mujeres"), "sexo")
validate_valores_esperados <- function(x, valores_esperados, var_name, allow_na = TRUE) {
  x_clean     <- if (allow_na) na.omit(x) else x
  x_unique    <- unique(as.character(x_clean))
  inesperados <- setdiff(x_unique, valores_esperados)

  if (length(inesperados) > 0) {
    hard_stop(
      FALSE,
      paste0("Variable '", var_name, "' tiene valores inesperados: ",
             paste(head(inesperados, 10), collapse = ", "),
             ifelse(length(inesperados) > 10, " ...", ""))
    )
  }

  cat("✅ Variable '", var_name, "': valores esperados OK\n", sep = "")
}


#' Validar cobertura temporal del panel
#'
#' Hard stop si el rango de periodo_num no coincide con el esperado.
#' Usada al final de cada script de datos base para garantizar integridad.
#'
#' @param df                    Data frame con columna periodo_num
#' @param periodo_min_esperado  Entero con periodo mínimo esperado (ej: 20164)
#' @param periodo_max_esperado  Entero con periodo máximo esperado (ej: 20252)
#'
#' @examples
#' validate_cobertura_temporal(datos, PERIODO_INI, PERIODO_FIN)
validate_cobertura_temporal <- function(df, periodo_min_esperado, periodo_max_esperado) {
  if (!"periodo_num" %in% names(df)) {
    hard_stop(FALSE, "Dataset no tiene columna 'periodo_num'.")
  }

  periodo_min_obs <- min(df$periodo_num, na.rm = TRUE)
  periodo_max_obs <- max(df$periodo_num, na.rm = TRUE)

  hard_stop(
    periodo_min_obs == periodo_min_esperado,
    paste0("Periodo mínimo observado (", periodo_min_obs,
           ") != esperado (", periodo_min_esperado, ")")
  )

  hard_stop(
    periodo_max_obs == periodo_max_esperado,
    paste0("Periodo máximo observado (", periodo_max_obs,
           ") != esperado (", periodo_max_esperado, ")")
  )

  cat("✅ Cobertura temporal OK: ", periodo_min_obs, " → ", periodo_max_obs, "\n", sep = "")
}

# ── Fin ───────────────────────────────────────────────────────────────────────
