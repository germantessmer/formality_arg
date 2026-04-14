# =============================================================================
# [EN] limpieza.R -- Data cleaning functions for EPH variables: income, age, tenure, haven labels
# INPUTS:  None (function definitions only)
# OUTPUTS: Functions: limpiar_edad(), limpiar_ingresos(), armar_antiguedad(), zap_num(), etc.
# =============================================================================
# ==============================================================================
# limpieza.R
# Funciones de limpieza y normalización de variables base
# ==============================================================================
# 📌 OBJETIVO:
#    Proveer funciones de limpieza reutilizables para variables EPH.
#    Se carga DESPUÉS de helpers.R y validaciones.R.
#
# 📦 DEPENDENCIAS (deben estar cargadas en el script principal):
#    haven, data.table, dplyr, purrr, tibble, stringr
#
# 📤 FUNCIONES EXPORTADAS:
#    · limpiar_edad()                  — numérico, reemplaza valores <= 0 con NA
#    · limpiar_ingresos()              — reemplaza códigos especiales EPH con NA
#    · armar_antiguedad()              — coalesce de columnas de antigüedad + limpia negativos
#    · zap_num()                       — zap labels haven → numérico
#    · zap_chr()                       — zap labels haven → character
#    · periodo_num()                   — año + trimestre → entero ordenable (YYYYT)
#    · parse_anio_trim_from_filename() — extrae año y trimestre del nombre de archivo EPH
# ==============================================================================


#' Limpiar variable edad
#'
#' Convierte edad a numérico y reemplaza valores <= 0 con NA.
#'
#' @param x Vector de edad (puede ser labelled haven)
#' @return  Vector numérico con edades válidas (> 0) o NA
#'
#' @examples
#' edad_limpia <- limpiar_edad(datos$edad)
limpiar_edad <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(haven::zap_labels(x))))
  data.table::fifelse(!is.na(x_num) & x_num <= 0, NA_real_, x_num)
}


#' Limpiar códigos especiales de ingresos
#'
#' Reemplaza los códigos especiales EPH (-7, -8, -9) con NA.
#'
#' @param x Vector de ingreso (puede ser labelled haven)
#' @return  Vector numérico con códigos especiales reemplazados por NA
#'
#' @examples
#' ingreso_limpio <- limpiar_ingresos(datos$ingreso_real_total_individual)
limpiar_ingresos <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(haven::zap_labels(x))))
  data.table::fifelse(x_num %in% c(-7, -8, -9), NA_real_, x_num)
}


#' Armar variable de antigüedad unificada
#'
#' Hace coalesce de múltiples columnas de antigüedad (principal, independiente,
#' anterior) y limpia valores negativos resultantes.
#'
#' @param df              Data frame con columnas de antigüedad
#' @param cols_antiguedad Vector character con nombres de columnas
#'                        (default: nombres estándar EPH)
#' @return                Vector numérico de antigüedad unificada
#'
#' @examples
#' datos$antiguedad <- armar_antiguedad(datos)
armar_antiguedad <- function(df,
                              cols_antiguedad = c("antiguedad_ocup_principal",
                                                  "antiguedad_ocup_indep",
                                                  "antiguedad_ocup_anterior")) {
  # Detectar columnas disponibles en el dataset
  cols_ok <- intersect(cols_antiguedad, names(df))

  if (length(cols_ok) == 0) {
    warning("armar_antiguedad: no se encontraron columnas de antigüedad. Devolviendo NA.",
            call. = FALSE)
    return(rep(NA_real_, nrow(df)))
  }

  # Convertir cada columna a numérico (eliminar etiquetas haven)
  mat <- purrr::map(cols_ok, ~ {
    suppressWarnings(as.numeric(as.character(haven::zap_labels(df[[.x]]))))
  })

  # Coalesce: primer valor no-NA entre las columnas disponibles
  out <- do.call(dplyr::coalesce, mat)

  # Limpiar negativos residuales
  out <- data.table::fifelse(!is.na(out) & out < 0, NA_real_, out)

  return(out)
}


#' Zap labels y convertir a numérico
#'
#' Helper para manejar variables labelled haven de forma segura.
#'
#' @param x Vector (puede ser labelled haven)
#' @return  Vector numérico
zap_num <- function(x) {
  suppressWarnings(as.numeric(as.character(haven::zap_labels(x))))
}


#' Zap labels y convertir a character
#'
#' Helper para manejar variables labelled haven de forma segura.
#'
#' @param x Vector (puede ser labelled haven)
#' @return  Vector character
zap_chr <- function(x) {
  as.character(haven::zap_labels(x))
}


#' Generar periodo_num (entero ordenable)
#'
#' Convierte año y trimestre a un número ordenable: YYYY * 10 + T.
#' Ejemplo: 2024, 3 → 20243.
#'
#' @param anio Vector de años
#' @param trim Vector de trimestres (1–4)
#' @return     Vector entero con periodo numérico
#'
#' @examples
#' datos$periodo_num <- periodo_num(datos$anio, datos$trimestre)
periodo_num <- function(anio, trim) {
  as.integer(anio) * 10L + as.integer(trim)
}


#' Parsear año y trimestre desde nombre de archivo EPH
#'
#' Extrae año y trimestre de archivos con patrón "EPH2024_T3.RData".
#' Devuelve un tibble con una fila por archivo.
#'
#' @param path Vector de rutas de archivos
#' @return     Tibble con columnas: archivo, anio, trim, periodo_num
#'
#' @examples
#' archivos <- list.files("C:/oes/eph_rdos/capa2/", pattern = "EPH.*\\.RData",
#'                        full.names = TRUE)
#' info <- parse_anio_trim_from_filename(archivos)
parse_anio_trim_from_filename <- function(path) {
  nm <- basename(path)
  m  <- stringr::str_match(nm, "EPH(\\d{4})_T([1-4])\\.RData")

  tibble::tibble(
    archivo = path,
    anio    = suppressWarnings(as.integer(m[, 2])),
    trim    = suppressWarnings(as.integer(m[, 3]))
  ) %>%
    dplyr::mutate(periodo_num = periodo_num(anio, trim))
}

# ── Fin ───────────────────────────────────────────────────────────────────────
