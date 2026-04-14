# =============================================================================
# [EN] 00_diccionarios.R -- Build variable dictionaries and crosswalk tables for education variables
# INPUTS:  C:/oes/eph_rdos/capa2/EPH*.RData (raw EPH microdata)
# OUTPUTS: rdos/inputs/diccionarios/00_diccionarios.rds, observed CSVs
# =============================================================================
# 🌟 00_diccionarios.R 🌟 ####

# OBJETIVO: Extraer valores exactos observados en los .RData crudos para
#           asistencia_escuela y nivel_educ_obtenido2, construir diccionarios
#           de mapeo numérico validados, y compilar artefacto consumible por Script 04.
#           El script es ONE-SHOT: crea CSVs pre-completados con taxonomía del
#           sistema educativo argentino validada en proyecto anterior.
# INPUTS:   C:/oes/eph_rdos/capa2/EPH*.RData  (crudos EPH, solo lectura)
# OUTPUTS:  rdos/inputs/diccionarios/observed/00_vals_asistencia.csv
#           rdos/inputs/diccionarios/observed/00_vals_nivel_educ.csv
#           rdos/inputs/diccionarios/observed/00_observed_cache.rds
#           rdos/inputs/diccionarios/00_dic_asistencia.csv
#           rdos/inputs/diccionarios/00_dic_nivel_educ.csv
#           rdos/inputs/diccionarios/00_diccionarios.rds   ← consumido por Script 04

# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(data.table)
  library(tictoc)
})

# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 00 [Diccionarios]")
start_time <- Sys.time()
cat("═══════════════════════════════════════════════════════\n")
cat("🚀 Script 00 iniciado:", as.character(start_time), "\n")
cat("═══════════════════════════════════════════════════════\n\n")

# 🔑 Paths locales (todos con prefijo 00_) -------------------------------------
f_obs_asist_csv <- file.path(DIR_DICC_OBS, "00_vals_asistencia.csv")
f_obs_nivel_csv <- file.path(DIR_DICC_OBS, "00_vals_nivel_educ.csv")
f_obs_cache     <- file.path(DIR_DICC_OBS, "00_observed_cache.rds")
f_dic_asist     <- file.path(DIR_DICC,     "00_dic_asistencia.csv")
f_dic_nivel     <- file.path(DIR_DICC,     "00_dic_nivel_educ.csv")
f_dic_rds       <- PATH_00_DICCIONARIOS    # rdos/inputs/diccionarios/00_diccionarios.rds

ts_run <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Función de normalización de keys (local — específica de este script)
# Resuelve diferencias de tilde/case entre trimestres EPH
key_norm <- function(x) iconv(trimws(tolower(as.character(x))), to = "ASCII//TRANSLIT")


# 🪫 1. Archivos EPH en rango --------------------------------------------------
cat("📂 Identificando archivos EPH en rango configurado...\n")

archivos_all <- list.files(RUTA_BASES, pattern = "^EPH\\d{4}_T[1-4]\\.RData$",
                           full.names = TRUE)
hard_stop(length(archivos_all) > 0,
          paste0("No se encontraron .RData en: ", RUTA_BASES))

df_arch <- parse_anio_trim_from_filename(archivos_all) %>%
  filter(!is.na(periodo_num),
         periodo_num >= PERIODO_INI,
         periodo_num <= PERIODO_FIN) %>%
  arrange(periodo_num)

hard_stop(nrow(df_arch) > 0,
          paste0("No hay archivos en el rango configurado (",
                 ANIO_INI, "T", TRIM_INI, " – ", ANIO_FIN, "T", TRIM_FIN,
                 "). Verificar parámetros o nombres de archivos."))

cat("✅ Archivos detectados:", nrow(df_arch), "\n")
cat("   Cobertura:", min(df_arch$periodo_num), "→", max(df_arch$periodo_num), "\n\n")


# 🪫 2. Valores observados (con cache) -----------------------------------------
cat("🔍 Extrayendo valores observados...\n")

# Helper: extrae valores únicos de columnas de interés desde un .RData
extract_vals_one_file <- function(path,
                                  cols_need = c("asistencia_escuela",
                                                "nivel_educ_obtenido2")) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  hard_stop(exists("datos", envir = e),
            paste0("Archivo no contiene objeto `datos`: ", basename(path)))

  dt <- get("datos", envir = e)
  setDT(dt)
  setnames(dt, tolower(names(dt)))

  cols_ok <- intersect(cols_need, names(dt))
  if (length(cols_ok) == 0) {
    return(list(asistencia_escuela = character(), nivel_educ_obtenido2 = character()))
  }

  for (cc in cols_ok) {
    if (haven::is.labelled(dt[[cc]])) dt[, (cc) := haven::zap_labels(dt[[cc]])]
  }

  limpia <- function(v) {
    v <- trimws(as.character(v))
    sort(unique(v[!is.na(v) & v != ""]))
  }

  list(
    asistencia_escuela   = if ("asistencia_escuela"   %in% cols_ok) limpia(dt[["asistencia_escuela"]])   else character(),
    nivel_educ_obtenido2 = if ("nivel_educ_obtenido2" %in% cols_ok) limpia(dt[["nivel_educ_obtenido2"]]) else character()
  )
}

# Usar cache si existe y es válido
observed <- NULL
if (file.exists(f_obs_cache)) {
  observed <- readRDS(f_obs_cache)
  if (!all(c("asistencia_escuela", "nivel_educ_obtenido2") %in% names(observed))) {
    observed <- NULL
    cat("   Cache inválido — re-escaneando...\n")
  } else {
    cat("   Cache encontrado. Usando valores pre-escaneados.\n")
  }
}

if (is.null(observed)) {
  cat("   Escaneando archivos crudos (primera vez, puede tardar ~5 min)...\n")

  vals_asist <- character()
  vals_nivel <- character()

  for (i in seq_len(nrow(df_arch))) {
    v <- extract_vals_one_file(df_arch$archivo[[i]])
    vals_asist <- sort(unique(c(vals_asist, v$asistencia_escuela)))
    vals_nivel <- sort(unique(c(vals_nivel, v$nivel_educ_obtenido2)))
    if (i %% 10 == 0) cat(sprintf("   ... %d / %d archivos\n", i, nrow(df_arch)))
  }

  observed <- list(
    asistencia_escuela   = vals_asist,
    nivel_educ_obtenido2 = vals_nivel,
    meta = list(ts_run = ts_run, n_archivos = nrow(df_arch),
                periodo_ini = PERIODO_INI, periodo_fin = PERIODO_FIN)
  )
  saveRDS(observed, f_obs_cache)
  cat("   Cache guardado.\n")
}

# Guardar CSVs de observados (auditoría humana)
tibble(valor = observed$asistencia_escuela)   %>% write_csv(f_obs_asist_csv)
tibble(valor = observed$nivel_educ_obtenido2) %>% write_csv(f_obs_nivel_csv)

cat("✅ Valores únicos asistencia_escuela:  ", length(observed$asistencia_escuela),  "\n")
cat("✅ Valores únicos nivel_educ_obtenido2:", length(observed$nivel_educ_obtenido2), "\n\n")


# 🪫 3. Crear / cargar diccionarios CSV ----------------------------------------
cat("📖 Preparando diccionarios CSV...\n")

# Taxonomía educativa argentina (sistema estándar, validada en proyecto anterior)
taxonomia_educ <- tribble(
  ~nivel_chr,                  ~anios_educ, ~edad_teorica, ~anios_objetivo,
  "Sin instrucción",                   0.0,           6.0,             0.0,
  "Primaria Incompleta",               3.5,          10.0,             3.5,
  "Primaria Completa",                 7.0,          12.0,             7.0,
  "Secundaria Incompleta",             9.5,          15.0,             9.5,
  "Secundaria Completa",              12.0,          18.0,            12.0,
  "Terciario Incompleto",             13.5,          20.0,            13.5,
  "Terciario Completo",               15.0,          22.0,            15.0,
  "Universitaria Incompleta",         14.0,          21.0,            14.0,
  "Universitaria Completa",           17.0,          23.0,            17.0
)

# Taxonomía de asistencia escolar
taxonomia_asist <- tribble(
  ~asist_chr,         ~asiste,       ~nota,
  "Si",               1L,            "Asiste actualmente",
  "Asistió",          0L,            "Asistió en el pasado, ya no asiste",
  "Nunca",            0L,            "Nunca asistió a establecimiento educativo",
  "No corresponde",   0L,            "No aplica (edad no escolar o ya graduado)",
  "Ns/Nr",            NA_integer_,   "No sabe / No responde"
)

# ·· 00_dic_asistencia.csv
if (!file.exists(f_dic_asist)) {
  cat("   Creando 00_dic_asistencia.csv (pre-completado con valores LEGACY)...\n")
  tibble(asist_chr = observed$asistencia_escuela) %>%
    left_join(taxonomia_asist, by = "asist_chr") %>%
    mutate(nota = coalesce(nota, "REVISAR: categoría no estándar")) %>%
    write_csv(f_dic_asist)
  cat("   ✅ Creado\n")
} else {
  cat("   00_dic_asistencia.csv ya existe. Cargando...\n")
}

# ·· 00_dic_nivel_educ.csv
if (!file.exists(f_dic_nivel)) {
  cat("   Creando 00_dic_nivel_educ.csv (pre-completado con taxonomía argentina)...\n")
  tibble(nivel_chr = observed$nivel_educ_obtenido2) %>%
    left_join(taxonomia_educ, by = "nivel_chr") %>%
    mutate(
      anios_educ     = coalesce(anios_educ,     0),
      edad_teorica   = coalesce(edad_teorica,   6),
      anios_objetivo = coalesce(anios_objetivo, 0),
      nota           = if_else(is.na(anios_educ), "REVISAR: categoría no estándar", "")
    ) %>%
    write_csv(f_dic_nivel)
  cat("   ✅ Creado\n")
} else {
  cat("   00_dic_nivel_educ.csv ya existe. Cargando...\n")
}

dic_asist <- readr::read_csv(f_dic_asist, show_col_types = FALSE)
dic_nivel <- readr::read_csv(f_dic_nivel, show_col_types = FALSE)

cat("✅ dic_asistencia:", nrow(dic_asist), "filas |",
    "dic_nivel_educ:", nrow(dic_nivel), "filas\n\n")


# 🪫 4. Validación de cobertura ------------------------------------------------
cat("🔎 Validando cobertura de diccionarios...\n\n")

# Schema mínimo
hard_stop(all(c("asist_chr", "asiste") %in% names(dic_asist)),
          "00_dic_asistencia.csv debe tener columnas: asist_chr, asiste.")
hard_stop(all(c("nivel_chr", "anios_educ", "edad_teorica", "anios_objetivo") %in% names(dic_nivel)),
          "00_dic_nivel_educ.csv debe tener columnas: nivel_chr, anios_educ, edad_teorica, anios_objetivo.")

# Cobertura: cada valor observado debe tener entrada en el diccionario
check_coverage <- function(observed_vec, dic_df, dic_key_col, what) {
  obs_keys <- sort(unique(key_norm(observed_vec)))
  dic_keys <- sort(unique(key_norm(dic_df[[dic_key_col]])))
  unmapped  <- setdiff(obs_keys, dic_keys)

  if (length(unmapped) > 0) {
    cat(sprintf("   ❌ %d valores sin mapear en '%s':\n", length(unmapped), what))
    for (v in unmapped) cat(sprintf("      - '%s'\n", v))
    hard_stop(FALSE,
              paste0("Cobertura incompleta en '", what, "'. ",
                     "Agregar filas faltantes al CSV y re-ejecutar."))
  }
  cat(sprintf("   ✅ %-25s cobertura 100%% (%d valores)\n", what, length(obs_keys)))
}

check_coverage(observed$asistencia_escuela,   dic_asist, "asist_chr", "asistencia_escuela")
check_coverage(observed$nivel_educ_obtenido2, dic_nivel, "nivel_chr", "nivel_educ_obtenido2")

bad_asiste <- dic_asist %>% filter(!is.na(asiste) & !(asiste %in% c(0L, 1L)))
hard_stop(nrow(bad_asiste) == 0,
          "00_dic_asistencia.csv: columna `asiste` debe contener solo {0, 1, NA}.")
cat("\n")


# 🪫 5. Compilar artefacto 00_diccionarios.rds ---------------------------------
cat("💾 Compilando artefacto 00_diccionarios.rds...\n")

lookup_asiste         <- setNames(dic_asist$asiste,         key_norm(dic_asist$asist_chr))
lookup_anios_educ     <- setNames(dic_nivel$anios_educ,     key_norm(dic_nivel$nivel_chr))
lookup_edad_teorica   <- setNames(dic_nivel$edad_teorica,   key_norm(dic_nivel$nivel_chr))
lookup_anios_objetivo <- setNames(dic_nivel$anios_objetivo, key_norm(dic_nivel$nivel_chr))

diccionarios_00 <- list(
  metadata = list(
    ts_run      = ts_run,
    periodo_ini = PERIODO_INI,
    periodo_fin = PERIODO_FIN,
    n_archivos  = nrow(df_arch),
    key_norm    = "iconv(trimws(tolower(x)), to = 'ASCII//TRANSLIT')"
  ),
  observed = list(
    asistencia_escuela   = observed$asistencia_escuela,
    nivel_educ_obtenido2 = observed$nivel_educ_obtenido2
  ),
  lookup = list(
    asiste          = lookup_asiste,
    anios_educ      = lookup_anios_educ,
    edad_teorica    = lookup_edad_teorica,
    anios_objetivo  = lookup_anios_objetivo
  ),
  paths = list(
    dic_asistencia = f_dic_asist,
    dic_nivel_educ = f_dic_nivel
  )
)

saveRDS(diccionarios_00, f_dic_rds)

cat("✅ Guardado:", basename(f_dic_rds), "\n")
cat(sprintf("   lookup_asiste:         %d entradas\n", length(lookup_asiste)))
cat(sprintf("   lookup_anios_educ:     %d entradas\n", length(lookup_anios_educ)))
cat(sprintf("   lookup_edad_teorica:   %d entradas\n", length(lookup_edad_teorica)))
cat(sprintf("   lookup_anios_objetivo: %d entradas\n\n", length(lookup_anios_objetivo)))


# 🧹 Limpieza de objetos intermedios -------------------------------------------
rm(list = c("archivos_all", "df_arch", "extract_vals_one_file", "check_coverage",
            "taxonomia_educ", "taxonomia_asist", "lookup_asiste", "lookup_anios_educ",
            "lookup_edad_teorica", "lookup_anios_objetivo", "dic_asist", "dic_nivel",
            "bad_asiste", "observed", "ts_run"))
gc()


# 📑 Checklist de salida -------------------------------------------------------
cat("═══════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 00:\n")
cat("═══════════════════════════════════════════════════════\n")
cat("   [✅] Archivos EPH escaneados en rango\n")
cat("   [", if (file.exists(f_obs_asist_csv)) "✅" else "❌", "] 00_vals_asistencia.csv\n",  sep = "")
cat("   [", if (file.exists(f_obs_nivel_csv)) "✅" else "❌", "] 00_vals_nivel_educ.csv\n",  sep = "")
cat("   [", if (file.exists(f_obs_cache))     "✅" else "❌", "] 00_observed_cache.rds\n",   sep = "")
cat("   [", if (file.exists(f_dic_asist))     "✅" else "❌", "] 00_dic_asistencia.csv\n",   sep = "")
cat("   [", if (file.exists(f_dic_nivel))     "✅" else "❌", "] 00_dic_nivel_educ.csv\n",   sep = "")
cat("   [", if (file.exists(f_dic_rds))       "✅" else "❌", "] 00_diccionarios.rds\n",     sep = "")
cat("   [✅] Cobertura 100%%\n")
cat("═══════════════════════════════════════════════════════\n\n")

cat("🎯 SIGUIENTE PASO: Ejecutar 01_carga_panel.R\n\n")


# ⌛ Tiempo final ---------------------------------------------------------------
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("═══════════════════════════════════════════════════════\n")
cat("✅ Script 00 finalizado:", as.character(end_time), "\n")
cat("⏱️  Tiempo total:", round(elapsed, 1), "segundos\n")
cat("═══════════════════════════════════════════════════════\n")
toc()
