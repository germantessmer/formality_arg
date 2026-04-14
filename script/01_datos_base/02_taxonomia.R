# =============================================================================
# [EN] 02_taxonomia.R -- Apply occupational taxonomy (sector/skill) and RF imputation for missing values
# INPUTS:  rdos/datos/01_panel_historico_raw.rds, rdos/inputs/diccionarios/00_diccionarios.rds
# OUTPUTS: rdos/datos/02_panel_con_taxonomia.rds
# =============================================================================
# 🌟 02_taxonomia.R 🌟 ####

# OBJETIVO: Aplicar taxonomía dura (sección/calificación) UNA VEZ para todo el proyecto
#           usando funciones centralizadas de taxonomia.R. Imputación RF para NAs
#           residuales en PEA. Genera el panel con variables de sección y calificación
#           como factores congelados (levels fijos para todo el pipeline).
# INPUTS:   PATH_01_PANEL_RAW    → rdos/datos/01_panel_historico_raw.rds
#           PATH_00_DICCIONARIOS → rdos/inputs/diccionarios/00_diccionarios.rds
# OUTPUTS:  PATH_02_PANEL_TAXONOMIA → rdos/datos/02_panel_con_taxonomia.rds
# NOTA:     mapear_seccion() y normalizar_calificacion() están en script/funciones/taxonomia.R
#           Los diccionarios de Script 00 son educativos; aquí se valida su existencia
#           y se loguea ts_run para trazabilidad. El mapeo de sección/calificación
#           usa hardcoding centralizado en taxonomia.R (no depende del objeto dic).

# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(stringr)
  library(tictoc)
  library(missRanger)
  library(ranger)
})

# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))
source(here::here("script", "funciones", "helpers.R"))
source(here::here("script", "funciones", "validaciones.R"))
source(here::here("script", "funciones", "taxonomia.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 02 [Taxonomía Única]")
start_time <- Sys.time()
cat("═══════════════════════════════════════════════════════\n")
cat("🚀 Script 02 iniciado:", as.character(start_time), "\n")
cat("═══════════════════════════════════════════════════════\n\n")


# 🪫 1. Carga de datos ---------------------------------------------------------
cat("📂 Cargando panel histórico raw...\n")

hard_stop(file.exists(PATH_01_PANEL_RAW),
          paste0("No existe 01_panel_historico_raw.rds. Ejecutar Script 01 primero.\n",
                 "   Ruta esperada: ", PATH_01_PANEL_RAW))

datos <- readRDS(PATH_01_PANEL_RAW)

cat("✅ Panel cargado:", format(nrow(datos), big.mark = ","), "observaciones x",
    ncol(datos), "variables\n\n")


# 🪫 2. Cargar diccionarios ----------------------------------------------------
cat("📖 Cargando diccionarios de Script 00...\n")

hard_stop(file.exists(PATH_00_DICCIONARIOS),
          paste0("No existen diccionarios. Ejecutar Script 00 primero.\n",
                 "   Ruta esperada: ", PATH_00_DICCIONARIOS))

dic <- readRDS(PATH_00_DICCIONARIOS)

# Validar estructura esperada
slots_esperados <- c("metadata", "observed", "lookup", "paths")
slots_faltantes <- setdiff(slots_esperados, names(dic))
hard_stop(length(slots_faltantes) == 0,
          paste0("Diccionario con estructura inesperada. Faltan slots: ",
                 paste(slots_faltantes, collapse = ", ")))

cat("✅ Diccionarios cargados\n")
cat("   Timestamp diccionario:", as.character(dic$metadata$ts_run), "\n\n")


# 🪫 3. Construir variables base de taxonomía ----------------------------------
cat("🏗️  Construyendo variables base de taxonomía...\n")

# Validar existencia de columnas fuente
hard_stop("seccion_ocupado" %in% names(datos) || "seccion_desocupado" %in% names(datos),
          "No existen columnas seccion_ocupado / seccion_desocupado en el dataset")

hard_stop("calificacion_ocupado" %in% names(datos) || "calificacion_desocupado" %in% names(datos),
          "No existen columnas calificacion_ocupado / calificacion_desocupado en el dataset")

# Rellenar columnas si alguna falta (seguridad ante trimestres incompletos)
if (!"seccion_ocupado"       %in% names(datos)) datos$seccion_ocupado       <- NA
if (!"seccion_desocupado"    %in% names(datos)) datos$seccion_desocupado    <- NA
if (!"calificacion_ocupado"  %in% names(datos)) datos$calificacion_ocupado  <- NA
if (!"calificacion_desocupado" %in% names(datos)) datos$calificacion_desocupado <- NA

# Unificar: coalesce ocupado → desocupado (igual para sección y calificación)
datos <- datos %>%
  mutate(
    seccion_texto_crudo = as.character(coalesce(
      zap_labels(seccion_ocupado),
      zap_labels(seccion_desocupado)
    )),
    calificacion_raw = as.character(coalesce(
      zap_labels(calificacion_ocupado),
      zap_labels(calificacion_desocupado)
    ))
  )

cat("✅ Variables base unificadas (coalesce ocupado/desocupado)\n\n")


# 🪫 4. Aplicar mapeo de taxonomía ---------------------------------------------
cat("🗂️  Aplicando mapeo de taxonomía...\n")

datos <- datos %>%
  mutate(
    seccion_raw       = mapear_seccion(seccion_texto_crudo),
    calificacion_norm = normalizar_calificacion(calificacion_raw)
  )

# Diagnóstico de cobertura del mapeo
n_sec_ok  <- sum(!is.na(datos$seccion_raw))
n_cal_ok  <- sum(!is.na(datos$calificacion_norm))
pct_sec   <- round(100 * n_sec_ok / nrow(datos), 1)
pct_cal   <- round(100 * n_cal_ok / nrow(datos), 1)

cat("✅ Mapeo aplicado\n")
cat("   Sección mapeada:      ", format(n_sec_ok, big.mark = ","), "(", pct_sec, "%)\n")
cat("   Calificación mapeada: ", format(n_cal_ok, big.mark = ","), "(", pct_cal, "%)\n\n")


# 🪫 5. Reglas de negocio (PEA) ------------------------------------------------
cat("🚦 Aplicando reglas de negocio (PEA)...\n")

cond_actividad_chr <- zap_chr(datos$condicion_actividad)

pea_flag <- !(str_detect(cond_actividad_chr,
                         regex("Inactivo|Menor", ignore_case = TRUE)) |
              is.na(cond_actividad_chr))

datos <- datos %>%
  mutate(
    pea_flag = pea_flag,

    # Regla 1: No PEA           → "No aplica (No PEA)"
    # Regla 2: PEA con sección  → usar sección mapeada
    # Regla 3: PEA sin sección + sin antigüedad → "Sin Experiencia Previa"
    # Regla 4: PEA sin sección + con antigüedad → NA residual (va a RF)
    seccion_imp = case_when(
      !pea_flag                                        ~ "No aplica (No PEA)",
      !is.na(seccion_raw)                              ~ seccion_raw,
      is.na(seccion_raw) & is.na(antiguedad)           ~ "Sin Experiencia Previa",
      TRUE                                             ~ NA_character_
    ),

    # Análogo para calificación
    calificacion_imp = case_when(
      !pea_flag                                        ~ "No aplica (No PEA)",
      !is.na(calificacion_norm)                        ~ calificacion_norm,
      is.na(calificacion_norm) & is.na(antiguedad)     ~ "Sin Calificación",
      TRUE                                             ~ NA_character_
    )
  )

n_pea     <- sum(datos$pea_flag)
pct_pea   <- round(100 * n_pea / nrow(datos), 1)
n_na_sec  <- sum(is.na(datos$seccion_imp[datos$pea_flag]))
n_na_cal  <- sum(is.na(datos$calificacion_imp[datos$pea_flag]))

cat("✅ Reglas aplicadas\n")
cat("   PEA:                     ", format(n_pea, big.mark = ","), "(", pct_pea, "%)\n")
cat("   NAs residuales sección:  ", format(n_na_sec, big.mark = ","), "(van a RF)\n")
cat("   NAs residuales calific.: ", format(n_na_cal, big.mark = ","), "(van a RF)\n\n")


# 🪫 6. Imputación RF (solo PEA con NAs residuales) ----------------------------
cat("🤖 Imputación RF de taxonomía...\n")

# Subset PEA con todas las variables predictoras
pea_base <- datos %>%
  filter(pea_flag) %>%
  transmute(
    id_individuo,
    edad             = zap_num(edad),
    sexo             = as.factor(zap_chr(sexo)),
    nivel_educ       = as.factor(zap_chr(nivel_educ_obtenido2)),
    aglomerado       = as.factor(zap_chr(aglomerado)),
    region           = as.factor(zap_chr(region)),
    seccion_imp      = as.factor(seccion_imp),
    calificacion_imp = as.factor(calificacion_imp)
  )

# ── Imputación SECCIÓN ────────────────────────────────────────────────────────
n_nas_seccion <- sum(is.na(pea_base$seccion_imp))

if (n_nas_seccion > 0) {
  cat("   Imputando sección (", format(n_nas_seccion, big.mark = ","),
      " NAs en PEA)...\n", sep = "")

  df_sec <- pea_base %>%
    mutate(target = seccion_imp) %>%
    select(id_individuo, target, edad, sexo, nivel_educ, aglomerado, region)

  # Imputar predictores con NAs antes de entrenar RF
  x_imp <- df_sec %>%
    select(-id_individuo, -target) %>%
    missRanger(pmm.k = 3, num.trees = 50, seed = SEED_GLOBAL,
               verbose = 0, num.threads = N_CORES)

  df_sec_imp <- bind_cols(
    df_sec %>% select(id_individuo, target),
    x_imp
  )

  train_idx  <- which(!is.na(df_sec_imp$target))
  target_idx <- which( is.na(df_sec_imp$target))

  if (length(target_idx) > 0 && length(train_idx) > 50) {
    rf_sec <- ranger(
      target ~ .,
      data          = df_sec_imp %>% select(-id_individuo) %>% slice(train_idx),
      num.trees     = 100,
      mtry          = 3,
      importance    = "impurity",
      min.node.size = 10,
      seed          = SEED_GLOBAL,
      num.threads   = N_CORES
    )

    pred_sec <- predict(
      rf_sec,
      data = df_sec_imp %>% select(-id_individuo, -target) %>% slice(target_idx)
    )$predictions

    df_sec_imp$target[target_idx] <- as.character(pred_sec)
  }

  pea_base <- pea_base %>%
    select(-seccion_imp) %>%
    left_join(df_sec_imp %>% select(id_individuo, seccion_imp = target),
              by = "id_individuo")

  cat("   ✅ Sección imputada\n")
} else {
  cat("   ✅ Sección sin NAs en PEA (skip RF)\n")
}

# ── Imputación CALIFICACIÓN ───────────────────────────────────────────────────
n_nas_calif <- sum(is.na(pea_base$calificacion_imp))

if (n_nas_calif > 0) {
  cat("   Imputando calificación (", format(n_nas_calif, big.mark = ","),
      " NAs en PEA)...\n", sep = "")

  df_cal <- pea_base %>%
    mutate(target = calificacion_imp) %>%
    select(id_individuo, target, edad, sexo, nivel_educ, aglomerado, region)

  x_imp <- df_cal %>%
    select(-id_individuo, -target) %>%
    missRanger(pmm.k = 3, num.trees = 50, seed = SEED_GLOBAL,
               verbose = 0, num.threads = N_CORES)

  df_cal_imp <- bind_cols(
    df_cal %>% select(id_individuo, target),
    x_imp
  )

  train_idx  <- which(!is.na(df_cal_imp$target))
  target_idx <- which( is.na(df_cal_imp$target))

  if (length(target_idx) > 0 && length(train_idx) > 50) {
    rf_cal <- ranger(
      target ~ .,
      data          = df_cal_imp %>% select(-id_individuo) %>% slice(train_idx),
      num.trees     = 100,
      mtry          = 3,
      importance    = "impurity",
      min.node.size = 10,
      seed          = SEED_GLOBAL,
      num.threads   = N_CORES
    )

    pred_cal <- predict(
      rf_cal,
      data = df_cal_imp %>% select(-id_individuo, -target) %>% slice(target_idx)
    )$predictions

    df_cal_imp$target[target_idx] <- as.character(pred_cal)
  }

  pea_base <- pea_base %>%
    select(-calificacion_imp) %>%
    left_join(df_cal_imp %>% select(id_individuo, calificacion_imp = target),
              by = "id_individuo")

  cat("   ✅ Calificación imputada\n")
} else {
  cat("   ✅ Calificación sin NAs en PEA (skip RF)\n")
}

# Merge de vuelta al dataset completo
datos <- datos %>%
  select(-seccion_imp, -calificacion_imp) %>%
  left_join(
    pea_base %>% select(id_individuo, seccion_imp, calificacion_imp),
    by = "id_individuo"
  )

cat("\n")


# 🪫 7. Freeze levels (factorización final) ------------------------------------
cat("🔒 Freezing levels finales...\n")

datos <- datos %>%
  mutate(
    seccion      = factor(seccion_imp,      levels = SECCION_LEVELS),
    calificacion = factor(calificacion_imp, levels = CALIFICACION_LEVELS)
  ) %>%
  select(-seccion_raw, -seccion_imp,
         -calificacion_norm, -calificacion_imp,
         -seccion_texto_crudo, -calificacion_raw)

cat("✅ Levels congelados\n")
cat("   Sección:      ", length(SECCION_LEVELS),      "levels\n")
cat("   Calificación: ", length(CALIFICACION_LEVELS), "levels\n\n")


# 🪫 8. Validaciones finales ----------------------------------------------------
cat("🔎 Validaciones finales...\n\n")

# Validación 1: 0 NAs en PEA
na_sec_pea  <- sum(is.na(datos$seccion[datos$pea_flag]))
na_cal_pea  <- sum(is.na(datos$calificacion[datos$pea_flag]))

hard_stop(na_sec_pea == 0,
          paste0("Quedan ", na_sec_pea, " NAs en seccion (PEA). Revisar imputación."))
hard_stop(na_cal_pea == 0,
          paste0("Quedan ", na_cal_pea, " NAs en calificacion (PEA). Revisar imputación."))

cat("✅ 0 NAs en taxonomía (PEA)\n")

# Validación 2: levels dentro de los esperados
validate_valores_esperados(datos$seccion,      SECCION_LEVELS,      "seccion",      allow_na = TRUE)
validate_valores_esperados(datos$calificacion, CALIFICACION_LEVELS, "calificacion", allow_na = TRUE)

cat("✅ Dimensiones finales:", format(nrow(datos), big.mark = ","), "observaciones\n\n")


# 🪫 9. Guardado ---------------------------------------------------------------
cat("💾 Guardando resultados...\n")

saveRDS(datos, PATH_02_PANEL_TAXONOMIA)

cat("✅ Guardado:", PATH_02_PANEL_TAXONOMIA, "\n")
cat("   Dimensiones:", format(nrow(datos), big.mark = ","), "x", ncol(datos), "\n")
cat("   Tamaño en memoria:", format(object.size(datos), units = "MB"), "\n\n")

rm(list = intersect(ls(), c("pea_base", "df_sec", "df_sec_imp",
                            "df_cal", "df_cal_imp", "x_imp",
                            "rf_sec", "rf_cal")))
gc()


# 📑 CHECKLIST SCRIPT 02 -------------------------------------------------------
cat("═══════════════════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 02:\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("   [✅] Panel raw cargado:", format(nrow(datos), big.mark = ","), "observaciones\n")
cat("   [✅] Diccionarios Script 00 validados (ts_run:", as.character(dic$metadata$ts_run), ")\n")
cat("   [✅] coalesce seccion_ocupado / seccion_desocupado → seccion_texto_crudo\n")
cat("   [✅] coalesce calificacion_ocupado / calificacion_desocupado → calificacion_raw\n")
cat("   [✅] Mapeo taxonomía (mapear_seccion / normalizar_calificacion)\n")
cat("   [✅] Reglas PEA (No aplica / Sin Experiencia / Sin Calificación)\n")
cat("   [✅] Imputación RF completada (seccion / calificacion)\n")
cat("   [✅] 0 NAs en PEA post-imputación\n")
cat("   [✅] Levels congelados:", length(SECCION_LEVELS), "secciones,",
    length(CALIFICACION_LEVELS), "calificaciones\n")
cat("   [", if (file.exists(PATH_02_PANEL_TAXONOMIA)) "✅" else "❌",
    "] Output: ", basename(PATH_02_PANEL_TAXONOMIA), "\n", sep = "")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("🎯 SIGUIENTE PASO: Ejecutar 03a_ingresos_limpieza.R\n\n")


# ⌛ Tiempo final ---------------------------------------------------------------
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("═══════════════════════════════════════════════════════\n")
cat("✅ Script 02 finalizado:", as.character(end_time), "\n")
cat("⏱️  Tiempo total:", round(elapsed, 1), "segundos\n")
cat("═══════════════════════════════════════════════════════\n")

toc()
