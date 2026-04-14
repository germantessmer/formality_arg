# =============================================================================
# [EN] 03b_ich.R -- Construct Housing Quality Index (ICH) via Multiple Correspondence Analysis
# INPUTS:  rdos/datos/03a_panel_ingresos_limpio.rds
# OUTPUTS: rdos/datos/03b_panel_base_completo.rds, rdos/contratos/03b_contrato_datos_base.rds
# =============================================================================
# 🌟 03b_ich.R 🌟 ####
#
# OBJETIVO:  Calcular ICH (Índice de Calidad Habitacional) para todo el panel
#            histórico mediante MCA sobre 10 indicadores de vivienda.
#            Mergear ich_score al panel, eliminar variables RAW y generar contrato.
#
# INPUTS:    PATH_03A_PANEL_INGRESOS → rdos/datos/03a_panel_ingresos_limpio.rds
#
# OUTPUTS:   PATH_03B_PANEL_ICH     → rdos/datos/03b_panel_base_completo.rds
#            PATH_CONTRATO_03B      → rdos/contratos/03b_contrato_datos_base.rds
#
# NOTA:      Requiere haber ejecutado 03a_ingresos_limpieza.R primero.
#            OpenBLAS activo → MCA usa múltiples núcleos automáticamente.
#            Checkpoint/Resume: saltea SVD si cache válido ya existe.


# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyverse)
  library(tictoc)
  library(missRanger)
  library(FactoMineR)
})


# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))


# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 03b [ICH]")
start_time <- Sys.time()
cat("═══════════════════════════════════════════════════════\n")
cat("🚀 Script 03b iniciado:", as.character(start_time), "\n")
cat("═══════════════════════════════════════════════════════\n\n")


# 🪫 0. Verificación entorno de cómputo ---------------------------------------
cat("🖥️  Verificando entorno de cómputo...\n")

# OpenBLAS: si está activo, MCA() usa múltiples núcleos automáticamente
blas_info      <- tryCatch(sessionInfo()$BLAS, error = function(e) "")
openblas_activo <- grepl("openblas|libopenblas", tolower(blas_info))

cat(sprintf("   BLAS detectado: %s\n",
            if (nchar(blas_info) > 0) blas_info else "(no detectado)"))
cat(sprintf("   OpenBLAS activo: %s\n",
            if (openblas_activo) "✅ SÍ — MCA multihilo"
            else "⚠️  NO — MCA en 1 hilo (verificar Rblas.dll)"))

# RAM disponible (sin wmic)
ram_libre <- tryCatch({
  if (requireNamespace("ps", quietly = TRUE)) {
    ps::ps_system_memory()$avail / (1024^3)
  } else NA_real_
}, error = function(e) NA_real_)

if (is.na(ram_libre)) {
  ram_libre <- 4.5
  cat("   RAM disponible: no detectada → modo MEDIO\n")
} else {
  cat(sprintf("   RAM disponible: %.1f GB\n", ram_libre))
}

.rf_params <- if (ram_libre >= 6) {
  list(trees = 20, label = "ALTO (≥6 GB)")
} else if (ram_libre >= 4) {
  list(trees = 15, label = "MEDIO (4-6 GB)")
} else {
  list(trees = 10, label = "BAJO (<4 GB)")
}

cat(sprintf("   Modo RAM: %s → missRanger trees: %d\n\n",
            .rf_params$label, .rf_params$trees))


# 🪫 1. Carga de datos ---------------------------------------------------------
cat("📂 Cargando panel con ingresos...\n")

hard_stop(file.exists(PATH_03A_PANEL_INGRESOS),
          "No existe 03a_panel_ingresos_limpio.rds. Ejecutar Script 03a primero.")

datos <- readRDS(PATH_03A_PANEL_INGRESOS)

cat("✅ Panel cargado:", format(nrow(datos), big.mark = ","), "observaciones\n\n")


# 🪫 2. Construcción ICH -------------------------------------------------------
cat("🏠 Construyendo ICH (índice calidad habitacional)...\n\n")

.cache_ich_path <- file.path(DIR_DATOS, "03b_cache_ich_scores.rds")

vars_ind <- c("ind_tipo_vivienda", "ind_nro_ambientes", "ind_tipo_piso",
              "ind_calidad_techo", "ind_agua", "ind_saneamiento",
              "ind_combustible", "ind_tenencia", "ind_tiene_garage",
              "ind_tiene_lavadero")

# ── Checkpoint: intentar cargar ICH ya calculado ──────────────────────────────
if (file.exists(.cache_ich_path)) {
  cat("   📦 Checkpoint ICH detectado. Validando...\n")
  cached_ich <- tryCatch(readRDS(.cache_ich_path), error = function(e) NULL)

  n_hogares_esperados <- datos %>%
    distinct(id_hogar, periodo_id) %>%
    nrow()

  .cache_valid <- !is.null(cached_ich) &&
    is.data.frame(cached_ich) &&
    all(c("id_hogar", "periodo_id", "ich_score") %in% names(cached_ich)) &&
    nrow(cached_ich) == n_hogares_esperados

  if (.cache_valid) {
    cat("   ✅ Checkpoint válido. Saltando MCA (ya fue calculado).\n")
    hogar_ich <- cached_ich
    rm(cached_ich); gc(verbose = FALSE)
  } else {
    cat("   ⚠️  Checkpoint inválido. Recalculando...\n")
    rm(cached_ich); gc(verbose = FALSE)
    .cache_valid <- FALSE
  }
} else {
  .cache_valid <- FALSE
}

# ── Calcular ICH si no hay checkpoint válido ──────────────────────────────────
if (!.cache_valid) {

  # 2.1. Dataset a nivel hogar
  cat("   📊 Agregando a nivel hogar...\n")

  hogar_base <- datos %>%
    group_by(id_hogar, periodo_id) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      id_hogar,
      periodo_id,
      pondera                = zap_num(pondera),
      tipo_vivienda          = zap_chr(tipo_vivienda),
      nro_ambientes_vivienda = zap_num(nro_ambientes_vivienda),
      tipo_piso              = zap_chr(tipo_piso),
      tipo_techo             = zap_chr(tipo_techo),
      revestimiento_techo    = zap_chr(revestimiento_techo),
      suministro_agua        = zap_chr(suministro_agua),
      acceso_agua            = zap_chr(acceso_agua),
      banio                  = zap_chr(banio),
      lugar_banio            = zap_chr(lugar_banio),
      caract_banio           = zap_chr(caract_banio),
      tipo_desague           = zap_chr(tipo_desague),
      combustible_cocina     = zap_chr(combustible_cocina),
      regimen_tenencia       = zap_chr(regimen_tenencia),
      garage                 = zap_chr(garage),
      lavadero               = zap_chr(lavadero)
    )

  cat("   ✅ Dataset hogar:", format(nrow(hogar_base), big.mark = ","), "hogares\n\n")

  # 2.2. Indicadores binarios
  cat("   🔨 Creando indicadores de vivienda...\n")

  hogar_ind <- hogar_base %>%
    mutate(
      ind_tipo_vivienda = case_when(
        tipo_vivienda %in% c("Ns/Nr", "No corresponde") ~ NA_character_,
        tipo_vivienda == "Casa"                          ~ "casa",
        tipo_vivienda == "Departamento"                  ~ "departamento",
        TRUE                                             ~ "otro_tipo_vivienda"
      ),
      ind_nro_ambientes = case_when(
        nro_ambientes_vivienda == 99  ~ NA_character_,
        nro_ambientes_vivienda >= 5   ~ "5_mas_ambientes",
        TRUE ~ paste0(nro_ambientes_vivienda, "_ambientes")
      ),
      ind_tipo_piso = case_when(
        stringr::str_detect(tipo_piso, "Terminado|Mosaico|Cerámica|Madera") ~ "piso_alta_calidad",
        stringr::str_detect(tipo_piso, "Sin terminación|Cemento")           ~ "piso_mediana_calidad",
        TRUE                                                                 ~ "piso_precario"
      ),
      ind_calidad_techo = case_when(
        tipo_techo %in% c("Baldosa", "Membrana", "Teja") ~ "techo_alta_solidez",
        revestimiento_techo == "Si"                       ~ "techo_calidad_media",
        TRUE                                              ~ "techo_precario"
      ),
      ind_agua = ifelse(
        suministro_agua == "Red" & acceso_agua == "En vivienda",
        "agua_red_interior", "agua_exterior"
      ),
      ind_saneamiento = ifelse(
        banio == "Si" & lugar_banio == "En vivienda" &
          caract_banio == "Con botón" &
          stringr::str_detect(tipo_desague, "Cloaca"),
        "saneamiento_optimo", "saneamiento_deficitario"
      ),
      ind_combustible = case_when(
        combustible_cocina == "Red"  ~ "gas_red",
        combustible_cocina == "Tubo" ~ "gas_tubo_garrafa",
        TRUE                         ~ "combustible_precario"
      ),
      ind_tenencia = case_when(
        stringr::str_detect(regimen_tenencia, "Propietario") ~ "propietario",
        stringr::str_detect(regimen_tenencia, "Inquilino")   ~ "inquilino",
        TRUE                                                  ~ "ocupante_precario"
      ),
      ind_tiene_garage   = ifelse(garage   == "Si", "si_garage",   "no_garage"),
      ind_tiene_lavadero = ifelse(lavadero == "Si", "si_lavadero", "no_lavadero")
    ) %>%
    mutate(across(starts_with("ind_"), as.factor))

  rm(hogar_base); gc(verbose = FALSE)
  cat("   ✅ 10 indicadores creados\n\n")

  # 2.3. Imputación RF (solo si hay NAs)
  cat("   🤖 Imputando indicadores faltantes...\n")
  na_count <- sapply(hogar_ind[vars_ind], function(x) sum(is.na(x)))

  if (any(na_count > 0)) {
    cat("   Variables con NAs:\n")
    print(na_count[na_count > 0])

    hogar_ind_imp <- hogar_ind %>%
      select(all_of(vars_ind)) %>%
      missRanger(
        pmm.k       = 3,
        num.trees   = .rf_params$trees,
        seed        = SEED_GLOBAL,
        verbose     = 0,
        num.threads = N_CORES
      )

    hogar_ind <- bind_cols(
      hogar_ind %>% select(-all_of(vars_ind)),
      hogar_ind_imp
    )
    rm(hogar_ind_imp); gc(verbose = FALSE)
    cat("   ✅ Indicadores imputados\n\n")
  } else {
    cat("   ✅ Sin NAs (skip RF)\n\n")
  }

  # 2.4. MCA — OpenBLAS paralela automáticamente si está configurado
  cat("   📐 Ejecutando MCA (ncp = 5)...\n")
  cat("   ⚠️  Esto puede tardar varios minutos\n")

  gc(verbose = FALSE)  # Pre-emptivo: memoria contigua para matrices SVD

  mca_data <- hogar_ind %>% select(all_of(vars_ind))

  mca_fit <- MCA(
    mca_data,
    ncp   = 5,
    graph = FALSE,
    row.w = hogar_ind$pondera
  )

  dim1_scores <- mca_fit$ind$coord[, 1]
  ich_min     <- min(dim1_scores)
  ich_max     <- max(dim1_scores)
  ich_score   <- 100 - ((dim1_scores - ich_min) / (ich_max - ich_min)) * 100

  hogar_ind$ich_score <- ich_score

  cat("   ✅ MCA completado\n")
  cat("   Varianza explicada (dim1):", round(mca_fit$eig[1, 2], 1), "%\n\n")

  rm(mca_data, dim1_scores, ich_min, ich_max, ich_score); gc(verbose = FALSE)

  # Preparar tabla para checkpoint y merge
  hogar_ich <- hogar_ind %>% select(id_hogar, periodo_id, ich_score)

  # Guardar checkpoint
  cat("   💾 Guardando checkpoint ICH...\n")
  tryCatch(
    saveRDS(hogar_ich, .cache_ich_path, compress = FALSE),
    error = function(e) cat("   ⚠️  No se pudo guardar checkpoint:", conditionMessage(e), "\n")
  )

  rm(hogar_ind); gc(verbose = FALSE)
}

# Guardar var_explicada para contrato (necesaria fuera del bloque)
var_explicada_dim1 <- if (exists("mca_fit")) {
  mca_fit$eig[1, 2]
} else {
  NA_real_
}

# Persistir mca_fit para diagnósticos en 03c (se elimina después)
if (exists("mca_fit")) {
  .path_mca_fit <- file.path(DIR_MODELOS, "03b_mca_fit.rds")
  saveRDS(mca_fit, .path_mca_fit)
  cat("   💾 mca_fit guardado:", basename(.path_mca_fit), "\n")
  rm(mca_fit)
}


# 🪫 3. Merge ICH → panel completo --------------------------------------------
cat("   🔗 Mergeando ICH al panel completo...\n")

# match() en lugar de left_join: evita duplicar datos en memoria
.key_panel <- paste0(datos$id_hogar,     "_", datos$periodo_id)
.key_hogar <- paste0(hogar_ich$id_hogar, "_", hogar_ich$periodo_id)
idx <- match(.key_panel, .key_hogar)

datos$ich_score <- hogar_ich$ich_score[idx]

rm(hogar_ich, idx, .key_panel, .key_hogar); gc(verbose = FALSE)

# Limpiar checkpoint tras merge exitoso
if (file.exists(.cache_ich_path)) {
  file.remove(.cache_ich_path)
  cat("   🗑️  Checkpoint limpiado\n")
}

cat("   ✅ ICH agregado al panel\n\n")


# 🪫 4. Validaciones -----------------------------------------------------------
cat("🔎 Validaciones finales...\n\n")

na_ich     <- sum(is.na(datos$ich_score))
pct_ich_ok <- round(100 * (1 - na_ich / nrow(datos)), 1)
cat("📊 ICH completitud:", pct_ich_ok, "% (", format(na_ich, big.mark = ","), "NAs)\n")
soft_warn(pct_ich_ok > 95,
          paste0("ICH tiene baja completitud (", pct_ich_ok, "%)"))

cor_subset <- datos %>%
  filter(!is.na(ich_score), !is.na(ingreso_real_final), ingreso_real_final > 0) %>%
  sample_n(min(50000, n()))

cor_ich_ingreso <- cor(cor_subset$ich_score,
                       log(cor_subset$ingreso_real_final + 1),
                       use = "complete.obs")

cat("📊 Correlación ICH-log(ingreso):", round(cor_ich_ingreso, 3), "\n")
soft_warn(cor_ich_ingreso > 0.2,
          paste0("Correlación ICH-ingreso baja (", round(cor_ich_ingreso, 3), ")"))

rm(cor_subset); gc(verbose = FALSE)
cat("\n")


# 🪫 5. Limpieza variables vivienda RAW ----------------------------------------
cat("🧹 Eliminando variables vivienda RAW (ya calculado ICH)...\n")

vars_vivienda_raw <- c(
  "tipo_vivienda", "nro_ambientes_vivienda", "tipo_piso", "tipo_techo",
  "revestimiento_techo", "suministro_agua", "acceso_agua", "banio",
  "lugar_banio", "caract_banio", "tipo_desague", "combustible_cocina",
  "regimen_tenencia", "garage", "lavadero"
)

datos <- datos %>% select(-any_of(vars_vivienda_raw))

cat("✅ Variables vivienda RAW eliminadas\n")
cat("   Dataset final:", format(ncol(datos), big.mark = ","), "columnas\n\n")


# 🪫 6. Guardado ---------------------------------------------------------------
cat("💾 Guardando resultados...\n")

gc(verbose = FALSE)

saveRDS(datos, PATH_03B_PANEL_ICH)
cat("✅ Panel guardado:", PATH_03B_PANEL_ICH, "\n")
cat("   Dimensiones:", format(nrow(datos), big.mark = ","), "×", ncol(datos), "\n")
cat("   Tamaño:", format(object.size(datos), units = "MB"), "\n\n")

contrato_03b <- list(
  timestamp              = Sys.time(),
  script                 = "03b_ich.R",
  anio_ini               = ANIO_INI,
  trim_ini               = TRIM_INI,
  anio_fin               = ANIO_FIN,
  trim_fin               = TRIM_FIN,
  periodo_ini            = PERIODO_INI,
  periodo_fin            = PERIODO_FIN,
  N_total                = nrow(datos),
  N_pea                  = sum(datos$pea_flag),
  levels_seccion         = levels(datos$seccion),
  levels_calificacion    = levels(datos$calificacion),
  var_explicada_dim1_mca = var_explicada_dim1,
  cor_ich_ingreso        = cor_ich_ingreso,
  pct_ich_completo       = pct_ich_ok,
  openblas_activo        = openblas_activo,
  path_diccionarios      = PATH_00_DICCIONARIOS,
  path_output            = PATH_03B_PANEL_ICH
)

saveRDS(contrato_03b, PATH_CONTRATO_03B)
cat("✅ Contrato guardado:", PATH_CONTRATO_03B, "\n\n")


# 📑 7. Checklist de salida ----------------------------------------------------
cat("═══════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 03b:\n")
cat("═══════════════════════════════════════════════════════\n")
cat("   [✅] OpenBLAS:", if (openblas_activo) "activo (MCA multihilo)" else "no detectado", "\n")
cat("   [✅] ICH calculado para TODO el panel histórico\n")
cat("   [✅] ICH completitud:", pct_ich_ok, "%\n")
cat("   [✅] Correlación ICH-ingreso:", round(cor_ich_ingreso, 3), "\n")
cat("   [✅] Variables vivienda RAW eliminadas\n")
cat("   [✅] Contrato generado (downstream)\n")
cat("   [✅] Output:", basename(PATH_03B_PANEL_ICH), "\n")
cat("═══════════════════════════════════════════════════════\n\n")

cat("🎯 SIGUIENTE PASO: Ejecutar 03c_reporte_base.R (opcional)\n")
cat("   O continuar con Capa 2: 04_proxies_simples.R\n\n")


# ⌛ Mensaje final -------------------------------------------------------------
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("═══════════════════════════════════════════════════════\n")
cat("✅ Script 03b finalizado:", as.character(end_time), "\n")
cat("⏱️  Tiempo total:", round(elapsed / 60, 1), "minutos\n")
cat("═══════════════════════════════════════════════════════\n")

toc()
