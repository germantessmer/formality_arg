# =============================================================================
# [EN] 03a_ingresos_limpieza.R -- Impute missing income (RF with checkpoint/resume), deflate, and clean columns
# INPUTS:  rdos/datos/02_panel_con_taxonomia.rds
# OUTPUTS: rdos/datos/03a_panel_ingresos_limpio.rds
# =============================================================================
# 🌟 03a_ingresos_limpieza.R 🌟 ####

# OBJETIVO: Imputar ingresos faltantes (RF, checkpoint/resume) + transformaciones
#           auxiliares + limpieza de columnas irrelevantes.
# INPUTS:   PATH_02_PANEL_TAXONOMIA → rdos/datos/02_panel_con_taxonomia.rds
# OUTPUTS:  PATH_03A_PANEL_INGRESOS → rdos/datos/03a_panel_ingresos_limpio.rds
# NOTA:     Script 03b calculará ICH sobre este output (variables vivienda RAW
#           se conservan hasta 03b, que las elimina tras calcular el índice).
#
# OPTIMIZACIONES (heredadas de LEGACY v2.0):
#   [1] Checkpoint/Resume: detecta cache de imputación, saltea RF si ya corrió
#   [2] RAM adaptativa: reduce num.trees si memoria disponible < umbrales
#   [3] missRanger 1 iteración implícita: suficiente para predictores auxiliares
#   [4] merge por match() in-place: evita duplicar datos en RAM (Windows)
#   [5] gc() pre-emptivo antes de cada paso pesado

# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyverse)
  library(tictoc)
  library(missRanger)
  library(ranger)
  library(data.table)
})

# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 03a [Ingresos + Limpieza]")
start_time <- Sys.time()
cat("═══════════════════════════════════════════════════════\n")
cat("🚀 Script 03a iniciado:", as.character(start_time), "\n")
cat("═══════════════════════════════════════════════════════\n\n")


# 🪫 0. Diagnóstico de RAM disponible ------------------------------------------
.get_ram_libre_gb <- function() {
  tryCatch({
    if (requireNamespace("ps", quietly = TRUE)) {
      mem <- ps::ps_system_memory()
      return(mem$avail / (1024^3))  # bytes → GB
    }
    NA_real_
  }, error = function(e) NA_real_)
}

ram_libre <- .get_ram_libre_gb()

if (!is.na(ram_libre)) {
  cat(sprintf("🖥️  RAM disponible: %.1f GB\n", ram_libre))
} else {
  cat("🖥️  RAM disponible: no detectada → usando modo MEDIO (conservador)\n")
  ram_libre <- 4.5
}

# Parámetros adaptativos según RAM libre
# NOTA: trees_ranger capped en 50 (valor probado con 634K casos en 15 GB RAM)
.rf_params <- if (ram_libre >= 6) {
  list(trees_missranger = 20, trees_ranger = 50, label = "ALTO (≥6 GB)")
} else if (ram_libre >= 4) {
  list(trees_missranger = 15, trees_ranger = 40, label = "MEDIO (4-6 GB)")
} else {
  list(trees_missranger = 10, trees_ranger = 30, label = "BAJO (<4 GB)")
}

cat(sprintf("⚙️  Modo RAM: %s → missRanger trees: %d | ranger trees: %d\n\n",
            .rf_params$label, .rf_params$trees_missranger, .rf_params$trees_ranger))


# 🪫 1. Carga de datos ---------------------------------------------------------
cat("📂 Cargando panel con taxonomía...\n")

hard_stop(file.exists(PATH_02_PANEL_TAXONOMIA),
          "No existe 02_panel_con_taxonomia.rds. Ejecutar Script 02 primero.")

datos <- readRDS(PATH_02_PANEL_TAXONOMIA)

cat("✅ Panel cargado:", format(nrow(datos), big.mark = ","), "observaciones x",
    ncol(datos), "variables\n\n")


# 🪫 2. Imputación de ingresos -------------------------------------------------
cat("💰 Imputando ingresos faltantes...\n")

# Limpiar códigos especiales
datos <- datos %>%
  mutate(
    ingreso_total_individual      = limpiar_ingresos(ingreso_total_individual),
    ingreso_real_total_individual = limpiar_ingresos(ingreso_real_total_individual)
  )

# Path del checkpoint (mismo directorio que los datos)
.cache_imp_path <- file.path(DIR_DATOS, "cache_03a_imputacion_ingresos.rds")

# Preparar subset para imputación (solo ocupados con sección válida)
cat("   Preparando subset ocupados...\n")
ocupados <- datos %>%
  filter(pea_flag,
         !is.na(seccion),
         seccion != "No aplica (No PEA)",
         seccion != "Sin Experiencia Previa") %>%
  transmute(
    id_individuo,
    ingreso_target = ingreso_real_total_individual,
    edad           = zap_num(edad),
    sexo           = as.factor(zap_chr(sexo)),
    nivel_educ     = as.factor(zap_chr(nivel_educ_obtenido2)),
    aglomerado     = as.factor(zap_chr(aglomerado)),
    seccion        = as.factor(as.character(seccion)),
    calificacion   = as.factor(as.character(calificacion)),
    antiguedad     = zap_num(antiguedad)
  )

n_nas_ingreso <- sum(is.na(ocupados$ingreso_target))

if (n_nas_ingreso > 0) {
  cat("   Imputando", format(n_nas_ingreso, big.mark = ","), "ingresos faltantes...\n")

  # ── Checkpoint: intentar cargar imputación previa ──────────────────────────
  if (file.exists(.cache_imp_path)) {
    cat("   📦 Checkpoint detectado. Validando compatibilidad...\n")
    cached_imp <- tryCatch(readRDS(.cache_imp_path), error = function(e) NULL)

    .cache_valid <- !is.null(cached_imp) &&
      is.data.frame(cached_imp) &&
      "id_individuo"         %in% names(cached_imp) &&
      "ingreso_real_imputado" %in% names(cached_imp) &&
      nrow(cached_imp) == nrow(ocupados)

    if (.cache_valid) {
      cat("   ✅ Checkpoint válido. Saltando RF (ya fue calculado).\n")
      df_ing_resultado <- cached_imp
      rm(cached_imp, ocupados); gc(verbose = FALSE)
    } else {
      cat("   ⚠️  Checkpoint inválido o incompatible. Recalculando...\n")
      rm(cached_imp); gc(verbose = FALSE)
      .cache_valid <- FALSE
    }
  } else {
    .cache_valid <- FALSE
  }

  # ── Calcular si no hay checkpoint válido ───────────────────────────────────
  if (!.cache_valid) {

    gc(verbose = FALSE)

    # Imputar predictores (1 iteración: suficiente para auxiliares)
    cat("   Imputando predictores auxiliares (missRanger, iter=1)...\n")
    x_imp <- ocupados %>%
      select(-id_individuo, -ingreso_target) %>%
      missRanger(
        pmm.k       = 3,
        num.trees   = .rf_params$trees_missranger,
        seed        = SEED_GLOBAL,
        verbose     = 0,
        num.threads = N_CORES
      )

    df_ing <- bind_cols(
      ocupados %>% select(id_individuo, ingreso_target),
      x_imp
    )

    rm(x_imp, ocupados); gc(verbose = FALSE)

    # Entrenar RF sobre casos con ingreso observado
    train_idx  <- which(!is.na(df_ing$ingreso_target) & df_ing$ingreso_target > 0)
    target_idx <- which(is.na(df_ing$ingreso_target) | df_ing$ingreso_target <= 0)

    if (length(target_idx) > 0 && length(train_idx) > 50) {
      cat(sprintf("   Entrenando RF (%s casos, %d trees)...\n",
                  format(length(train_idx), big.mark = ","), .rf_params$trees_ranger))

      gc(verbose = FALSE)

      rf_ing <- ranger(
        ingreso_target ~ .,
        data          = df_ing %>% select(-id_individuo) %>% slice(train_idx),
        num.trees     = .rf_params$trees_ranger,
        mtry          = 3,
        importance    = "none",
        min.node.size = 20,
        seed          = SEED_GLOBAL,
        num.threads   = N_CORES
      )

      cat("   Prediciendo...\n")
      pred_ing <- predict(
        rf_ing,
        data = df_ing %>% select(-id_individuo, -ingreso_target) %>% slice(target_idx)
      )$predictions

      df_ing$ingreso_target[target_idx] <- pmax(pred_ing, 0)

      rm(rf_ing, pred_ing); gc(verbose = FALSE)
    }

    # Preparar resultado para checkpoint y merge
    df_ing_resultado <- df_ing %>%
      select(id_individuo, ingreso_real_imputado = ingreso_target)

    # Guardar checkpoint
    cat("   💾 Guardando checkpoint de imputación...\n")
    tryCatch(
      saveRDS(df_ing_resultado, .cache_imp_path, compress = FALSE),
      error = function(e) cat("   ⚠️  No se pudo guardar checkpoint:", conditionMessage(e), "\n")
    )

    rm(df_ing, train_idx, target_idx); gc(verbose = FALSE)
  }

  # ── Merge por match() in-place (evita fragmentación de RAM en Windows) ──────
  cat("   Mergeando (match in-place)...\n")

  idx <- match(datos$id_individuo, df_ing_resultado$id_individuo)
  datos$ingreso_real_final <- coalesce(
    df_ing_resultado$ingreso_real_imputado[idx],
    datos$ingreso_real_total_individual
  )

  rm(df_ing_resultado, idx); gc(verbose = FALSE)

  # Limpiar checkpoint tras merge exitoso
  if (file.exists(.cache_imp_path)) {
    file.remove(.cache_imp_path)
    cat("   🗑️  Checkpoint limpiado\n")
  }

  cat("   ✅ Ingresos imputados\n\n")

} else {
  datos <- datos %>%
    mutate(ingreso_real_final = ingreso_real_total_individual)
  cat("   ✅ Sin NAs en ingresos (skip RF)\n\n")
}


# 🪫 3. Transformaciones finales -----------------------------------------------
cat("🔎 Creando transformaciones finales...\n")

# Transformación 1: edad al cuadrado
datos <- datos %>%
  mutate(edad_cuadrado = edad^2)

cat("✅ edad_cuadrado creada\n")

# Transformación 2: lugar_nacimiento (recodificación a 3 categorías)
datos <- datos %>%
  mutate(
    lugar_nacimiento = case_when(
      lugar_nacimiento %in% c("Localidad", "Provincia", "Otra provincia") ~ "Argentina",
      lugar_nacimiento == "País limítrofe" ~ "Pais_Limitrofe",
      lugar_nacimiento == "Otro país"      ~ "Otro_Pais",
      TRUE                                 ~ NA_character_
    )
  )

cat("✅ lugar_nacimiento recodificado (Argentina, Pais_Limitrofe, Otro_Pais)\n\n")


# 🪫 4. Limpieza de columnas irrelevantes --------------------------------------
cat("🧹 Limpiando columnas irrelevantes...\n")

n_cols_antes <- ncol(datos)

# Variables a mantener (núcleo estratégico + variables LASSO + proxies)
# NOTA: tamanio_empresa excluida — no existe en dataset de origen y ningún
#       script downstream la consume (rastreado en todos los LEGACY_).
vars_core <- c(

  # IDs (12)
  "id_individuo", "id_individuo_hist", "id_hogar",
  "periodo_id", "periodo_num", "periodo", "anio", "trimestre",
  "codusu", "nro_hogar", "componente", "mas_500",

  # Demográficas (10)
  "edad", "edad_cuadrado", "sexo", "aglomerado", "region", "pondera",
  "parentesco", "estado_civil", "alfabetizacion", "lugar_nacimiento",

  # Educación (5)
  "nivel_educ_obtenido2", "asistencia_escuela", "tipo_escuela",
  "nivel_educ_cursado", "anio_aprobado",

  # Laborales (7) — tamanio_empresa excluida
  "seccion", "calificacion", "antiguedad",
  "condicion_actividad", "pea_flag",
  "categoria_ocupacional", "nbi",

  # Hogar y Estructura (5)
  "miembros_hogar", "menores10", "mayores10",
  "principal_tareas_hogar", "otros_tareas_hogar",

  # Económicas (4)
  "ingreso_real_final", "ingreso_real_total_individual",
  "ingreso_total_individual", "ingreso_real_capita_familiar",

  # Vivienda RAW — conservar para Script 03b (ICH), se eliminan allí
  "tipo_vivienda", "nro_ambientes_vivienda", "tipo_piso", "tipo_techo",
  "revestimiento_techo", "suministro_agua", "acceso_agua", "banio",
  "lugar_banio", "caract_banio", "tipo_desague", "combustible_cocina",
  "regimen_tenencia", "garage", "lavadero",

  # Formalidad oficial (1)
  "formalidad_empleo",

  # Fuentes de vida (11)
  "vive_alquiler", "vive_ganancias_negocio", "vive_renta_financiera",
  "vive_beca", "vive_cuota_alimenticia", "vive_ahorros",
  "vive_prestamos_personas", "vive_prestamos_financieros",
  "vive_financiamiento", "vive_venta_bienes", "vive_otro_ingreso"
)

# CRÍTICO: agregar TODAS las variables busca_trabajo_* dinámicamente
vars_busca <- names(datos)[str_detect(names(datos), "^busca_trabajo")]
vars_core  <- union(vars_core, vars_busca)

# Seleccionar solo columnas relevantes
datos <- datos %>%
  select(any_of(vars_core))

n_cols_despues <- ncol(datos)
pct_reduccion  <- round(100 * (n_cols_antes - n_cols_despues) / n_cols_antes, 1)

cat("✅ Columnas reducidas:", n_cols_antes, "→", n_cols_despues,
    "(reducción:", pct_reduccion, "%)\n")
cat("   Incluye variables vivienda RAW para Script 03b\n\n")


# 🪫 5. Guardado ---------------------------------------------------------------
cat("💾 Guardando resultado...\n")

gc(verbose = FALSE)

saveRDS(datos, PATH_03A_PANEL_INGRESOS)

cat("✅ Guardado:", PATH_03A_PANEL_INGRESOS, "\n")
cat("   Dimensiones:", format(nrow(datos), big.mark = ","), "x", ncol(datos), "\n")
cat("   Tamaño en memoria:", format(object.size(datos), units = "MB"), "\n\n")


# 📑 CHECKLIST SCRIPT 03a ------------------------------------------------------
cat("═══════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 03a:\n")
cat("═══════════════════════════════════════════════════════\n")
cat("   [✅] RAM adaptativa:", .rf_params$label, "\n")
cat("   [✅] Ingresos imputados (RF ocupados, checkpoint/resume)\n")
cat("   [✅] edad_cuadrado creada\n")
cat("   [✅] lugar_nacimiento recodificado (3 categorías)\n")
cat("   [✅] Columnas reducidas:", n_cols_antes, "→", n_cols_despues, "\n")
cat("   [✅] Variables vivienda RAW conservadas (para ICH en 03b)\n")
cat("   [✅] busca_trabajo_* incluidas dinámicamente\n")
cat("   [", if (file.exists(PATH_03A_PANEL_INGRESOS)) "✅" else "❌",
    "] Output: ", basename(PATH_03A_PANEL_INGRESOS), "\n", sep = "")
cat("═══════════════════════════════════════════════════════\n\n")

cat("🎯 SIGUIENTE PASO: Ejecutar 03b_ich.R\n\n")


# ⌛ Tiempo final ---------------------------------------------------------------
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("═══════════════════════════════════════════════════════\n")
cat("✅ Script 03a finalizado:", as.character(end_time), "\n")
cat("⏱️  Tiempo total:", round(elapsed / 60, 1), "minutos\n")
cat("═══════════════════════════════════════════════════════\n")

toc()
