# =============================================================================
# [EN] 08_backcasting_GLM.R -- Apply LASSO-GLM to full panel for historical backcasting (PEA + working-age universes)
# INPUTS:  rdos/datos/08_panel_formalidad_LPM*.rds, models/contracts from 07a-07b GLM
# OUTPUTS: rdos/datos/08_panel_formalidad_GLM*.rds, rdos/contratos/08_contrato_backcasting_GLM*.rds
# =============================================================================
# 🌟 08_backcasting_GLM.R 🌟 ####
# OBJETIVO: Aplicar modelo LASSO-GLM (07b base, sin interacciones) al panel
#           completo (1.79M obs, 2016T4–2025T3) para predecir formalidad donde
#           no está observada. Doble universo de backcasting:
#             _pea  → PEA (Ocupados + Desocupados) con θ
#             _edad → Población 18-60 años con θ (incluye inactivos)
#
# INPUTS:
#   DIR_DATOS     / 08_panel_formalidad_{LPM}.rds   ← base de 90 cols (input)
#   DIR_MODELOS   / 07_modelo_lasso_{GLM}.rds       (cv.glmnet binomial)
#   DIR_MODELOS   / 07_recipe_lasso_{GLM}.rds       (recipe prepped para bake)
#   DIR_CONTRATOS / 07a_contrato_lasso_{GLM}.rds    (vars seleccionadas)
#   DIR_CONTRATOS / 07b_contrato_postlasso_{GLM}.rds (umbral Youden = 0.5872)
#
# OUTPUTS:
#   DIR_DATOS     / 08_panel_formalidad_{GLM}.rds   (~100 cols, LPM + 10 GLM)
#   DIR_CONTRATOS / 08_contrato_backcasting_{GLM}.rds
#
# DIFERENCIAS CLAVE vs 08_backcasting_LPM.R:
#   - Panel INPUT es el panel LPM (90 cols), NO 06_theta_predichos.rds
#   - tipo_estimacion_pea/edad NO se recrean (LN1 — ya existen del LPM)
#   - predict(..., type = "response") → probabilidades en [0,1] nativas (L63)
#   - NO se necesita clipping: flag_pred siempre FALSE (LN3)
#   - 10 columnas nuevas con sufijo _{SUFIJO} (sin tipo_estimacion, sin cols aux)
#   - umbral Youden = 0.5872 (hardcodeado; fuente primaria: c07b$umbral_youden)
#   - Desocupados: GLM es más apropiado que LPM (no hay extrapolación fuera [0,1])
#
# LECCIONES APLICADAS: 26 (MCC as.numeric), 38 (%||%), 44 (c07b$umbral[1]),
#   45 (theta_A/B → theta_A_mA/B_mA), 56 (motor Rmd), 58 (MCC N>50k as.numeric),
#   61 (step_normalize na_rm=TRUE), 63 (type="response"), 65 (gsub 4 barras),
#   LN1 (no recrear tipo_estimacion), LN2 (umbral cal en 08), LN3 (flag FALSE),
#   LN4 (no mencionar pred fuera [0,1] en reportes)
#
# TIEMPO ESTIMADO: ~3-5 min (bake 1.3M obs + predict sparse binomial)


# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(glmnet)
  library(Matrix)
  library(pROC)
  library(tictoc)
})


# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b


# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 08 Backcasting GLM")
start_time <- Sys.time()
cat("═══════════════════════════════════════════════════════════════════\n")
cat("SCRIPT 08 — BACKCASTING DE FORMALIDAD LABORAL (GLM)\n")
cat("Doble universo: PEA (_pea) + Edad 18-60 (_edad)\n")
cat("Inicio:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

set.seed(SEED_GLOBAL)


# 🔑 Paths locales -------------------------------------------------------------
# ── Constantes ────────────────────────────────────────────────────────────────
SUFIJO     <- SUFIJO_MODELO_GLM   # e.g. "GLM4T" — definido en parametros.R
EDAD_MIN   <- 18L
EDAD_MAX   <- 60L
ID_VAR     <- "id_individuo_hist"

NIVELES_VALIDOS <- c("Formal oficial", "Informal oficial")

# Umbral Youden — leído desde contrato (fuente primaria: c07b$umbral_youden)
# La variable UMBRAL_YOUDEN_HARDCODE se elimina; el umbral se extrae post-readRDS.
# Ver sección [1] más abajo.

# Labels que indican "no aplica" en variables laborales → convertir a NA
# para que step_impute_mode del recipe las impute con la moda del training
LABELS_NO_APLICA <- c("No aplica (No PEA)", "No aplica",
                       "Sin Experiencia Previa", "Sin Calificación")

# ── Nombres dinámicos de columnas de predicción ─────────────────────────────
COL_PROB_RAW_PEA   <- paste0("prob_formal_raw_", SUFIJO, "_pea")
COL_PROB_PEA       <- paste0("prob_formal_", SUFIJO, "_pea")
COL_CLASE_PEA      <- paste0("formalidad_clase_", SUFIJO, "_pea")
COL_FLAG_PEA       <- paste0("flag_pred_", SUFIJO, "_pea")
COL_CLASE_CAL_PEA  <- paste0("formalidad_clase_cal_", SUFIJO, "_pea")
COL_PROB_RAW_EDAD  <- paste0("prob_formal_raw_", SUFIJO, "_edad")
COL_PROB_EDAD      <- paste0("prob_formal_", SUFIJO, "_edad")
COL_CLASE_EDAD     <- paste0("formalidad_clase_", SUFIJO, "_edad")
COL_FLAG_EDAD      <- paste0("flag_pred_", SUFIJO, "_edad")
COL_CLASE_CAL_EDAD <- paste0("formalidad_clase_cal_", SUFIJO, "_edad")

# Columnas LPM esperadas en el panel de entrada
COL_LPM_PROB_RAW_PEA  <- paste0("prob_formal_raw_", SUFIJO_MODELO_LPM, "_pea")
COL_LPM_PROB_PEA      <- paste0("prob_formal_", SUFIJO_MODELO_LPM, "_pea")
COL_LPM_CLASE_PEA     <- paste0("formalidad_clase_", SUFIJO_MODELO_LPM, "_pea")
COL_LPM_CLASE_CAL_PEA <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_LPM, "_pea")
COL_LPM_FLAG_PEA      <- paste0("flag_pred_", SUFIJO_MODELO_LPM, "_pea")
COL_LPM_PROB_RAW_EDAD <- paste0("prob_formal_raw_", SUFIJO_MODELO_LPM, "_edad")
COL_LPM_PROB_EDAD     <- paste0("prob_formal_", SUFIJO_MODELO_LPM, "_edad")
COL_LPM_CLASE_EDAD    <- paste0("formalidad_clase_", SUFIJO_MODELO_LPM, "_edad")
COL_LPM_CLASE_CAL_EDAD <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_LPM, "_edad")
COL_LPM_FLAG_EDAD     <- paste0("flag_pred_", SUFIJO_MODELO_LPM, "_edad")

# ── Paths ─────────────────────────────────────────────────────────────────────
# Input base: panel LPM ya construido (90 cols con tipo_estimacion)
PATH_PANEL        <- file.path(DIR_DATOS,     paste0("08_panel_formalidad_", SUFIJO_MODELO_LPM, ".rds"))
PATH_MODELO       <- file.path(DIR_MODELOS,   paste0("07_modelo_lasso_",    SUFIJO, ".rds"))
PATH_RECIPE       <- file.path(DIR_MODELOS,   paste0("07_recipe_lasso_",    SUFIJO, ".rds"))
PATH_07A_CONTRATO <- file.path(DIR_CONTRATOS, paste0("07a_contrato_lasso_", SUFIJO, ".rds"))
PATH_07B_CONTRATO <- file.path(DIR_CONTRATOS, paste0("07b_contrato_postlasso_", SUFIJO, ".rds"))

PATH_PANEL_OUT    <- file.path(DIR_DATOS,     paste0("08_panel_formalidad_", SUFIJO, ".rds"))
PATH_CONTRATO_OUT <- file.path(DIR_CONTRATOS, paste0("08_contrato_backcasting_", SUFIJO, ".rds"))


# 🪫 1. CARGA DE DATOS Y OBJETOS ------------------------------------------------
cat("── 1. Carga de datos y objetos ────────────────────────────────────\n")

hard_stop(file.exists(PATH_PANEL),
          paste("Panel LPM no encontrado:", PATH_PANEL))
hard_stop(file.exists(PATH_MODELO),
          paste("Modelo GLM no encontrado:", PATH_MODELO))
hard_stop(file.exists(PATH_RECIPE),
          paste("Recipe GLM no encontrado:", PATH_RECIPE))
hard_stop(file.exists(PATH_07A_CONTRATO),
          paste("Contrato 07a GLM no encontrado:", PATH_07A_CONTRATO))
hard_stop(file.exists(PATH_07B_CONTRATO),
          paste("Contrato 07b GLM no encontrado:", PATH_07B_CONTRATO))

panel        <- readRDS(PATH_PANEL)
cv_fit       <- readRDS(PATH_MODELO)
recipe_lasso <- readRDS(PATH_RECIPE)
c07a         <- readRDS(PATH_07A_CONTRATO)
c07b         <- readRDS(PATH_07B_CONTRATO)

cat("  Panel LPM cargado:", format(nrow(panel), big.mark = ","), "obs ×", ncol(panel), "vars\n")
cat("  Modelo GLM (cv.glmnet binomial): λ.1se =", round(cv_fit$lambda.1se, 6), "\n")
cat("  Vars seleccionadas (c07a):", c07a$n_vars_sel_1se, "\n")

# ── Umbral Youden desde contrato (fuente primaria) ────────────────────────────
umbral <- tryCatch({
  val <- c07b$umbral_youden %||% c07b$metricas_clf$umbral[1]
  as.numeric(val)[1]
}, error = function(e) NA_real_)

hard_stop(!is.na(umbral) && umbral > 0 && umbral < 1,
          paste("Umbral Youden no recuperado de c07b — revisar contrato:", PATH_07B_CONTRATO))
cat("  Umbral Youden (c07b):", round(umbral, 4), "\n")


# 🪫 2. VERIFICACIONES INICIALES ------------------------------------------------
cat("\n── 2. Verificaciones iniciales ─────────────────────────────────────\n")

N_PANEL_ORIGINAL <- nrow(panel)  # guardar para verificar integridad post-proceso
hard_stop(ID_VAR %in% names(panel),
          paste(ID_VAR, "no encontrado en el panel"))
hard_stop("periodo_id" %in% names(panel),
          "periodo_id no encontrada en el panel")
hard_stop("condicion_actividad" %in% names(panel),
          "condicion_actividad no encontrada en el panel")
hard_stop(all(c("theta_A", "theta_B") %in% names(panel)),
          "theta_A y/o theta_B no encontradas en el panel")
hard_stop("edad" %in% names(panel),
          "Variable edad no encontrada en el panel")
hard_stop("formalidad_empleo" %in% names(panel),
          "formalidad_empleo no encontrada en el panel")

# Verificar que tipo_estimacion ya existe del LPM (LN1)
hard_stop("tipo_estimacion_pea" %in% names(panel),
          "tipo_estimacion_pea NO encontrada en panel LPM — revisar input")
hard_stop("tipo_estimacion_edad" %in% names(panel),
          "tipo_estimacion_edad NO encontrada en panel LPM — revisar input")

cat("  [✅] tipo_estimacion_pea y tipo_estimacion_edad presentes (heredados de LPM)\n")

# Verificar formalidad_valida (puede ya existir del LPM)
if (!"formalidad_valida" %in% names(panel)) {
  panel <- panel %>%
    mutate(
      formalidad_valida = if_else(
        formalidad_empleo %in% NIVELES_VALIDOS,
        formalidad_empleo,
        NA_character_
      )
    )
  cat("  formalidad_valida creada\n")
} else {
  cat("  formalidad_valida heredada del LPM\n")
}

# Periodos con formalidad observada
periodos_con_formalidad <- panel %>%
  filter(!is.na(formalidad_valida)) %>%
  pull(periodo_id) %>% unique() %>% sort()

cat("  Periodos con formalidad observada:",
    paste(periodos_con_formalidad, collapse = ", "), "\n")
hard_stop(length(periodos_con_formalidad) >= 1,
          "No se encontraron periodos con formalidad observada")

TRIMESTRES_TRAINING <- if (exists("TRIMESTRES_FORMALIDAD")) {
  cat("  Usando TRIMESTRES_FORMALIDAD de parametros.R:",
      paste(TRIMESTRES_FORMALIDAD, collapse = ", "), "\n")
  TRIMESTRES_FORMALIDAD
} else {
  cat("  Auto-detectados como TRIMESTRES_TRAINING\n")
  periodos_con_formalidad
}

# Distribución de tipo_estimacion
cat("\n  tipo_estimacion_pea:\n")
print(table(panel$tipo_estimacion_pea, useNA = "ifany"))
cat("\n  tipo_estimacion_edad:\n")
print(table(panel$tipo_estimacion_edad, useNA = "ifany"))

# Cobertura θ por condición de actividad
cat("\n  Cobertura θ por condicion_actividad:\n")
panel %>%
  group_by(condicion_actividad) %>%
  summarise(
    n_total      = n(),
    n_con_theta  = sum(!is.na(theta_A) & !is.na(theta_B)),
    pct_cobertura = round(n_con_theta / n_total * 100, 1),
    .groups = "drop"
  ) %>%
  print()

cat("  Rango de edad:", range(panel$edad, na.rm = TRUE), "\n")
cat("  N en rango", EDAD_MIN, "-", EDAD_MAX, ":",
    format(sum(panel$edad >= EDAD_MIN & panel$edad <= EDAD_MAX, na.rm = TRUE),
           big.mark = ","), "\n")

cat("  [✅] Verificaciones iniciales OK\n")


# 🪫 3. TIPO_ESTIMACION — NO SE RECREA (LN1) ------------------------------------
cat("\n── 3. tipo_estimacion — heredado del panel LPM ─────────────────────\n")
cat("  (LN1) tipo_estimacion_pea y tipo_estimacion_edad ya existen del LPM.\n")
cat("  No se recrean. Se reutilizan directamente para el universo GLM.\n")

# Verificar consistencia: no debe haber NAs en tipo_estimacion
n_na_pea  <- sum(is.na(panel$tipo_estimacion_pea))
n_na_edad <- sum(is.na(panel$tipo_estimacion_edad))
if (n_na_pea  > 0) cat("  ⚠️ WARN:", n_na_pea,  "NAs en tipo_estimacion_pea\n")
if (n_na_edad > 0) cat("  ⚠️ WARN:", n_na_edad, "NAs en tipo_estimacion_edad\n")

cat("  [✅] tipo_estimacion verificados — sin recreación\n")


# 🪫 4. UNIVERSO UNIÓN ----------------------------------------------------------
cat("\n── 4. Construcción del universo unión ──────────────────────────────\n")

TIPOS_ELEGIBLES_PEA  <- c("Observado", "Backcasting", "Potencial_desocupado")
TIPOS_ELEGIBLES_EDAD <- c("Observado", "Backcasting", "Potencial_desocupado",
                           "Potencial_inactivo")

panel <- panel %>%
  mutate(
    elegible_pea   = tipo_estimacion_pea  %in% TIPOS_ELEGIBLES_PEA,
    elegible_edad  = tipo_estimacion_edad %in% TIPOS_ELEGIBLES_EDAD,
    elegible_union = elegible_pea | elegible_edad
  )

n_elegible_pea   <- sum(panel$elegible_pea)
n_elegible_edad  <- sum(panel$elegible_edad)
n_elegible_union <- sum(panel$elegible_union)
n_solo_pea       <- sum(panel$elegible_pea & !panel$elegible_edad)
n_solo_edad      <- sum(!panel$elegible_pea & panel$elegible_edad)
n_ambos          <- sum(panel$elegible_pea & panel$elegible_edad)

cat("  Elegibles PEA:        ", format(n_elegible_pea,   big.mark = ","), "\n")
cat("  Elegibles Edad 18-60: ", format(n_elegible_edad,  big.mark = ","), "\n")
cat("  Unión:                ", format(n_elegible_union, big.mark = ","), "\n")
cat("    Solo PEA:           ", format(n_solo_pea,  big.mark = ","),
    "(PEA fuera de 18-60)\n")
cat("    Solo Edad:          ", format(n_solo_edad, big.mark = ","),
    "(Inactivos 18-60 con θ)\n")
cat("    Ambos:              ", format(n_ambos, big.mark = ","), "\n")

# Guardar posiciones para re-armado por posición (más seguro que join)
idx_union <- which(panel$elegible_union)

universo_union <- panel[idx_union, ]
cat("  Universo unión extraído:", format(nrow(universo_union), big.mark = ","), "obs\n")

cat("  [✅] Universo unión construido\n")


# 🪫 5. BAKE — PREPARACIÓN DE FEATURES ------------------------------------------
cat("\n── 5. Preparación de features (bake) ──────────────────────────────\n")

# Guardar columnas de identificación antes de manipular
id_cols_union <- universo_union %>%
  select(all_of(ID_VAR), periodo_id, condicion_actividad,
         tipo_estimacion_pea, tipo_estimacion_edad,
         elegible_pea, elegible_edad,
         formalidad_valida, pondera)

# 5a. Recodificar lugar_nacimiento (misma lógica que 07a)
if ("lugar_nacimiento" %in% names(universo_union)) {
  universo_union <- universo_union %>%
    mutate(lugar_nacimiento = case_when(
      lugar_nacimiento %in% c("Localidad", "Provincia", "Otra provincia") ~ "Argentina",
      lugar_nacimiento == "País limítrofe" ~ "Pais_Limitrofe",
      lugar_nacimiento == "Otro país"      ~ "Otro_Pais",
      TRUE ~ lugar_nacimiento
    ))
  cat("  lugar_nacimiento recodificado (3 niveles)\n")
}

# 5b. Excluir variables que no son predictores
# Incluye las columnas LPM ya presentes en el panel base
vars_excluir <- c("pondera", "formalidad_empleo", "formalidad_valida", "codusu",
                   ID_VAR, "id_individuo", "periodo_id", "anio",
                   "periodo_num", "condicion_actividad",
                   # NOTA: trimestre NO se excluye — es regresor estacional en 07a
                   # Columnas creadas aquí / heredadas de LPM
                   "tipo_estimacion_pea", "tipo_estimacion_edad",
                   "elegible_pea", "elegible_edad", "elegible_union",
                   # Columnas LPM en el panel (no predictores del GLM)
                   COL_LPM_PROB_RAW_PEA, COL_LPM_PROB_PEA,
                   COL_LPM_CLASE_PEA, COL_LPM_CLASE_CAL_PEA,
                   COL_LPM_FLAG_PEA,
                   COL_LPM_PROB_RAW_EDAD, COL_LPM_PROB_EDAD,
                   COL_LPM_CLASE_EDAD, COL_LPM_CLASE_CAL_EDAD,
                   COL_LPM_FLAG_EDAD,
                   "formalidad_valida")

vars_excluir_presentes <- intersect(vars_excluir, names(universo_union))
features_union <- universo_union %>%
  select(-all_of(vars_excluir_presentes))

# 5c. Renombrar theta_A/theta_B → theta_A_mA/theta_B_mA (lección 45)
# El recipe fue entrenado con sufijo _mA (modelo A del heterofactor)
if ("theta_A" %in% names(features_union) && !"theta_A_mA" %in% names(features_union)) {
  features_union <- features_union %>%
    rename(theta_A_mA = theta_A, theta_B_mA = theta_B)
  cat("  theta_A → theta_A_mA, theta_B → theta_B_mA (match recipe)\n")
}

# 5d. Convertir labels "No aplica" a NA para imputación por recipe
n_converted <- 0L
features_union <- features_union %>%
  mutate(across(where(is.character), function(x) {
    mask <- x %in% LABELS_NO_APLICA
    n_converted <<- n_converted + sum(mask, na.rm = TRUE)
    if_else(mask, NA_character_, x)
  }))
cat("  Labels 'No aplica' convertidas a NA:", format(n_converted, big.mark = ","), "\n")

# 5e. Agregar formalidad_bin = NA (outcome dummy para el recipe)
features_union <- features_union %>%
  mutate(formalidad_bin = NA_real_)

# 5f. Verificar variables del recipe
vars_recipe         <- recipe_lasso$var_info$variable
vars_faltantes_recipe <- setdiff(vars_recipe, names(features_union))
if (length(vars_faltantes_recipe) > 0) {
  cat("  ⚠️ Variables esperadas por recipe NO encontradas en features:\n")
  cat("    ", paste(vars_faltantes_recipe, collapse = ", "), "\n")
}

vars_extra <- setdiff(names(features_union), vars_recipe)
if (length(vars_extra) > 0) {
  cat("  Variables extra (no en recipe, se ignoran):", length(vars_extra), "\n")
  features_union <- features_union %>% select(any_of(vars_recipe))
}

cat("  Dimensiones features para bake:", nrow(features_union), "×", ncol(features_union), "\n")

# 5g. BAKE
cat("  Ejecutando bake()...\n")
tic("bake")
baked_union <- bake(recipe_lasso, new_data = features_union)
toc()

# Eliminar outcome del resultado
if ("formalidad_bin" %in% names(baked_union)) {
  baked_union <- baked_union %>% select(-formalidad_bin)
}

cat("  Dimensiones post-bake:", nrow(baked_union), "×", ncol(baked_union), "\n")

# Verificar NAs post-bake
n_na_post_bake <- sum(is.na(baked_union))
if (n_na_post_bake > 0) {
  cat("  ⚠️ WARN:", format(n_na_post_bake, big.mark = ","),
      "NAs post-bake. Columnas con NAs:\n")
  na_por_col <- colSums(is.na(baked_union))
  print(na_por_col[na_por_col > 0])
}

cat("  [✅] Bake completado\n")


# 🪫 6. PREDICT (cv.glmnet binomial — type = "response") ------------------------
cat("\n── 6. Predicción (cv.glmnet binomial) ──────────────────────────────\n")

# 6a. Convertir a sparse matrix (dgCMatrix) — eficiente para 1.3M obs
cat("  Convirtiendo a sparse matrix...\n")
tic("sparse matrix")
X_union <- Matrix(as.matrix(baked_union), sparse = TRUE)
toc()
cat("  Dimensiones X_union:", nrow(X_union), "×", ncol(X_union), "\n")

# Liberar memoria del tibble baked
rm(baked_union, features_union); gc(verbose = FALSE)

# 6b. Predict con cv.glmnet binomial — type = "response" → probs en [0,1]
# (lección L63) El GLM garantiza [0,1] nativamente — NO se necesita clipping
cat("  Ejecutando predict(cv_fit, type = 'response', s = 'lambda.1se')...\n")
tic("predict")
pred_raw <- as.vector(
  predict(cv_fit, newx = X_union, s = "lambda.1se", type = "response")
)
toc()

# Liberar matrix
rm(X_union); gc(verbose = FALSE)

cat("  Predicciones generadas:", format(length(pred_raw), big.mark = ","), "\n")
hard_stop(length(pred_raw) == nrow(universo_union),
          "Longitud de predicciones != N universo unión")

# 6c. Verificar rango [0,1] (invariante del GLM binomial)
n_fuera     <- sum(pred_raw < 0 | pred_raw > 1, na.rm = TRUE)
n_fuera_neg <- sum(pred_raw < 0, na.rm = TRUE)
n_fuera_pos <- sum(pred_raw > 1, na.rm = TRUE)
pct_fuera   <- round(mean(pred_raw < 0 | pred_raw > 1) * 100, 4)

# (LN3) El GLM garantiza [0,1] → flag siempre FALSE.
# pred_clip = pred_raw sin modificación, pero se mantiene el nombre para
# consistencia estructural con el pipeline LPM.
pred_clip <- pred_raw   # [0,1] nativo, no se modifica

if (n_fuera > 0) {
  cat(sprintf("  ⚠️ WARN: %d pred fuera de [0,1] (%.4f%%) — inesperado en GLM binomial\n",
              n_fuera, pct_fuera))
  cat("    Negativas:", n_fuera_neg, "| Positivas:", n_fuera_pos, "\n")
  # Clipping de seguridad solo si se detectan valores anómalos
  pred_clip <- pmax(0, pmin(1, pred_raw))
  cat("  Clipping de seguridad aplicado.\n")
} else {
  cat(sprintf("  Pred fuera de [0,1]: %d (%.4f%%) ✅ — invariante GLM confirmada\n",
              n_fuera, pct_fuera))
}

pred_clase <- if_else(pred_clip >= umbral, "Formal", "Informal")
flag_fuera <- pred_raw < 0 | pred_raw > 1   # siempre FALSE en GLM

cat("  Media prob_formal:", round(mean(pred_clip), 4), "\n")
cat("  Umbral Youden:", round(umbral, 4), "\n")
cat("  Tasa formal predicha (unión):",
    round(mean(pred_clase == "Formal") * 100, 2), "%\n")

cat("  [✅] Predicción completada\n")


# 🪫 7. ASIGNACIÓN DOBLE — SPLIT _pea / _edad -----------------------------------
cat("\n── 7. Asignación doble de predicciones ─────────────────────────────\n")

pred_union <- tibble(
  prob_raw   = pred_raw,
  prob_clip  = pred_clip,
  clase      = pred_clase,
  flag_fuera = flag_fuera
)

en_pea  <- id_cols_union$elegible_pea
en_edad <- id_cols_union$elegible_edad

pred_union <- pred_union %>%
  mutate(
    # Columnas PEA
    !!COL_PROB_RAW_PEA  := if_else(en_pea, prob_raw,   NA_real_),
    !!COL_PROB_PEA      := if_else(en_pea, prob_clip,  NA_real_),
    !!COL_CLASE_PEA     := if_else(en_pea, clase,      NA_character_),
    !!COL_FLAG_PEA      := if_else(en_pea, flag_fuera, NA),
    # Columnas Edad
    !!COL_PROB_RAW_EDAD := if_else(en_edad, prob_raw,   NA_real_),
    !!COL_PROB_EDAD     := if_else(en_edad, prob_clip,  NA_real_),
    !!COL_CLASE_EDAD    := if_else(en_edad, clase,      NA_character_),
    !!COL_FLAG_EDAD     := if_else(en_edad, flag_fuera, NA)
  )

cat("  Universo PEA:\n")
cat("    N con predicción:", format(sum(en_pea), big.mark = ","), "\n")
cat("    Tasa formal:", round(mean(pred_union[[COL_CLASE_PEA]] == "Formal",
                                    na.rm = TRUE) * 100, 2), "%\n")
cat("    Pred fuera [0,1]:", round(mean(pred_union[[COL_FLAG_PEA]],
                                         na.rm = TRUE) * 100, 4), "%\n")

cat("  Universo Edad 18-60:\n")
cat("    N con predicción:", format(sum(en_edad), big.mark = ","), "\n")
cat("    Tasa formal:", round(mean(pred_union[[COL_CLASE_EDAD]] == "Formal",
                                    na.rm = TRUE) * 100, 2), "%\n")
cat("    Pred fuera [0,1]:", round(mean(pred_union[[COL_FLAG_EDAD]],
                                         na.rm = TRUE) * 100, 4), "%\n")

cat("  [✅] Asignación doble completada\n")


# 🪫 8. VALIDACIÓN EN VENTANA DE TRAINING ---------------------------------------
cat("\n── 8. Validación en ventana de training ────────────────────────────\n")

# Observados: Ocupados con formalidad válida (idénticos en ambos universos)
mask_obs <- id_cols_union$tipo_estimacion_pea == "Observado"

comp_training <- tibble(
  y_obs     = as.integer(id_cols_union$formalidad_valida[mask_obs] == "Formal oficial"),
  pred_prob = pred_clip[mask_obs],
  pred_cls  = pred_clase[mask_obs],
  pondera   = id_cols_union$pondera[mask_obs],
  periodo   = id_cols_union$periodo_id[mask_obs]
)

cat("  N Observados para validación:", format(nrow(comp_training), big.mark = ","), "\n")

# 8a. AUC global (ponderado)
roc_train <- roc(response  = comp_training$y_obs,
                 predictor = comp_training$pred_prob,
                 weights   = comp_training$pondera / mean(comp_training$pondera),
                 quiet     = TRUE)
auc_global_train <- as.numeric(auc(roc_train))
cat("  AUC global (training, ponderado):", round(auc_global_train, 4), "\n")
cat("  [Referencia 07b test AUC-ROC: 0.8686]\n")

# 8b. MAE global
mae_global_train <- mean(abs(comp_training$pred_prob - comp_training$y_obs))
cat("  MAE global (training):", round(mae_global_train, 4), "\n")

# 8c. Comparación tasa obs vs pred por trimestre (umbral Youden)
comp_por_trimestre <- comp_training %>%
  group_by(periodo = periodo) %>%
  summarise(
    n            = n(),
    tasa_obs     = round(weighted.mean(y_obs, pondera), 4),
    tasa_pred_cl = round(weighted.mean(as.integer(pred_cls == "Formal"), pondera), 4),
    tasa_pred_pr = round(weighted.mean(pred_prob, pondera), 4),
    .groups      = "drop"
  ) %>%
  mutate(
    delta_cl_pp = round((tasa_pred_cl - tasa_obs) * 100, 2),
    delta_pr_pp = round((tasa_pred_pr - tasa_obs) * 100, 2)
  )

cat("\n  Comparación tasa obs vs pred por trimestre:\n")
cat("  (tasa_pred_cl = clasificación con umbral Youden 0.5872; tasa_pred_pr = prob media)\n")
print(comp_por_trimestre, n = Inf)

delta_max <- max(abs(comp_por_trimestre$delta_cl_pp))
if (delta_max < 2) {
  cat("  [✅] Delta máximo:", delta_max, "pp < 2 pp → calibración excelente\n")
} else if (delta_max < 5) {
  cat("  [✅] Delta máximo:", delta_max, "pp < 5 pp → calibración aceptable\n")
  cat("       NOTA: delta > 2 pp. Documentar en paper.\n")
} else {
  cat("  [⚠️] Delta máximo:", delta_max, "pp ≥ 5 pp → revisar calibración\n")
}

# 8d. Tabla de confusión
pred_clase_obs <- as.integer(comp_training$pred_cls == "Formal")
cm <- table(Real = comp_training$y_obs, Predicho = pred_clase_obs)
cat("\n  Matriz de confusión (no ponderada):\n")
print(cm)

tp <- as.numeric(cm[2, 2]); tn <- as.numeric(cm[1, 1])
fp <- as.numeric(cm[1, 2]); fn <- as.numeric(cm[2, 1])
n_cm <- tp + tn + fp + fn

acc_train  <- (tp + tn) / n_cm
sens_train <- tp / (tp + fn)
esp_train  <- tn / (tn + fp)
f1_train   <- 2 * (tp / (tp + fp)) * sens_train / ((tp / (tp + fp)) + sens_train)

# MCC — lección 26 + 58: as.numeric() en todos los términos
mcc_train  <- (as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)) /
  sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) *
         as.numeric(tn + fp) * as.numeric(tn + fn))

cat(sprintf("  Accuracy: %.4f | Sens: %.4f | Esp: %.4f | F1: %.4f | MCC: %.4f\n",
            acc_train, sens_train, esp_train, f1_train, mcc_train))

# 8e. Umbral de calibración (iguala tasa predicha ≈ tasa observada)
# (LN2) El umbral de calibración GLM se determina aquí empíricamente,
# usando la misma metodología que el LPM.
# Nota: H-L p=0.457 indica calibración nativa excelente del GLM binomial.
# El umbral de calibración complementa esta calibración probabilística
# con una alineación de tasas de clasificación.
tasa_obs_global <- weighted.mean(comp_training$y_obs, comp_training$pondera)
umbrales_grid   <- seq(0.30, 0.75, by = 0.005)
tasas_pred_grid <- sapply(umbrales_grid, function(u) {
  weighted.mean(as.integer(comp_training$pred_prob >= u), comp_training$pondera)
})
umbral_calibracion <- umbrales_grid[which.min(abs(tasas_pred_grid - tasa_obs_global))]
tasa_pred_cal      <- tasas_pred_grid[which.min(abs(tasas_pred_grid - tasa_obs_global))]

cat(sprintf("\n  Umbral de calibración: %.4f (iguala tasa obs %.2f%% ≈ pred %.2f%%)\n",
            umbral_calibracion, tasa_obs_global * 100, tasa_pred_cal * 100))

# Métricas con umbral de calibración
pred_cal_obs <- as.integer(comp_training$pred_prob >= umbral_calibracion)
cm_cal <- table(Real = comp_training$y_obs, Predicho = pred_cal_obs)
tp_c <- as.numeric(cm_cal[2, 2]); tn_c <- as.numeric(cm_cal[1, 1])
fp_c <- as.numeric(cm_cal[1, 2]); fn_c <- as.numeric(cm_cal[2, 1])
acc_cal  <- (tp_c + tn_c) / sum(cm_cal)
sens_cal <- tp_c / (tp_c + fn_c)
esp_cal  <- tn_c / (tn_c + fp_c)
f1_cal   <- 2 * tp_c / (2 * tp_c + fp_c + fn_c)
mcc_cal  <- (as.numeric(tp_c) * as.numeric(tn_c) - as.numeric(fp_c) * as.numeric(fn_c)) /
  sqrt(as.numeric(tp_c + fp_c) * as.numeric(tp_c + fn_c) *
         as.numeric(tn_c + fp_c) * as.numeric(tn_c + fn_c))

cat(sprintf("  Métricas umbral calibración: Acc %.4f | Sens %.4f | Esp %.4f | F1 %.4f | MCC %.4f\n",
            acc_cal, sens_cal, esp_cal, f1_cal, mcc_cal))

# Comparación por trimestre con umbral de calibración
comp_por_trim_cal <- comp_training %>%
  group_by(periodo = periodo) %>%
  summarise(
    tasa_obs      = round(weighted.mean(y_obs, pondera), 4),
    tasa_pred_cal = round(weighted.mean(as.integer(pred_prob >= umbral_calibracion), pondera), 4),
    .groups = "drop"
  ) %>%
  mutate(delta_cal_pp = round((tasa_pred_cal - tasa_obs) * 100, 2))

cat("\n  Delta por trimestre con umbral calibración:\n")
print(comp_por_trim_cal, n = Inf)
delta_max_cal <- max(abs(comp_por_trim_cal$delta_cal_pp))
cat("  Delta máximo (calibración):", delta_max_cal, "pp\n")

cat("  [✅] Validación training completada\n")


# 🪫 9. HOLD-OUT (si hay obs con formalidad fuera de ventana training) ----------
cat("\n── 9. Validación hold-out ──────────────────────────────────────────\n")

mask_holdout <- !is.na(id_cols_union$formalidad_valida) &
  !(id_cols_union$periodo_id %in% TRIMESTRES_TRAINING)

metricas_holdout <- NULL

if (sum(mask_holdout) >= 50) {
  cat("  Obs hold-out encontradas:", format(sum(mask_holdout), big.mark = ","), "\n")

  holdout <- tibble(
    y_obs     = as.integer(id_cols_union$formalidad_valida[mask_holdout] == "Formal oficial"),
    pred_prob = pred_clip[mask_holdout],
    pondera   = id_cols_union$pondera[mask_holdout],
    periodo   = id_cols_union$periodo_id[mask_holdout]
  )

  roc_ho <- roc(holdout$y_obs, holdout$pred_prob, quiet = TRUE)

  metricas_holdout <- list(
    periodos = unique(holdout$periodo),
    n        = nrow(holdout),
    auc      = as.numeric(auc(roc_ho)),
    mae      = mean(abs(holdout$pred_prob - holdout$y_obs)),
    accuracy = mean((holdout$pred_prob >= umbral) == holdout$y_obs)
  )

  cat("  AUC hold-out:", round(metricas_holdout$auc, 4), "\n")
  cat("  MAE hold-out:", round(metricas_holdout$mae, 4), "\n")

  if (metricas_holdout$auc < 0.75) {
    cat("  [⚠️] AUC hold-out < 0.75. Documentar como limitación.\n")
  } else {
    cat("  [✅] AUC hold-out ≥ 0.75. Generalización OK.\n")
  }
} else {
  cat("  No hay suficientes obs con formalidad fuera de ventana training (",
      sum(mask_holdout), " encontradas).\n")
  cat("  Hold-out no disponible. Documentar en paper.\n")
}


# 🪫 10. ENSAMBLADO DEL PANEL FINAL ---------------------------------------------
cat("\n── 10. Ensamblado del panel final ──────────────────────────────────\n")

# Seleccionar columnas nuevas del pred_union (solo las del SUFIJO)
cols_nuevas <- pred_union %>%
  select(all_of(c(COL_PROB_RAW_PEA, COL_PROB_PEA, COL_CLASE_PEA, COL_FLAG_PEA,
                  COL_PROB_RAW_EDAD, COL_PROB_EDAD, COL_CLASE_EDAD, COL_FLAG_EDAD)))

# Inicializar 10 columnas GLM en el panel completo como NA
panel[[COL_PROB_RAW_PEA]]  <- NA_real_
panel[[COL_PROB_PEA]]      <- NA_real_
panel[[COL_CLASE_PEA]]     <- NA_character_
panel[[COL_FLAG_PEA]]      <- NA
panel[[COL_PROB_RAW_EDAD]] <- NA_real_
panel[[COL_PROB_EDAD]]     <- NA_real_
panel[[COL_CLASE_EDAD]]    <- NA_character_
panel[[COL_FLAG_EDAD]]     <- NA

# Asignar por posición (idx_union mapea panel → universo_union)
panel[[COL_PROB_RAW_PEA]][idx_union]  <- cols_nuevas[[COL_PROB_RAW_PEA]]
panel[[COL_PROB_PEA]][idx_union]      <- cols_nuevas[[COL_PROB_PEA]]
panel[[COL_CLASE_PEA]][idx_union]     <- cols_nuevas[[COL_CLASE_PEA]]
panel[[COL_FLAG_PEA]][idx_union]      <- cols_nuevas[[COL_FLAG_PEA]]
panel[[COL_PROB_RAW_EDAD]][idx_union] <- cols_nuevas[[COL_PROB_RAW_EDAD]]
panel[[COL_PROB_EDAD]][idx_union]     <- cols_nuevas[[COL_PROB_EDAD]]
panel[[COL_CLASE_EDAD]][idx_union]    <- cols_nuevas[[COL_CLASE_EDAD]]
panel[[COL_FLAG_EDAD]][idx_union]     <- cols_nuevas[[COL_FLAG_EDAD]]

# Columnas con umbral de calibración (determinado en sección 8e)
clase_cal_raw <- if_else(pred_clip >= umbral_calibracion, "Formal", "Informal")

panel[[COL_CLASE_CAL_PEA]]  <- NA_character_
panel[[COL_CLASE_CAL_EDAD]] <- NA_character_
panel[[COL_CLASE_CAL_PEA]][idx_union]  <- if_else(en_pea,  clase_cal_raw, NA_character_)
panel[[COL_CLASE_CAL_EDAD]][idx_union] <- if_else(en_edad, clase_cal_raw, NA_character_)

cat("  Clasificación con umbral calibración (", round(umbral_calibracion, 4),
    ") agregada\n", sep = "")

# Eliminar columnas auxiliares de elegibilidad
panel <- panel %>%
  select(-elegible_pea, -elegible_edad, -elegible_union)

cat("  Panel final:", format(nrow(panel), big.mark = ","), "×", ncol(panel), "\n")
cat("  Columnas nuevas", SUFIJO, ": 10 (2 prob_raw, 2 prob, 2 clase_youden,",
    "2 clase_cal, 2 flag)\n")

# Verificar: N total no cambió
hard_stop(nrow(panel) == N_PANEL_ORIGINAL,
          paste("Panel final tiene", nrow(panel), "obs — esperadas", N_PANEL_ORIGINAL))

# Verificar: no hay NAs inesperados en elegibles
n_na_prob_pea  <- sum(is.na(panel[[COL_PROB_PEA]]) &
                        panel$tipo_estimacion_pea %in% TIPOS_ELEGIBLES_PEA)
n_na_prob_edad <- sum(is.na(panel[[COL_PROB_EDAD]]) &
                        panel$tipo_estimacion_edad %in% TIPOS_ELEGIBLES_EDAD)

if (n_na_prob_pea  > 0) cat("  ⚠️ WARN:", n_na_prob_pea,  "NAs en", COL_PROB_PEA, "para PEA elegible\n")
if (n_na_prob_edad > 0) cat("  ⚠️ WARN:", n_na_prob_edad, "NAs en", COL_PROB_EDAD, "para Edad elegible\n")

# Distribución final — Universo PEA
cat("\n  Distribución final — Universo PEA:\n")
resumen_pea <- panel %>%
  group_by(tipo_estimacion_pea) %>%
  summarise(
    n = n(),
    pct = round(n / nrow(panel) * 100, 2),
    n_con_pred = sum(!is.na(.data[[COL_PROB_PEA]])),
    tasa_formal_youden = round(mean(.data[[COL_CLASE_PEA]] == "Formal", na.rm = TRUE) * 100, 1),
    tasa_formal_cal    = round(mean(.data[[COL_CLASE_CAL_PEA]] == "Formal", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(resumen_pea)

# Distribución final — Universo Edad
cat("\n  Distribución final — Universo Edad 18-60:\n")
resumen_edad <- panel %>%
  group_by(tipo_estimacion_edad) %>%
  summarise(
    n = n(),
    pct = round(n / nrow(panel) * 100, 2),
    n_con_pred = sum(!is.na(.data[[COL_PROB_EDAD]])),
    tasa_formal_youden = round(mean(.data[[COL_CLASE_EDAD]] == "Formal", na.rm = TRUE) * 100, 1),
    tasa_formal_cal    = round(mean(.data[[COL_CLASE_CAL_EDAD]] == "Formal", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(resumen_edad)

cat("  [✅] Panel final ensamblado\n")


# 🪫 11. GUARDADO: PANEL + CONTRATO ---------------------------------------------
cat("\n── 11. Guardado de outputs ─────────────────────────────────────────\n")

# 11a. Panel final
saveRDS(panel, PATH_PANEL_OUT)
cat("  [✅] Panel guardado:", basename(PATH_PANEL_OUT), "\n")
cat("       ", format(nrow(panel), big.mark = ","), "×", ncol(panel), "\n")

# 11b. Contrato
resumen_desocu_pea <- panel %>%
  filter(tipo_estimacion_pea == "Potencial_desocupado") %>%
  summarise(
    n = n(),
    tasa_formal_youden = mean(.data[[COL_CLASE_PEA]] == "Formal", na.rm = TRUE),
    tasa_formal_cal    = mean(.data[[COL_CLASE_CAL_PEA]] == "Formal", na.rm = TRUE)
  )

resumen_inactivos_edad <- panel %>%
  filter(tipo_estimacion_edad == "Potencial_inactivo") %>%
  summarise(
    n = n(),
    tasa_formal_youden = mean(.data[[COL_CLASE_EDAD]] == "Formal", na.rm = TRUE),
    tasa_formal_cal    = mean(.data[[COL_CLASE_CAL_EDAD]] == "Formal", na.rm = TRUE)
  )

contrato_08 <- list(
  script              = "08_backcasting_GLM.R",
  fecha               = Sys.time(),
  sufijo_modelo       = SUFIJO,

  # Modelo usado
  modelo_origen       = "07b (base, sin interacciones, cv.glmnet binomial)",
  lambda_1se          = cv_fit$lambda.1se,
  umbral_youden       = umbral,
  umbral_calibracion  = umbral_calibracion,

  # Métricas de referencia (07b test)
  auc_roc_test_07b    = 0.8686,
  hl_pvalue_07b       = 0.457,
  pred_fuera_01_07b   = 0.00,

  # Panel
  id_var_usada        = ID_VAR,
  panel_input         = paste0("08_panel_formalidad_", SUFIJO_MODELO_LPM, ".rds"),
  n_panel_total       = nrow(panel),
  n_cols_panel        = ncol(panel),
  nota_estructura     = paste0(
    "Panel ", SUFIJO, " = panel ", SUFIJO_MODELO_LPM, " (90 cols) + 10 cols ", SUFIJO, ". ",
    "tipo_estimacion_pea/edad heredados del LPM (no recreados). ",
    "formalidad_valida heredada del LPM."
  ),

  # Universos
  universo_pea = list(
    n_elegible          = n_elegible_pea,
    n_por_tipo          = table(panel$tipo_estimacion_pea),
    pct_pred_fuera_01   = round(mean(panel[[COL_FLAG_PEA]], na.rm = TRUE) * 100, 4),
    pred_fuera_negativas = sum(panel[[COL_PROB_RAW_PEA]] < 0, na.rm = TRUE),
    pred_fuera_positivas = sum(panel[[COL_PROB_RAW_PEA]] > 1, na.rm = TRUE),
    media_prob_formal   = round(mean(panel[[COL_PROB_PEA]], na.rm = TRUE), 4),
    resumen_tipos       = resumen_pea,
    n_desocupados_pred  = resumen_desocu_pea$n,
    tasa_formal_desocupados_youden = round(resumen_desocu_pea$tasa_formal_youden, 4),
    tasa_formal_desocupados_cal    = round(resumen_desocu_pea$tasa_formal_cal, 4)
  ),

  universo_edad = list(
    edad_min            = EDAD_MIN,
    edad_max            = EDAD_MAX,
    n_elegible          = n_elegible_edad,
    n_por_tipo          = table(panel$tipo_estimacion_edad),
    pct_pred_fuera_01   = round(mean(panel[[COL_FLAG_EDAD]], na.rm = TRUE) * 100, 4),
    pred_fuera_negativas = sum(panel[[COL_PROB_RAW_EDAD]] < 0, na.rm = TRUE),
    pred_fuera_positivas = sum(panel[[COL_PROB_RAW_EDAD]] > 1, na.rm = TRUE),
    media_prob_formal   = round(mean(panel[[COL_PROB_EDAD]], na.rm = TRUE), 4),
    resumen_tipos       = resumen_edad,
    n_inactivos_pred    = resumen_inactivos_edad$n,
    tasa_formal_inactivos_youden = round(resumen_inactivos_edad$tasa_formal_youden, 4),
    tasa_formal_inactivos_cal    = round(resumen_inactivos_edad$tasa_formal_cal, 4)
  ),

  # Unión
  n_union             = n_elegible_union,
  n_solo_pea          = n_solo_pea,
  n_solo_edad         = n_solo_edad,
  n_ambos             = n_ambos,

  # Predicciones globales
  pct_pred_fuera_01_union = pct_fuera,

  # Validación en ventana training
  auc_global_train    = auc_global_train,
  mae_global_train    = mae_global_train,
  accuracy_train      = acc_train,
  f1_train            = f1_train,
  mcc_train           = mcc_train,
  comp_por_trimestre  = comp_por_trimestre,
  delta_max_pp        = delta_max,
  nota_calibracion    = paste0(
    "GLM binomial garantiza pred en [0,1] nativamente (H-L p=0.457 en test). ",
    "Delta max (clasificación Youden) = ", delta_max, " pp. ",
    "Umbral Youden (", round(umbral, 4), ") optimiza sens+esp. ",
    "Umbral calibración (", round(umbral_calibracion, 4), ") iguala tasa predicha ≈ observada. ",
    "A diferencia del LPM, no hay clipping: pred_raw = pred_clip siempre."
  ),

  # Métricas con umbral de calibración
  metricas_calibracion = list(
    umbral       = umbral_calibracion,
    accuracy     = acc_cal,
    sensibilidad = sens_cal,
    especificidad = esp_cal,
    f1           = f1_cal,
    mcc          = mcc_cal,
    delta_max_pp = delta_max_cal,
    comp_por_trimestre = comp_por_trim_cal
  ),

  # Hold-out
  metricas_holdout    = metricas_holdout,

  # Notas metodológicas
  nota_desocupados    = paste0(
    "Desocupados (Potencial_desocupado): el GLM binomial es más apropiado que el LPM ",
    "para este subgrupo porque garantiza probabilidades en [0,1] sin clipping. ",
    "En el LPM, el 81% de desocupados tenía pred < 0 (clipping masivo). ",
    "Con el GLM, las probabilidades son válidas aunque el perfil θ_A sea extremo ",
    "(función logística acota la extrapolación). Variables laborales provienen del ",
    "empleo anterior (coalesce ≤3 años) para desocupados con experiencia reciente. ",
    "Sin experiencia laboral reciente: imputación genérica por step_impute_mode."
  ),
  nota_inactivos      = paste0(
    "Inactivos 18-60 (Potencial_inactivo, solo universo _edad): ",
    "formalidad potencial estructural dado perfil latente (θ) y covariables. ",
    "Variables laborales imputadas por step_impute_mode (moda del training). ",
    "Interpretar con cautela en paper. El GLM produce probabilidades acotadas ",
    "incluso para perfiles extremos, lo que mejora la calidad de la imputación ",
    "respecto al LPM (sin clipping masivo)."
  )
)

saveRDS(contrato_08, PATH_CONTRATO_OUT)
cat("  [✅] Contrato guardado:", basename(PATH_CONTRATO_OUT), "\n")


# 📑 Checklist -----------------------------------------------------------------

end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("📋 CHECKLIST SCRIPT 08 GLM:\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("   [✅] Panel: %s obs (esperado: %s)\n",
            format(nrow(panel), big.mark = ","),
            format(N_PANEL_ORIGINAL, big.mark = ",")))
cat(sprintf("   [✅] Universo PEA: %s obs con predicción\n",
            format(n_elegible_pea, big.mark = ",")))
cat(sprintf("   [✅] Universo Edad 18-60: %s obs con predicción\n",
            format(n_elegible_edad, big.mark = ",")))
cat(sprintf("   [✅] Pred fuera [0,1] (GLM garantiza 0.00%%): %.4f%%\n", pct_fuera))
cat(sprintf("   [✅] AUC global (training): %.4f [referencia test 07b: 0.8686]\n",
            auc_global_train))
cat(sprintf("   [✅] Umbral Youden: %.4f | Umbral calibración: %.4f\n",
            umbral, umbral_calibracion))
cat(sprintf("   [✅] Delta max (Youden): %.2f pp | (Calibración): %.2f pp\n",
            delta_max, delta_max_cal))
if (!is.null(metricas_holdout)) {
  cat(sprintf("   [✅] AUC hold-out: %.4f\n", metricas_holdout$auc))
} else {
  cat("   [ℹ️] Hold-out no disponible\n")
}
cat("───────────────────────────────────────────────────────────────────\n")
cat("   Outputs:\n")
cat("   →", PATH_PANEL_OUT, "\n")
cat("   →", PATH_CONTRATO_OUT, "\n")
cat("───────────────────────────────────────────────────────────────────\n")
cat("   SIGUIENTE: 08b_reporte_backcasting_GLM.R (reporte HTML)\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("   ⏱️  Tiempo total: %.1f segundos\n\n", elapsed))

toc()
