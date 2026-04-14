# =============================================================================
# [EN] 08a_backcasting_LPM.R -- Apply LASSO-LPM to full panel for historical backcasting (PEA + working-age universes)
# INPUTS:  rdos/datos/06_theta_predichos.rds, models/contracts from 07a-07b LPM
# OUTPUTS: rdos/datos/08_panel_formalidad_LPM*.rds, rdos/contratos/08_contrato_backcasting_LPM*.rds
# =============================================================================
# 🌟 08a_backcasting_LPM.R 🌟 ####
# OBJETIVO: Aplicar modelo LASSO-LPM (07b base, sin interacciones) al panel
#           completo (1.79M obs, 2016T4–2025T3) para predecir formalidad donde
#           no está observada. Doble universo de backcasting:
#             _pea  → PEA (Ocupados + Desocupados) con θ
#             _edad → Población 18-60 años con θ (incluye inactivos)
#
# INPUTS:
#   DIR_DATOS     / 06_theta_predichos.rds        (1,795,386 × 77)
#   DIR_MODELOS   / 07_modelo_lasso_{SUFIJO}.rds     (cv.glmnet)
#   DIR_MODELOS   / 07_recipe_lasso_{SUFIJO}.rds     (recipe prepped para bake)
#   DIR_CONTRATOS / 07a_contrato_lasso_{SUFIJO}.rds  (vars seleccionadas)
#   DIR_CONTRATOS / 07b_contrato_postlasso_{SUFIJO}.rds (umbral Youden)
#
# OUTPUTS:
#   DIR_DATOS     / 08_panel_formalidad_{SUFIJO}.rds
#   DIR_CONTRATOS / 08_contrato_backcasting_{SUFIJO}.rds
#
# LECCIONES APLICADAS: 26 (MCC as.numeric), 30 (emparejamiento_selectivo),
#   38 (%||%), 44 (c07b metricas_clf WIDE: $umbral[1]),
#   45 (theta_A/theta_B → theta_A_mA/theta_B_mA para match recipe)
#
# TIEMPO ESTIMADO: ~3-5 min (bake 1.3M obs + predict sparse)


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
tic("Script 08 Backcasting LPM")
start_time <- Sys.time()
cat("═══════════════════════════════════════════════════════════════════\n")
cat("SCRIPT 08 — BACKCASTING DE FORMALIDAD LABORAL (LPM)\n")
cat("Doble universo: PEA (_pea) + Edad 18-60 (_edad)\n")
cat("Inicio:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

set.seed(SEED_GLOBAL)


# 🔑 Paths locales -------------------------------------------------------------
# ── Constantes ────────────────────────────────────────────────────────────────
SUFIJO     <- SUFIJO_MODELO_LPM  # e.g. "LPM4T" — definido en parametros.R
EDAD_MIN   <- 18L
EDAD_MAX   <- 60L
ID_VAR     <- "id_individuo_hist"

NIVELES_VALIDOS <- c("Formal oficial", "Informal oficial")

# Labels que indican "no aplica" en variables laborales — convertir a NA para
# que step_impute_mode del recipe las impute con la moda del training
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

# ── Paths ─────────────────────────────────────────────────────────────────────
PATH_PANEL        <- file.path(DIR_DATOS, "06_theta_predichos.rds")
PATH_MODELO       <- file.path(DIR_MODELOS, paste0("07_modelo_lasso_", SUFIJO, ".rds"))
PATH_RECIPE       <- file.path(DIR_MODELOS, paste0("07_recipe_lasso_", SUFIJO, ".rds"))
PATH_07A_CONTRATO <- file.path(DIR_CONTRATOS, paste0("07a_contrato_lasso_", SUFIJO, ".rds"))
PATH_07B_CONTRATO <- file.path(DIR_CONTRATOS, paste0("07b_contrato_postlasso_", SUFIJO, ".rds"))

PATH_PANEL_OUT    <- file.path(DIR_DATOS, paste0("08_panel_formalidad_", SUFIJO, ".rds"))
PATH_CONTRATO_OUT <- file.path(DIR_CONTRATOS, paste0("08_contrato_backcasting_", SUFIJO, ".rds"))


# 🪫 1. CARGA DE DATOS Y OBJETOS ------------------------------------------------
cat("── 1. Carga de datos y objetos ────────────────────────────────────\n")

hard_stop(file.exists(PATH_PANEL),
          paste("Panel no encontrado:", PATH_PANEL))
hard_stop(file.exists(PATH_MODELO),
          paste("Modelo no encontrado:", PATH_MODELO))
hard_stop(file.exists(PATH_RECIPE),
          paste("Recipe no encontrado:", PATH_RECIPE))
hard_stop(file.exists(PATH_07A_CONTRATO),
          paste("Contrato 07a no encontrado:", PATH_07A_CONTRATO))
hard_stop(file.exists(PATH_07B_CONTRATO),
          paste("Contrato 07b no encontrado:", PATH_07B_CONTRATO))

panel        <- readRDS(PATH_PANEL)
cv_fit       <- readRDS(PATH_MODELO)
recipe_lasso <- readRDS(PATH_RECIPE)
c07a         <- readRDS(PATH_07A_CONTRATO)
c07b         <- readRDS(PATH_07B_CONTRATO)

cat("  Panel cargado:", format(nrow(panel), big.mark = ","), "obs ×", ncol(panel), "vars\n")
cat("  Modelo LASSO: λ.1se =", round(cv_fit$lambda.1se, 6), "\n")
cat("  Vars seleccionadas (c07a):", c07a$n_vars_sel_1se, "\n")

# Extraer umbral Youden — lección 44: c07b$metricas_clf es WIDE
umbral <- c07b$metricas_clf$umbral[1]
cat("  Umbral Youden (c07b):", round(umbral, 4), "\n")
hard_stop(!is.na(umbral) && umbral > 0 && umbral < 1,
          paste("Umbral Youden inválido:", umbral))


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

# Crear formalidad_valida
panel <- panel %>%
  mutate(
    formalidad_valida = if_else(
      formalidad_empleo %in% NIVELES_VALIDOS,
      formalidad_empleo,
      NA_character_
    )
  )

# Verificar periodos con formalidad observada
periodos_con_formalidad <- panel %>%
  filter(!is.na(formalidad_valida)) %>%
  pull(periodo_id) %>% unique() %>% sort()

cat("  Periodos con formalidad observada:", 
    paste(periodos_con_formalidad, collapse = ", "), "\n")
hard_stop(length(periodos_con_formalidad) >= 1,
          "No se encontraron periodos con formalidad observada")

# Usar TRIMESTRES_FORMALIDAD de parametros.R si existe, sino auto-detectar
TRIMESTRES_TRAINING <- if (exists("TRIMESTRES_FORMALIDAD")) {
  cat("  Usando TRIMESTRES_FORMALIDAD de parametros.R:", 
      paste(TRIMESTRES_FORMALIDAD, collapse = ", "), "\n")
  TRIMESTRES_FORMALIDAD
} else {
  cat("  Auto-detectados como TRIMESTRES_TRAINING\n")
  periodos_con_formalidad
}

# Distribución de condicion_actividad
cat("\n  condicion_actividad:\n")
print(table(panel$condicion_actividad, useNA = "ifany"))

# Cobertura θ por condición de actividad
cat("\n  Cobertura θ por condicion_actividad:\n")
panel %>%
  group_by(condicion_actividad) %>%
  summarise(
    n_total = n(),
    n_con_theta = sum(!is.na(theta_A) & !is.na(theta_B)),
    pct_cobertura = round(n_con_theta / n_total * 100, 1),
    .groups = "drop"
  ) %>%
  print()

# Distribución de edad
cat("\n  Rango de edad:", range(panel$edad, na.rm = TRUE), "\n")
cat("  N en rango", EDAD_MIN, "-", EDAD_MAX, ":",
    format(sum(panel$edad >= EDAD_MIN & panel$edad <= EDAD_MAX, na.rm = TRUE), 
           big.mark = ","), "\n")

cat("  [✅] Verificaciones iniciales OK\n")


# 🪫 3. TIPO_ESTIMACION DOBLE ---------------------------------------------------
cat("\n── 3. Construcción de tipo_estimacion (doble universo) ────────────\n")

panel <- panel %>%
  mutate(
    # ── Universo PEA ──
    tipo_estimacion_pea = case_when(
      condicion_actividad %in% c("Inactivo", "Menor", 
                                  "No respuesta", "Ns/Nr") ~ "No_aplica",
      condicion_actividad %in% c("Ocupado", "Desocupado") &
        (is.na(theta_A) | is.na(theta_B))              ~ "Sin_theta",
      condicion_actividad == "Ocupado" &
        !is.na(formalidad_valida)                       ~ "Observado",
      condicion_actividad == "Ocupado" &
        is.na(formalidad_valida)                        ~ "Backcasting",
      condicion_actividad == "Desocupado"               ~ "Potencial_desocupado",
      TRUE                                              ~ NA_character_
    ),
    # ── Universo Edad 18-60 ──
    tipo_estimacion_edad = case_when(
      edad < EDAD_MIN | edad > EDAD_MAX                 ~ "No_aplica",
      condicion_actividad %in% c("No respuesta", "Ns/Nr") ~ "No_aplica",
      is.na(theta_A) | is.na(theta_B)                   ~ "Sin_theta",
      condicion_actividad == "Ocupado" &
        !is.na(formalidad_valida)                        ~ "Observado",
      condicion_actividad == "Ocupado" &
        is.na(formalidad_valida)                         ~ "Backcasting",
      condicion_actividad == "Desocupado"                ~ "Potencial_desocupado",
      condicion_actividad == "Inactivo"                  ~ "Potencial_inactivo",
      condicion_actividad == "Menor"                     ~ "No_aplica",
      TRUE                                               ~ NA_character_
    )
  )

cat("  tipo_estimacion_pea:\n")
print(table(panel$tipo_estimacion_pea, useNA = "ifany"))
cat("\n  tipo_estimacion_edad:\n")
print(table(panel$tipo_estimacion_edad, useNA = "ifany"))

# Verificar: no debería haber NAs en ningún tipo_estimacion
n_na_pea  <- sum(is.na(panel$tipo_estimacion_pea))
n_na_edad <- sum(is.na(panel$tipo_estimacion_edad))
if (n_na_pea > 0)  cat("  ⚠️ WARN:", n_na_pea, "NAs en tipo_estimacion_pea\n")
if (n_na_edad > 0) cat("  ⚠️ WARN:", n_na_edad, "NAs en tipo_estimacion_edad\n")

cat("  [✅] tipo_estimacion construidos\n")


# 🪫 4. UNIVERSO UNIÓN ----------------------------------------------------------
cat("\n── 4. Construcción del universo unión ──────────────────────────────\n")

# Tipos elegibles por universo
TIPOS_ELEGIBLES_PEA  <- c("Observado", "Backcasting", "Potencial_desocupado")
TIPOS_ELEGIBLES_EDAD <- c("Observado", "Backcasting", "Potencial_desocupado", 
                           "Potencial_inactivo")

# Flags de elegibilidad
panel <- panel %>%
  mutate(
    elegible_pea  = tipo_estimacion_pea  %in% TIPOS_ELEGIBLES_PEA,
    elegible_edad = tipo_estimacion_edad %in% TIPOS_ELEGIBLES_EDAD,
    elegible_union = elegible_pea | elegible_edad
  )

n_elegible_pea   <- sum(panel$elegible_pea)
n_elegible_edad  <- sum(panel$elegible_edad)
n_elegible_union <- sum(panel$elegible_union)
n_solo_pea       <- sum(panel$elegible_pea & !panel$elegible_edad)
n_solo_edad      <- sum(!panel$elegible_pea & panel$elegible_edad)
n_ambos          <- sum(panel$elegible_pea & panel$elegible_edad)

cat("  Elegibles PEA:        ", format(n_elegible_pea, big.mark = ","), "\n")
cat("  Elegibles Edad 18-60: ", format(n_elegible_edad, big.mark = ","), "\n")
cat("  Unión:                ", format(n_elegible_union, big.mark = ","), "\n")
cat("    Solo PEA:           ", format(n_solo_pea, big.mark = ","),
    "(PEA fuera de 18-60)\n")
cat("    Solo Edad:          ", format(n_solo_edad, big.mark = ","),
    "(Inactivos 18-60 con θ)\n")
cat("    Ambos:              ", format(n_ambos, big.mark = ","), "\n")

# Guardar posiciones para re-armado posterior (más seguro que join)
idx_union <- which(panel$elegible_union)

# Extraer subset del universo unión
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

# 5b. Preparar features: excluir variables que no son predictores
vars_excluir <- c("pondera", "formalidad_empleo", "formalidad_valida", "codusu",
                   # IDs y variables auxiliares
                   ID_VAR, "id_individuo", "periodo_id", "anio",
                   "periodo_num", "condicion_actividad",
                   # NOTA: trimestre NO se excluye — es regresor estacional en 07a
                   # Columnas creadas en este script
                   "tipo_estimacion_pea", "tipo_estimacion_edad",
                   "elegible_pea", "elegible_edad", "elegible_union")

# Solo excluir las que existen
vars_excluir_presentes <- intersect(vars_excluir, names(universo_union))
features_union <- universo_union %>%
  select(-all_of(vars_excluir_presentes))

# 5c. Renombrar theta_A/theta_B → theta_A_mA/theta_B_mA
# El panel tiene theta_A y theta_B (nombres genéricos del merge en 06a).
# El recipe fue entrenado con theta_A_mA y theta_B_mA (sufijo _mA = modelo A
# del heterofactor). bake() exige los nombres exactos del training.
if ("theta_A" %in% names(features_union) && !"theta_A_mA" %in% names(features_union)) {
  features_union <- features_union %>%
    rename(theta_A_mA = theta_A, theta_B_mA = theta_B)
  cat("  theta_A → theta_A_mA, theta_B → theta_B_mA (match recipe)\n")
}

# 5e. Convertir labels "No aplica" a NA para que recipe los impute
# Necesario para inactivos en universo edad que tienen labels especiales
# en variables laborales (seccion, calificacion, etc.)
n_converted <- 0L
features_union <- features_union %>%
  mutate(across(where(is.character), function(x) {
    mask <- x %in% LABELS_NO_APLICA
    n_converted <<- n_converted + sum(mask, na.rm = TRUE)
    if_else(mask, NA_character_, x)
  }))
cat("  Labels 'No aplica' convertidas a NA:", format(n_converted, big.mark = ","), "\n")

# 5f. Agregar formalidad_bin = NA (el recipe la referencia como outcome)
features_union <- features_union %>%
  mutate(formalidad_bin = NA_real_)

# 5g. Verificar que las variables del recipe están presentes
vars_recipe <- recipe_lasso$var_info$variable
vars_faltantes_recipe <- setdiff(vars_recipe, names(features_union))
if (length(vars_faltantes_recipe) > 0) {
  cat("  ⚠️ Variables esperadas por recipe NO encontradas en features:\n")
  cat("    ", paste(vars_faltantes_recipe, collapse = ", "), "\n")
  cat("    Esto puede causar errores en bake(). Revisar pipeline.\n")
}

vars_extra <- setdiff(names(features_union), vars_recipe)
if (length(vars_extra) > 0) {
  cat("  Variables extra (no en recipe, se ignoran):", length(vars_extra), "\n")
  # Seleccionar solo las que el recipe conoce para evitar warnings
  features_union <- features_union %>% select(any_of(vars_recipe))
}

cat("  Dimensiones features para bake:", nrow(features_union), "×", ncol(features_union), "\n")

# 5h. BAKE
cat("  Ejecutando bake()...\n")
tic("bake")
baked_union <- bake(recipe_lasso, new_data = features_union)
toc()

# Eliminar formalidad_bin del resultado (es outcome, no predictor)
if ("formalidad_bin" %in% names(baked_union)) {
  baked_union <- baked_union %>% select(-formalidad_bin)
}

cat("  Dimensiones post-bake:", nrow(baked_union), "×", ncol(baked_union), "\n")

# Verificar NAs post-bake (no debería haber: recipe imputa)
n_na_post_bake <- sum(is.na(baked_union))
if (n_na_post_bake > 0) {
  cat("  ⚠️ WARN:", format(n_na_post_bake, big.mark = ","), 
      "NAs post-bake. Columnas con NAs:\n")
  na_por_col <- colSums(is.na(baked_union))
  print(na_por_col[na_por_col > 0])
}

cat("  [✅] Bake completado\n")


# 🪫 6. PREDICT -----------------------------------------------------------------
cat("\n── 6. Predicción (glmnet) ──────────────────────────────────────────\n")

# 6a. Convertir a sparse matrix (dgCMatrix) — eficiente para 1.3M obs
cat("  Convirtiendo a sparse matrix...\n")
tic("sparse matrix")
X_union <- Matrix(as.matrix(baked_union), sparse = TRUE)
toc()
cat("  Dimensiones X_union:", nrow(X_union), "×", ncol(X_union), "\n")

# Liberar memoria del tibble baked
rm(baked_union, features_union); gc(verbose = FALSE)

# 6b. Predict con cv.glmnet directo (NO tidymodels predict)
cat("  Ejecutando predict(cv_fit, s = 'lambda.1se')...\n")
tic("predict")
pred_raw <- as.vector(predict(cv_fit, newx = X_union, s = "lambda.1se"))
toc()

# Liberar matrix
rm(X_union); gc(verbose = FALSE)

cat("  Predicciones generadas:", format(length(pred_raw), big.mark = ","), "\n")
hard_stop(length(pred_raw) == nrow(universo_union),
          "Longitud de predicciones != N universo unión")

# 6c. Clip [0,1] + clasificar
pred_clip <- pmax(0, pmin(1, pred_raw))
pred_clase <- if_else(pred_clip >= umbral, "Formal", "Informal")
flag_fuera <- pred_raw < 0 | pred_raw > 1

n_fuera      <- sum(flag_fuera)
n_fuera_neg  <- sum(pred_raw < 0)
n_fuera_pos  <- sum(pred_raw > 1)
pct_fuera    <- round(mean(flag_fuera) * 100, 2)

cat("  Pred. fuera de [0,1]:", format(n_fuera, big.mark = ","),
    sprintf("(%.2f%%)", pct_fuera), "\n")
cat("    Negativas:", format(n_fuera_neg, big.mark = ","),
    " | Positivas:", format(n_fuera_pos, big.mark = ","), "\n")
cat("  Media prob_formal (clipeada):", round(mean(pred_clip), 4), "\n")
cat("  Umbral Youden:", round(umbral, 4), "\n")
cat("  Tasa formal predicha (unión):", 
    round(mean(pred_clase == "Formal") * 100, 2), "%\n")

# Diagnóstico: pred fuera [0,1] por tipo de estimación
cat("\n  Pred fuera [0,1] por tipo (PEA):\n")
tibble(
  tipo = id_cols_union$tipo_estimacion_pea,
  fuera = flag_fuera
) %>%
  group_by(tipo) %>%
  summarise(n = n(), pct_fuera = round(mean(fuera) * 100, 2), .groups = "drop") %>%
  filter(!is.na(tipo)) %>%
  print()

cat("  [✅] Predicción completada\n")


# 🪫 7. ASIGNACIÓN DOBLE — SPLIT _pea / _edad -----------------------------------
cat("\n── 7. Asignación doble de predicciones ─────────────────────────────\n")

# Construir tibble de predicciones para el universo unión
pred_union <- tibble(
  prob_raw   = pred_raw,
  prob_clip  = pred_clip,
  clase      = pred_clase,
  flag_fuera = flag_fuera
)

# Flags de elegibilidad para el universo unión (mismas posiciones)
en_pea  <- id_cols_union$elegible_pea
en_edad <- id_cols_union$elegible_edad

# Asignar NA donde la obs no pertenece al universo correspondiente
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

# Reporte rápido por universo
cat("  Universo PEA:\n")
cat("    N con predicción:", format(sum(en_pea), big.mark = ","), "\n")
cat("    Tasa formal:", round(mean(pred_union[[COL_CLASE_PEA]] == "Formal",
                                    na.rm = TRUE) * 100, 2), "%\n")
cat("    Pred fuera [0,1]:", round(mean(pred_union[[COL_FLAG_PEA]],
                                         na.rm = TRUE) * 100, 2), "%\n")

cat("  Universo Edad 18-60:\n")
cat("    N con predicción:", format(sum(en_edad), big.mark = ","), "\n")
cat("    Tasa formal:", round(mean(pred_union[[COL_CLASE_EDAD]] == "Formal",
                                    na.rm = TRUE) * 100, 2), "%\n")
cat("    Pred fuera [0,1]:", round(mean(pred_union[[COL_FLAG_EDAD]],
                                         na.rm = TRUE) * 100, 2), "%\n")

cat("  [✅] Asignación doble completada\n")


# 🪫 8. VALIDACIÓN EN VENTANA DE TRAINING ---------------------------------------
cat("\n── 8. Validación en ventana de training ────────────────────────────\n")

# Los Observados son idénticos en ambos universos (Ocupados con formalidad válida)
# La validación se hace una sola vez sobre todos los Observados de la unión
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

# 8b. MAE global
mae_global_train <- mean(abs(comp_training$pred_prob - comp_training$y_obs))
cat("  MAE global (training):", round(mae_global_train, 4), "\n")

# 8c. Comparación tasa obs vs pred por trimestre
# Usar tasa de clasificación (pred >= umbral), no prob media — consistente con LEGACY
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
cat("  (tasa_pred_cl = clasificación con umbral Youden; tasa_pred_pr = prob media)\n")
print(comp_por_trimestre, n = Inf)

delta_max <- max(abs(comp_por_trimestre$delta_cl_pp))
if (delta_max < 6) {
  cat("  [✅] Delta máximo:", delta_max, "pp < 6 pp → calibración aceptable para LPM\n")
  if (delta_max > 2) {
    cat("       NOTA: delta > 2 pp. Documentar en paper como limitación LPM.\n")
    cat("       Umbral Youden optimiza sens+esp, no calibración de tasas.\n")
  }
} else {
  cat("  [⚠️] Delta máximo:", delta_max, "pp ≥ 6 pp → revisar calibración\n")
}

# 8d. Tabla de confusión ponderada
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

# MCC — lección 26: as.numeric() en todos los términos
mcc_train  <- (as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)) /
  sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) *
         as.numeric(tn + fp) * as.numeric(tn + fn))

cat(sprintf("  Accuracy: %.4f | Sens: %.4f | Esp: %.4f | F1: %.4f | MCC: %.4f\n",
            acc_train, sens_train, esp_train, f1_train, mcc_train))

# 8e. Umbral de calibración (iguala tasa predicha ≈ tasa observada)
# El umbral Youden maximiza sens+esp pero sub-clasifica ~5 pp.
# Para series temporales de backcasting, interesa calibración de tasas.
tasa_obs_global <- weighted.mean(comp_training$y_obs, comp_training$pondera)
umbrales_grid <- seq(0.30, 0.70, by = 0.005)
tasas_pred_grid <- sapply(umbrales_grid, function(u) {
  weighted.mean(as.integer(comp_training$pred_prob >= u), comp_training$pondera)
})
umbral_calibracion <- umbrales_grid[which.min(abs(tasas_pred_grid - tasa_obs_global))]
tasa_pred_cal <- tasas_pred_grid[which.min(abs(tasas_pred_grid - tasa_obs_global))]

cat(sprintf("\n  Umbral de calibración: %.4f (iguala tasa obs %.2f%% ≈ pred %.2f%%)\n",
            umbral_calibracion, tasa_obs_global * 100, tasa_pred_cal * 100))

# Métricas con umbral de calibración
pred_cal_obs <- as.integer(comp_training$pred_prob >= umbral_calibracion)
cm_cal <- table(Real = comp_training$y_obs, Predicho = pred_cal_obs)
tp_c <- as.numeric(cm_cal[2,2]); tn_c <- as.numeric(cm_cal[1,1])
fp_c <- as.numeric(cm_cal[1,2]); fn_c <- as.numeric(cm_cal[2,1])
acc_cal  <- (tp_c + tn_c) / sum(cm_cal)
sens_cal <- tp_c / (tp_c + fn_c)
esp_cal  <- tn_c / (tn_c + fp_c)
f1_cal   <- 2 * tp_c / (2 * tp_c + fp_c + fn_c)
mcc_cal  <- (as.numeric(tp_c) * as.numeric(tn_c) - as.numeric(fp_c) * as.numeric(fn_c)) /
  sqrt(as.numeric(tp_c + fp_c) * as.numeric(tp_c + fn_c) *
         as.numeric(tn_c + fp_c) * as.numeric(tn_c + fn_c))

cat(sprintf("  Métricas umbral calibración: Acc %.4f | Sens %.4f | Esp %.4f | F1 %.4f | MCC %.4f\n",
            acc_cal, sens_cal, esp_cal, f1_cal, mcc_cal))

# Comparación tasa obs vs pred con umbral de calibración por trimestre
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

# Buscar obs con formalidad observada que no están en TRIMESTRES_TRAINING
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

# Seleccionar solo las columnas nuevas del pred_union
cols_nuevas <- pred_union %>%
  select(starts_with("prob_formal_"), starts_with("formalidad_clase_"), 
         starts_with("flag_pred_"))

# Inicializar columnas nuevas en el panel completo como NA
panel[[COL_PROB_RAW_PEA]]   <- NA_real_
panel[[COL_PROB_PEA]]       <- NA_real_
panel[[COL_CLASE_PEA]]      <- NA_character_
panel[[COL_FLAG_PEA]]       <- NA
panel[[COL_PROB_RAW_EDAD]]  <- NA_real_
panel[[COL_PROB_EDAD]]      <- NA_real_
panel[[COL_CLASE_EDAD]]     <- NA_character_
panel[[COL_FLAG_EDAD]]      <- NA

# Asignar por posición (idx_union mapea panel → universo_union)
panel[[COL_PROB_RAW_PEA]][idx_union]  <- cols_nuevas[[COL_PROB_RAW_PEA]]
panel[[COL_PROB_PEA]][idx_union]      <- cols_nuevas[[COL_PROB_PEA]]
panel[[COL_CLASE_PEA]][idx_union]     <- cols_nuevas[[COL_CLASE_PEA]]
panel[[COL_FLAG_PEA]][idx_union]      <- cols_nuevas[[COL_FLAG_PEA]]
panel[[COL_PROB_RAW_EDAD]][idx_union] <- cols_nuevas[[COL_PROB_RAW_EDAD]]
panel[[COL_PROB_EDAD]][idx_union]     <- cols_nuevas[[COL_PROB_EDAD]]
panel[[COL_CLASE_EDAD]][idx_union]    <- cols_nuevas[[COL_CLASE_EDAD]]
panel[[COL_FLAG_EDAD]][idx_union]     <- cols_nuevas[[COL_FLAG_EDAD]]

# Clasificación alternativa con umbral de calibración (iguala tasa obs)
# Para series temporales de backcasting, esta clasificación es más apropiada
# que la de Youden (que optimiza sens+esp, no calibración de tasas)
clase_cal_raw <- if_else(pred_clip >= umbral_calibracion, "Formal", "Informal")

panel[[COL_CLASE_CAL_PEA]]  <- NA_character_
panel[[COL_CLASE_CAL_EDAD]] <- NA_character_
panel[[COL_CLASE_CAL_PEA]][idx_union]  <- if_else(en_pea, clase_cal_raw, NA_character_)
panel[[COL_CLASE_CAL_EDAD]][idx_union] <- if_else(en_edad, clase_cal_raw, NA_character_)

cat("  Clasificación con umbral calibración (", round(umbral_calibracion, 4), 
    ") agregada\n", sep = "")

# Eliminar columnas auxiliares de flags (no necesarias en output final)
panel <- panel %>%
  select(-elegible_pea, -elegible_edad, -elegible_union)

cat("  Panel final:", format(nrow(panel), big.mark = ","), "×", ncol(panel), "\n")

# Verificar: N total no cambió
hard_stop(nrow(panel) == N_PANEL_ORIGINAL,
          paste("Panel final tiene", nrow(panel), "obs — esperadas", N_PANEL_ORIGINAL))

# Verificar: no hay NAs inesperados en PEA con θ
n_na_prob_pea <- sum(is.na(panel[[COL_PROB_PEA]]) &
                       panel$tipo_estimacion_pea %in% TIPOS_ELEGIBLES_PEA)
n_na_prob_edad <- sum(is.na(panel[[COL_PROB_EDAD]]) &
                        panel$tipo_estimacion_edad %in% TIPOS_ELEGIBLES_EDAD)

if (n_na_prob_pea > 0)  cat("  ⚠️ WARN:", n_na_prob_pea, "NAs en", COL_PROB_PEA, "para PEA elegible\n")
if (n_na_prob_edad > 0) cat("  ⚠️ WARN:", n_na_prob_edad, "NAs en", COL_PROB_EDAD, "para Edad elegible\n")

# Distribución final por tipo_estimacion (PEA)
cat("\n  Distribución final — Universo PEA:\n")
resumen_pea <- panel %>%
  group_by(tipo_estimacion_pea) %>%
  summarise(
    n = n(),
    pct = round(n / nrow(panel) * 100, 2),
    n_con_pred = sum(!is.na(.data[[COL_PROB_PEA]])),
    tasa_formal_youden = round(mean(.data[[COL_CLASE_PEA]] == "Formal", na.rm = TRUE) * 100, 1),
    tasa_formal_cal = round(mean(.data[[COL_CLASE_CAL_PEA]] == "Formal", na.rm = TRUE) * 100, 1),
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
    tasa_formal_cal = round(mean(.data[[COL_CLASE_CAL_EDAD]] == "Formal", na.rm = TRUE) * 100, 1),
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
# Resumen desocupados (formalidad potencial)
resumen_desocu_pea <- panel %>%
  filter(tipo_estimacion_pea == "Potencial_desocupado") %>%
  summarise(
    n = n(),
    tasa_formal_youden = mean(.data[[COL_CLASE_PEA]] == "Formal", na.rm = TRUE),
    tasa_formal_cal = mean(.data[[COL_CLASE_CAL_PEA]] == "Formal", na.rm = TRUE)
  )

# Resumen inactivos (solo universo edad)
resumen_inactivos_edad <- panel %>%
  filter(tipo_estimacion_edad == "Potencial_inactivo") %>%
  summarise(
    n = n(),
    tasa_formal_youden = mean(.data[[COL_CLASE_EDAD]] == "Formal", na.rm = TRUE),
    tasa_formal_cal = mean(.data[[COL_CLASE_CAL_EDAD]] == "Formal", na.rm = TRUE)
  )

contrato_08 <- list(
  script              = "08_backcasting_LPM.R",
  fecha               = Sys.time(),
  sufijo_modelo       = SUFIJO,
  
  # Modelo usado
  modelo_origen       = "07b (base, sin interacciones)",
  lambda_1se          = cv_fit$lambda.1se,
  umbral_youden       = umbral,
  umbral_calibracion  = umbral_calibracion,
  
  # Panel
  id_var_usada        = ID_VAR,
  n_panel_total       = nrow(panel),
  n_cols_panel        = ncol(panel),
  
  # Universos
  universo_pea = list(
    n_elegible          = n_elegible_pea,
    n_por_tipo          = table(panel$tipo_estimacion_pea),
    pct_pred_fuera_01   = round(mean(panel[[COL_FLAG_PEA]], na.rm = TRUE) * 100, 2),
    pred_fuera_negativas = sum(panel[[COL_PROB_RAW_PEA]] < 0, na.rm = TRUE),
    pred_fuera_positivas = sum(panel[[COL_PROB_RAW_PEA]] > 1, na.rm = TRUE),
    media_prob_formal   = round(mean(panel[[COL_PROB_PEA]], na.rm = TRUE), 4),
    resumen_tipos       = resumen_pea,
    n_desocupados_pred  = resumen_desocu_pea$n,
    tasa_formal_desocupados_youden = round(resumen_desocu_pea$tasa_formal_youden, 4),
    tasa_formal_desocupados_cal = round(resumen_desocu_pea$tasa_formal_cal, 4)
  ),
  
  universo_edad = list(
    edad_min            = EDAD_MIN,
    edad_max            = EDAD_MAX,
    n_elegible          = n_elegible_edad,
    n_por_tipo          = table(panel$tipo_estimacion_edad),
    pct_pred_fuera_01   = round(mean(panel[[COL_FLAG_EDAD]], na.rm = TRUE) * 100, 2),
    pred_fuera_negativas = sum(panel[[COL_PROB_RAW_EDAD]] < 0, na.rm = TRUE),
    pred_fuera_positivas = sum(panel[[COL_PROB_RAW_EDAD]] > 1, na.rm = TRUE),
    media_prob_formal   = round(mean(panel[[COL_PROB_EDAD]], na.rm = TRUE), 4),
    resumen_tipos       = resumen_edad,
    n_inactivos_pred    = resumen_inactivos_edad$n,
    tasa_formal_inactivos_youden = round(resumen_inactivos_edad$tasa_formal_youden, 4),
    tasa_formal_inactivos_cal = round(resumen_inactivos_edad$tasa_formal_cal, 4)
  ),
  
  # Unión (para eficiencia del cómputo)
  n_union             = n_elegible_union,
  n_solo_pea          = n_solo_pea,
  n_solo_edad         = n_solo_edad,
  n_ambos             = n_ambos,
  
  # Predicciones globales (unión)
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
    "Delta max (clasificación Youden) = ", delta_max, " pp. ",
    "Umbral Youden (", round(umbral, 4), ") optimiza sens+esp, no calibración de tasas. ",
    "Umbral calibración (", round(umbral_calibracion, 4), ") iguala tasa predicha ≈ observada. ",
    "Limitación conocida del LPM. Ref: Angrist & Pischke (2009)."
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
    "Desocupados (Potencial_desocupado): 81% pred fuera de [0,1]. ",
    "Causa: θ_A medio = 0.99 (vs 0.14 en Observados), con β(θ_A) < 0 → ",
    "extrapolación lineal produce pred ≪ 0. Variables laborales (seccion, ",
    "calificacion, categoria_ocupacional, antiguedad) provienen del empleo ",
    "anterior (coalesce, ≤3 años) para desocupados con experiencia reciente. ",
    "Solo desocupados sin experiencia laboral reciente reciben imputación ",
    "genérica (moda del training). Subgrupo no cubierto relevante: ",
    "desocupados jóvenes en búsqueda de primer empleo. ",
    "Tasa formal potencial (6.8% Youden) es cota inferior conservadora ",
    "por clipping masivo. El GLM (acotado en [0,1]) es más apropiado para ",
    "este subgrupo."
  ),
  nota_inactivos      = paste0(
    "Inactivos 18-60 (Potencial_inactivo, solo universo _edad): ",
    "formalidad potencial estructural dado perfil latente (θ) y covariables. ",
    "Variables laborales no disponibles para inactivos (nunca fueron PEA o ",
    "sin empleo reciente) → imputadas con moda del training por ",
    "step_impute_mode (Comercio/Operativo/Empleado/antigüedad 0). ",
    "Interpretar con cautela en paper. La tasa formal potencial inactivos ",
    "refleja principalmente el perfil θ + educación + demografía."
  )
)

saveRDS(contrato_08, PATH_CONTRATO_OUT)
cat("  [✅] Contrato guardado:", basename(PATH_CONTRATO_OUT), "\n")


# 📑 Checklist -----------------------------------------------------------------

end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("📋 CHECKLIST SCRIPT 08:\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("   [✅] Panel: %s obs (esperado: %s)\n",
            format(nrow(panel), big.mark = ","),
            format(N_PANEL_ORIGINAL, big.mark = ",")))
cat(sprintf("   [✅] Universo PEA: %s obs con predicción\n",
            format(n_elegible_pea, big.mark = ",")))
cat(sprintf("   [✅] Universo Edad 18-60: %s obs con predicción\n",
            format(n_elegible_edad, big.mark = ",")))
cat(sprintf("   [✅] Pred fuera [0,1] (unión): %.2f%%\n", pct_fuera))
cat(sprintf("   [✅] AUC global (training): %.4f\n", auc_global_train))
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
cat("   SIGUIENTE: 08b_reporte_backcasting_LPM.R (reporte HTML)\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("   ⏱️  Tiempo total: %.1f segundos\n\n", elapsed))

toc()
