# =============================================================================
# [EN] 08_backcasting_SLS.R -- Apply post-LASSO SLS to full panel for historical backcasting (PEA + working-age universes)
# INPUTS:  rdos/datos/08_panel_formalidad_GLM*.rds, models/contracts from 07a-07b SLS
# OUTPUTS: rdos/datos/08_panel_formalidad_SLS*.rds, rdos/contratos/08_contrato_backcasting_SLS*.rds
# =============================================================================
# 🌟 08_backcasting_SLS.R 🌟 ####
# OBJETIVO: Aplicar modelo post-LASSO SLS (07b base, sin interacciones) al panel
#           completo (1.79M obs, 2016T4–2025T3) para predecir formalidad donde
#           no está observada. Doble universo de backcasting:
#             _pea  → PEA (Ocupados + Desocupados) con θ
#             _edad → Población 18-60 años con θ (incluye inactivos)
#
# INPUTS:
#   DIR_DATOS     / 08_panel_formalidad_{GLM}.rds     ← panel acumulado (100 cols)
#   DIR_MODELOS   / 07b_postlasso_{SLS}.rds           ← objeto lm (NO cv.glmnet)
#   DIR_MODELOS   / 07_recipe_lasso_{SLS}.rds         ← recipe prepped (family gaussian)
#   DIR_CONTRATOS / 07a_contrato_lasso_{SLS}.rds
#   DIR_CONTRATOS / 07b_contrato_postlasso_{SLS}.rds
#
# OUTPUTS:
#   DIR_DATOS     / 08_panel_formalidad_{SLS}.rds     ← ~110 cols (100 + 10 SLS)
#   DIR_CONTRATOS / 08_contrato_backcasting_{SLS}.rds
#   (Sufijos dinámicos vía SUFIJO_MODELO_* de parametros.R)
#
# LECCIONES APLICADAS:
#   L44  — c07b$metricas_clf WIDE: $umbral[1]
#   L45  — theta_A/theta_B → theta_A_mA/theta_B_mA para match recipe
#   L58  — MCC con N > 50,000: as.numeric() en tp/tn/fp/fn
#   L64  — Hardcode como fuente primaria; tryCatch para contrato (informativo)
#   L67  — Modelo SLS = objeto lm. predict(lm_obj, newdata=...) sin type=
#   L73  — Clipping OBLIGATORIO: pmax(0, pmin(1, pred)) en backcasting SLS
#   LN1  — tipo_estimacion_* heredados del LPM — NO recrear; hard_stop si faltan
#   LN4  — Sección fuera [0,1] ACTIVA en SLS: 0% en κ̂γ / ~21% esperado OOS
#
# DIFERENCIAS CLAVE vs LPM:
#   - Panel entrada: 08_panel_formalidad_{GLM}.rds (no 06_theta_predichos.rds)
#   - Objeto modelo: lm (no cv.glmnet) → predict.lm(), no predict.glmnet()
#   - Sin sparse matrix → predict.lm() recibe data.frame explícito
#   - Sección [3] eliminada → tipo_estimacion heredados; solo verificación
#   - flag_pred_{SLS}_*: TRUE donde raw ∉ [0,1] (≠ GLM donde siempre FALSE)
#   - Benchmark pred fuera [0,1]: ~21% (test SLS) → no es error
#
# DIFERENCIAS CLAVE vs GLM:
#   - Clipping necesario (GLM garantiza [0,1]; SLS no garantiza OOS)
#   - flag_pred activo (GLM siempre FALSE; SLS TRUE donde clip actúa)
#   - predict() sin type= (GLM usa type="response"; SLS usa lm estándar)
#
# TIEMPO ESTIMADO: ~3-5 min (bake 1.3M obs + predict.lm vectorizado)


# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(pROC)
  library(tictoc)
})


# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b


# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 08 Backcasting SLS")
start_time <- Sys.time()
cat("═══════════════════════════════════════════════════════════════════\n")
cat("SCRIPT 08 — BACKCASTING DE FORMALIDAD LABORAL (SLS)\n")
cat("Modelo: post-LASSO Sequential Least Squares (Horrace & Oaxaca 2003)\n")
cat("Doble universo: PEA (_pea) + Edad 18-60 (_edad)\n")
cat("Inicio:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

set.seed(SEED_GLOBAL)


# 🔑 Paths locales -------------------------------------------------------------
# ── Constantes ────────────────────────────────────────────────────────────────
SUFIJO     <- SUFIJO_MODELO_SLS   # dinámico vía parametros.R (e.g., "SLS4T")
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

# Columnas de modelos previos esperadas en el panel acumulado
COL_LPM_PROB_PEA      <- paste0("prob_formal_", SUFIJO_MODELO_LPM, "_pea")
COL_LPM_CLASE_PEA     <- paste0("formalidad_clase_", SUFIJO_MODELO_LPM, "_pea")
COL_LPM_CLASE_CAL_PEA <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_LPM, "_pea")
COL_GLM_PROB_PEA      <- paste0("prob_formal_", SUFIJO_MODELO_GLM, "_pea")
COL_GLM_CLASE_PEA     <- paste0("formalidad_clase_", SUFIJO_MODELO_GLM, "_pea")
COL_GLM_CLASE_CAL_PEA <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_GLM, "_pea")

# ── Valores de referencia — leídos desde contratos después de la carga [sección 1]
# Las variables _HC se asignan abajo, post-readRDS, para que el contrato sea
# la fuente primaria (no un fallback). Ver sección [1] más abajo.

TIPOS_ELEGIBLES_PEA  <- c("Observado", "Backcasting", "Potencial_desocupado")
TIPOS_ELEGIBLES_EDAD <- c("Observado", "Backcasting", "Potencial_desocupado",
                           "Potencial_inactivo")

# ── Paths ─────────────────────────────────────────────────────────────────────
# Panel de entrada: panel acumulado con columnas LPM + GLM ya agregadas
PATH_PANEL        <- PATH_08_PANEL_GLM     # dinámico vía parametros.R

PATH_MODELO       <- PATH_07_POSTLASSO_SLS  # objeto lm post-LASSO SLS
PATH_RECIPE       <- PATH_07_RECIPE_SLS     # recipe prepped (family gaussian)
PATH_07A_CONTRATO <- PATH_07A_CONTRATO_SLS  # contrato 07a SLS
PATH_07B_CONTRATO <- PATH_07B_CONTRATO_SLS  # contrato 07b SLS

PATH_PANEL_OUT    <- PATH_08_PANEL_SLS      # panel output con columnas SLS
PATH_CONTRATO_OUT <- PATH_08_CONTRATO_SLS   # contrato backcasting SLS


# 🪫 1. CARGA DE DATOS Y OBJETOS ------------------------------------------------
cat("── 1. Carga de datos y objetos ────────────────────────────────────\n")

hard_stop(file.exists(PATH_PANEL),
          paste("Panel GLM no encontrado:", PATH_PANEL))
hard_stop(file.exists(PATH_MODELO),
          paste("Modelo SLS (lm) no encontrado:", PATH_MODELO))
hard_stop(file.exists(PATH_RECIPE),
          paste("Recipe SLS no encontrado:", PATH_RECIPE))
hard_stop(file.exists(PATH_07A_CONTRATO),
          paste("Contrato 07a SLS no encontrado:", PATH_07A_CONTRATO))
hard_stop(file.exists(PATH_07B_CONTRATO),
          paste("Contrato 07b SLS no encontrado:", PATH_07B_CONTRATO))

panel        <- readRDS(PATH_PANEL)
lm_obj       <- readRDS(PATH_MODELO)     # [L67] objeto lm, NO cv.glmnet
recipe_lasso <- readRDS(PATH_RECIPE)
c07a         <- readRDS(PATH_07A_CONTRATO)
c07b         <- readRDS(PATH_07B_CONTRATO)

cat("  Panel GLM cargado:", format(nrow(panel), big.mark = ","), "obs ×",
    ncol(panel), "vars\n")

# Verificar que el objeto cargado es efectivamente un lm
hard_stop(inherits(lm_obj, "lm"),
          paste("PATH_07_POSTLASSO_SLS no contiene un objeto lm. Clase:",
                paste(class(lm_obj), collapse = "/")))
cat("  Modelo SLS: objeto lm ✓ | N coefs =", length(coef(lm_obj)), "\n")
cat("  Recipe SLS: family gaussian (igual que LPM) ✓\n")
cat("  Vars seleccionadas (c07a):", c07a$n_vars_sel_1se %||% "N/D", "\n")

# ── Valores de referencia desde contratos (fuente primaria) ─────────────────
.sc08 <- function(fn, fb = NA_real_) {
  v <- tryCatch(fn(), error = function(e) fb)
  if (is.null(v) || length(v) == 0) return(fb)
  suppressWarnings(as.numeric(v[[1]]))
}
.mc08 <- function(obj, col) .sc08(function() {
  mc <- obj$metricas_clf
  if (is.null(mc) || nrow(mc) == 0) return(NA_real_)
  cn <- col[col %in% names(mc)][1]; if (is.na(cn)) return(NA_real_)
  mc[[cn]][1]
})

UMBRAL_YOUDEN_HC  <- .mc08(c07b, c("umbral", "umbral_youden"))
AUC_ROC_HC        <- .sc08(function() c07b$auc_roc)
F1_HC             <- .mc08(c07b, c("f1", "F1"))
MCC_HC            <- .mc08(c07b, c("mcc", "MCC"))
PCT_FUERA_TEST_HC <- .sc08(function() c07b$pct_fuera_01_test)
N_TRAIN_TOTAL_HC  <- .sc08(function() c07a$n_train)
N_KAPPA_HC        <- .sc08(function() c07b$n_final)
N_TEST_HC         <- .sc08(function() c07a$n_test)

# Verificar que todos los valores críticos se pudieron leer
.na08 <- c(
  if (is.na(UMBRAL_YOUDEN_HC)) "UMBRAL_YOUDEN_HC" else character(0),
  if (is.na(AUC_ROC_HC))       "AUC_ROC_HC"       else character(0),
  if (is.na(N_TRAIN_TOTAL_HC)) "N_TRAIN_TOTAL_HC"  else character(0),
  if (is.na(N_TEST_HC))        "N_TEST_HC"         else character(0),
  if (is.na(N_KAPPA_HC))       "N_KAPPA_HC"        else character(0)
)
if (length(.na08) > 0)
  stop(sprintf("[08_SLS] Valores críticos NA desde contratos:\n  %s\n",
               paste(.na08, collapse = ", ")))

cat("  Refs. desde contratos: UMBRAL=", round(UMBRAL_YOUDEN_HC, 4),
    "| AUC=", round(AUC_ROC_HC, 4),
    "| N_train=", format(as.integer(N_TRAIN_TOTAL_HC), big.mark=","),
    "| N_kappa=", format(as.integer(N_KAPPA_HC), big.mark=","),
    "| pct_fuera_test=", round(PCT_FUERA_TEST_HC, 2), "\n")

umbral <- UMBRAL_YOUDEN_HC
hard_stop(!is.na(umbral) && umbral > 0 && umbral < 1,
          paste("Umbral Youden inválido:", umbral))
cat("  Umbral Youden activo:", round(umbral, 4), "\n")


# 🪫 2. VERIFICACIONES INICIALES ------------------------------------------------
cat("\n── 2. Verificaciones iniciales ─────────────────────────────────────\n")

N_PANEL_ORIGINAL <- nrow(panel)  # guardar para verificar integridad post-proceso
hard_stop(ncol(panel) >= 100,
          paste("Panel GLM esperado ≥ 100 columnas. Encontrado:", ncol(panel)))
hard_stop(ID_VAR %in% names(panel),
          paste(ID_VAR, "no encontrado en el panel"))
hard_stop("periodo_id" %in% names(panel),
          "periodo_id no encontrada en el panel")

# [LN1] tipo_estimacion_* DEBEN existir — heredados del LPM, NO recrear
hard_stop("tipo_estimacion_pea" %in% names(panel),
          paste("[LN1] tipo_estimacion_pea no encontrada.",
                "Verificar que el input es", basename(PATH_PANEL),
                "(no 06_theta_predichos.rds)"))
hard_stop("tipo_estimacion_edad" %in% names(panel),
          paste("[LN1] tipo_estimacion_edad no encontrada.",
                "Verificar que el input es", basename(PATH_PANEL)))

# Verificar integridad del panel acumulativo (columnas LPM y GLM deben existir)
cols_lpm_esperadas <- c(COL_LPM_PROB_PEA, COL_LPM_CLASE_PEA, COL_LPM_CLASE_CAL_PEA)
cols_glm_esperadas <- c(COL_GLM_PROB_PEA, COL_GLM_CLASE_PEA, COL_GLM_CLASE_CAL_PEA)
cols_faltantes_check <- setdiff(c(cols_lpm_esperadas, cols_glm_esperadas), names(panel))
if (length(cols_faltantes_check) > 0) {
  cat("  [⚠️] WARN: columnas del panel acumulativo no encontradas:\n")
  cat("    ", paste(cols_faltantes_check, collapse = ", "), "\n")
} else {
  cat("  Columnas", SUFIJO_MODELO_LPM, "y", SUFIJO_MODELO_GLM, "presentes en panel ✓\n")
}

# Verificar formalidad_valida (creada en 08_LPM, debe existir)
hard_stop("formalidad_valida" %in% names(panel),
          paste("formalidad_valida no encontrada — verificar que el panel es el acumulado", SUFIJO_MODELO_GLM))

# Verificar que los theta están (necesarios para recipe)
hard_stop(all(c("theta_A", "theta_B") %in% names(panel)),
          "theta_A y/o theta_B no encontradas en el panel")
hard_stop("edad" %in% names(panel),
          "Variable edad no encontrada en el panel")

# Verificar periodos con formalidad observada
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

cat("  [✅] Verificaciones iniciales OK\n")


# 🪫 3. TIPO_ESTIMACION — VERIFICACIÓN (heredados del LPM, NO recrear [LN1]) ----
cat("\n── 3. Verificación tipo_estimacion (heredado del LPM) ──────────────\n")
cat("  [LN1] tipo_estimacion_pea y tipo_estimacion_edad NO se recrean.\n")
cat("  Fueron construidos en 08_backcasting_LPM.R y heredados al panel acumulado.\n\n")

cat("  tipo_estimacion_pea:\n")
print(table(panel$tipo_estimacion_pea, useNA = "ifany"))
cat("\n  tipo_estimacion_edad:\n")
print(table(panel$tipo_estimacion_edad, useNA = "ifany"))

# Verificar que no hay NAs inesperados
n_na_pea  <- sum(is.na(panel$tipo_estimacion_pea))
n_na_edad <- sum(is.na(panel$tipo_estimacion_edad))
if (n_na_pea > 0)  cat("  [⚠️] WARN:", n_na_pea, "NAs en tipo_estimacion_pea\n")
if (n_na_edad > 0) cat("  [⚠️] WARN:", n_na_edad, "NAs en tipo_estimacion_edad\n")

cat("  [✅] tipo_estimacion verificados\n")


# 🪫 4. UNIVERSO UNIÓN ----------------------------------------------------------
cat("\n── 4. Construcción del universo unión ──────────────────────────────\n")

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

# 5b. Construir lista de exclusión defensiva
# Excluir:
#   (i)  variables de identificación / auxiliares (igual que LPM)
#   (ii) columnas acumuladas de modelos anteriores (LPM, GLM) — NO contaminar bake
#        Se detectan por starts_with para robustez ante cambios futuros
vars_excluir_base <- c(
  "pondera", "formalidad_empleo", "formalidad_valida", "codusu",
  ID_VAR, "id_individuo", "periodo_id", "anio",
  "periodo_num", "condicion_actividad",
  # NOTA: trimestre NO se excluye — es regresor estacional en 07a
  "tipo_estimacion_pea", "tipo_estimacion_edad",
  "elegible_pea", "elegible_edad", "elegible_union"
)

# Detectar columnas de modelos previos (LPM, GLM — y SLS si existieran)
# Patrón: prob_formal_*, formalidad_clase_*, flag_pred_*
cols_modelos_previos <- names(universo_union) %>%
  .[startsWith(., "prob_formal_") |
    startsWith(., "formalidad_clase_") |
    startsWith(., "formalidad_clase_cal_") |
    startsWith(., "flag_pred_")]

if (length(cols_modelos_previos) > 0) {
  cat("  Columnas de modelos previos excluidas del bake (",
      length(cols_modelos_previos), "):",
      paste(head(cols_modelos_previos, 6), collapse = ", "),
      if (length(cols_modelos_previos) > 6) "..." else "", "\n")
}

vars_excluir <- unique(c(vars_excluir_base, cols_modelos_previos))
vars_excluir_presentes <- intersect(vars_excluir, names(universo_union))
features_union <- universo_union %>%
  select(-all_of(vars_excluir_presentes))

# 5c. Renombrar theta_A/theta_B → theta_A_mA/theta_B_mA [L45]
# El recipe SLS reutiliza el mismo recipe que LPM (entrenado con theta_A_mA)
if ("theta_A" %in% names(features_union) && !"theta_A_mA" %in% names(features_union)) {
  features_union <- features_union %>%
    rename(theta_A_mA = theta_A, theta_B_mA = theta_B)
  cat("  theta_A → theta_A_mA, theta_B → theta_B_mA (match recipe) [L45]\n")
}

# 5d. Convertir labels "No aplica" a NA para que recipe los impute
n_converted <- 0L
features_union <- features_union %>%
  mutate(across(where(is.character), function(x) {
    mask <- x %in% LABELS_NO_APLICA
    n_converted <<- n_converted + sum(mask, na.rm = TRUE)
    if_else(mask, NA_character_, x)
  }))
cat("  Labels 'No aplica' convertidas a NA:", format(n_converted, big.mark = ","), "\n")

# 5e. Agregar formalidad_bin = NA (el recipe la referencia como outcome)
features_union <- features_union %>%
  mutate(formalidad_bin = NA_real_)

# 5f. Verificar variables del recipe vs features disponibles
vars_recipe <- recipe_lasso$var_info$variable
vars_faltantes_recipe <- setdiff(vars_recipe, names(features_union))
if (length(vars_faltantes_recipe) > 0) {
  cat("  [⚠️] Variables esperadas por recipe NO encontradas en features:\n")
  cat("    ", paste(vars_faltantes_recipe, collapse = ", "), "\n")
}

vars_extra <- setdiff(names(features_union), vars_recipe)
if (length(vars_extra) > 0) {
  cat("  Variables extra (no en recipe, se filtran):", length(vars_extra), "\n")
  features_union <- features_union %>% select(any_of(vars_recipe))
}

cat("  Dimensiones features para bake:", nrow(features_union), "×",
    ncol(features_union), "\n")

# 5g. BAKE
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
  cat("  [⚠️] WARN:", format(n_na_post_bake, big.mark = ","),
      "NAs post-bake. Columnas con NAs:\n")
  na_por_col <- colSums(is.na(baked_union))
  print(na_por_col[na_por_col > 0])
}

cat("  [✅] Bake completado\n")

# Liberar memoria
rm(features_union, universo_union); gc(verbose = FALSE)


# 🪫 6. PREDICT — objeto lm post-LASSO SLS --------------------------------------
cat("\n── 6. Predicción (lm post-LASSO SLS) ──────────────────────────────\n")

# [L67] SLS = objeto lm. predict.lm() — NO sparse matrix, NO s="lambda.1se",
#        NO type= argumento.
# [L67] Conversión explícita a data.frame: predict.lm() puede dar comportamiento
#        inconsistente con tibbles cuando hay factores residuales o columnas con
#        atributos extra. Conversión explícita garantiza compatibilidad.
cat("  Convirtiendo baked_union a data.frame explícito...\n")
tic("as.data.frame")
df_union <- as.data.frame(baked_union)
toc()

# Verificar alineación de columnas entre recipe y modelo lm
# (el modelo lm espera exactamente las vars de su fórmula)
coef_names   <- names(coef(lm_obj))
coef_names   <- coef_names[coef_names != "(Intercept)"]
cols_faltantes_lm <- setdiff(coef_names, names(df_union))
if (length(cols_faltantes_lm) > 0) {
  cat("  [⚠️] WARN: columnas esperadas por lm_obj no encontradas en df_union:",
      length(cols_faltantes_lm), "\n")
  cat("    Primeras:", paste(head(cols_faltantes_lm, 5), collapse = ", "), "\n")
  cat("    Esto puede causar NAs en predict. Revisar recipe vs modelo.\n")
} else {
  cat("  Columnas lm_obj alineadas con df_union ✓ (",
      length(coef_names), " predictores)\n")
}

# Liberar tibble baked (ya no necesario)
rm(baked_union); gc(verbose = FALSE)

# Predict: predict.lm() devuelve vector named numeric
cat("  Ejecutando predict(lm_obj, newdata = df_union)...\n")
tic("predict lm")
pred_raw <- as.vector(predict(lm_obj, newdata = df_union))
toc()

# Liberar data.frame
rm(df_union); gc(verbose = FALSE)

cat("  Predicciones generadas:", format(length(pred_raw), big.mark = ","), "\n")
hard_stop(length(pred_raw) == length(idx_union),
          paste("Longitud predicciones (", length(pred_raw),
                ") != N universo unión (", length(idx_union), ")"))

# [L73] Clipping OBLIGATORIO — SLS no garantiza acotamiento OOS
# A diferencia del GLM (binomial garantiza [0,1]), el SLS es LPM iterativo
# y puede predecir fuera de [0,1] en nuevas observaciones (θ distintos al train)
pred_clip  <- pmax(0, pmin(1, pred_raw))
pred_clase <- if_else(pred_clip >= umbral, "Formal", "Informal")
flag_fuera <- pred_raw < 0 | pred_raw > 1

n_fuera     <- sum(flag_fuera)
n_fuera_neg <- sum(pred_raw < 0)
n_fuera_pos <- sum(pred_raw > 1)
pct_fuera   <- round(mean(flag_fuera) * 100, 2)

# [LN4] Benchmark: ~21% esperado (igual que test SLS). No es un error —
# es comportamiento estructural del SLS fuera del soporte de entrenamiento.
cat("\n  ── [LN4] Predicciones fuera de [0,1] (SLS) ──\n")
cat(sprintf("  En κ̂γ (train, acotado por construcción):  0.00%%\n"))
cat(sprintf("  En test SLS (benchmark):                  %s\n",
            if (is.na(PCT_FUERA_TEST_HC)) "pendiente (actualizar post-corrida)"
            else sprintf("%.2f%%", PCT_FUERA_TEST_HC)))
cat(sprintf("  En backcasting (observado):               %.2f%%\n", pct_fuera))

if (pct_fuera > 35) {
  cat("  [⚠️] Supera umbral de alerta (35%) — revisar distribución de θ en panel\n")
} else if (!is.na(PCT_FUERA_TEST_HC) && pct_fuera > PCT_FUERA_TEST_HC * 1.5) {
  cat("  [⚠️] Supera 1.5× benchmark test — documentar en paper\n")
} else {
  cat("  [✅] Dentro del rango esperado para SLS\n")
}

cat("    Negativas:", format(n_fuera_neg, big.mark = ","),
    " | Positivas:", format(n_fuera_pos, big.mark = ","), "\n")
cat("  Media prob_formal (clipeada):", round(mean(pred_clip), 4), "\n")
cat("  Umbral Youden:", round(umbral, 4), "\n")
cat("  Tasa formal predicha (unión):",
    round(mean(pred_clase == "Formal") * 100, 2), "%\n")

# Diagnóstico: pred fuera [0,1] por tipo de estimación (PEA)
cat("\n  Pred fuera [0,1] por tipo (PEA):\n")
tibble(
  tipo  = id_cols_union$tipo_estimacion_pea,
  fuera = flag_fuera
) %>%
  group_by(tipo) %>%
  summarise(n = n(), pct_fuera = round(mean(fuera) * 100, 2), .groups = "drop") %>%
  filter(!is.na(tipo)) %>%
  print()

cat("  [✅] Predicción completada\n")


# 🪫 7. ASIGNACIÓN DOBLE — SPLIT _pea / _edad -----------------------------------
cat("\n── 7. Asignación doble de predicciones ─────────────────────────────\n")

# Flags de elegibilidad para el universo unión (posiciones alineadas con idx_union)
en_pea  <- id_cols_union$elegible_pea
en_edad <- id_cols_union$elegible_edad

# Construir tibble con las 8 columnas base (sin _cal, que se agrega en [10])
pred_union <- tibble(
  # Columnas PEA
  !!COL_PROB_RAW_PEA  := if_else(en_pea, pred_raw,   NA_real_),
  !!COL_PROB_PEA      := if_else(en_pea, pred_clip,  NA_real_),
  !!COL_CLASE_PEA     := if_else(en_pea, pred_clase, NA_character_),
  !!COL_FLAG_PEA      := if_else(en_pea, flag_fuera, NA),
  # Columnas Edad
  !!COL_PROB_RAW_EDAD := if_else(en_edad, pred_raw,   NA_real_),
  !!COL_PROB_EDAD     := if_else(en_edad, pred_clip,  NA_real_),
  !!COL_CLASE_EDAD    := if_else(en_edad, pred_clase, NA_character_),
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

if (abs(auc_global_train - AUC_ROC_HC) > 0.01) {
  cat("  [⚠️] AUC training (", round(auc_global_train, 4),
      ") difiere del benchmark (", AUC_ROC_HC, ") > 0.01\n", sep = "")
}

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
cat("  (tasa_pred_cl = clasificación Youden", round(umbral, 4),
    "; tasa_pred_pr = prob media)\n")
print(comp_por_trimestre, n = Inf)

delta_max <- max(abs(comp_por_trimestre$delta_cl_pp))
if (delta_max < 6) {
  cat("  [✅] Delta máximo:", delta_max, "pp < 6 pp → calibración aceptable\n")
  if (delta_max > 2) {
    cat("       NOTA: delta > 2 pp. Umbral Youden (", round(umbral, 4),
        ") optimiza sens+esp, no calibración de tasas.\n", sep = "")
    cat("       Umbral de calibración (sección 8e) mejora este delta.\n")
  }
} else {
  cat("  [⚠️] Delta máximo:", delta_max, "pp ≥ 6 pp → revisar calibración\n")
}

# 8d. Tabla de confusión + métricas [L58: as.numeric() en MCC]
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
# [L58] MCC: as.numeric() obligatorio cuando N > 50,000 (overflow integer)
mcc_train  <- (as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)) /
  sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) *
         as.numeric(tn + fp) * as.numeric(tn + fn))

cat(sprintf("  Accuracy: %.4f | Sens: %.4f | Esp: %.4f | F1: %.4f | MCC: %.4f\n",
            acc_train, sens_train, esp_train, f1_train, mcc_train))

# 8e. Umbral de calibración (iguala tasa predicha ≈ tasa observada)
# Misma metodología que LPM y GLM. El umbral Youden sub-clasifica ~X pp;
# para series temporales de backcasting, la calibración de tasas es prioritaria.
tasa_obs_global <- weighted.mean(comp_training$y_obs, comp_training$pondera)
umbrales_grid   <- seq(0.30, 0.70, by = 0.005)
tasas_pred_grid <- sapply(umbrales_grid, function(u) {
  weighted.mean(as.integer(comp_training$pred_prob >= u), comp_training$pondera)
})
umbral_calibracion <- umbrales_grid[which.min(abs(tasas_pred_grid - tasa_obs_global))]
tasa_pred_cal      <- tasas_pred_grid[which.min(abs(tasas_pred_grid - tasa_obs_global))]

cat(sprintf("\n  Umbral de calibración: %.4f (iguala tasa obs %.2f%% ≈ pred %.2f%%)\n",
            umbral_calibracion, tasa_obs_global * 100, tasa_pred_cal * 100))

# [LN4] Nota metodológica SLS específica
cat("  [LN4] En κ̂γ (train SLS): pred fuera [0,1] = 0.00% (acotado por construcción)\n")
cat(sprintf("  [LN4] En backcasting: %.2f%% (clipping pmax/pmin aplicado)\n", pct_fuera))

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

# Inicializar las 10 columnas SLS como NA en el panel completo
panel[[COL_PROB_RAW_PEA]]   <- NA_real_
panel[[COL_PROB_PEA]]       <- NA_real_
panel[[COL_CLASE_PEA]]      <- NA_character_
panel[[COL_CLASE_CAL_PEA]]  <- NA_character_
panel[[COL_FLAG_PEA]]       <- NA

panel[[COL_PROB_RAW_EDAD]]  <- NA_real_
panel[[COL_PROB_EDAD]]      <- NA_real_
panel[[COL_CLASE_EDAD]]     <- NA_character_
panel[[COL_CLASE_CAL_EDAD]] <- NA_character_
panel[[COL_FLAG_EDAD]]      <- NA

# Asignar por posición (idx_union mapea panel → universo unión)
panel[[COL_PROB_RAW_PEA]][idx_union]  <- pred_union[[COL_PROB_RAW_PEA]]
panel[[COL_PROB_PEA]][idx_union]      <- pred_union[[COL_PROB_PEA]]
panel[[COL_CLASE_PEA]][idx_union]     <- pred_union[[COL_CLASE_PEA]]
panel[[COL_FLAG_PEA]][idx_union]      <- pred_union[[COL_FLAG_PEA]]

panel[[COL_PROB_RAW_EDAD]][idx_union] <- pred_union[[COL_PROB_RAW_EDAD]]
panel[[COL_PROB_EDAD]][idx_union]     <- pred_union[[COL_PROB_EDAD]]
panel[[COL_CLASE_EDAD]][idx_union]    <- pred_union[[COL_CLASE_EDAD]]
panel[[COL_FLAG_EDAD]][idx_union]     <- pred_union[[COL_FLAG_EDAD]]

# Clasificación con umbral de calibración (para series temporales)
clase_cal_raw <- if_else(pred_clip >= umbral_calibracion, "Formal", "Informal")
panel[[COL_CLASE_CAL_PEA]][idx_union]  <-
  if_else(en_pea,  clase_cal_raw, NA_character_)
panel[[COL_CLASE_CAL_EDAD]][idx_union] <-
  if_else(en_edad, clase_cal_raw, NA_character_)

cat("  Clasificación con umbral calibración (",
    round(umbral_calibracion, 4), ") agregada\n", sep = "")

# Eliminar columnas auxiliares de flags (no en output final)
panel <- panel %>%
  select(-elegible_pea, -elegible_edad, -elegible_union)

cat("  Panel final:", format(nrow(panel), big.mark = ","), "×", ncol(panel), "\n")

# Verificar: N total no cambió
hard_stop(nrow(panel) == N_PANEL_ORIGINAL,
          paste("Panel final tiene", nrow(panel), "obs — esperadas", N_PANEL_ORIGINAL))

# Verificar: no hay NAs inesperados en elegibles
n_na_prob_pea  <- sum(is.na(panel[[COL_PROB_PEA]]) &
                        panel$tipo_estimacion_pea %in% TIPOS_ELEGIBLES_PEA)
n_na_prob_edad <- sum(is.na(panel[[COL_PROB_EDAD]]) &
                        panel$tipo_estimacion_edad %in% TIPOS_ELEGIBLES_EDAD)

if (n_na_prob_pea > 0)
  cat("  [⚠️] WARN:", n_na_prob_pea, "NAs en", COL_PROB_PEA, "para PEA elegible\n")
if (n_na_prob_edad > 0)
  cat("  [⚠️] WARN:", n_na_prob_edad, "NAs en", COL_PROB_EDAD, "para Edad elegible\n")

# Distribución final por tipo_estimacion (PEA)
cat("\n  Distribución final — Universo PEA:\n")
resumen_pea <- panel %>%
  group_by(tipo_estimacion_pea) %>%
  summarise(
    n = n(),
    pct = round(n / nrow(panel) * 100, 2),
    n_con_pred = sum(!is.na(.data[[COL_PROB_PEA]])),
    tasa_formal_youden = round(mean(.data[[COL_CLASE_PEA]] == "Formal",
                                    na.rm = TRUE) * 100, 1),
    tasa_formal_cal = round(mean(.data[[COL_CLASE_CAL_PEA]] == "Formal",
                                  na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(resumen_pea)

cat("\n  Distribución final — Universo Edad 18-60:\n")
resumen_edad <- panel %>%
  group_by(tipo_estimacion_edad) %>%
  summarise(
    n = n(),
    pct = round(n / nrow(panel) * 100, 2),
    n_con_pred = sum(!is.na(.data[[COL_PROB_EDAD]])),
    tasa_formal_youden = round(mean(.data[[COL_CLASE_EDAD]] == "Formal",
                                    na.rm = TRUE) * 100, 1),
    tasa_formal_cal = round(mean(.data[[COL_CLASE_CAL_EDAD]] == "Formal",
                                  na.rm = TRUE) * 100, 1),
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

# 11b. Resúmenes para contrato
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

# 11c. Contrato
contrato_08 <- list(
  script              = "08_backcasting_SLS.R",
  fecha               = Sys.time(),
  sufijo_modelo       = SUFIJO,

  # Modelo usado
  modelo_origen       = "07b (base, sin interacciones) — objeto lm post-LASSO SLS",
  tipo_modelo         = "lm (Sequential Least Squares — Horrace & Oaxaca 2003)",
  n_coefs_lm          = length(coef(lm_obj)),
  umbral_youden       = umbral,
  umbral_calibracion  = umbral_calibracion,

  # Panel
  id_var_usada        = ID_VAR,
  n_panel_total       = nrow(panel),
  n_cols_panel        = ncol(panel),

  # Universos
  universo_pea = list(
    n_elegible             = n_elegible_pea,
    n_por_tipo             = table(panel$tipo_estimacion_pea),
    pct_pred_fuera_01      = round(mean(panel[[COL_FLAG_PEA]], na.rm = TRUE) * 100, 2),
    pct_pred_fuera_01_test = PCT_FUERA_TEST_HC,
    pred_fuera_negativas   = sum(panel[[COL_PROB_RAW_PEA]] < 0, na.rm = TRUE),
    pred_fuera_positivas   = sum(panel[[COL_PROB_RAW_PEA]] > 1, na.rm = TRUE),
    media_prob_formal      = round(mean(panel[[COL_PROB_PEA]], na.rm = TRUE), 4),
    resumen_tipos          = resumen_pea,
    n_desocupados_pred     = resumen_desocu_pea$n,
    tasa_formal_desocupados_youden = round(resumen_desocu_pea$tasa_formal_youden, 4),
    tasa_formal_desocupados_cal    = round(resumen_desocu_pea$tasa_formal_cal, 4)
  ),

  universo_edad = list(
    edad_min               = EDAD_MIN,
    edad_max               = EDAD_MAX,
    n_elegible             = n_elegible_edad,
    n_por_tipo             = table(panel$tipo_estimacion_edad),
    pct_pred_fuera_01      = round(mean(panel[[COL_FLAG_EDAD]], na.rm = TRUE) * 100, 2),
    pred_fuera_negativas   = sum(panel[[COL_PROB_RAW_EDAD]] < 0, na.rm = TRUE),
    pred_fuera_positivas   = sum(panel[[COL_PROB_RAW_EDAD]] > 1, na.rm = TRUE),
    media_prob_formal      = round(mean(panel[[COL_PROB_EDAD]], na.rm = TRUE), 4),
    resumen_tipos          = resumen_edad,
    n_inactivos_pred       = resumen_inactivos_edad$n,
    tasa_formal_inactivos_youden = round(resumen_inactivos_edad$tasa_formal_youden, 4),
    tasa_formal_inactivos_cal    = round(resumen_inactivos_edad$tasa_formal_cal, 4)
  ),

  # Unión
  n_union              = n_elegible_union,
  n_solo_pea           = n_solo_pea,
  n_solo_edad          = n_solo_edad,
  n_ambos              = n_ambos,
  pct_pred_fuera_01_union = pct_fuera,

  # Validación en ventana training
  auc_global_train     = auc_global_train,
  mae_global_train     = mae_global_train,
  accuracy_train       = acc_train,
  f1_train             = f1_train,
  mcc_train            = mcc_train,
  comp_por_trimestre   = comp_por_trimestre,
  delta_max_pp         = delta_max,

  nota_calibracion = paste0(
    "Delta max (Youden) = ", delta_max, " pp. ",
    "Umbral Youden (", round(umbral, 4), ") optimiza sens+esp. ",
    "Umbral calibración (", round(umbral_calibracion, 4),
    ") iguala tasa predicha ≈ observada. ",
    "Limitación SLS: acotamiento [0,1] garantizado solo en κ̂γ (N=",
    if (is.na(N_KAPPA_HC)) "pendiente" else N_KAPPA_HC, "). ",
    "OOS: ~", round(pct_fuera, 1), "% fuera [0,1] (benchmark test: ",
    if (is.na(PCT_FUERA_TEST_HC)) "pendiente" else PCT_FUERA_TEST_HC, "%). ",
    "Clipping pmax(0, pmin(1, pred)) aplicado. ",
    "Ref: Horrace & Oaxaca (2003)."
  ),

  # Métricas con umbral de calibración
  metricas_calibracion = list(
    umbral        = umbral_calibracion,
    accuracy      = acc_cal,
    sensibilidad  = sens_cal,
    especificidad = esp_cal,
    f1            = f1_cal,
    mcc           = mcc_cal,
    delta_max_pp  = delta_max_cal,
    comp_por_trimestre = comp_por_trim_cal
  ),

  # Hold-out
  metricas_holdout = metricas_holdout,

  # Benchmarks del modelo (hardcoded desde HANDOFF D25)
  benchmarks_modelo = list(
    auc_roc        = AUC_ROC_HC,
    f1             = F1_HC,
    mcc            = MCC_HC,
    umbral_youden  = UMBRAL_YOUDEN_HC,
    n_train        = N_TRAIN_TOTAL_HC,
    n_kappa        = N_KAPPA_HC,
    n_test         = N_TEST_HC,
    pct_fuera_test = PCT_FUERA_TEST_HC
  ),

  # Notas metodológicas
  nota_sls = paste0(
    "SLS = Sequential Least Squares (Horrace & Oaxaca 2003). ",
    "Algoritmo de recorte iterativo sobre LPM: estima OLS, recorta obs fuera [0,1], ",
    "re-estima sobre el subconjunto acotado κ̂γ (N=",
    if (is.na(N_KAPPA_HC)) "pendiente" else N_KAPPA_HC, ", ",
    if (is.na(N_KAPPA_HC)) "pendiente" else round(100 * N_KAPPA_HC / N_TRAIN_TOTAL_HC, 1),
    "% del train). ",
    "La garantía de acotamiento aplica SOLO en κ̂γ. ",
    "OOS: predicciones fuera de [0,1] son esperadas (~",
    if (is.na(PCT_FUERA_TEST_HC)) "pendiente" else PCT_FUERA_TEST_HC, "% en test). ",
    "El clipping no afecta las probabilidades predichas — solo su rango."
  ),
  nota_desocupados = paste0(
    "Desocupados (Potencial_desocupado): pred fuera [0,1] estructuralmente elevada ",
    "(θ_A distinto al perfil de κ̂γ). Variables laborales del empleo anterior ",
    "(coalesce ≤3 años) para desocupados con experiencia reciente. ",
    "Sin experiencia: imputación genérica (moda training). ",
    "El GLM (acotado [0,1]) es más apropiado para este subgrupo."
  ),
  nota_inactivos = paste0(
    "Inactivos 18-60 (Potencial_inactivo, solo universo _edad): ",
    "variables laborales imputadas con moda training (step_impute_mode). ",
    "Interpretar como formalidad potencial dado perfil θ + educación + demografía. ",
    "Interpretar con cautela en paper."
  )
)

saveRDS(contrato_08, PATH_CONTRATO_OUT)
cat("  [✅] Contrato guardado:", basename(PATH_CONTRATO_OUT), "\n")


# 📑 Checklist -----------------------------------------------------------------

end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("📋 CHECKLIST SCRIPT 08 SLS:\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("   [✅] Panel: %s obs (esperado: %s)\n",
            format(nrow(panel), big.mark = ","),
            format(N_PANEL_ORIGINAL, big.mark = ",")))
cat(sprintf("   [✅] Columnas panel: %d (esperado: ~110)\n", ncol(panel)))
cat(sprintf("   [✅] Universo PEA: %s obs con predicción\n",
            format(n_elegible_pea, big.mark = ",")))
cat(sprintf("   [✅] Universo Edad 18-60: %s obs con predicción\n",
            format(n_elegible_edad, big.mark = ",")))
cat(sprintf("   [✅] Pred fuera [0,1] (unión): %.2f%% (benchmark test: %s)\n",
            pct_fuera,
            if (is.na(PCT_FUERA_TEST_HC)) "pendiente" else sprintf("%.2f%%", PCT_FUERA_TEST_HC)))
cat(sprintf("   [✅] AUC global (training): %.4f (benchmark: %.4f)\n",
            auc_global_train, AUC_ROC_HC))
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
cat("   SIGUIENTE: 08b_reporte_backcasting_SLS.R (reporte HTML)\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("   ⏱️  Tiempo total: %.1f segundos\n\n", elapsed))

toc()
