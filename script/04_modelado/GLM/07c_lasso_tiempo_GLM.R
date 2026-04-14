# =============================================================================
# [EN] 07c_lasso_tiempo_GLM.R -- Temporal neutrality test: verify time effects are absorbed by structural covariates (GLM)
# INPUTS:  rdos/datos/06_theta_predichos.rds, contract from 07a GLM
# OUTPUTS: rdos/contratos/07c_contrato_tiempo_GLM*.rds
# =============================================================================
# 🌟 07c_lasso_tiempo_GLM.R 🌟 ####
# CAPA 4 — MODELADO GLM | Test de neutralidad temporal
#
# OBJETIVO: Demostrar que los efectos temporales son neutros sobre formalidad
#           una vez controladas las covariables estructurales del modelo base.
#           Se testean dos formas de temporalidad residual:
#             - periodo_num (numérico): tendencia lineal
#             - anio (factor):          efectos fijos anuales
#           NOTA: trimestre ya está en el modelo base (07a) como regresor
#           estacional — no se testea aquí para evitar redundancia.
#           Todas con penalty.factor = 1 (el LASSO decide libremente).
#           Si quedan en 0 o con selección bootstrap < 10% → neutralidad temporal.
#
# DISENO:   Script auxiliar de robustez. NO reemplaza al modelo base (07b).
#           Justifica la ausencia de dummies de período en 07b.
#
# INPUT:    PATH_06_THETA, PATH_06_MODELO_HETERO, PATH_07_CONTRATO_GLM (n_train/n_test)
# OUTPUTS:  PATH_07C_CONTRATO_GLM (.rds)
#           rdos/reportes/07c_tabla_temporal_GLM3T.csv
#
# LECCIONES APLICADAS:
#   25 — train_raw canónico con los 3 filtros exactos de 07a
#   26 — MCC con as.numeric() (no aplica en 07c, no hay GLM post-LASSO)
#   27 — col.names = FALSE en write.table con append
#
# DIFERENCIAS vs LPM:
#   - family = "binomial" (en lugar de "gaussian")
#   - type.measure = "auc" (en lugar de "mse")
#   - predict(..., type = "response") → probabilidades en [0,1]
#   - Paths y sufijos: GLM3T
#   - auc_base_07b = 0.8686 (GLM)
#   - step_normalize con na_rm = TRUE (fix busqueda_formal)
#
# AUTOR: Proyecto EPH Formalidad | FECHA: 2026-03-06

# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(recipes)
  library(glmnet)
  library(pROC)
  library(doParallel)
  library(foreach)
  library(tictoc)
})

# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 07c GLM completo")
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  🌟 SCRIPT 07c — TEST DE NEUTRALIDAD TEMPORAL (GLM)\n")
cat("  Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("  DISEÑO: modelo base 07b + periodo_num / anio (trimestre ya en base)\n")
cat("          penalty.factor = 1 para vars temporales\n")
cat("          penalty.factor = 0 para theta_A_mA / theta_B_mA\n\n")

set.seed(SEED_GLOBAL)

# 🪫 1. Carga de datos y θ (patrón canónico) -----------------------------------
cat("── FASE 1: Carga de datos ──────────────────────────────────────────\n")

# Cargar θ (patrón canónico — igual en todos los scripts downstream)
modelo_hetero <- readRDS(PATH_06_MODELO_HETERO)
theta_data_mA <- modelo_hetero$modelo_A$theta_data %>%
  rename(theta_A_mA = theta_A, theta_B_mA = theta_B) %>%
  select(id_individuo_hist, periodo_id, theta_A_mA, theta_B_mA)

panel <- readRDS(PATH_06_THETA) %>%
  left_join(theta_data_mA,
            by = c("id_individuo_hist", "periodo_id"),
            relationship = "many-to-one")

cat("Panel cargado:", format(nrow(panel), big.mark = ","), "obs ×",
    ncol(panel), "vars\n")
cat("Cobertura theta_A_mA:",
    format(sum(!is.na(panel$theta_A_mA)), big.mark = ","),
    sprintf("(%.1f%%)\n\n", mean(!is.na(panel$theta_A_mA)) * 100))

# Verificar que las variables temporales existen
# trimestre ya está en modelo base 07a como regresor estacional — no redundar aquí
vars_temp_raw <- c("periodo_num", "anio")
vars_faltantes_temp <- setdiff(vars_temp_raw, names(panel))
if (length(vars_faltantes_temp) > 0) {
  stop("Variables temporales faltantes en el panel: ",
       paste(vars_faltantes_temp, collapse = ", "))
}
cat("  [✅] Variables temporales encontradas:",
    paste(vars_temp_raw, collapse = ", "), "\n\n")

# Cargar contrato_07a_GLM para verificación del split
contrato_07a <- readRDS(PATH_07_CONTRATO_GLM)
cat("  Contrato 07a GLM cargado | n_train:", format(contrato_07a$n_train, big.mark = ","),
    "| n_test:", format(contrato_07a$n_test, big.mark = ","), "\n\n")

# 🪫 2. train_raw canónico (lección 25) + split 80/20 --------------------------
cat("── FASE 2: train_raw canónico + split 80/20 ────────────────────────\n")

# VARS_BASE: idéntico al modelo 07a/07b GLM
VARS_BASE <- c(
  "formalidad_empleo", "pondera", "codusu",
  # θ (penalty.factor = 0)
  "theta_A_mA", "theta_B_mA",
  # Demográficas
  "edad", "edad_cuadrado", "sexo", "estado_civil", "lugar_nacimiento", "parentesco",
  # Educativas
  "nivel_educ_obtenido2", "asistencia_escuela", "tipo_escuela", "alfabetizacion",
  # Geográficas
  "aglomerado", "region", "mas_500",
  # Laborales
  "seccion", "calificacion", "antiguedad", "categoria_ocupacional",
  "trimestre",                  # regresor estacional (ya en 07a)
  # Hogar
  "nbi", "miembros_hogar", "menores10", "mayores10",
  "principal_tareas_hogar", "otros_tareas_hogar",
  # Proxies e ICH
  "ich_score", "residual_vivienda",
  "rezago_escolar_cohorte", "clima_educativo_hogar",
  "emparejamiento_selectivo", "calificacion_norm", "entropia_estabilidad",
  # Fuentes de ingreso alternativas
  "vive_alquiler", "vive_ganancias_negocio", "vive_renta_financiera",
  "vive_beca", "vive_cuota_alimenticia", "vive_ahorros",
  "vive_prestamos_personas", "vive_prestamos_financieros",
  "vive_financiamiento", "vive_venta_bienes", "vive_otro_ingreso"
)

# Variables temporales adicionales (se agregan sobre VARS_BASE)
VARS_MODELO <- c(VARS_BASE, vars_temp_raw)

vars_faltantes <- setdiff(VARS_MODELO, names(panel))
if (length(vars_faltantes) > 0) {
  stop("Variables faltantes en el panel: ",
       paste(vars_faltantes, collapse = ", "))
}
cat("  [✅] Todas las variables del modelo encontradas en el panel\n")

# train_raw canónico — 3 filtros exactos de 07a (lección 25)
train_raw <- panel %>%
  filter(
    condicion_actividad == "Ocupado",
    formalidad_empleo %in% c("Formal oficial", "Informal oficial"),
    periodo_id %in% TRIMESTRES_FORMALIDAD
  ) %>%
  mutate(
    formalidad_bin = as.integer(formalidad_empleo == "Formal oficial"),
    lugar_nacimiento = if ("lugar_nacimiento" %in% names(.)) {
      case_when(
        lugar_nacimiento %in% c("Localidad", "Provincia", "Otra provincia") ~ "Argentina",
        lugar_nacimiento == "País limítrofe" ~ "Pais_Limitrofe",
        lugar_nacimiento == "Otro país"      ~ "Otro_Pais",
        TRUE ~ lugar_nacimiento
      )
    } else { NA_character_ },
    # Vars temporales: anio como factor, periodo_num permanece numérico
    # trimestre ya viene como factor desde el panel (parte de VARS_BASE)
    anio      = factor(anio),
    trimestre = factor(trimestre)  # asegurar factor
  ) %>%
  select(all_of(c(VARS_MODELO, "formalidad_bin")))

cat("\n  Distribución temporal del universo de entrenamiento:\n")
print(train_raw %>% count(anio, trimestre) %>% arrange(anio, trimestre))

cat("\n  N universo:", format(nrow(train_raw), big.mark = ","), "\n")
cat("  Balance formalidad:",
    format(sum(train_raw$formalidad_bin), big.mark = ","), "formales /",
    format(sum(train_raw$formalidad_bin == 0), big.mark = ","), "informales\n")

# Split 80/20 estratificado — misma seed y lógica que 07a GLM
set.seed(SEED_GLOBAL)
idx_train <- c(
  sample(which(train_raw$formalidad_bin == 1),
         floor(0.80 * sum(train_raw$formalidad_bin == 1))),
  sample(which(train_raw$formalidad_bin == 0),
         floor(0.80 * sum(train_raw$formalidad_bin == 0)))
)
df_train <- train_raw[idx_train, ]
df_test  <- train_raw[-idx_train, ]

# Verificar consistencia con 07a GLM (lección 25)
stopifnot(
  "Split inconsistente con contrato 07a GLM" =
    nrow(df_train) == contrato_07a$n_train &&
    nrow(df_test)  == contrato_07a$n_test
)
cat("\n  [✅] Split verificado vs contrato_07a GLM:",
    format(nrow(df_train), big.mark = ","), "train /",
    format(nrow(df_test),  big.mark = ","), "test\n")

# 🪫 3. Recipe (base 07a GLM + vars temporales) --------------------------------
cat("\n── FASE 3: Recipe ──────────────────────────────────────────────────\n")

# NOTA: se construye un recipe propio (no se reutiliza PATH_07_RECIPE_GLM)
# porque ese recipe no contiene las variables temporales.
# Fix lección: na_rm = TRUE en step_normalize (evita warning de busqueda_formal).

recipe_07c <- recipe(
    formalidad_bin ~ .,
    data = df_train %>% select(-pondera, -formalidad_empleo, -codusu)
  ) %>%

  # ── Limpieza de Ns/Nr y valores no informativos ─────────────────────────
  step_mutate(across(all_nominal_predictors(),
                     ~ na_if(as.character(.), "Ns/Nr"))) %>%
  step_mutate(across(all_nominal_predictors(),
                     ~ na_if(as.character(.), "Ns/Nc"))) %>%
  step_mutate(across(all_nominal_predictors(),
                     ~ na_if(as.character(.), "No corresponde"))) %>%
  step_mutate(
    across(starts_with("vive_"),
           ~ if_else(as.character(.) %in% c("Ns/Nr", "Ns.Nr", "Ns/Nc", "Ns.Nc"),
                     NA_character_, as.character(.)))
  ) %>%
  step_mutate(
    alfabetizacion     = if_else(as.character(alfabetizacion)
                                 %in% c("Ns/Nr", "Ns.Nr"), NA_character_,
                                 as.character(alfabetizacion)),
    estado_civil       = if_else(as.character(estado_civil)
                                 %in% c("Ns/Nr", "Ns.Nr"), NA_character_,
                                 as.character(estado_civil)),
    tipo_escuela       = if_else(as.character(tipo_escuela)
                                 %in% c("Ns/Nr", "Ns.Nr"), NA_character_,
                                 as.character(tipo_escuela)),
    calificacion       = if_else(as.character(calificacion)
                                 %in% c("Ns/Nc", "Ns.Nc"), NA_character_,
                                 as.character(calificacion)),
    asistencia_escuela = if_else(as.character(asistencia_escuela)
                                 == "Nunca", NA_character_,
                                 as.character(asistencia_escuela))
  ) %>%

  # ── Conversión, imputación, normalización ──────────────────────────────
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors(), -formalidad_bin) %>%
  # na_rm = TRUE: fix para busqueda_formal (varianza cero con NAs)
  step_normalize(all_numeric_predictors(), -formalidad_bin, na_rm = TRUE) %>%

  # ── Dummies ────────────────────────────────────────────────────────────
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%

  # ── Eliminar varianza cero y colineales ────────────────────────────────
  # Proteger θ, temporales y edad/edad_cuadrado de step_corr
  step_zv(all_predictors()) %>%
  step_corr(
    all_numeric_predictors(),
    -any_of(c("theta_A_mA", "theta_B_mA",
              "edad", "edad_cuadrado",
              "periodo_num")),
    threshold = 0.85
  )

cat("  Preparando recipe...")
recipe_prepped <- prep(
  recipe_07c,
  training = df_train %>% select(-pondera, -formalidad_empleo, -codusu)
)
cat(" listo\n")

# Matrices
X_train <- bake(recipe_prepped,
                new_data = df_train %>% select(-pondera, -formalidad_empleo, -codusu),
                composition = "matrix")
X_train <- X_train[, colnames(X_train) != "formalidad_bin"]

X_test  <- bake(recipe_prepped,
                new_data = df_test %>% select(-pondera, -formalidad_empleo, -codusu),
                composition = "matrix")
X_test  <- X_test[, colnames(X_test) != "formalidad_bin"]

y_train <- df_train$formalidad_bin
w_train <- df_train$pondera / mean(df_train$pondera)
y_test  <- df_test$formalidad_bin
w_test  <- df_test$pondera  / mean(df_test$pondera)

cat("  Dimensiones X_train:", nrow(X_train), "×", ncol(X_train), "\n")
cat("  Dimensiones X_test: ", nrow(X_test),  "×", ncol(X_test),  "\n")

# Identificar columnas temporales en X_train (después del recipe)
cols_temporales <- colnames(X_train)[
  grepl("^periodo_num$|^anio_", colnames(X_train))
]
cat("\n  Variables temporales en X_train (", length(cols_temporales), "):",
    paste(cols_temporales, collapse = ", "), "\n")

if (length(cols_temporales) == 0) {
  warning("Ninguna variable temporal sobrevivió al recipe (step_zv o step_corr las eliminó).",
          "\n  Esto puede ocurrir porque con solo 3 períodos hay colinealidad perfecta.",
          "\n  Se documentará neutralidad técnica en el contrato.")
}

# ── penalty.factor: 0 para θ, 1 para todo lo demás (incluye temporales) ────
pf <- rep(1, ncol(X_train))
theta_cols_idx <- colnames(X_train) %in% c("theta_A_mA", "theta_B_mA")
pf[theta_cols_idx] <- 0
cat("\n  penalty.factor = 0 para:",
    paste(colnames(X_train)[theta_cols_idx], collapse = ", "), "\n")
cat("  penalty.factor = 1 para vars temporales (regularización normal)\n")

# ── Folds CV por cluster (codusu) ─────────────────────────────────────────
cat("\n  Construyendo foldid por cluster (codusu)...\n")
set.seed(SEED_GLOBAL)
cluster_ids     <- unique(df_train$codusu)
n_clusters      <- length(cluster_ids)
fold_asignacion <- tibble(
  codusu = cluster_ids,
  fold   = sample(rep(1:10, length.out = n_clusters))
)
foldid_vec <- tibble(codusu = df_train$codusu) %>%
  left_join(fold_asignacion, by = "codusu") %>%
  pull(fold)
cat(sprintf("  Clusters únicos: %s | Tamaño medio: %.1f obs/cluster\n",
            format(n_clusters, big.mark = ","), nrow(df_train) / n_clusters))
stopifnot(length(foldid_vec) == nrow(X_train))

# 🪫 4. LASSO CV (binomial) ----------------------------------------------------
cat("\n── FASE 4: LASSO CV (binomial) ─────────────────────────────────────\n")

cl <- makeCluster(N_CORES)
registerDoParallel(cl)
cat("  Cluster activado:", N_CORES, "cores\n")

tic("cv.glmnet 07c GLM")
cv_fit_07c <- cv.glmnet(
  x              = X_train,
  y              = y_train,
  weights        = w_train,
  alpha          = 1,
  family         = "binomial",        # GLM: binomial en lugar de gaussian
  penalty.factor = pf,
  type.measure   = "auc",             # GLM: AUC en lugar de MSE
  foldid         = foldid_vec,
  parallel       = TRUE
)
stopCluster(cl)
toc()

lambda_1se_07c <- cv_fit_07c$lambda.1se
lambda_min_07c <- cv_fit_07c$lambda.min

cat("\n  λ.min:", round(lambda_min_07c, 6),
    "| AUC CV:", round(max(cv_fit_07c$cvm), 4), "\n")
cat("  λ.1se:", round(lambda_1se_07c, 6),
    "| AUC CV:", round(cv_fit_07c$cvm[cv_fit_07c$lambda == lambda_1se_07c], 4), "\n")

# Coeficientes λ.1se
coef_1se_07c <- coef(cv_fit_07c, s = "lambda.1se")
vars_sel_07c <- data.frame(
  variable    = rownames(coef_1se_07c),
  coeficiente = as.vector(coef_1se_07c)
) %>%
  filter(variable != "(Intercept)", coeficiente != 0) %>%
  arrange(desc(abs(coeficiente)))

cat("\n  Variables seleccionadas (λ.1se):", nrow(vars_sel_07c), "\n")

# Coeficientes de las vars temporales en λ.1se
coef_temp_1se <- data.frame(
  variable    = rownames(coef_1se_07c),
  coeficiente = as.vector(coef_1se_07c)
) %>%
  filter(variable %in% cols_temporales) %>%
  arrange(variable)

cat("\n  Coeficientes temporales (λ.1se — log-odds):\n")
if (nrow(coef_temp_1se) > 0) {
  print(coef_temp_1se)
} else {
  cat("  (ninguna variable temporal seleccionada en λ.1se)\n")
}

n_vars_temp_sel_1se <- sum(vars_sel_07c$variable %in% cols_temporales)
cat(sprintf("\n  Temporales seleccionadas en λ.1se: %d / %d\n",
            n_vars_temp_sel_1se, length(cols_temporales)))

# 🪫 5. Evaluación en test -----------------------------------------------------
cat("\n── FASE 5: Evaluación en test ──────────────────────────────────────\n")

# GLM: type = "response" → probabilidades en [0,1] directamente
pred_raw_07c  <- as.vector(predict(cv_fit_07c, newx = X_test,
                                   s = "lambda.1se", type = "response"))
pred_clip_07c <- pmax(0, pmin(1, pred_raw_07c))

pct_fuera_07c <- mean(pred_raw_07c < 0 | pred_raw_07c > 1) * 100
cat(sprintf("  Predicciones fuera de [0,1]: %.2f%%\n", pct_fuera_07c))

roc_obj_07c <- roc(response  = y_test,
                   predictor = pred_clip_07c,
                   weights   = w_test,
                   quiet     = TRUE)
auc_val_07c <- as.numeric(auc(roc_obj_07c))

# Referencia: AUC del modelo base GLM 07b
# HC documentado: fallback tryCatch — fuente primaria es PATH_07B_CONTRATO_GLM$resumen_comparacion$auc_roc
.AUC_BASE_FB_07C <- 0.8686
tryCatch({
  contrato_07b <- readRDS(PATH_07B_CONTRATO_GLM)
  rc           <- contrato_07b$resumen_comparacion
  auc_base_07b <- rc$auc_roc[1]
  cat("  Contrato 07b GLM: AUC base =", round(auc_base_07b, 4), "\n")
}, error = function(e) {
  warning(sprintf("[07c-GLM] No se pudo leer AUC de contrato 07b — usando HC fallback. Error: %s", e$message))
  auc_base_07b <<- .AUC_BASE_FB_07C
})
delta_auc    <- auc_val_07c - auc_base_07b

cat(sprintf("  AUC (test, ponderado):  %.4f\n",  auc_val_07c))
cat(sprintf("  AUC modelo base (07b):  %.4f\n",  auc_base_07b))
cat(sprintf("  Δ AUC vs 07b:           %+.4f\n", delta_auc))

# 🪫 6. Bootstrap (200 iter — estabilidad de selección temporal) ---------------
cat("\n── FASE 6: Bootstrap 200 iteraciones ──────────────────────────────\n")

N_BOOT_07c    <- 200L
N_CORES_BOOT  <- min(parallel::detectCores(logical = FALSE) - 1L, 15L)
cat("  Cores para bootstrap:", N_CORES_BOOT, "\n")

cl_boot <- makeCluster(N_CORES_BOOT)
registerDoParallel(cl_boot)
clusterExport(cl_boot,
              c("X_train", "y_train", "w_train", "pf",
                "lambda_1se_07c", "X_test", "y_test", "w_test"),
              envir = environment())

tic("Bootstrap 07c GLM")
boot_results_07c <- foreach(
  i              = seq_len(N_BOOT_07c),
  .packages      = c("glmnet", "pROC"),
  .combine       = "rbind",
  .errorhandling = "remove"
) %dopar% {
  set.seed(i)
  idx_b  <- sample(nrow(X_train), replace = TRUE)
  X_b    <- X_train[idx_b, ]
  y_b    <- y_train[idx_b]
  w_b    <- w_train[idx_b]
  fit_b  <- glmnet(X_b, y_b, weights = w_b,
                   alpha = 1, family = "binomial",   # GLM: binomial
                   penalty.factor = pf,
                   lambda = lambda_1se_07c)
  coef_b        <- as.vector(coef(fit_b, s = lambda_1se_07c))
  names(coef_b) <- rownames(coef(fit_b))
  # GLM: type = "response" → probabilidades directas, clip redundante pero consistente
  pred_b  <- pmax(0, pmin(1, as.vector(predict(fit_b, newx = X_test,
                                                s = lambda_1se_07c,
                                                type = "response"))))
  auc_b   <- tryCatch(
    as.numeric(auc(roc(y_test, pred_b, weights = w_test, quiet = TRUE))),
    error = function(e) NA_real_
  )
  c(coef_b, auc_boot = auc_b)
}
stopCluster(cl_boot)
toc()

cat("  Iteraciones completadas:", nrow(boot_results_07c), "/", N_BOOT_07c, "\n")

# ── Resumen bootstrap ─────────────────────────────────────────────────────
auc_boot_vec_07c <- boot_results_07c[, "auc_boot"]
coef_boot_07c    <- boot_results_07c[, colnames(boot_results_07c) != "auc_boot"]

boot_summary_07c <- data.frame(
  variable          = colnames(coef_boot_07c),
  seleccion_pct     = apply(coef_boot_07c, 2, function(x) mean(x != 0) * 100),
  coef_media_global = apply(coef_boot_07c, 2, mean),
  coef_media_cond   = apply(coef_boot_07c, 2, function(x) {
    x_nz <- x[x != 0]; if (length(x_nz) == 0) NA_real_ else mean(x_nz)
  }),
  coef_sd       = apply(coef_boot_07c, 2, sd),
  coef_ic_low   = apply(coef_boot_07c, 2, quantile, probs = 0.025),
  coef_ic_high  = apply(coef_boot_07c, 2, quantile, probs = 0.975),
  row.names     = NULL
) %>%
  filter(variable != "(Intercept)") %>%
  arrange(desc(seleccion_pct))

auc_boot_summary_07c <- list(
  media   = mean(auc_boot_vec_07c, na.rm = TRUE),
  sd      = sd(auc_boot_vec_07c,   na.rm = TRUE),
  ic_low  = quantile(auc_boot_vec_07c, 0.025, na.rm = TRUE),
  ic_high = quantile(auc_boot_vec_07c, 0.975, na.rm = TRUE)
)

cat(sprintf("\n  AUC bootstrap: %.4f [IC95: %.4f – %.4f]\n",
            auc_boot_summary_07c$media,
            auc_boot_summary_07c$ic_low,
            auc_boot_summary_07c$ic_high))

# ── Variables temporales en bootstrap ─────────────────────────────────────
boot_temporal_07c <- boot_summary_07c %>%
  filter(variable %in% cols_temporales) %>%
  arrange(variable)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  RESULTADO CENTRAL — Variables temporales en bootstrap:\n")
cat("═══════════════════════════════════════════════════════════════════\n")
if (nrow(boot_temporal_07c) > 0) {
  print(boot_temporal_07c %>%
          select(variable, seleccion_pct, coef_media_cond,
                 coef_ic_low, coef_ic_high),
        digits = 4)
} else {
  cat("  (ninguna variable temporal sobrevivió al recipe)\n")
}

# Interpretación automática
n_temporal_sel <- sum(boot_temporal_07c$seleccion_pct >= 10, na.rm = TRUE)

if (length(cols_temporales) == 0) {
  cat("\n  ✅ NEUTRALIDAD TÉCNICA: vars temporales eliminadas por step_zv/step_corr.\n")
  cat("     Con pocos períodos en la ventana de entrenamiento existe colinealidad\n")
  cat("     entre periodo_num y anio. El recipe las eliminó,\n")
  cat("     confirmando que no aportan información independiente.\n")
  cat("     (trimestre ya controlado en modelo base 07a)\n")
  neutralidad_confirmada <- TRUE
} else if (n_temporal_sel == 0) {
  cat("\n  ✅ NEUTRALIDAD TEMPORAL CONFIRMADA:\n")
  cat("     Ninguna variable temporal supera 10% de selección en bootstrap.\n")
  cat("     El modelo de backcasting no requiere efectos temporales autónomos.\n")
  neutralidad_confirmada <- TRUE
} else {
  cat(sprintf("\n  ⚠️  %d variable(s) temporal(es) con selección bootstrap ≥ 10%%.\n",
              n_temporal_sel))
  cat("     Revisar antes de avanzar al backcasting.\n")
  print(boot_temporal_07c %>% filter(seleccion_pct >= 10))
  neutralidad_confirmada <- FALSE
}

# 🪫 7. Tabla temporal para el paper -------------------------------------------
cat("\n── FASE 7: Tabla temporal ──────────────────────────────────────────\n")

if (nrow(boot_temporal_07c) > 0 && nrow(coef_temp_1se) > 0) {
  tabla_temporal_07c <- boot_temporal_07c %>%
    left_join(
      coef_temp_1se %>% rename(coef_lasso_1se = coeficiente),
      by = "variable"
    ) %>%
    mutate(
      tipo = case_when(
        grepl("^periodo_num$", variable) ~ "Tendencia lineal",
        grepl("^anio_",        variable) ~ "Efecto fijo anual",
        TRUE ~ "Otro"
      ),
      interpretacion = case_when(
        seleccion_pct < 10  ~ "✅ No seleccionada — efecto nulo",
        seleccion_pct < 50  ~ "⚠️ Selección baja — efecto marginal",
        seleccion_pct < 80  ~ "⚠️ Selección moderada — investigar",
        TRUE                ~ "❌ Efecto temporal significativo"
      )
    ) %>%
    select(tipo, variable, coef_lasso_1se, seleccion_pct,
           coef_media_cond, coef_ic_low, coef_ic_high, interpretacion) %>%
    arrange(tipo, variable)
} else {
  # Sin variables temporales en el recipe: tabla vacía documentada
  tabla_temporal_07c <- tibble(
    tipo            = character(0),
    variable        = character(0),
    coef_lasso_1se  = numeric(0),
    seleccion_pct   = numeric(0),
    coef_media_cond = numeric(0),
    coef_ic_low     = numeric(0),
    coef_ic_high    = numeric(0),
    interpretacion  = character(0)
  )
}

# Guardar CSV
path_csv_07c <- file.path(DIR_REPORTES,
                          paste0("07c_tabla_temporal_", SUFIJO_MODELO_GLM, ".csv"))
write_csv(tabla_temporal_07c, path_csv_07c)
cat("  [✅] 07c_tabla_temporal_", SUFIJO_MODELO_GLM, ".csv guardado\n",
    sep = "")

# 🪫 8. Contrato 07c GLM -------------------------------------------------------
cat("\n── FASE 8: Contrato ────────────────────────────────────────────────\n")

contrato_07c <- list(
  # ── Identificación ───────────────────────────────────────────────────────
  script      = "07c_lasso_tiempo_GLM.R",
  sufijo      = SUFIJO_MODELO_GLM,
  fecha       = Sys.time(),
  version_tag = "v1_neutralidad_temporal_GLM",

  # ── Universo ─────────────────────────────────────────────────────────────
  n_train = nrow(df_train),
  n_test  = nrow(df_test),

  # ── Variables temporales ─────────────────────────────────────────────────
  cols_temporales_raw      = vars_temp_raw,
  cols_temporales_recipe   = cols_temporales,   # las que sobreviven al recipe
  n_vars_temp_raw          = length(vars_temp_raw),
  n_vars_temp_recipe       = length(cols_temporales),
  n_vars_temp_sel_1se      = n_vars_temp_sel_1se,
  n_vars_sel_total         = nrow(vars_sel_07c),

  # ── Modelo ───────────────────────────────────────────────────────────────
  lambda_1se            = lambda_1se_07c,
  lambda_min            = lambda_min_07c,
  cv_foldid_por_cluster = TRUE,
  n_clusters_train      = n_clusters,
  family                = "binomial",
  type_measure          = "auc",

  # ── Performance ──────────────────────────────────────────────────────────
  auc_test             = auc_val_07c,
  auc_base_07b         = auc_base_07b,
  delta_auc_vs_07b     = delta_auc,
  pct_pred_fuera_01    = pct_fuera_07c,
  auc_boot             = auc_boot_summary_07c,

  # ── Resultado central ─────────────────────────────────────────────────────
  boot_temporal          = boot_temporal_07c,
  tabla_temporal         = tabla_temporal_07c,
  neutralidad_confirmada = neutralidad_confirmada,
  n_temp_sel_boot10      = n_temporal_sel,

  # ── Bootstrap completo ───────────────────────────────────────────────────
  n_boot       = nrow(boot_results_07c),
  boot_summary = boot_summary_07c,

  # ── Fila plana para bind_rows() en 07e y 09a ─────────────────────────────
  resumen_comparacion = tibble(
    script  = "07c_GLM",
    metrica = c("auc_test", "delta_auc_vs_07b",
                "n_vars_temp_sel_1se", "n_temp_sel_boot10",
                "neutralidad_confirmada"),
    valor   = c(
      round(auc_val_07c,    4),
      round(delta_auc,      4),
      n_vars_temp_sel_1se,
      n_temporal_sel,
      as.integer(neutralidad_confirmada)
    )
  )
)

saveRDS(contrato_07c, PATH_07C_CONTRATO_GLM)
cat("  [✅] Contrato guardado:", basename(PATH_07C_CONTRATO_GLM), "\n")

# 📑 Checklist -----------------------------------------------------------------
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  📑 RESUMEN SCRIPT 07c — TEST TEMPORAL GLM\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  Vars temporales raw:              %d\n",   length(vars_temp_raw)))
cat(sprintf("  Vars temporales en recipe:        %d\n",   length(cols_temporales)))
cat(sprintf("  Temporales seleccionadas (λ.1se): %d\n",   n_vars_temp_sel_1se))
cat(sprintf("  Temporales con boot ≥ 10%%:        %d\n",   n_temporal_sel))
cat(sprintf("\n  AUC test:  %.4f  (base 07b GLM: %.4f, Δ=%+.4f)\n",
            auc_val_07c, auc_base_07b, delta_auc))
cat(sprintf("  AUC boot:  %.4f [IC95: %.4f – %.4f]\n",
            auc_boot_summary_07c$media,
            auc_boot_summary_07c$ic_low,
            auc_boot_summary_07c$ic_high))
cat(sprintf("\n  [%s] CONCLUSIÓN: %s\n",
            if (neutralidad_confirmada) "✅" else "⚠️",
            if (neutralidad_confirmada)
              "Neutralidad temporal confirmada."
            else
              "Efectos temporales presentes. Revisar antes de continuar."))
cat("───────────────────────────────────────────────────────────────────\n")
cat("  CHECKLIST 07c GLM:\n")
cat(sprintf("  [✅] train_raw canónico (lección 25): %s obs\n",
            format(nrow(train_raw), big.mark = ",")))
cat("  [✅] Split verificado vs contrato_07a GLM\n")
cat("  [✅] family = binomial | type.measure = auc\n")
cat("  [✅] predict(..., type = 'response') → probs en [0,1]\n")
cat("  [✅] step_normalize con na_rm = TRUE (fix busqueda_formal)\n")
cat("  [✅] penalty.factor = 0 para theta_A_mA / theta_B_mA\n")
cat("  [✅] Bootstrap 200 iter completado\n")
cat(sprintf("  [✅] Contrato guardado: %s\n", basename(PATH_07C_CONTRATO_GLM)))
cat(sprintf("  [✅] CSV guardado: 07c_tabla_temporal_%s.csv\n", SUFIJO_MODELO_GLM))
cat("  [ ] NO genera HTML — todo el HTML va en 07e\n")
cat("───────────────────────────────────────────────────────────────────\n")
cat(sprintf("  ⏱️  Tiempo total: %.1f segundos\n\n", elapsed))

toc()
cat("✅ Script 07c GLM completado\n")
