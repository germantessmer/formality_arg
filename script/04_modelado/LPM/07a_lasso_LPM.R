# =============================================================================
# [EN] 07a_lasso_LPM.R -- LASSO variable selection (LPM/gaussian) with unpenalized latent factors and bootstrap stability
# INPUTS:  rdos/datos/06_theta_predichos.rds, rdos/modelos/06_modelo_heterofactor.rds
# OUTPUTS: rdos/modelos/07_modelo_lasso_LPM*.rds, rdos/contratos/07a_contrato_lasso_LPM*.rds
# =============================================================================
# 🌟 07a_lasso_LPM.R 🌟 ####
# OBJETIVO:
#    Selección de variables mediante LASSO (LPM gaussiano) para predecir
#    formalidad laboral. Incluye θ_A y θ_B del Modelo A (heterofactor) con
#    penalty.factor = 0 (nunca regularizados). Bootstrap Nivel B para
#    estabilidad de selección e IC del AUC.
#
# INPUTS:
#    - PATH_06_THETA         → rdos/datos/06_theta_predichos.rds       (1,795,386 × 77)
#    - PATH_06_MODELO_HETERO → rdos/modelos/06_modelo_heterofactor.rds (θ Modelo A)
#
# OUTPUTS:
#    - PATH_07_MODELO_LASSO  → rdos/modelos/07_modelo_lasso_LPM3T.rds
#    - PATH_07_RECIPE_LASSO  → rdos/modelos/07_recipe_lasso_LPM3T.rds
#    - 07a_vars_sel_LPM3T.csv → rdos/reportes/
#    - PATH_07_CONTRATO      → rdos/contratos/07a_contrato_lasso_LPM3T.rds
#
# DECISIONES DE DISEÑO:
#    - θ del Modelo A (parsimonioso, 6/8 criterios ✅); renombrados theta_A_mA / theta_B_mA
#    - ingreso_real_capita_familiar EXCLUIDO (Opción A de 06d: θ_A capta esa dimensión)
#    - horas_trabajadas INCLUIDA (KB_04)
#    - emparejamiento_selectivo (nombre real del panel, ≡ assortative_mating del LEGACY)
#    - CV por cluster codusu (10 folds, anti-leakage intra-hogar)
#    - family = "gaussian" (LPM), type.measure = "mse"
#
# TIEMPO ESTIMADO: ~25–35 minutos (bootstrap 200 iter, paralelo)

# 📚 Librerias -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(glmnet)
  library(pROC)
  library(doParallel)
  library(foreach)
  library(tictoc)
  library(recipes)
})

# 🔧 Cargar configuracion y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 07a completo")
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  🚀 SCRIPT 07a — LASSO LPM\n")
cat(sprintf("  Sufijo de modelo: %s | N_TRIMESTRES_TRAINING: %d\n",
            SUFIJO_MODELO_LPM, N_TRIMESTRES_TRAINING))
cat(sprintf("  Inicio: %s\n", format(start_time, "%Y-%m-%d %H:%M:%S")))
cat("═══════════════════════════════════════════════════════════════════\n\n")

set.seed(SEED_GLOBAL)


# 🪫 1. Carga del panel y extraccion de θ del Modelo A -------------------------

cat("── 1. Carga de datos ───────────────────────────────────────────────\n")

# ── 1a. Panel con observables (θ del Modelo B viven aquí — NO se usan) ────────
hard_stop(file.exists(PATH_06_THETA),
          paste0("No existe PATH_06_THETA: ", PATH_06_THETA))

tic("Carga panel")
panel <- readRDS(PATH_06_THETA)
toc()
cat(sprintf("   Panel cargado: %s obs × %s vars\n",
            format(nrow(panel), big.mark = ","), ncol(panel)))

# ── 1b. Extracción de θ del Modelo A ──────────────────────────────────────────
hard_stop(file.exists(PATH_06_MODELO_HETERO),
          paste0("No existe PATH_06_MODELO_HETERO: ", PATH_06_MODELO_HETERO))

modelo_hetero <- readRDS(PATH_06_MODELO_HETERO)

hard_stop(!is.null(modelo_hetero$modelo_A),
          "modelo_hetero$modelo_A es NULL. Verificar estructura del .rds.")
hard_stop(!is.null(modelo_hetero$modelo_A$theta_data),
          "modelo_hetero$modelo_A$theta_data es NULL.")

theta_data_mA <- modelo_hetero$modelo_A$theta_data

# Validar columnas esperadas
hard_stop("id_individuo_hist" %in% names(theta_data_mA),
          "theta_data_mA no tiene columna id_individuo_hist")
hard_stop("periodo_id" %in% names(theta_data_mA),
          "theta_data_mA no tiene columna periodo_id")
hard_stop(any(c("theta_A", "thetaA", "theta_a") %in% names(theta_data_mA)),
          "theta_data_mA no tiene columna theta_A (ni variantes)")

# Renombrar: theta_A_mA / theta_B_mA para distinguir del Modelo B
theta_data_mA <- theta_data_mA %>%
  rename(
    theta_A_mA = any_of(c("theta_A", "thetaA", "theta_a")),
    theta_B_mA = any_of(c("theta_B", "thetaB", "theta_b"))
  ) %>%
  select(id_individuo_hist, periodo_id, theta_A_mA, theta_B_mA)

cat(sprintf("   theta_data_mA: %s obs × 4 cols\n",
            format(nrow(theta_data_mA), big.mark = ",")))
cat(sprintf("   Columnas θ: theta_A_mA (media=%.4f, SD=%.4f) | theta_B_mA (media=%.4f, SD=%.4f)\n",
            mean(theta_data_mA$theta_A_mA, na.rm = TRUE),
            sd(theta_data_mA$theta_A_mA,   na.rm = TRUE),
            mean(theta_data_mA$theta_B_mA, na.rm = TRUE),
            sd(theta_data_mA$theta_B_mA,   na.rm = TRUE)))

# ── 1c. Join al panel — CRÍTICO: por ambas columnas para evitar inflación ─────
# Lección 15: joinear solo por id_individuo_hist multiplica filas silenciosamente.
# Siempre usar c("id_individuo_hist", "periodo_id") con relationship = "many-to-one"
panel <- panel %>%
  left_join(theta_data_mA,
            by           = c("id_individuo_hist", "periodo_id"),
            relationship = "many-to-one")

n_con_tA <- sum(!is.na(panel$theta_A_mA))
pct_tA   <- n_con_tA / nrow(panel) * 100

cat(sprintf("   Panel post-join: %s obs × %s vars\n",
            format(nrow(panel), big.mark = ","), ncol(panel)))
cat(sprintf("   Cobertura theta_A_mA: %s (%.1f%%) — esperado ~79%%\n",
            format(n_con_tA, big.mark = ","), pct_tA))

if (pct_tA < 70) {
  warning(sprintf("⚠️  Cobertura θ_A_mA baja: %.1f%% < 70%%. Verificar join.", pct_tA))
}

rm(modelo_hetero, theta_data_mA)
gc(verbose = FALSE)


# 🪫 2. Preparacion del universo de entrenamiento ------------------------------

cat("\n── 2. Universo de entrenamiento ────────────────────────────────────\n")

# ── 2a. Variables del modelo ──────────────────────────────────────────────────
# Lista extendida (LEGACY + horas_trabajadas de KB_04)
# ingreso_real_capita_familiar EXCLUIDO (Opción A de 06d)
VARS_MODELO_07a <- c(
  # Meta-variables (no entran al recipe)
  "formalidad_empleo", "pondera", "codusu",
  # θ Modelo A — penalty.factor = 0 (nunca regularizados)
  "theta_A_mA", "theta_B_mA",
  # Demográficas
  "edad", "edad_cuadrado", "sexo", "estado_civil",
  "lugar_nacimiento", "parentesco",
  # Educativas
  "nivel_educ_obtenido2", "asistencia_escuela", "tipo_escuela", "alfabetizacion",
  # Geográficas
  # region EXCLUIDA: es función determinista de aglomerado (agrupación
  # administrativa INDEC sin diferencial económico, legal ni de mercado
  # laboral propio). Incluirla introduce redundancia en el recipe y puede
  # sesgar la selección del LASSO en favor de dummies de región a expensas
  # de dummies de aglomerado más granulares. (DEUDA-5, resuelta D30)
  "aglomerado", "mas_500",
  # Laborales
  "seccion", "calificacion", "antiguedad", "categoria_ocupacional",
  "horas_trabajadas",           # KB_04
  "trimestre",                  # regresor estacional (Q1-Q4)
  # Hogar y estructura
  "nbi", "miembros_hogar", "menores10", "mayores10",
  "principal_tareas_hogar", "otros_tareas_hogar",
  # Proxies heterofactor / ICH
  "ich_score", "residual_vivienda",
  "rezago_escolar_cohorte", "clima_educativo_hogar",
  "emparejamiento_selectivo",   # nombre real del panel
  "calificacion_norm", "entropia_estabilidad",
  "busqueda_formal",
  # Fuentes de vida del hogar
  "vive_alquiler", "vive_ganancias_negocio", "vive_renta_financiera",
  "vive_beca", "vive_cuota_alimenticia", "vive_ahorros",
  "vive_prestamos_personas", "vive_prestamos_financieros",
  "vive_financiamiento", "vive_venta_bienes", "vive_otro_ingreso"
)

# Verificar existencia — excluir faltantes en vez de hard_stop (LASSO es robusto)
vars_faltantes <- setdiff(VARS_MODELO_07a, names(panel))
if (length(vars_faltantes) > 0) {
  cat("   ⚠️  Variables NO encontradas en el panel:\n")
  cat("       ", paste(vars_faltantes, collapse = ", "), "\n")
  VARS_MODELO_07a <- intersect(VARS_MODELO_07a, names(panel))
  cat(sprintf("   → Continuando con %d variables\n", length(VARS_MODELO_07a)))
} else {
  cat(sprintf("   [✅] Las %d variables del listado están en el panel\n",
              length(VARS_MODELO_07a)))
}

# ── 2b. Filtro training universe ──────────────────────────────────────────────
train_raw <- panel %>%
  filter(
    condicion_actividad == "Ocupado",
    formalidad_empleo %in% c("Formal oficial", "Informal oficial"),
    periodo_id %in% TRIMESTRES_FORMALIDAD
  ) %>%
  select(all_of(VARS_MODELO_07a)) %>%
  mutate(
    formalidad_bin = as.integer(formalidad_empleo == "Formal oficial"),
    lugar_nacimiento = if ("lugar_nacimiento" %in% names(.)) {
      case_when(
        lugar_nacimiento %in% c("Localidad", "Provincia", "Otra provincia") ~ "Argentina",
        lugar_nacimiento == "País limítrofe" ~ "Pais_Limitrofe",
        lugar_nacimiento == "Otro país"      ~ "Otro_Pais",
        TRUE ~ lugar_nacimiento
      )
    } else { NA_character_ }
  )

n_train_universe <- nrow(train_raw)
hard_stop(n_train_universe > 10000,
          sprintf("N training universe muy bajo: %d. Verificar filtros.", n_train_universe))

cat(sprintf("   N training universe: %s (esperado ~61,531)\n",
            format(n_train_universe, big.mark = ",")))
cat(sprintf("   Cobertura θ_A_mA en training: %s (%.1f%%)\n",
            format(sum(!is.na(train_raw$theta_A_mA)), big.mark = ","),
            mean(!is.na(train_raw$theta_A_mA)) * 100))
cat(sprintf("   Balance: %s formales / %s informales (%.1f%% formal)\n",
            format(sum(train_raw$formalidad_bin == 1), big.mark = ","),
            format(sum(train_raw$formalidad_bin == 0), big.mark = ","),
            mean(train_raw$formalidad_bin) * 100))

rm(panel)
gc(verbose = FALSE)

# ── 2c. Split 80/20 estratificado ────────────────────────────────────────────
set.seed(SEED_GLOBAL)
idx_train <- c(
  sample(which(train_raw$formalidad_bin == 1),
         floor(0.80 * sum(train_raw$formalidad_bin == 1))),
  sample(which(train_raw$formalidad_bin == 0),
         floor(0.80 * sum(train_raw$formalidad_bin == 0)))
)

df_train <- train_raw[idx_train, ]
df_test  <- train_raw[-idx_train, ]

cat(sprintf("\n   Split 80/20 estratificado:\n"))
cat(sprintf("   Train: %s obs | %.1f%% formal\n",
            format(nrow(df_train), big.mark = ","),
            mean(df_train$formalidad_bin) * 100))
cat(sprintf("   Test:  %s obs | %.1f%% formal\n",
            format(nrow(df_test), big.mark = ","),
            mean(df_test$formalidad_bin) * 100))

rm(train_raw, idx_train)
gc(verbose = FALSE)


# 🪫 3. Recipe y matrices X / y ------------------------------------------------

cat("\n── 3. Recipe LASSO ─────────────────────────────────────────────────\n")

VARS_META     <- c("formalidad_empleo", "pondera", "codusu", "formalidad_bin")
VARS_PROT     <- intersect(c("theta_A_mA", "theta_B_mA", "edad", "edad_cuadrado"),
                           setdiff(VARS_MODELO_07a, VARS_META))

df_recipe      <- df_train %>% select(-pondera, -formalidad_empleo, -codusu)
df_test_recipe <- df_test  %>% select(-pondera, -formalidad_empleo, -codusu)

recipe_lasso_lpm <- recipe(formalidad_bin ~ ., data = df_recipe) %>%
  
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
  
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors(), -formalidad_bin) %>%
  step_normalize(all_numeric_predictors(), -formalidad_bin) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(),
            -any_of(VARS_PROT),
            threshold = 0.85)

cat("   Preparando recipe...\n")
tic("prep recipe")
recipe_prepped <- prep(recipe_lasso_lpm, training = df_recipe)
toc()
cat("   [✅] Recipe preparado\n")

bake_matrix <- function(recipe, df) {
  bake(recipe, new_data = df, composition = "matrix") %>%
    { .[, colnames(.) != "formalidad_bin", drop = FALSE] }
}

X_train <- bake_matrix(recipe_prepped, df_recipe)
y_train <- df_train$formalidad_bin
w_train <- df_train$pondera / mean(df_train$pondera)

X_test  <- bake_matrix(recipe_prepped, df_test_recipe)
y_test  <- df_test$formalidad_bin
w_test  <- df_test$pondera / mean(df_test$pondera)

cat(sprintf("   X_train: %s × %s | X_test: %s × %s\n",
            format(nrow(X_train), big.mark = ","), ncol(X_train),
            format(nrow(X_test),  big.mark = ","), ncol(X_test)))

pf <- rep(1, ncol(X_train))
theta_cols <- colnames(X_train) %in% c("theta_A_mA", "theta_B_mA")
pf[theta_cols] <- 0

cat(sprintf("   penalty.factor=0: %s\n",
            paste(colnames(X_train)[theta_cols], collapse = ", ")))
if (sum(theta_cols) == 0) {
  warning("⚠️  theta_A_mA / theta_B_mA NO encontradas en X_train. Verificar recipe.")
}

rm(df_recipe, df_test_recipe)
gc(verbose = FALSE)


# 🪫 4. LASSO con cross-validation (folds por cluster codusu) ------------------

cat("\n── 4. LASSO CV (folds por hogar) ───────────────────────────────────\n")

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

hard_stop(length(foldid_vec) == nrow(X_train),
          "foldid_vec y X_train tienen distinto número de filas.")

cat(sprintf("   Clusters únicos: %s | Tamaño medio: %.2f obs\n",
            format(n_clusters, big.mark = ","), nrow(df_train) / n_clusters))
cat(sprintf("   Cores: %d\n", N_CORES))

cl <- makeCluster(N_CORES)
registerDoParallel(cl)
cat("   Cluster activado. Corriendo cv.glmnet...\n")

tic("cv.glmnet")
cv_fit <- cv.glmnet(
  x              = X_train,
  y              = y_train,
  weights        = w_train,
  alpha          = 1,
  family         = "gaussian",
  penalty.factor = pf,
  type.measure   = "mse",
  foldid         = foldid_vec,
  parallel       = TRUE
)
stopCluster(cl)
toc()

lambda_1se <- cv_fit$lambda.1se
lambda_min <- cv_fit$lambda.min

cat(sprintf("   λ.min:  %.6f | MSE CV: %.4f\n",
            lambda_min, min(cv_fit$cvm)))
cat(sprintf("   λ.1se:  %.6f | MSE CV: %.4f\n",
            lambda_1se, cv_fit$cvm[cv_fit$lambda == lambda_1se]))

coef_1se <- coef(cv_fit, s = "lambda.1se")
vars_sel  <- data.frame(
  variable    = rownames(coef_1se),
  coeficiente = as.vector(coef_1se)
) %>%
  filter(variable != "(Intercept)", coeficiente != 0) %>%
  arrange(desc(abs(coeficiente)))

coef_min <- coef(cv_fit, s = "lambda.min")
vars_sel_min <- data.frame(
  variable    = rownames(coef_min),
  coeficiente = as.vector(coef_min)
) %>%
  filter(variable != "(Intercept)", coeficiente != 0) %>%
  arrange(desc(abs(coeficiente)))

cat(sprintf("\n   Variables seleccionadas (λ.1se): %d\n", nrow(vars_sel)))
cat(sprintf("   Variables seleccionadas (λ.min): %d\n",   nrow(vars_sel_min)))
cat(sprintf("   theta_A_mA: %s | theta_B_mA: %s\n",
            if ("theta_A_mA" %in% vars_sel$variable) "[✅] SÍ" else "[❌] NO",
            if ("theta_B_mA" %in% vars_sel$variable) "[✅] SÍ" else "[❌] NO"))


# 🪫 5. Evaluacion en test set -------------------------------------------------

cat("\n── 5. Evaluación en test ────────────────────────────────────────────\n")

pred_raw  <- as.vector(predict(cv_fit, newx = X_test, s = "lambda.1se"))
pred_clip <- pmax(0, pmin(1, pred_raw))
pct_fuera <- mean(pred_raw < 0 | pred_raw > 1) * 100

cat(sprintf("   Predicciones fuera de [0,1] (test):  %.2f%%\n", pct_fuera))
if (pct_fuera > 5) {
  warning(sprintf("⚠️  %.2f%% predicciones fuera de [0,1]. Documentar en paper.", pct_fuera))
} else {
  cat("   [✅] LPM dentro del umbral aceptable (<5%)\n")
}

# Predicciones fuera de [0,1] en el conjunto de ENTRENAMIENTO
# Necesario para la Tabla 45 del paper (calibración y admisibilidad)
pred_raw_train  <- as.vector(predict(cv_fit, newx = X_train, s = "lambda.1se"))
pct_fuera_train <- mean(pred_raw_train < 0 | pred_raw_train > 1) * 100
cat(sprintf("   Predicciones fuera de [0,1] (train): %.2f%%\n", pct_fuera_train))
rm(pred_raw_train)

roc_obj <- roc(response  = y_test,
               predictor = pred_clip,
               weights   = w_test,
               quiet     = TRUE)
auc_val  <- as.numeric(auc(roc_obj))

youden <- coords(roc_obj, "best", best.method = "youden", ret = "all")
umbral  <- youden$threshold[1]

cat(sprintf("   AUC (test, ponderado): %.4f\n", auc_val))
cat(sprintf("   Umbral Youden:         %.4f\n", umbral))

if (auc_val < 0.70) {
  warning("⚠️  AUC < 0.70. No alcanza criterio de aceptación mínima.")
} else {
  cat("   [✅] AUC supera criterio de aceptación (>0.70)\n")
}

pred_clase <- as.integer(pred_clip >= umbral)
cm   <- table(Real = y_test, Predicho = pred_clase)
acc  <- sum(diag(cm)) / sum(cm)
sens <- if (nrow(cm) >= 2 && ncol(cm) >= 2) cm[2, 2] / sum(cm[2, ]) else NA_real_
esp  <- if (nrow(cm) >= 2 && ncol(cm) >= 2) cm[1, 1] / sum(cm[1, ]) else NA_real_

cat("\n   Matriz de confusión:\n")
print(cm)
cat(sprintf("   Accuracy:              %.4f\n", acc))
cat(sprintf("   Sensibilidad (formal): %.4f\n", sens))
cat(sprintf("   Especificidad:         %.4f\n", esp))


# 🪫 6. Bootstrap Nivel B (200 iteraciones) ------------------------------------

cat("\n── 6. Bootstrap Nivel B (200 iter) ─────────────────────────────────\n")

N_BOOT       <- 200L
N_CORES_BOOT <- min(N_CORES, 15L)
cat(sprintf("   Cores para bootstrap: %d\n", N_CORES_BOOT))

cl_boot <- makeCluster(N_CORES_BOOT)
registerDoParallel(cl_boot)
clusterExport(cl_boot,
              c("X_train", "y_train", "w_train", "pf",
                "lambda_1se", "X_test", "y_test", "w_test"),
              envir = environment())

tic("Bootstrap")
boot_results <- foreach(
  i              = seq_len(N_BOOT),
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
                   alpha = 1, family = "gaussian",
                   penalty.factor = pf, lambda = lambda_1se)
  
  coef_b <- as.vector(coef(fit_b, s = lambda_1se))
  names(coef_b) <- rownames(coef(fit_b))
  
  pred_b <- pmax(0, pmin(1, as.vector(predict(fit_b, newx = X_test, s = lambda_1se))))
  auc_b  <- tryCatch(
    as.numeric(auc(roc(y_test, pred_b, weights = w_test, quiet = TRUE))),
    error = function(e) NA_real_
  )
  
  c(coef_b, auc_boot = auc_b)
}
stopCluster(cl_boot)
toc()

cat(sprintf("   Iteraciones completadas: %d / %d\n", nrow(boot_results), N_BOOT))

auc_boot_vec <- boot_results[, "auc_boot"]
coef_boot    <- boot_results[, colnames(boot_results) != "auc_boot", drop = FALSE]

boot_summary <- data.frame(
  variable          = colnames(coef_boot),
  seleccion_pct     = apply(coef_boot, 2, function(x) mean(x != 0) * 100),
  coef_media_global = apply(coef_boot, 2, mean),
  coef_media_cond   = apply(coef_boot, 2, function(x) {
    x_nz <- x[x != 0]; if (length(x_nz) == 0) NA_real_ else mean(x_nz)
  }),
  coef_sd      = apply(coef_boot, 2, sd),
  coef_ic_low  = apply(coef_boot, 2, quantile, probs = 0.025),
  coef_ic_high = apply(coef_boot, 2, quantile, probs = 0.975),
  row.names    = NULL
) %>%
  filter(variable != "(Intercept)") %>%
  arrange(desc(seleccion_pct))

auc_boot_summ <- list(
  media   = mean(auc_boot_vec, na.rm = TRUE),
  sd      = sd(auc_boot_vec,   na.rm = TRUE),
  ic_low  = quantile(auc_boot_vec, 0.025, na.rm = TRUE),
  ic_high = quantile(auc_boot_vec, 0.975, na.rm = TRUE)
)

vars_estables <- boot_summary %>% filter(seleccion_pct >= 80)

cat(sprintf("   AUC bootstrap: %.4f [IC95: %.4f – %.4f]\n",
            auc_boot_summ$media, auc_boot_summ$ic_low, auc_boot_summ$ic_high))
cat(sprintf("   Variables estables (≥80%%): %d\n", nrow(vars_estables)))

for (th in c("theta_A_mA", "theta_B_mA")) {
  fila <- boot_summary %>% filter(variable == th)
  if (nrow(fila) > 0) {
    cat(sprintf("   %s: %.1f%% bootstraps | β medio cond=%.4f\n",
                th, fila$seleccion_pct, fila$coef_media_cond))
  }
}


# 🪫 7. Outputs ----------------------------------------------------------------

cat("\n── 7. Guardando outputs ─────────────────────────────────────────────\n")

saveRDS(cv_fit,        PATH_07_MODELO_LASSO)
cat(sprintf("   [✅] %s\n", basename(PATH_07_MODELO_LASSO)))

saveRDS(recipe_prepped, PATH_07_RECIPE_LASSO)
cat(sprintf("   [✅] %s\n", basename(PATH_07_RECIPE_LASSO)))

path_vars_csv <- file.path(DIR_REPORTES, paste0("07a_vars_sel_", SUFIJO_MODELO_LPM, ".csv"))
vars_sel_export <- vars_sel %>%
  left_join(
    boot_summary %>%
      select(variable, seleccion_pct, coef_media_cond, coef_ic_low, coef_ic_high),
    by = "variable"
  ) %>%
  arrange(desc(abs(coeficiente)))
write_csv(vars_sel_export, path_vars_csv)
cat(sprintf("   [✅] %s\n", basename(path_vars_csv)))

contrato_07a <- list(
  script        = "07a_lasso_LPM.R",
  sufijo_modelo = SUFIJO_MODELO_LPM,
  fecha         = Sys.time(),
  input_panel   = basename(PATH_06_THETA),
  input_hetero  = basename(PATH_06_MODELO_HETERO),
  # Universo
  n_train             = nrow(df_train),
  n_test              = nrow(df_test),
  pct_formal_train    = mean(df_train$formalidad_bin),
  n_clusters_train    = n_clusters,
  tam_medio_cluster   = nrow(df_train) / n_clusters,
  cobertura_theta_pct = pct_tA,
  # Modelo
  lambda_1se          = lambda_1se,
  lambda_min          = lambda_min,
  n_vars_candidatas   = ncol(X_train),
  n_vars_sel_1se      = nrow(vars_sel),
  n_vars_sel_min      = nrow(vars_sel_min),
  # θ
  theta_A_mA_seleccionado = "theta_A_mA" %in% vars_sel$variable,
  theta_B_mA_seleccionado = "theta_B_mA" %in% vars_sel$variable,
  penalty_factor_theta    = 0,
  # Performance
  auc_test          = auc_val,
  umbral_youden     = umbral,
  accuracy          = acc,
  sensibilidad      = sens,
  especificidad     = esp,
  pct_pred_fuera_01       = pct_fuera,        # test set
  pct_pred_fuera_01_train = pct_fuera_train,  # train set (para Tabla 45 paper)
  # Bootstrap
  n_boot             = nrow(boot_results),
  auc_boot           = auc_boot_summ,
  n_vars_estables_80 = nrow(vars_estables),
  boot_summary       = boot_summary,
  # CV
  cv_foldid_por_cluster = TRUE,
  n_folds               = 10L,
  type_measure          = "mse",
  family                = "gaussian",
  # Decisiones de diseño
  ingreso_excluido      = TRUE,
  horas_trabajadas_incl = "horas_trabajadas" %in% VARS_MODELO_07a,
  emparejamiento_var    = "emparejamiento_selectivo",
  vars_seleccionadas    = vars_sel$variable
)
saveRDS(contrato_07a, PATH_07_CONTRATO)
cat(sprintf("   [✅] %s\n", basename(PATH_07_CONTRATO)))


# 📑 Resumen final -------------------------------------------------------------

end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  📋 RESUMEN SCRIPT 07a — LASSO LPM\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  Sufijo modelo:        %s\n",   SUFIJO_MODELO_LPM))
cat(sprintf("  N training:           %s\n",   format(nrow(df_train), big.mark = ",")))
cat(sprintf("  N test:               %s\n",   format(nrow(df_test),  big.mark = ",")))
cat(sprintf("  Variables candidatas: %d\n",   ncol(X_train)))
cat(sprintf("  Vars sel (λ.1se):     %d\n",   nrow(vars_sel)))
cat(sprintf("  Vars sel (λ.min):     %d\n",   nrow(vars_sel_min)))
cat(sprintf("  Vars estables (≥80%%): %d\n",  nrow(vars_estables)))
cat(sprintf("  AUC test:             %.4f\n", auc_val))
cat(sprintf("  AUC boot [IC95]:      %.4f [%.4f – %.4f]\n",
            auc_boot_summ$media, auc_boot_summ$ic_low, auc_boot_summ$ic_high))
cat(sprintf("  λ.1se:                %.6f\n", lambda_1se))
cat(sprintf("  θ_A_mA:               %s\n",
            if ("theta_A_mA" %in% vars_sel$variable) "[✅] SÍ" else "[❌] NO"))
cat(sprintf("  θ_B_mA:               %s\n",
            if ("theta_B_mA" %in% vars_sel$variable) "[✅] SÍ" else "[❌] NO"))
cat(sprintf("  Pred. fuera [0,1] test:  %.2f%%\n", pct_fuera))
cat(sprintf("  Pred. fuera [0,1] train: %.2f%%\n", pct_fuera_train))
cat(sprintf("  Bootstrap:            %d/%d iter\n", nrow(boot_results), N_BOOT))
cat(sprintf("  Tiempo total:         %.1f minutos\n", elapsed))
cat("\n  Top 10 variables por |coeficiente|:\n")
print(head(vars_sel %>% select(variable, coeficiente), 10), row.names = FALSE)
cat("\n  📑 CHECKLIST DE SALIDAS:\n")
cat(sprintf("  [%s] Modelo LASSO:  %s\n",
            if (file.exists(PATH_07_MODELO_LASSO)) "✅" else "❌",
            basename(PATH_07_MODELO_LASSO)))
cat(sprintf("  [%s] Recipe:        %s\n",
            if (file.exists(PATH_07_RECIPE_LASSO)) "✅" else "❌",
            basename(PATH_07_RECIPE_LASSO)))
cat(sprintf("  [%s] CSV vars sel:  %s\n",
            if (file.exists(path_vars_csv)) "✅" else "❌",
            basename(path_vars_csv)))
cat(sprintf("  [%s] Contrato 07a:  %s\n",
            if (file.exists(PATH_07_CONTRATO)) "✅" else "❌",
            basename(PATH_07_CONTRATO)))
cat("═══════════════════════════════════════════════════════════════════\n")

rm(df_train, df_test, X_train, X_test, y_train, y_test,
   w_train, w_test, coef_boot, boot_results)
gc(verbose = FALSE)

toc()
cat("\n✅ Script 07a completado exitosamente\n")