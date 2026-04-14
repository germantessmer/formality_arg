# =============================================================================
# [EN] 07d_lasso_interacciones_GLM.R -- Interaction robustness: test sector x occupation interactions via bootstrap LASSO (GLM)
# INPUTS:  rdos/datos/06_theta_predichos.rds, contracts from 07a/07b GLM
# OUTPUTS: rdos/contratos/07d_contrato_interacciones_GLM*.rds
# =============================================================================
# 🌟 07d_lasso_interacciones_GLM.R 🌟 ####
# CAPA 4 — MODELADO GLM | Test de interacciones seccion x categoria_ocupacional
#
# OBJETIVO: Evaluar si las interacciones seccion x categoria_ocupacional mejoran
#           las métricas respecto al modelo base 07b GLM. Las interacciones capturan
#           que el efecto sectorial no es aditivo con la categoría ocupacional
#           (ej: cuenta propia en Construcción != cuenta propia en Finanzas).
#
# DISENO:   Recipe base 07a + step_interact post-dummies
#           Interacciones: penalty.factor = 1 (LASSO decide libremente)
#           theta_A_mA / theta_B_mA: penalty.factor = 0 (igual que modelo base)
#           Bootstrap 200 iter para estabilidad de selección de interacciones
#           GLM post-LASSO (binomial) para comparación directa de Pseudo-R2 vs 07b
#
# RESULTADO ESPERADO: ninguna interacción estable (>=80% bootstrap) ->
#           justifica parsimonia del modelo base 07b
#
# INPUT:    PATH_06_THETA, PATH_06_MODELO_HETERO
#           PATH_07_CONTRATO_GLM (n_train/n_test)
#           PATH_07B_CONTRATO_GLM (métricas base)
# OUTPUTS:  PATH_07D_CONTRATO_GLM (.rds)
#           rdos/reportes/07d_comp_coefs_GLM3T.csv
#           rdos/reportes/07d_tabla_glm_GLM3T.txt
#
# LECCIONES APLICADAS:
#   25 — train_raw canónico con los 3 filtros exactos de 07a
#   26 — MCC con as.numeric() para evitar integer overflow
#   27 — col.names = FALSE en write.table con append
#   30 — emparejamiento_selectivo (no assortative_mating)
#
# DIFERENCIAS vs LPM:
#   - family = "binomial" (cv.glmnet, glmnet, glm post-LASSO)
#   - type.measure = "auc"
#   - predict(..., type = "response") -> probabilidades en [0,1]
#   - GLM post-LASSO en lugar de OLS: Pseudo-R2 McFadden, AIC, sin BP/RESET
#   - AUC_BASE_07B = leído de contrato_07b$resumen_comparacion (HC fallback: 0.8686)
#   - step_normalize con na_rm = TRUE (fix busqueda_formal)
#   - Paths y sufijos: GLM3T
#
# AUTOR: Proyecto EPH Formalidad | FECHA: 2026-03-06

# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(recipes)
  library(glmnet)
  library(pROC)
  library(PRROC)
  library(doParallel)
  library(foreach)
  library(car)
  library(lmtest)
  library(sandwich)
  library(broom)
  library(tictoc)
})

# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 07d GLM completo")
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  🌟 SCRIPT 07d — LASSO CON INTERACCIONES (GLM)\n")
cat("  Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("  DISEÑO: modelo base + interacciones seccion × categoria_ocupacional\n")
cat("          penalty.factor = 1 para interacciones (LASSO decide)\n")
cat("          penalty.factor = 0 para theta_A_mA / theta_B_mA\n\n")

set.seed(SEED_GLOBAL)

# 🪫 1. Carga de datos y θ (patrón canónico) -----------------------------------
cat("── FASE 1: Carga de datos ──────────────────────────────────────────\n")

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

# Cargar contratos de referencia GLM
contrato_07a <- readRDS(PATH_07_CONTRATO_GLM)
contrato_07b <- readRDS(PATH_07B_CONTRATO_GLM)
cat("  Contrato 07a GLM: n_train =", format(contrato_07a$n_train, big.mark = ","),
    "| n_test =", format(contrato_07a$n_test, big.mark = ","), "\n")
# Valores de referencia 07b GLM — lectura dinámica desde contrato (fuente primaria)
# HC como fallback en caso de que el contrato no esté disponible.
# HC documentado: valores validados al ejecutar 07b (ver ESTADO_PROYECTO_D48.md)
.AUC_BASE_FB        <- 0.8686   # HC fallback
.PSEUDO_R2_BASE_FB  <- 0.342    # HC documentado: fallback tryCatch — fuente primaria es rc$r2
.MCC_BASE_FB        <- 0.5763   # HC fallback
.F1_BASE_FB         <- 0.8069   # HC fallback

tryCatch({
  rc <- contrato_07b$resumen_comparacion   # WIDE: 1 fila (D47)
  AUC_BASE_07B       <- rc$auc_roc[1]     # WIDE: columna directa
  PSEUDO_R2_BASE_07B <- rc$r2[1]          # WIDE: pseudo-R2 McFadden
  F1_BASE_07B        <- rc$f1[1]          # WIDE: columna directa
  MCC_BASE_07B       <- rc$mcc[1]         # WIDE: columna directa
  cat("  Contrato 07b GLM: AUC =", round(AUC_BASE_07B, 4),
      "| Pseudo-R2 =", round(PSEUDO_R2_BASE_07B, 4),
      "| F1 =", round(F1_BASE_07B, 4),
      "| MCC =", round(MCC_BASE_07B, 4), "\n\n")
}, error = function(e) {
  warning(sprintf("[07d GLM] No se pudieron leer metricas del contrato 07b - usando HC fallback. Error: %s", e$message))
  AUC_BASE_07B       <<- .AUC_BASE_FB
  PSEUDO_R2_BASE_07B <<- .PSEUDO_R2_BASE_FB
  F1_BASE_07B        <<- .F1_BASE_FB
  MCC_BASE_07B       <<- .MCC_BASE_FB
  cat("  Contrato 07b GLM cargado (metricas desde HC fallback):",
      "AUC =", AUC_BASE_07B, "| Pseudo-R2 =", PSEUDO_R2_BASE_07B, "\n\n")
})

# 🪫 2. train_raw canónico (lección 25) + split 80/20 --------------------------
cat("── FASE 2: train_raw canónico + split 80/20 ────────────────────────\n")

VARS_MODELO <- c(
  "formalidad_empleo", "pondera", "codusu",
  "theta_A_mA", "theta_B_mA",
  "edad", "edad_cuadrado", "sexo", "estado_civil", "lugar_nacimiento", "parentesco",
  "nivel_educ_obtenido2", "asistencia_escuela", "tipo_escuela", "alfabetizacion",
  "aglomerado", "region", "mas_500",
  "seccion", "calificacion", "antiguedad", "categoria_ocupacional",
  "nbi", "miembros_hogar", "menores10", "mayores10",
  "principal_tareas_hogar", "otros_tareas_hogar",
  "ich_score", "residual_vivienda",
  "rezago_escolar_cohorte", "clima_educativo_hogar",
  "emparejamiento_selectivo", "calificacion_norm", "entropia_estabilidad",  # lección 30
  "vive_alquiler", "vive_ganancias_negocio", "vive_renta_financiera",
  "vive_beca", "vive_cuota_alimenticia", "vive_ahorros",
  "vive_prestamos_personas", "vive_prestamos_financieros",
  "vive_financiamiento", "vive_venta_bienes", "vive_otro_ingreso"
)

vars_faltantes <- setdiff(VARS_MODELO, names(panel))
if (length(vars_faltantes) > 0) {
  stop("Variables faltantes en el panel: ", paste(vars_faltantes, collapse = ", "))
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
    } else { NA_character_ }
  ) %>%
  select(all_of(c(VARS_MODELO, "formalidad_bin")))

cat("\n  N universo:", format(nrow(train_raw), big.mark = ","), "\n")
cat("  Balance:", format(sum(train_raw$formalidad_bin), big.mark = ","),
    "formales /", format(sum(train_raw$formalidad_bin == 0), big.mark = ","),
    "informales\n")

# Distribución cruzada seccion × categoria (anticipar sparsidad)
cat("\n  Celdas seccion × categoria_ocupacional con N más bajo:\n")
dist_cruce <- train_raw %>%
  count(seccion, categoria_ocupacional, name = "n") %>%
  arrange(n) %>%
  head(8)
print(dist_cruce)
cat(sprintf("  Total celdas únicas: %d\n",
            nrow(train_raw %>% distinct(seccion, categoria_ocupacional))))

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

# 🪫 3. Recipe con interacciones -----------------------------------------------
cat("\n── FASE 3: Recipe con interacciones ────────────────────────────────\n")
cat("  Estrategia: dummies base → step_interact → step_zv → step_corr\n")
cat("  Interacciones: seccion_* × categoria_ocupacional_*\n")

recipe_07d <- recipe(
    formalidad_bin ~ .,
    data = df_train %>% select(-pondera, -formalidad_empleo, -codusu)
  ) %>%

  # ── Limpieza Ns/Nr ──────────────────────────────────────────────────────
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

  # ── Dummies base (necesarias antes del step_interact) ──────────────────
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%

  # ── Interacciones seccion × categoria_ocupacional ──────────────────────
  # Se crean sobre las dummies ya generadas. LASSO las regulariza con pf=1.
  step_interact(
    terms = ~ starts_with("seccion_"):starts_with("categoria_ocupacional_")
  ) %>%

  # ── Eliminar varianza cero y colineales ────────────────────────────────
  step_zv(all_predictors()) %>%
  step_corr(
    all_numeric_predictors(),
    -any_of(c("theta_A_mA", "theta_B_mA", "edad", "edad_cuadrado")),
    threshold = 0.85
  )

cat("\n  Preparando recipe (puede tardar por interacciones)...\n")
tic("prep recipe 07d GLM")
recipe_prepped_07d <- prep(
  recipe_07d,
  training = df_train %>% select(-pondera, -formalidad_empleo, -codusu)
)
toc()

# Matrices
X_train <- bake(recipe_prepped_07d,
                new_data = df_train %>% select(-pondera, -formalidad_empleo, -codusu),
                composition = "matrix")
X_train <- X_train[, colnames(X_train) != "formalidad_bin"]

X_test  <- bake(recipe_prepped_07d,
                new_data = df_test %>% select(-pondera, -formalidad_empleo, -codusu),
                composition = "matrix")
X_test  <- X_test[, colnames(X_test) != "formalidad_bin"]

y_train <- df_train$formalidad_bin
w_train <- df_train$pondera / mean(df_train$pondera)
y_test  <- df_test$formalidad_bin
w_test  <- df_test$pondera  / mean(df_test$pondera)

cat("  Dimensiones X_train:", nrow(X_train), "×", ncol(X_train), "\n")
cat("  Dimensiones X_test: ", nrow(X_test),  "×", ncol(X_test),  "\n")

# Identificar columnas de interacción (prefijo generado por step_interact)
cols_interact <- colnames(X_train)[
  grepl("^seccion_.*_x_categoria_ocupacional_", colnames(X_train))
]
cat("\n  Columnas de interacción generadas:", length(cols_interact), "\n")
cat("  Columnas base (sin interacciones):",
    ncol(X_train) - length(cols_interact), "\n")

# ── penalty.factor: 0 para θ, 1 para todo lo demás ─────────────────────────
pf <- rep(1, ncol(X_train))
theta_cols_idx <- colnames(X_train) %in% c("theta_A_mA", "theta_B_mA")
pf[theta_cols_idx] <- 0

if (sum(theta_cols_idx) == 0) {
  warning("⚠️ theta_A_mA / theta_B_mA NO encontradas en X_train. Verificar recipe.")
}
cat("\n  penalty.factor = 0 para:",
    paste(colnames(X_train)[theta_cols_idx], collapse = ", "), "\n")
cat("  penalty.factor = 1 para interacciones\n")

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

tic("cv.glmnet 07d GLM")
cv_fit_07d <- cv.glmnet(
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

lambda_1se_07d <- cv_fit_07d$lambda.1se
lambda_min_07d <- cv_fit_07d$lambda.min

cat("\n  λ.min:", round(lambda_min_07d, 6),
    "| AUC CV:", round(max(cv_fit_07d$cvm), 4), "\n")
cat("  λ.1se:", round(lambda_1se_07d, 6),
    "| AUC CV:", round(cv_fit_07d$cvm[cv_fit_07d$lambda == lambda_1se_07d], 4), "\n")

# Coeficientes λ.1se
coef_1se_07d <- coef(cv_fit_07d, s = "lambda.1se")
vars_sel_07d <- data.frame(
  variable    = rownames(coef_1se_07d),
  coeficiente = as.vector(coef_1se_07d)
) %>%
  filter(variable != "(Intercept)", coeficiente != 0) %>%
  arrange(desc(abs(coeficiente)))

vars_sel_min_07d <- data.frame(
  variable    = rownames(coef(cv_fit_07d, s = "lambda.min")),
  coeficiente = as.vector(coef(cv_fit_07d, s = "lambda.min"))
) %>%
  filter(variable != "(Intercept)", coeficiente != 0)

n_interact_sel_1se <- sum(vars_sel_07d$variable %in% cols_interact)

cat("\n  Variables seleccionadas (λ.1se):", nrow(vars_sel_07d), "\n")
cat("  ├ Interacciones seleccionadas:  ", n_interact_sel_1se, "/",
    length(cols_interact), "\n")
cat("  └ Variables base seleccionadas: ",
    nrow(vars_sel_07d) - n_interact_sel_1se, "\n")
cat("  theta_A_mA sel:", ifelse("theta_A_mA" %in% vars_sel_07d$variable, "✅ SÍ", "❌ NO"), "\n")
cat("  theta_B_mA sel:", ifelse("theta_B_mA" %in% vars_sel_07d$variable, "✅ SÍ", "❌ NO"), "\n")
cat("  Variables seleccionadas (λ.min):", nrow(vars_sel_min_07d), "\n")

if (n_interact_sel_1se > 0) {
  cat("\n  Interacciones seleccionadas (λ.1se — log-odds):\n")
  print(vars_sel_07d %>%
          filter(variable %in% cols_interact) %>%
          select(variable, coeficiente))
}

# 🪫 5. Evaluación en test -----------------------------------------------------
cat("\n── FASE 5: Evaluación en test ──────────────────────────────────────\n")

# GLM: type = "response" → probabilidades en [0,1] directamente
pred_raw_07d  <- as.vector(predict(cv_fit_07d, newx = X_test,
                                   s = "lambda.1se", type = "response"))
pred_clip_07d <- pmax(0, pmin(1, pred_raw_07d))

pct_fuera_07d <- mean(pred_raw_07d < 0 | pred_raw_07d > 1) * 100
cat(sprintf("  Predicciones fuera de [0,1]: %.2f%%\n", pct_fuera_07d))

roc_obj_07d <- roc(response  = y_test,
                   predictor = pred_clip_07d,
                   weights   = w_test,
                   ci        = TRUE,
                   ci.method = "delong",
                   quiet     = TRUE)
auc_val_07d <- as.numeric(auc(roc_obj_07d))
auc_ci_07d  <- as.numeric(ci(roc_obj_07d))

cat(sprintf("\n  AUC test (07d inter.):  %.4f [IC95: %.4f – %.4f]\n",
            auc_val_07d, auc_ci_07d[1], auc_ci_07d[3]))
cat(sprintf("  AUC modelo base (07b):  %.4f\n", AUC_BASE_07B))
cat(sprintf("  Δ AUC vs 07b:           %+.4f\n", auc_val_07d - AUC_BASE_07B))

# Umbral Youden y curva ROC
youden_07d <- coords(roc_obj_07d, "best", best.method = "youden", ret = "all")
umbral_07d <- youden_07d$threshold[1]

roc_df_07d <- data.frame(
  especificidad = roc_obj_07d$specificities,
  sensibilidad  = roc_obj_07d$sensitivities
)

# AUC-PR
pr_obj_07d <- pr.curve(
  scores.class0 = pred_clip_07d[y_test == 1],
  scores.class1 = pred_clip_07d[y_test == 0],
  curve = TRUE
)
pr_auc_07d <- pr_obj_07d$auc.integral
pr_df_07d  <- as.data.frame(pr_obj_07d$curve) %>%
  setNames(c("recall", "precision", "threshold"))

# Matriz de confusión y métricas
pred_clase_07d <- as.integer(pred_clip_07d >= umbral_07d)
cm_07d <- table(Real = y_test, Predicho = pred_clase_07d)
cat("\n  Matriz de confusión:\n")
print(cm_07d)

tp <- cm_07d[2,2]; tn <- cm_07d[1,1]
fp <- cm_07d[1,2]; fn <- cm_07d[2,1]
n  <- sum(cm_07d)

acc_07d  <- (tp + tn) / n
sens_07d <- tp / (tp + fn)
esp_07d  <- tn / (tn + fp)
ppv_07d  <- tp / (tp + fp)
npv_07d  <- tn / (tn + fn)
f1_07d   <- 2 * ppv_07d * sens_07d / (ppv_07d + sens_07d)

# MCC con as.numeric() — lección 26
mcc_07d <- (as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)) /
  sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) *
         as.numeric(tn + fp) * as.numeric(tn + fn))

p_obs_07d <- acc_07d
p_exp_07d <- ((tp+fn)/n)*((tp+fp)/n) + ((tn+fp)/n)*((tn+fn)/n)
kappa_07d <- (p_obs_07d - p_exp_07d) / (1 - p_exp_07d)

metricas_clf_07d <- tibble(
  Metrica   = c("AUC-ROC", "AUC-PR", "Accuracy",
                "Sensibilidad", "Especificidad", "Precision (PPV)", "NPV",
                "F1-Score", "MCC", "Cohen Kappa", "Umbral Youden"),
  Valor_07d = round(c(auc_val_07d, pr_auc_07d, acc_07d,
                      sens_07d, esp_07d, ppv_07d, npv_07d,
                      f1_07d, mcc_07d, kappa_07d, umbral_07d), 4),
  Valor_07b = c(AUC_BASE_07B, 0.8819, 0.7889,
                0.7805, 0.7999, NA_real_, NA_real_,
                F1_BASE_07B, MCC_BASE_07B, 0.5747, 0.5606),
  Delta     = round(Valor_07d - Valor_07b, 4)
)
cat("\n  Métricas de clasificación (07d GLM vs 07b GLM):\n")
print(metricas_clf_07d, n = Inf)

# Hosmer-Lemeshow
hl_df_07d <- data.frame(pred = pred_clip_07d, real = y_test) %>%
  mutate(grupo = ntile(pred, 10)) %>%
  group_by(grupo) %>%
  summarise(n = n(), obs_mean = mean(real), pred_mean = mean(pred), .groups = "drop")
hl_stat_07d <- sum(hl_df_07d$n * (hl_df_07d$obs_mean - hl_df_07d$pred_mean)^2 /
                     (hl_df_07d$pred_mean * (1 - hl_df_07d$pred_mean)))
hl_pval_07d <- pchisq(hl_stat_07d, df = 8, lower.tail = FALSE)
cat(sprintf("\n  Hosmer-Lemeshow: χ²=%.3f, p=%s\n",
            hl_stat_07d, format.pval(hl_pval_07d, digits = 3)))

# 🪫 6. Bootstrap (200 iter — estabilidad de interacciones) --------------------
cat("\n── FASE 6: Bootstrap 200 iteraciones ──────────────────────────────\n")

N_BOOT_07d    <- 200L
N_CORES_BOOT  <- min(parallel::detectCores(logical = FALSE) - 1L, 15L)
cat("  Cores para bootstrap:", N_CORES_BOOT, "\n")

cl_boot <- makeCluster(N_CORES_BOOT)
registerDoParallel(cl_boot)
clusterExport(cl_boot,
              c("X_train", "y_train", "w_train", "pf",
                "lambda_1se_07d", "X_test", "y_test", "w_test"),
              envir = environment())

tic("Bootstrap 07d GLM")
boot_results_07d <- foreach(
  i              = seq_len(N_BOOT_07d),
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
                   lambda = lambda_1se_07d)
  coef_b        <- as.vector(coef(fit_b, s = lambda_1se_07d))
  names(coef_b) <- rownames(coef(fit_b))
  # GLM: type = "response" → probabilidades directas
  pred_b  <- pmax(0, pmin(1, as.vector(predict(fit_b, newx = X_test,
                                                s = lambda_1se_07d,
                                                type = "response"))))
  auc_b   <- tryCatch(
    as.numeric(auc(roc(y_test, pred_b, weights = w_test, quiet = TRUE))),
    error = function(e) NA_real_
  )
  c(coef_b, auc_boot = auc_b)
}
stopCluster(cl_boot)
toc()

cat("  Iteraciones completadas:", nrow(boot_results_07d), "/", N_BOOT_07d, "\n")

auc_boot_vec_07d <- boot_results_07d[, "auc_boot"]
coef_boot_07d    <- boot_results_07d[, colnames(boot_results_07d) != "auc_boot"]

boot_summary_07d <- data.frame(
  variable          = colnames(coef_boot_07d),
  seleccion_pct     = apply(coef_boot_07d, 2, function(x) mean(x != 0) * 100),
  coef_media_global = apply(coef_boot_07d, 2, mean),
  coef_media_cond   = apply(coef_boot_07d, 2, function(x) {
    x_nz <- x[x != 0]; if (length(x_nz) == 0) NA_real_ else mean(x_nz)
  }),
  coef_sd       = apply(coef_boot_07d, 2, sd),
  coef_ic_low   = apply(coef_boot_07d, 2, quantile, probs = 0.025),
  coef_ic_high  = apply(coef_boot_07d, 2, quantile, probs = 0.975),
  row.names     = NULL
) %>%
  filter(variable != "(Intercept)") %>%
  arrange(desc(seleccion_pct))

auc_boot_summary_07d <- list(
  media   = mean(auc_boot_vec_07d, na.rm = TRUE),
  sd      = sd(auc_boot_vec_07d,   na.rm = TRUE),
  ic_low  = quantile(auc_boot_vec_07d, 0.025, na.rm = TRUE),
  ic_high = quantile(auc_boot_vec_07d, 0.975, na.rm = TRUE)
)

cat(sprintf("\n  AUC bootstrap: %.4f [IC95: %.4f – %.4f]\n",
            auc_boot_summary_07d$media,
            auc_boot_summary_07d$ic_low,
            auc_boot_summary_07d$ic_high))
cat("  Variables estables (≥80% bootstrap):",
    sum(boot_summary_07d$seleccion_pct >= 80), "\n")

# Interacciones en bootstrap — resultado central
boot_interact_07d  <- boot_summary_07d %>% filter(variable %in% cols_interact)
n_interact_estable <- sum(boot_interact_07d$seleccion_pct >= 80, na.rm = TRUE)
n_interact_sel_10  <- sum(boot_interact_07d$seleccion_pct >= 10, na.rm = TRUE)

cat(sprintf("  Interacciones estables (≥80%% bootstrap): %d / %d\n",
            n_interact_estable, length(cols_interact)))
cat(sprintf("  Interacciones con selección ≥10%% bootstrap: %d / %d\n",
            n_interact_sel_10, length(cols_interact)))

# θ en bootstrap
for (th in c("theta_A_mA", "theta_B_mA")) {
  fila <- boot_summary_07d %>% filter(variable == th)
  if (nrow(fila) > 0)
    cat(sprintf("  %s: %.1f%% bootstraps | β med cond = %.4f\n",
                th, fila$seleccion_pct, fila$coef_media_cond))
}

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  RESULTADO CENTRAL — Interacciones en bootstrap (top 10 por selección):\n")
cat("═══════════════════════════════════════════════════════════════════\n")
if (nrow(boot_interact_07d) > 0) {
  print(boot_interact_07d %>%
          arrange(desc(seleccion_pct)) %>%
          head(10) %>%
          select(variable, seleccion_pct, coef_media_cond, coef_ic_low, coef_ic_high),
        digits = 4)
} else {
  cat("  (ninguna interacción sobrevivió al recipe)\n")
}

# 🪫 7. GLM post-LASSO (comparación directa con 07b) --------------------------
cat("\n── FASE 7: GLM post-LASSO ──────────────────────────────────────────\n")
cat("  Modelo: glm(family = binomial) con vars seleccionadas por λ.1se\n")
cat("  Errores: robustos HC1 clusterizados por codusu\n")

vars_sel_1se_07d  <- vars_sel_07d$variable
vars_disponibles  <- intersect(vars_sel_1se_07d, colnames(X_train))
vars_ausentes_glm <- setdiff(vars_sel_1se_07d, colnames(X_train))
if (length(vars_ausentes_glm) > 0)
  cat("  ⚠️ Variables ausentes en X_train:",
      paste(vars_ausentes_glm, collapse = ", "), "\n")

df_glm_07d <- as.data.frame(X_train) %>%
  select(any_of(vars_sel_1se_07d)) %>%
  mutate(
    formalidad_bin = y_train,
    codusu         = df_train$codusu
  )

formula_glm_07d <- as.formula(
  paste("formalidad_bin ~", paste(vars_disponibles, collapse = " + "))
)

tic("glm post-LASSO 07d")
m_glm_07d <- glm(formula_glm_07d, data = df_glm_07d, family = binomial(link = "logit"))
toc()

# Errores robustos HC1 clusterizados por codusu
vcov_cl_07d      <- vcovCL(m_glm_07d, cluster = ~codusu, type = "HC1")
test_robusto_07d <- coeftest(m_glm_07d, vcov = vcov_cl_07d)
tabla_glm_07d    <- tidy(test_robusto_07d) %>%
  mutate(
    significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )

# Pseudo-R² McFadden
ll_modelo_07d <- as.numeric(logLik(m_glm_07d))
m_nulo_07d    <- glm(formalidad_bin ~ 1, data = df_glm_07d,
                     family = binomial(link = "logit"))
ll_nulo_07d   <- as.numeric(logLik(m_nulo_07d))
pseudo_r2_07d <- 1 - ll_modelo_07d / ll_nulo_07d
aic_07d       <- AIC(m_glm_07d)
n_obs_07d     <- nrow(df_glm_07d)

cat(sprintf("  Pseudo-R² McFadden: %.4f | Base 07b: %.4f | Δ=%+.4f\n",
            pseudo_r2_07d, PSEUDO_R2_BASE_07B,
            pseudo_r2_07d - PSEUDO_R2_BASE_07B))
cat(sprintf("  AIC: %.1f\n", aic_07d))
cat(sprintf("  N obs: %s | Variables: %d\n",
            format(n_obs_07d, big.mark = ","), length(vars_disponibles)))

# VIF (solo para variables base, no interacciones — puede fallar con multicolinealidad alta)
vif_vals_07d <- tryCatch({
  v <- car::vif(m_glm_07d)
  if (is.matrix(v)) {
    data.frame(variable = rownames(v), GVIF = v[, "GVIF"],
               df = v[, "Df"], GVIF_adj = v[, "GVIF^(1/(2*Df))"])
  } else {
    data.frame(variable = names(v), GVIF = v, df = 1, GVIF_adj = sqrt(v))
  }
}, error = function(e) {
  cat("  ⚠️ VIF no calculable:", conditionMessage(e), "\n"); NULL
})

if (!is.null(vif_vals_07d)) {
  n_vif_alto_07d <- sum(vif_vals_07d$GVIF_adj > 3.16)
  cat(sprintf("  VIF_adj > 3.16: %d variables (base 07b: 4)\n", n_vif_alto_07d))
  tabla_glm_07d <- tabla_glm_07d %>%
    left_join(vif_vals_07d %>% select(variable, GVIF_adj),
              by = c("term" = "variable"))
} else {
  n_vif_alto_07d <- NA_integer_
  tabla_glm_07d  <- tabla_glm_07d %>% mutate(GVIF_adj = NA_real_)
}

# 🪫 8. Outputs ----------------------------------------------------------------
cat("\n── FASE 8: Guardando outputs ────────────────────────────────────────\n")

# 1. CSV — tabla comparativa LASSO vs GLM post-LASSO (con flag de interacción)
coef_lasso_df_07d <- data.frame(
  variable   = vars_sel_1se_07d,
  coef_lasso = as.vector(coef_1se_07d)[
    match(vars_sel_1se_07d, rownames(coef_1se_07d))
  ]
)
coef_glm_df_07d <- tabla_glm_07d %>%
  filter(term != "(Intercept)") %>%
  select(variable = term, coef_glm = estimate, se_glm = std.error,
         p_glm = p.value, sig_glm = significancia)
comp_coefs_07d <- coef_lasso_df_07d %>%
  left_join(coef_glm_df_07d, by = "variable") %>%
  left_join(
    boot_summary_07d %>% select(variable, seleccion_pct, coef_ic_low, coef_ic_high),
    by = "variable"
  ) %>%
  mutate(es_interaccion = variable %in% cols_interact) %>%
  arrange(desc(abs(coef_lasso)))

path_csv_07d <- file.path(DIR_REPORTES,
                          paste0("07d_comp_coefs_", SUFIJO_MODELO_GLM, ".csv"))
write_csv(comp_coefs_07d, path_csv_07d)
cat("  [✅] 07d_comp_coefs_", SUFIJO_MODELO_GLM, ".csv guardado\n", sep = "")

# 2. TXT — tabla GLM con encabezado (lección 27: col.names = FALSE)
tabla_glm_txt_07d <- tabla_glm_07d %>%
  mutate(
    across(c(estimate, std.error, statistic), ~ round(., 6)),
    p.value  = round(p.value, 4),
    GVIF_adj = round(GVIF_adj, 3)
  ) %>%
  rename(Variable = term, Coef_HC1 = estimate, SE_robusto = std.error,
         z_stat = statistic, p_valor = p.value,
         Significancia = significancia, VIF_adj = GVIF_adj) %>%
  arrange(p_valor)

encabezado_07d <- c(
  "================================================================",
  paste0("MODELO GLM POST-LASSO 07d — CON INTERACCIONES seccion x categoria"),
  paste0("Sufijo modelo: ", SUFIJO_MODELO_GLM),
  paste0("Fecha: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "================================================================",
  "Variable dependiente: P(Formalidad = 1) — logit binomial",
  "Errores estándar: Robustos HC1 clusterizados por hogar (codusu)",
  paste0("N observaciones:  ", format(n_obs_07d, big.mark = ",")),
  paste0("Variables modelo: ", length(vars_disponibles),
         "  (base 07b: 86)"),
  paste0("Pseudo-R² McFadden: ", round(pseudo_r2_07d, 4),
         "  (base 07b: ", PSEUDO_R2_BASE_07B, ")"),
  paste0("AIC: ", round(aic_07d, 1)),
  paste0("Hosmer-Lemeshow:    χ²=", round(hl_stat_07d, 3),
         " p = ", format.pval(hl_pval_07d, digits = 3)),
  paste0("VIF_adj > 3.16:   ", ifelse(is.na(n_vif_alto_07d), "N/A", n_vif_alto_07d),
         " variables  (base 07b: 4)"),
  "Sig: *** p<0.001 | ** p<0.01 | * p<0.05 | . p<0.10",
  "================================================================", ""
)

path_txt_07d <- file.path(DIR_REPORTES,
                          paste0("07d_tabla_glm_", SUFIJO_MODELO_GLM, ".txt"))
writeLines(encabezado_07d, path_txt_07d)
write.table(tabla_glm_txt_07d, path_txt_07d,
            sep = "\t", row.names = FALSE,
            quote = FALSE, na = "", append = TRUE,
            col.names = FALSE)    # lección 27
cat("  [✅] 07d_tabla_glm_", SUFIJO_MODELO_GLM, ".txt guardado\n", sep = "")

# 3. Contrato 07d GLM
contrato_07d <- list(
  # ── Identificación ───────────────────────────────────────────────────────
  script      = "07d_lasso_interacciones_GLM.R",
  sufijo      = SUFIJO_MODELO_GLM,
  fecha       = Sys.time(),
  version_tag = "v1_interacciones_GLM",

  # ── Universo ─────────────────────────────────────────────────────────────
  n_train          = nrow(df_train),
  n_test           = nrow(df_test),
  n_clusters_train = n_clusters,
  cv_foldid_por_cluster = TRUE,

  # ── Interacciones ─────────────────────────────────────────────────────────
  n_interact_candidatas  = length(cols_interact),
  n_interact_sel_1se     = n_interact_sel_1se,
  n_interact_sel_boot10  = n_interact_sel_10,
  n_interact_estable     = n_interact_estable,
  cols_interact_sel      = vars_sel_07d$variable[vars_sel_07d$variable %in% cols_interact],

  # ── Modelo LASSO ──────────────────────────────────────────────────────────
  lambda_1se        = lambda_1se_07d,
  lambda_min        = lambda_min_07d,
  family            = "binomial",
  type_measure      = "auc",
  n_vars_candidatas = ncol(X_train),
  n_vars_sel_1se    = nrow(vars_sel_07d),
  n_vars_sel_min    = nrow(vars_sel_min_07d),
  theta_A_sel       = "theta_A_mA" %in% vars_sel_07d$variable,
  theta_B_sel       = "theta_B_mA" %in% vars_sel_07d$variable,

  # ── Performance LASSO ─────────────────────────────────────────────────────
  auc_test          = auc_val_07d,
  auc_base_07b      = AUC_BASE_07B,
  delta_auc_vs_07b  = auc_val_07d - AUC_BASE_07B,
  auc_boot          = auc_boot_summary_07d,
  pct_pred_fuera_01 = pct_fuera_07d,
  umbral_youden     = umbral_07d,
  hl_stat           = hl_stat_07d,
  hl_pval           = hl_pval_07d,
  metricas_clf      = metricas_clf_07d,

  # ── GLM post-LASSO ────────────────────────────────────────────────────────
  glm_pseudo_r2       = pseudo_r2_07d,
  glm_pseudo_r2_base  = PSEUDO_R2_BASE_07B,
  delta_pseudo_r2     = pseudo_r2_07d - PSEUDO_R2_BASE_07B,
  glm_aic             = aic_07d,
  glm_n_vars          = length(vars_disponibles),
  glm_hl_stat         = hl_stat_07d,
  glm_hl_pval         = hl_pval_07d,
  n_vif_alto          = n_vif_alto_07d,
  vcov_type           = "vcovCL",
  cluster_var         = "codusu",

  # ── Tablas ────────────────────────────────────────────────────────────────
  tabla_glm     = tabla_glm_07d,
  vif_tabla     = vif_vals_07d,
  comp_coefs    = comp_coefs_07d,
  boot_summary  = boot_summary_07d,
  boot_interact = boot_interact_07d,
  hl_df         = hl_df_07d,
  roc_df        = roc_df_07d,
  pr_df         = pr_df_07d,

  # ── Fila plana para bind_rows() en 07e y 09a ─────────────────────────────
  resumen_comparacion = tibble(
    script    = "07d_GLM",
    metrica   = c("auc_test", "delta_auc_vs_07b",
                  "pseudo_r2", "delta_pseudo_r2",
                  "n_vars_glm", "n_interact_sel_1se",
                  "n_interact_sel_boot10", "n_interact_estable",
                  "mcc", "f1"),
    valor     = c(
      round(auc_val_07d,                              4),
      round(auc_val_07d - AUC_BASE_07B,              4),
      round(pseudo_r2_07d,                            4),
      round(pseudo_r2_07d - PSEUDO_R2_BASE_07B,       4),
      length(vars_disponibles),
      n_interact_sel_1se,
      n_interact_sel_10,
      n_interact_estable,
      round(mcc_07d,  4),
      round(f1_07d,   4)
    )
  )
)

saveRDS(contrato_07d, PATH_07D_CONTRATO_GLM)
cat("  [✅] Contrato guardado:", basename(PATH_07D_CONTRATO_GLM), "\n")

# 📑 Checklist -----------------------------------------------------------------
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  📑 RESUMEN SCRIPT 07d — INTERACCIONES vs BASE 07b (GLM)\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  %-32s %10s %10s %10s\n", "Métrica", "Base 07b", "07d Inter.", "Δ"))
cat(sprintf("  %s\n", strrep("─", 64)))

metricas_comp <- list(
  list("Vars candidatas",        116,                ncol(X_train),              NA),
  list("Vars sel (λ.1se)",       86,                 nrow(vars_sel_07d),         NA),
  list("  └ Interacc. sel",      0,                  n_interact_sel_1se,         NA),
  list("  └ Interacc. estables", 0,                  n_interact_estable,         NA),
  list("Pseudo-R² McFadden",     PSEUDO_R2_BASE_07B, pseudo_r2_07d,              pseudo_r2_07d - PSEUDO_R2_BASE_07B),
  list("AUC test",               AUC_BASE_07B,       auc_val_07d,                auc_val_07d - AUC_BASE_07B),
  list("AUC boot (media)",       0.8631,             auc_boot_summary_07d$media, auc_boot_summary_07d$media - 0.8631),
  list("F1",                     F1_BASE_07B,        f1_07d,                     f1_07d - F1_BASE_07B),
  list("MCC",                    MCC_BASE_07B,       mcc_07d,                    mcc_07d - MCC_BASE_07B),
  list("VIF_adj > 3.16",         4,                  ifelse(is.na(n_vif_alto_07d), NA, n_vif_alto_07d), NA)
)

for (m in metricas_comp) {
  if (is.na(m[[4]])) {
    cat(sprintf("  %-32s %10s %10s\n",
                m[[1]], as.character(m[[2]]), as.character(m[[3]])))
  } else {
    cat(sprintf("  %-32s %10.4f %10.4f %+10.4f\n",
                m[[1]], as.numeric(m[[2]]), as.numeric(m[[3]]), as.numeric(m[[4]])))
  }
}

cat(sprintf("\n  %s\n", strrep("─", 64)))
if (n_interact_estable == 0) {
  cat("  ✅ PARSIMONIA CONFIRMADA: ninguna interacción estable (≥80% bootstrap).\n")
  cat("     Las interacciones no aportan señal robusta.\n")
  cat("     CONCLUSIÓN: mantener modelo base 07b GLM.\n")
} else {
  cat(sprintf("  ⚠️  %d interacción(es) estable(s) en bootstrap.\n", n_interact_estable))
  if (auc_val_07d > AUC_BASE_07B) {
    cat(sprintf("     AUC mejora %+.4f respecto a 07b. Evaluar adopción.\n",
                auc_val_07d - AUC_BASE_07B))
  }
}

cat("───────────────────────────────────────────────────────────────────\n")
cat("  CHECKLIST 07d GLM:\n")
cat(sprintf("  [✅] train_raw canónico (lección 25): %s obs\n",
            format(nrow(train_raw), big.mark = ",")))
cat("  [✅] Split verificado vs contrato_07a GLM\n")
cat("  [✅] family = binomial | type.measure = auc\n")
cat("  [✅] predict(..., type = 'response') → probs en [0,1]\n")
cat("  [✅] step_normalize con na_rm = TRUE (fix busqueda_formal)\n")
cat("  [✅] emparejamiento_selectivo (lección 30)\n")
cat("  [✅] MCC con as.numeric() (lección 26)\n")
cat("  [✅] col.names = FALSE en write.table (lección 27)\n")
cat("  [✅] penalty.factor = 0 para theta_A_mA / theta_B_mA\n")
cat("  [✅] Bootstrap 200 iter completado\n")
cat("  [✅] GLM post-LASSO: Pseudo-R² McFadden + errores robustos HC1\n")
cat(sprintf("  [✅] Contrato guardado: %s\n", basename(PATH_07D_CONTRATO_GLM)))
cat(sprintf("  [✅] CSV guardado: 07d_comp_coefs_%s.csv\n", SUFIJO_MODELO_GLM))
cat(sprintf("  [✅] TXT guardado: 07d_tabla_glm_%s.txt\n", SUFIJO_MODELO_GLM))
cat("  [ ] NO genera HTML — todo el HTML va en 07e\n")
cat("───────────────────────────────────────────────────────────────────\n")
cat(sprintf("  ⏱️  Tiempo total: %.1f segundos\n\n", elapsed))

toc()
cat("✅ Script 07d GLM completado\n")
