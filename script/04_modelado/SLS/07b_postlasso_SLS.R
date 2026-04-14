# =============================================================================
# [EN] 07b_postlasso_SLS.R -- Post-LASSO SLS with iterative trimming (Horrace-Oaxaca 2003), clustered SEs, classification metrics
# INPUTS:  Contracts/models from 07a SLS, rdos/datos/06_theta_predichos.rds
# OUTPUTS: rdos/contratos/07b_contrato_postlasso_SLS*.rds
# =============================================================================
# 🌟 07b_postlasso_SLS.R 🌟 ####
# OBJETIVO : OLS post-LASSO con recorte iterativo κ̂γ (Sequential Least Squares,
#            Horrace & Oaxaca 2003, IZA DP No. 703, §3) + SE clusterizados
#            sobre la submuestra convergida + métricas de clasificación en test.
#
#            DIFERENCIA CENTRAL vs LPM: en lugar de un único lm() sobre train
#            completo, se aplica el algoritmo iterativo:
#              (1) Estimar OLS sobre muestra actual
#              (2) Predecir ŷ sobre esa misma muestra
#              (3) Conservar solo {i | ŷ_i ∈ [0,1]} → nuevo κ̂γ
#              (4) Repetir hasta convergencia
#            El estimador final β̃ sobre κ̂γ^(J) es consistente (Teorema 8).
#            Por construcción: 0% predicciones fuera de [0,1] en κ̂γ.
#
# INPUTS   : PATH_07A_CONTRATO_SLS  → contrato 07a (vars seleccionadas, split)
#            PATH_07_MODELO_SLS     → cv.glmnet object SLS
#            PATH_07_RECIPE_SLS     → recipe preparado (bake)
#            PATH_06_THETA          → panel con θ predichos
#            PATH_06_MODELO_HETERO  → para extraer theta_data Modelo A
#
# OUTPUTS  : PATH_07_POSTLASSO_SLS  → rdos/modelos/07b_postlasso_SLS3T.rds (lm final)
#            PATH_07B_CONTRATO_SLS  → rdos/contratos/07b_contrato_postlasso_SLS3T.rds
#            07b_comp_coefs_SLS3T.csv  → rdos/reportes/
#            07b_tabla_ols_SLS3T.txt   → rdos/reportes/
#
# DIAGNOSTICO CENTRAL (SLS): % pérdida muestral por recorte
#   Alerta si pct_loss > 20% (indicaría especificación incorrecta)
#
# REFERENCIA: Horrace & Oaxaca (2003), IZA DP No. 703

# 📚 Librerias -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(recipes)
  library(glmnet)
  library(pROC)
  library(car)        # VIF / GVIF
  library(lmtest)     # Breusch-Pagan, RESET
  library(sandwich)   # vcovCL
  library(broom)      # tidy / glance
  library(PRROC)      # AUC-PR
  library(tictoc)
})

# 🔧 Cargar configuracion y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("07b_SLS completo")
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  🌟 07b_postlasso_SLS.R 🌟\n")
cat("  SLS: OLS + recorte iterativo κ̂γ | SE clusterizados | Métricas\n")
cat("  Referencia: Horrace & Oaxaca (2003), IZA DP No. 703, §3\n")
cat(sprintf("  Inicio: %s\n", format(start_time, "%Y-%m-%d %H:%M:%S")))
cat("═══════════════════════════════════════════════════════════════════\n\n")


# 🪫 1. Carga de inputs + reconstruccion split ---------------------------------
cat("── FASE 1: Carga de inputs + reconstrucción split ──────────────────\n")

# 1a. Contratos y modelos de 07a SLS
cat("  Cargando contrato_07a SLS...\n")
contrato_07a   <- readRDS(PATH_07A_CONTRATO_SLS)
cv_fit         <- readRDS(PATH_07_MODELO_SLS)
recipe_prepped <- readRDS(PATH_07_RECIPE_SLS)

cat(sprintf("  [✅] Contrato 07a SLS: %d vars seleccionadas (λ.1se)\n",
            length(contrato_07a$vars_seleccionadas)))
cat(sprintf("       Recipe: %s\n", contrato_07a$input_recipe))

# 1b. Panel con θ + join Modelo A
cat("  Cargando panel θ...\n")
panel_raw     <- readRDS(PATH_06_THETA)
modelo_hetero <- readRDS(PATH_06_MODELO_HETERO)

# Extraer theta_data del Modelo A — patrón canónico (lección 15/16)
# [L45] rename antes de bake()
theta_data_mA <- modelo_hetero$modelo_A$theta_data %>%
  rename(theta_A_mA = theta_A, theta_B_mA = theta_B) %>%
  select(id_individuo_hist, periodo_id, theta_A_mA, theta_B_mA)

panel <- panel_raw %>%
  left_join(
    theta_data_mA,
    by           = c("id_individuo_hist", "periodo_id"),
    relationship = "many-to-one"
  )

cat(sprintf("  [✅] Panel: %s obs × %d vars | θ_A cobertura: %.1f%%\n",
            format(nrow(panel), big.mark = ","),
            ncol(panel),
            mean(!is.na(panel$theta_A_mA)) * 100))

# 1c. Reconstrucción EXACTA del split 80/20 (mismo SEED_GLOBAL que 07a)
cat("  Reconstruyendo split 80/20 con SEED_GLOBAL...\n")

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
  )

set.seed(SEED_GLOBAL)
idx_train <- c(
  sample(which(train_raw$formalidad_bin == 1),
         floor(0.80 * sum(train_raw$formalidad_bin == 1))),
  sample(which(train_raw$formalidad_bin == 0),
         floor(0.80 * sum(train_raw$formalidad_bin == 0)))
)
df_train <- train_raw[idx_train, ]
df_test  <- train_raw[-idx_train, ]

cat(sprintf("  [✅] Train: %s obs | Test: %s obs | Formal train: %.1f%%\n",
            format(nrow(df_train), big.mark = ","),
            format(nrow(df_test),  big.mark = ","),
            mean(df_train$formalidad_bin) * 100))

# Verificar consistencia con 07a
stopifnot(
  "Split inconsistente con 07a SLS — verificar SEED_GLOBAL" =
    nrow(df_train) == contrato_07a$n_train &&
    nrow(df_test)  == contrato_07a$n_test
)
cat("  [✅] Split consistente con 07a SLS\n")

# 1d. Bake del recipe → data frames de features
cat("  Aplicando recipe (bake)...\n")
X_train_df <- bake(recipe_prepped, new_data = df_train)
X_test_df  <- bake(recipe_prepped, new_data = df_test)

y_train <- df_train$formalidad_bin
y_test  <- df_test$formalidad_bin

cat(sprintf("  [✅] X_train: %s × %d | X_test: %s × %d\n",
            format(nrow(X_train_df), big.mark = ","), ncol(X_train_df),
            format(nrow(X_test_df),  big.mark = ","), ncol(X_test_df)))

rm(panel_raw, modelo_hetero, theta_data_mA, panel, train_raw, idx_train)
gc(verbose = FALSE)


# 🪫 2. SLS — OLS con recorte iterativo κ̂γ (Horrace & Oaxaca 2003, §3) --------
cat("\n── FASE 2: SLS — Recorte iterativo κ̂γ ─────────────────────────────\n")
cat("   Teorema 1 (H&O 2003): OLS sobre muestra completa es sesgado/\n")
cat("   inconsistente cuando existe i: xiβ ∉ [0,1]. El estimador β̃\n")
cat("   sobre κ̂γ^(J) (Teorema 8) es consistente para β.\n\n")

# Variables del OLS post-LASSO (seleccionadas por LASSO en 07a)
vars_sel_1se     <- contrato_07a$vars_seleccionadas
vars_disponibles <- intersect(vars_sel_1se, colnames(X_train_df))
vars_ausentes    <- setdiff(vars_sel_1se, colnames(X_train_df))

if (length(vars_ausentes) > 0) {
  cat(sprintf("  [⚠️] Variables en LASSO ausentes en X_train (%d): %s\n",
              length(vars_ausentes), paste(vars_ausentes, collapse = ", ")))
} else {
  cat(sprintf("  [✅] Las %d variables de LASSO presentes en X_train\n",
              length(vars_disponibles)))
}

# Data frame de trabajo: features + target + codusu (para vcovCL final)
df_07b_sls <- X_train_df %>%
  select(all_of(vars_disponibles)) %>%
  mutate(
    formalidad_bin = y_train,
    codusu         = df_train$codusu   # necesario para SE clusterizados en muestra final
  )

formula_07b_sls <- as.formula(
  paste("formalidad_bin ~", paste(vars_disponibles, collapse = " + "))
)

# ── Algoritmo iterativo ────────────────────────────────────────────────────────
MAX_ITER  <- 50L
iter      <- 1L
converged <- FALSE
data_sls  <- df_07b_sls          # copia de trabajo (se recorta en cada iter)
n_inicial <- nrow(df_07b_sls)
historial_iter <- integer(0)     # N por iteración (para diagnóstico)

cat(sprintf("  N inicial (train): %s\n", format(n_inicial, big.mark = ",")))
cat(sprintf("  Fórmula: formalidad_bin ~ [%d predictores]\n", length(vars_disponibles)))
cat(sprintf("  Máximo de iteraciones: %d\n\n", MAX_ITER))

tic("SLS recorte iterativo")

while (!converged && iter <= MAX_ITER) {

  # Paso A: OLS sobre muestra actual
  modelo_temp <- lm(formula_07b_sls, data = data_sls)

  # Paso B: Predecir ŷ sobre la misma muestra
  preds_iter <- predict(modelo_temp, newdata = data_sls)

  # Paso C: Identificar κ̂γ = {i | ŷ_i ∈ [0,1]}
  keep_logical <- (preds_iter >= 0 & preds_iter <= 1)
  n_recortadas <- sum(!keep_logical)

  historial_iter <- c(historial_iter, nrow(data_sls))

  cat(sprintf("  Iter %2d: N=%s | Recortadas=%s | Fuera[0,1]=%.2f%%\n",
              iter,
              format(nrow(data_sls), big.mark = ","),
              format(n_recortadas, big.mark = ","),
              mean(!keep_logical) * 100))

  # Paso D: ¿Convergencia?
  if (all(keep_logical)) {
    converged  <- TRUE
    modelo_sls <- modelo_temp      # estimador final β̃_{κ̂γ^(J)}
    cat(sprintf("\n  [✅] Convergencia en iteración %d\n", iter))
  } else {
    data_sls <- data_sls[keep_logical, ]
    iter     <- iter + 1L
  }
}

toc()

# Fallback si no converge
if (!converged) {
  warning(sprintf("⚠️  SLS no convergió en %d iteraciones. Usando última iteración.", MAX_ITER))
  modelo_sls <- modelo_temp
  cat(sprintf("  [⚠️] Usando modelo de iteración %d (sin convergencia formal)\n", MAX_ITER))
}

# ── Detección y eliminación de coeficientes aliasados ─────────────────────────
# Tras el recorte κ̂γ, algunos dummies pueden quedar con un solo nivel en la
# submuestra (varianza cero), generando colinealidad perfecta. alias() los detecta
# y se hace un refit limpio para evitar rank-deficiency en vcovCL / predict / VIF.
alias_info <- tryCatch(alias(modelo_sls), error = function(e) NULL)

if (!is.null(alias_info) && !is.null(alias_info$Complete) && nrow(alias_info$Complete) > 0) {
  aliased_vars <- rownames(alias_info$Complete)
  cat(sprintf("\n  [⚠️] %d coef(s) aliasados en κ̂γ (var. cero tras recorte):\n",
              length(aliased_vars)))
  cat("       ", paste(aliased_vars, collapse = ", "), "\n")
  cat("  → Eliminando y refitando OLS sobre κ̂γ...\n")

  vars_disponibles <- setdiff(vars_disponibles, aliased_vars)
  formula_07b_sls  <- as.formula(
    paste("formalidad_bin ~", paste(vars_disponibles, collapse = " + "))
  )
  modelo_sls <- lm(formula_07b_sls, data = data_sls)
  cat(sprintf("  [✅] Refit limpio: %d predictores\n", length(vars_disponibles)))
} else {
  cat("\n  [✅] Sin coeficientes aliasados\n")
}

# ── Estadísticas de recorte ────────────────────────────────────────────────────
n_final   <- nrow(data_sls)
n_trimmed <- n_inicial - n_final
pct_loss  <- round((n_trimmed / n_inicial) * 100, 2)
n_iters   <- iter

cat(sprintf("\n  ── Estadísticas de recorte κ̂γ ──────────────────────────────\n"))
cat(sprintf("  N inicial:          %s\n",   format(n_inicial, big.mark = ",")))
cat(sprintf("  N final (κ̂γ):      %s\n",   format(n_final,   big.mark = ",")))
cat(sprintf("  Obs recortadas:     %s\n",   format(n_trimmed, big.mark = ",")))
cat(sprintf("  Pérdida muestral:   %.2f%%\n", pct_loss))
cat(sprintf("  Iteraciones:        %d\n",   n_iters))

if (pct_loss > 20) {
  warning(sprintf("⚠️  Pérdida muestral alta: %.2f%% > 20%%. Revisar especificación.", pct_loss))
  cat("  [⚠️] Pérdida muestral > 20%% — documentar en paper, revisar especificación\n")
} else {
  cat(sprintf("  [✅] Pérdida muestral dentro del umbral aceptable (<20%%)\n"))
}

# ── SE clusterizados sobre la muestra final κ̂γ ────────────────────────────────
# Los errores se calculan sobre data_sls (submuestra convergida), no sobre el train completo
cat("\n  Aplicando vcovCL (cluster = codusu, HC1) sobre κ̂γ...\n")
vcov_07b_cl      <- vcovCL(modelo_sls, cluster = ~codusu, type = "HC1")
test_07b_robusto <- coeftest(modelo_sls, vcov = vcov_07b_cl)

tabla_07b_ols <- tidy(test_07b_robusto) %>%
  mutate(
    significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )

glance_07b_ols <- glance(modelo_sls)

cat(sprintf("  [✅] R²: %.4f | R² ajustado: %.4f | N(κ̂γ): %s\n",
            glance_07b_ols$r.squared,
            glance_07b_ols$adj.r.squared,
            format(glance_07b_ols$nobs, big.mark = ",")))
cat(sprintf("       F-stat: %.2f (df1=%d, df2=%d) | p=%s\n",
            glance_07b_ols$statistic,
            glance_07b_ols$df,
            glance_07b_ols$df.residual,
            format.pval(glance_07b_ols$p.value, digits = 3)))

# θ en el OLS final
for (th in c("theta_A_mA", "theta_B_mA")) {
  fila_th <- tabla_07b_ols %>% filter(term == th)
  if (nrow(fila_th) > 0) {
    cat(sprintf("       %s: β=%.4f | SE=%.4f | p=%s %s\n",
                th,
                fila_th$estimate,
                fila_th$std.error,
                format.pval(fila_th$p.value, digits = 3),
                fila_th$significancia))
  }
}

# Guardar objeto modelo_sls (para backcasting en 08_SLS)
saveRDS(modelo_sls, PATH_07_POSTLASSO_SLS)
cat(sprintf("\n  [✅] Modelo SLS guardado: %s\n", basename(PATH_07_POSTLASSO_SLS)))


# 🪫 3. Tests de especificacion (sobre modelo en κ̂γ) ---------------------------
cat("\n── FASE 3: Tests de especificación (sobre κ̂γ) ──────────────────────\n")

# 3a. Breusch-Pagan (heterocedasticidad) — aplicado sobre la muestra final
bp_07b_test <- bptest(modelo_sls)
cat(sprintf("  Breusch-Pagan: χ²=%.2f, df=%d, p=%s %s\n",
            as.numeric(bp_07b_test$statistic),
            as.integer(bp_07b_test$parameter),
            format.pval(bp_07b_test$p.value, digits = 3),
            ifelse(bp_07b_test$p.value < 0.05,
                   "→ heterocedasticidad confirmada [HC1 aplicados ✅]",
                   "[✅] homocedasticidad no rechazada")))

# 3b. Ramsey RESET (forma funcional)
reset_07b_test <- resettest(modelo_sls, power = 2:3, type = "fitted")
cat(sprintf("  Ramsey RESET:  F=%.3f, df1=%d, df2=%d, p=%s %s\n",
            as.numeric(reset_07b_test$statistic),
            as.integer(reset_07b_test$parameter[1]),
            as.integer(reset_07b_test$parameter[2]),
            format.pval(reset_07b_test$p.value, digits = 3),
            ifelse(reset_07b_test$p.value < 0.05,
                   "[⚠️] no-linealidad sugerida — documentar en paper",
                   "[✅] forma funcional no rechazada")))

# 3c. VIF / GVIF (multicolinealidad)
cat("  Calculando VIF/GVIF...\n")
vif_07b_vals <- tryCatch({
  vif_raw <- vif(modelo_sls)
  if (is.matrix(vif_raw)) {
    vif_df <- as.data.frame(vif_raw) %>%
      rownames_to_column("variable") %>%
      rename(
        GVIF     = GVIF,
        Df       = Df,
        GVIF_adj = `GVIF^(1/(2*Df))`
      ) %>%
      mutate(vif_comparable = GVIF_adj^2)
  } else {
    vif_df <- tibble(variable = names(vif_raw), VIF = vif_raw) %>%
      mutate(vif_comparable = VIF)
  }
  vif_df
}, error = function(e) {
  cat(sprintf("  [⚠️] VIF no calculado: %s\n", conditionMessage(e)))
  NULL
})

if (!is.null(vif_07b_vals)) {
  n_07b_vif_alto <- sum(vif_07b_vals$vif_comparable > 10, na.rm = TRUE)
  cat(sprintf("  [%s] Variables con VIF/GVIF² > 10: %d\n",
              ifelse(n_07b_vif_alto == 0, "✅", "⚠️"), n_07b_vif_alto))
  if (n_07b_vif_alto > 0) {
    vif_07b_vals %>%
      filter(vif_comparable > 10) %>%
      arrange(desc(vif_comparable)) %>%
      print()
  }
} else {
  n_07b_vif_alto <- NA_integer_
}


# 🪫 4. Clasificacion en test set ----------------------------------------------
cat("\n── FASE 4: Clasificación en test set ──────────────────────────────\n")
cat("   NOTA: El modelo SLS garantiza ŷ ∈ [0,1] sobre κ̂γ (train).\n")
cat("   Sobre el test set pueden existir predicciones fuera de [0,1];\n")
cat("   se reportan y se clipean para métricas de clasificación.\n\n")

# Predicciones OLS sobre test
df_07b_test_pred <- X_test_df %>%
  select(all_of(vars_disponibles))

pred_07b_raw  <- predict(modelo_sls, newdata = df_07b_test_pred)
pred_07b_clip <- pmax(0, pmin(1, pred_07b_raw))

pct_07b_fuera <- mean(pred_07b_raw < 0 | pred_07b_raw > 1) * 100
cat(sprintf("  Predicciones fuera [0,1] en test: %.2f%% %s\n",
            pct_07b_fuera,
            ifelse(pct_07b_fuera == 0, "[✅] 0% — SLS efectivo",
                   ifelse(pct_07b_fuera < 10, "[✅]", "[⚠️] documentar"))))
cat(sprintf("  (En κ̂γ train: 0.00%% por construcción ✅)\n"))

# 4a. AUC-ROC con IC DeLong
roc_07b_obj <- roc(y_test, pred_07b_clip,
                   levels = c(0, 1), direction = "<", quiet = TRUE)
auc_07b_val <- as.numeric(auc(roc_07b_obj))
auc_07b_ci  <- as.numeric(ci.auc(roc_07b_obj, method = "delong"))

# Curva ROC como data frame (para graficar en 07e)
roc_07b_df <- tibble(
  fpr         = 1 - roc_07b_obj$specificities,
  tpr         = roc_07b_obj$sensitivities,
  umbral_grid = roc_07b_obj$thresholds
) %>% arrange(fpr)

cat(sprintf("  AUC-ROC (DeLong): %.4f [IC95: %.4f – %.4f]\n",
            auc_07b_val, auc_07b_ci[1], auc_07b_ci[3]))
cat(sprintf("  Ref. LPM: 0.8659 | GLM: 0.8686  (ver KB_04 para comparación completa)\n"))
cat(sprintf("  ΔAUC vs LASSO 07a: %+.4f\n",
            auc_07b_val - contrato_07a$auc_test_lasso))

# 4b. AUC-PR (Precision-Recall)
pr_07b_obj <- pr.curve(
  scores.class0 = pred_07b_clip[y_test == 1],
  scores.class1 = pred_07b_clip[y_test == 0],
  curve = TRUE
)
pr_07b_auc <- pr_07b_obj$auc.integral

pr_07b_df <- as.data.frame(pr_07b_obj$curve) %>%
  setNames(c("recall", "precision", "umbral_grid"))

cat(sprintf("  AUC-PR: %.4f\n", pr_07b_auc))

# 4c. Umbral Youden → métricas derivadas
coords_07b <- coords(roc_07b_obj, x = "best", best.method = "youden",
                     ret = c("threshold", "sensitivity", "specificity"),
                     transpose = FALSE)
umbral_07b <- coords_07b$threshold[1]

pred_07b_clase <- as.integer(pred_07b_clip >= umbral_07b)
tp <- sum(pred_07b_clase == 1 & y_test == 1)
tn <- sum(pred_07b_clase == 0 & y_test == 0)
fp <- sum(pred_07b_clase == 1 & y_test == 0)
fn <- sum(pred_07b_clase == 0 & y_test == 1)

precision_07b <- tp / (tp + fp)
recall_07b    <- tp / (tp + fn)
f1_07b        <- 2 * precision_07b * recall_07b / (precision_07b + recall_07b)
mcc_07b       <- (as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)) /
                  sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) *
                       as.numeric(tn + fp) * as.numeric(tn + fn))
accuracy_07b  <- (tp + tn) / (tp + tn + fp + fn)
p_o <- accuracy_07b
p_e <- ((tp + fn) / length(y_test)) * ((tp + fp) / length(y_test)) +
       ((tn + fp) / length(y_test)) * ((tn + fn) / length(y_test))
kappa_07b <- (p_o - p_e) / (1 - p_e)

cat(sprintf("  Umbral Youden: %.4f\n", umbral_07b))
cat(sprintf("  Accuracy: %.4f | Sensibilidad: %.4f | Especificidad: %.4f\n",
            accuracy_07b, recall_07b, coords_07b$specificity[1]))
cat(sprintf("  F1: %.4f | MCC: %.4f | Kappa: %.4f\n",
            f1_07b, mcc_07b, kappa_07b))

metricas_07b_clf <- tibble(
  umbral        = umbral_07b,
  accuracy      = accuracy_07b,
  sensibilidad  = recall_07b,
  especificidad = coords_07b$specificity[1],
  precision     = precision_07b,
  f1            = f1_07b,
  mcc           = mcc_07b,
  kappa         = kappa_07b
)

# 4d. Hosmer-Lemeshow — se reporta como referencia aunque no aplica en modelo lineal
# Para comparación directa con GLM en 07e; p-value del SLS no tiene la misma
# interpretación (el test asume función de enlace logit / probabilidad acotada).
cat("\n  [INFO] Hosmer-Lemeshow: calculado para comparabilidad con GLM.\n")
cat("  Para modelos lineales (LPM/SLS) el test no aplica formalmente;\n")
cat("  la acotación de predicciones la garantiza κ̂γ por construcción.\n")

n_hl   <- length(y_test)
# ntile() en lugar de cut(quantile()) — evita el error "breaks no únicos"
# cuando las predicciones están muy concentradas en pocos valores distintos
grp_hl <- ntile(pred_07b_clip, 10)
hl_07b_df <- tibble(
  grupo     = grp_hl,
  observado = y_test,
  predicho  = pred_07b_clip
) %>%
  group_by(grupo) %>%
  summarise(
    n          = n(),
    obs_formal = sum(observado),
    pred_media = mean(predicho),
    .groups    = "drop"
  )

hl_07b_stat <- sum((hl_07b_df$obs_formal - hl_07b_df$n * hl_07b_df$pred_media)^2 /
                   (hl_07b_df$n * hl_07b_df$pred_media * (1 - hl_07b_df$pred_media)),
                   na.rm = TRUE)
hl_07b_pval <- pchisq(hl_07b_stat, df = 8, lower.tail = FALSE)

cat(sprintf("  H-L (referencia): χ²=%.3f, df=8, p=%s\n",
            hl_07b_stat, format.pval(hl_07b_pval, digits = 3)))
cat("  → Para interpretación formal ver GLM (07b_postlasso_GLM.R)\n")


# 🪫 5. Tabla comparativa LASSO vs SLS -----------------------------------------
cat("\n── FASE 5: Tabla comparativa LASSO vs SLS ──────────────────────────\n")

# Extraer coefs LASSO de cv_fit para λ.1se
coefs_07b_lasso_raw <- coef(cv_fit, s = "lambda.1se")
coefs_07b_lasso <- tibble(
  variable   = rownames(coefs_07b_lasso_raw)[-1],
  coef_lasso = as.numeric(coefs_07b_lasso_raw)[-1]
) %>% filter(coef_lasso != 0)

# Tabla comparativa: LASSO vs SLS (OLS en κ̂γ)
comp_07b_coefs <- tabla_07b_ols %>%
  rename(variable = term) %>%
  filter(variable != "(Intercept)") %>%
  select(variable, estimate, std.error, p.value, significancia) %>%
  rename(
    coef_sls  = estimate,
    se_sls_cl = std.error,
    p_sls     = p.value,
    sig_sls   = significancia
  ) %>%
  left_join(coefs_07b_lasso, by = "variable") %>%
  mutate(
    delta_coef = coef_sls - coef_lasso,
    en_lasso   = !is.na(coef_lasso)
  ) %>%
  arrange(desc(abs(coef_sls)))

cat(sprintf("  [✅] Tabla comparativa: %d variables\n", nrow(comp_07b_coefs)))

# Tabla comparativa de performance (λ.min vs λ.1se vs SLS)
comp_07b_lambda <- tibble(
  especificacion = c("LASSO λ.min", "LASSO λ.1se", "SLS (OLS en κ̂γ)"),
  n_vars         = c(contrato_07a$n_vars_sel_min,
                     contrato_07a$n_vars_sel_1se,
                     length(vars_disponibles)),
  auc_test       = c(NA_real_, contrato_07a$auc_test_lasso, auc_07b_val),
  r2             = c(NA_real_, NA_real_, glance_07b_ols$r.squared),
  r2_adj         = c(NA_real_, NA_real_, glance_07b_ols$adj.r.squared),
  n_muestra      = c(NA_real_, contrato_07a$n_train, n_final),
  pct_muestra    = c(NA_real_, 100, 100 - pct_loss)
)

print(comp_07b_lambda)


# 🪫 6. Outputs — CSV, TXT, contrato ------------------------------------------
cat("\n── FASE 6: Guardando outputs ───────────────────────────────────────\n")

# 6a. CSV comparativo LASSO vs SLS
ruta_07b_csv <- file.path(DIR_REPORTES,
                          paste0("07b_comp_coefs_", SUFIJO_MODELO_SLS, ".csv"))
write_csv(comp_07b_coefs, ruta_07b_csv)
cat(sprintf("  [✅] %s\n", basename(ruta_07b_csv)))

# 6b. TXT tabla OLS completa con encabezado
tabla_07b_ols_txt <- tabla_07b_ols %>%
  select(term, estimate, std.error, statistic, p.value, significancia) %>%
  rename(
    variable = term,
    coef_sls = estimate,
    se_cl    = std.error,
    t_stat   = statistic,
    p_valor  = p.value,
    sig      = significancia
  )

encabezado_07b <- c(
  paste0("OLS post-LASSO (SLS — κ̂γ) | Modelo ", SUFIJO_MODELO_SLS),
  paste0("Referencia: Horrace & Oaxaca (2003), IZA DP No. 703, Teorema 8"),
  paste0("Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("SE clusterizados por codusu (HC1) — Cameron & Miller (2015)"),
  paste0("N inicial=", format(n_inicial, big.mark = ","),
         " | N final (κ̂γ)=", format(n_final, big.mark = ","),
         " | Pérdida=", pct_loss, "%",
         " | Iteraciones=", n_iters),
  paste0("R²=", round(glance_07b_ols$r.squared, 4),
         " | R²adj=", round(glance_07b_ols$adj.r.squared, 4),
         " | F=", round(glance_07b_ols$statistic, 2)),
  "Sig: *** p<0.001 | ** p<0.01 | * p<0.05 | . p<0.10",
  "================================================================",
  ""
)

ruta_07b_txt <- file.path(DIR_REPORTES,
                          paste0("07b_tabla_ols_", SUFIJO_MODELO_SLS, ".txt"))
writeLines(encabezado_07b, ruta_07b_txt)
write.table(tabla_07b_ols_txt, ruta_07b_txt,
            sep = "\t", row.names = FALSE,
            quote = FALSE, na = "", append = TRUE, col.names = FALSE)
cat(sprintf("  [✅] %s\n", basename(ruta_07b_txt)))

# 6c. Contrato 07b SLS
contrato_07b_sls <- list(
  # Identificación
  script        = "07b_postlasso_SLS.R",
  modelo        = "SLS",
  sufijo_modelo = SUFIJO_MODELO_SLS,
  fecha         = Sys.time(),
  version_tag   = paste0("v1_postlasso_", SUFIJO_MODELO_SLS),
  referencia    = "Horrace & Oaxaca (2003), IZA DP No. 703",

  # Universo
  n_train        = nrow(df_train),
  n_test         = nrow(df_test),
  n_vars_ols     = length(vars_disponibles),
  vars_ols       = vars_disponibles,

  # Estadísticas de recorte κ̂γ — diagnóstico central del SLS
  n_inicial      = n_inicial,
  n_final        = n_final,
  n_trimmed      = n_trimmed,
  pct_loss       = pct_loss,
  n_iteraciones  = n_iters,
  converged      = converged,
  historial_n    = historial_iter,
  alerta_pct_loss = pct_loss > 20,

  # OLS (sobre κ̂γ)
  ols_r2         = glance_07b_ols$r.squared,
  ols_r2_adj     = glance_07b_ols$adj.r.squared,
  ols_n          = glance_07b_ols$nobs,
  ols_f_stat     = glance_07b_ols$statistic,
  ols_f_pval     = glance_07b_ols$p.value,
  ols_df         = glance_07b_ols$df,
  ols_df_res     = glance_07b_ols$df.residual,
  vcov_type      = "vcovCL",
  cluster_var    = "codusu",
  cluster_sobre  = "muestra_final_kappa_gamma",

  # Tests de especificación
  bp_stat        = as.numeric(bp_07b_test$statistic),
  bp_pval        = bp_07b_test$p.value,
  reset_stat     = as.numeric(reset_07b_test$statistic),
  reset_pval     = reset_07b_test$p.value,
  n_vif_alto     = n_07b_vif_alto,

  # Clasificación (test set)
  auc_roc        = auc_07b_val,
  auc_roc_ci     = auc_07b_ci,
  auc_pr         = pr_07b_auc,
  metricas_clf   = metricas_07b_clf,
  umbral_youden  = umbral_07b,
  pct_fuera_01_test  = pct_07b_fuera,
  pct_fuera_01_train = 0.0,          # 0% por construcción en κ̂γ
  hl_stat        = hl_07b_stat,
  hl_pval        = hl_07b_pval,
  hl_nota        = "H-L no aplica formalmente para modelos lineales. Calculado como referencia vs GLM.",

  # Tablas
  tabla_ols      = tabla_07b_ols,
  vif_tabla      = vif_07b_vals,
  comp_coefs     = comp_07b_coefs,
  comp_lambda    = comp_07b_lambda,
  hl_df          = hl_07b_df,

  # Elementos para comparación downstream (07e, 08_SLS)
  pred_test      = list(raw = pred_07b_raw, clip = pred_07b_clip),
  roc_df         = roc_07b_df,
  pr_df          = pr_07b_df,
  residuos_ols   = residuals(modelo_sls),

  # Fila plana para bind_rows(c1$resumen_comparacion, c2$resumen_comparacion)
  resumen_comparacion = tibble(
    version        = paste0("postlasso_", SUFIJO_MODELO_SLS),
    fecha          = format(Sys.time(), "%Y-%m-%d %H:%M"),
    n_vars_ols     = length(vars_disponibles),
    r2             = glance_07b_ols$r.squared,
    r2_adj         = glance_07b_ols$adj.r.squared,
    f_stat         = glance_07b_ols$statistic,
    bp_pval        = bp_07b_test$p.value,
    reset_pval     = reset_07b_test$p.value,
    n_vif_alto     = n_07b_vif_alto,
    auc_roc        = auc_07b_val,
    auc_roc_ci_lo  = auc_07b_ci[1],
    auc_roc_ci_hi  = auc_07b_ci[3],
    auc_pr         = pr_07b_auc,
    f1             = f1_07b,
    mcc            = mcc_07b,
    kappa          = kappa_07b,
    hl_pval        = hl_07b_pval,
    umbral_youden  = umbral_07b,
    # Columnas específicas SLS (NA en LPM/GLM para bind_rows limpio)
    n_inicial      = as.numeric(n_inicial),
    n_final_kappa  = as.numeric(n_final),
    pct_loss       = pct_loss,
    n_iteraciones  = as.numeric(n_iters),
    pct_fuera_test = pct_07b_fuera
  )
)

saveRDS(contrato_07b_sls, PATH_07B_CONTRATO_SLS)
cat(sprintf("  [✅] %s\n", basename(PATH_07B_CONTRATO_SLS)))


# 📑 Resumen final -------------------------------------------------------------
cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  RESUMEN 07b_postlasso_SLS.R\n")
cat("  (Horrace & Oaxaca 2003 — Sequential Least Squares)\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  Recorte iterativo κ̂γ:\n"))
cat(sprintf("    N inicial:        %s\n",   format(n_inicial, big.mark = ",")))
cat(sprintf("    N final (κ̂γ):    %s\n",   format(n_final,   big.mark = ",")))
cat(sprintf("    Pérdida:          %.2f%%\n", pct_loss))
cat(sprintf("    Iteraciones:      %d\n",   n_iters))
cat(sprintf("    Convergencia:     %s\n",   ifelse(converged, "✅ SÍ", "⚠️ NO")))
cat(sprintf("  OLS (κ̂γ): N=%d vars | R²=%.4f | R²adj=%.4f\n",
            length(vars_disponibles),
            glance_07b_ols$r.squared,
            glance_07b_ols$adj.r.squared))
cat(sprintf("  Breusch-Pagan:      p=%s\n",
            format.pval(bp_07b_test$p.value, digits = 3)))
cat(sprintf("  Ramsey RESET:       p=%s\n",
            format.pval(reset_07b_test$p.value, digits = 3)))
cat(sprintf("  VIF/GVIF>10:        %s\n",
            ifelse(is.na(n_07b_vif_alto), "N/A", as.character(n_07b_vif_alto))))
cat(sprintf("  AUC-ROC (test):     %.4f [%.4f – %.4f]\n",
            auc_07b_val, auc_07b_ci[1], auc_07b_ci[3]))
cat(sprintf("  ΔAUC vs 07a LASSO:  %+.4f\n",
            auc_07b_val - contrato_07a$auc_test_lasso))
cat(sprintf("  AUC-PR:             %.4f\n", pr_07b_auc))
cat(sprintf("  F1: %.4f | MCC: %.4f | Kappa: %.4f\n",
            f1_07b, mcc_07b, kappa_07b))
cat(sprintf("  H-L (ref.):         p=%s [N/A para modelo lineal]\n",
            format.pval(hl_07b_pval, digits = 3)))
cat(sprintf("  Pred fuera [0,1] en test:  %.2f%%\n", pct_07b_fuera))
cat(sprintf("  Pred fuera [0,1] en κ̂γ:   0.00%% ✅ (por construcción)\n"))
cat("\n  Outputs:\n")
cat(sprintf("    [✅] %s\n", basename(PATH_07_POSTLASSO_SLS)))
cat(sprintf("    [✅] %s\n", basename(PATH_07B_CONTRATO_SLS)))
cat(sprintf("    [✅] %s\n", basename(ruta_07b_csv)))
cat(sprintf("    [✅] %s\n", basename(ruta_07b_txt)))
cat("\n  ▶  Siguiente paso: 07c_lasso_tiempo_SLS.R\n")
cat("═══════════════════════════════════════════════════════════════════\n")

end_time <- Sys.time()
toc()
cat(sprintf("  Tiempo total: %.1f min\n",
            as.numeric(difftime(end_time, start_time, units = "mins"))))
cat("\n✅ Script 07b_SLS completado\n")
