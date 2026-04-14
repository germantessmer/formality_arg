# =============================================================================
# [EN] 07b_postlasso_GLM.R -- Post-LASSO GLM binomial with clustered SEs, average marginal effects, and classification metrics
# INPUTS:  Contracts/models from 07a GLM, rdos/datos/06_theta_predichos.rds
# OUTPUTS: rdos/contratos/07b_contrato_postlasso_GLM*.rds
# =============================================================================
# 🌟 07b_postlasso_GLM.R 🌟 ####
# OBJETIVO : GLM binomial post-LASSO con SE clusterizados + AMEs
#            + tests de especificación + métricas de clasificación en test set
# INPUTS   : PATH_07_CONTRATO_GLM   → contrato 07a GLM (vars seleccionadas, split)
#            PATH_07_MODELO_GLM     → cv.glmnet object (binomial)
#            PATH_07_RECIPE_GLM     → recipe preparado (bake)
#            PATH_06_THETA          → panel con θ predichos
#            PATH_06_MODELO_HETERO  → para extraer theta_data Modelo A
# OUTPUTS  : PATH_07B_CONTRATO_GLM → rdos/contratos/07b_contrato_postlasso_GLM3T.rds
#            07b_comp_coefs_GLM3T.csv  → tabla LASSO vs GLM (log-odds + AME)
#            07b_tabla_glm_GLM3T.txt   → tabla GLM completa con encabezado
#
# NOTA METODOLOGICA:
#   Los coeficientes del GLM son log-odds (no efectos marginales directos).
#   Los AMEs (Average Marginal Effects) son comparables en escala con los
#   coeficientes del LPM. Se calculan con marginaleffects::avg_slopes().
#   Pseudo-R² McFadden reemplaza R² del OLS para bondad de ajuste.
#   Breusch-Pagan y RESET no aplican al GLM binomial (no son OLS).

# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(recipes)
  library(glmnet)
  library(pROC)
  library(car)             # VIF / GVIF
  library(sandwich)        # vcovCL
  library(lmtest)          # coeftest
  library(broom)           # tidy / glance
  library(PRROC)           # AUC-PR
  library(marginaleffects) # avg_slopes (AMEs)
  library(tictoc)
})

# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("07b completo")
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  🌟 07b_postlasso_GLM.R 🌟\n")
cat("  GLM binomial post-LASSO | SE clusterizados | AMEs | Métricas\n")
cat("  Inicio:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# 🪫 1. Carga de inputs + reconstrucción split ---------------------------------
cat("── FASE 1: Carga de inputs + reconstrucción split ──────────────────\n")

# 1a. Contratos y modelos de 07a GLM
cat("  Cargando contrato_07a GLM...\n")
contrato_07a   <- readRDS(PATH_07_CONTRATO_GLM)
cv_fit         <- readRDS(PATH_07_MODELO_GLM)
recipe_prepped <- readRDS(PATH_07_RECIPE_GLM)

cat(sprintf("  [✅] Contrato 07a GLM: %d vars seleccionadas (λ.1se)\n",
            length(contrato_07a$vars_seleccionadas)))

# 1b. Panel con θ + join Modelo A
cat("  Cargando panel θ...\n")
panel_raw     <- readRDS(PATH_06_THETA)
modelo_hetero <- readRDS(PATH_06_MODELO_HETERO)

# Extraer theta_data del Modelo A — patrón canónico (lección 15/16)
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

# 1c. Reconstrucción EXACTA del split 80/20 (misma seed → mismo df_train/df_test que 07a)
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
  "Split inconsistente con 07a — verificar SEED_GLOBAL" =
    nrow(df_train) == contrato_07a$n_train &&
    nrow(df_test)  == contrato_07a$n_test
)
cat("  [✅] Split consistente con 07a GLM\n")

# 1d. Bake del recipe → data frames de features
cat("  Aplicando recipe (bake)...\n")
X_train_df <- bake(recipe_prepped, new_data = df_train)
X_test_df  <- bake(recipe_prepped, new_data = df_test)

y_train <- df_train$formalidad_bin
y_test  <- df_test$formalidad_bin

# Matrices para glmnet (referencia)
X_train <- as.matrix(X_train_df)
X_test  <- as.matrix(X_test_df)

cat(sprintf("  [✅] X_train: %s × %d | X_test: %s × %d\n",
            format(nrow(X_train), big.mark = ","), ncol(X_train),
            format(nrow(X_test),  big.mark = ","), ncol(X_test)))

rm(panel_raw, modelo_hetero, theta_data_mA, panel, train_raw, idx_train)
gc(verbose = FALSE)

# 🪫 2. GLM binomial post-LASSO ------------------------------------------------
cat("\n── FASE 2: GLM binomial post-LASSO ─────────────────────────────────\n")

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

# Data frame para glm() — features + target + codusu (para vcovCL)
df_07b_glm <- X_train_df %>%
  select(all_of(vars_disponibles)) %>%
  mutate(
    formalidad_bin = y_train,
    codusu         = df_train$codusu   # identificador hogar para clustering SE
  )

formula_07b_glm <- as.formula(
  paste("formalidad_bin ~", paste(vars_disponibles, collapse = " + "))
)

cat("  Estimando GLM binomial (logit)...\n")
tic("glm binomial")
m_07b_glm <- glm(formula_07b_glm,
                 data   = df_07b_glm,
                 family = binomial(link = "logit"))
toc()

# SE clusterizados por hogar (HC1) — Cameron & Miller (2015)
# Corrige correlación intra-hogar (panel rotante EPH)
cat("  Aplicando vcovCL (cluster = codusu, HC1)...\n")
vcov_07b_cl      <- vcovCL(m_07b_glm, cluster = ~codusu, type = "HC1")
test_07b_robusto <- coeftest(m_07b_glm, vcov = vcov_07b_cl)

tabla_07b_glm <- tidy(test_07b_robusto) %>%
  mutate(
    significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )

# Bondad de ajuste GLM — pseudo-R² McFadden, AIC, BIC, deviance
glm_null_dev   <- m_07b_glm$null.deviance
glm_res_dev    <- m_07b_glm$deviance
pseudo_r2_mf   <- 1 - (glm_res_dev / glm_null_dev)   # McFadden
aic_07b        <- AIC(m_07b_glm)
bic_07b        <- BIC(m_07b_glm)
n_obs_07b      <- nrow(df_07b_glm)
df_res_07b     <- m_07b_glm$df.residual

cat(sprintf("  [✅] Pseudo-R² McFadden: %.4f\n", pseudo_r2_mf))
cat(sprintf("       AIC: %.2f | BIC: %.2f | N: %s\n",
            aic_07b, bic_07b, format(n_obs_07b, big.mark = ",")))
cat(sprintf("       Deviance residual: %.2f (df=%d)\n", glm_res_dev, df_res_07b))
cat(sprintf("       Deviance nula:     %.2f\n", glm_null_dev))

# θ en el GLM — mostrar log-odds con SE clusterizados
for (th in c("theta_A_mA", "theta_B_mA")) {
  fila_th <- tabla_07b_glm %>% filter(term == th)
  if (nrow(fila_th) > 0) {
    cat(sprintf("       %s: log-odds=%.4f | SE=%.4f | p=%s %s\n",
                th,
                fila_th$estimate,
                fila_th$std.error,
                format.pval(fila_th$p.value, digits = 3),
                fila_th$significancia))
  }
}

# 🪫 3. Tests de especificación ------------------------------------------------
cat("\n── FASE 3: Tests de especificación ────────────────────────────────\n")

# 3a. Breusch-Pagan y RESET: N/A para GLM binomial
cat("  [INFO] Breusch-Pagan y Ramsey RESET no aplican al GLM binomial.\n")
cat("         El GLM modela la varianza como Var(y) = μ(1-μ) por construcción.\n")
cat("         La heterocedasticidad está incorporada en la función de varianza.\n")
bp_07b_test    <- list(statistic = NA_real_, parameter = NA_integer_,
                       p.value   = NA_real_)
reset_07b_test <- list(statistic = NA_real_, parameter = c(NA_integer_, NA_integer_),
                       p.value   = NA_real_)

# 3b. VIF / GVIF (multicolinealidad — aplica igual al GLM)
cat("  Calculando VIF/GVIF...\n")
vif_07b_vals <- tryCatch({
  vif_raw <- vif(m_07b_glm)
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

# 🪫 4. AMEs (Average Marginal Effects) ----------------------------------------
cat("\n── FASE 4: Average Marginal Effects (AMEs) ─────────────────────────\n")
cat("  Los AMEs son comparables en escala con los coeficientes del LPM.\n")

cat("  Calculando avg_slopes() [paralelo + submuestra]...\n")
N_AME_SUBSAMPLE <- min(10000L, nrow(df_07b_glm))
set.seed(SEED_GLOBAL)
idx_ame <- sample(nrow(df_07b_glm), N_AME_SUBSAMPLE)

library(future)
plan(multisession, workers = N_CORES)
tic("AMEs")
ames_07b <- tryCatch({
  avg_slopes(m_07b_glm, newdata = df_07b_glm[idx_ame, ])
}, error = function(e) {
  cat(sprintf("  [⚠️] AMEs no calculados: %s\n", conditionMessage(e)))
  NULL
})
toc()
plan(sequential)
cat(sprintf("  [INFO] Submuestra AME: %s obs (de %s train)\n",
            format(N_AME_SUBSAMPLE, big.mark = ","),
            format(nrow(df_07b_glm), big.mark = ",")))

if (!is.null(ames_07b)) {
  ames_07b_df <- as.data.frame(ames_07b) %>%
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
    rename(
      variable  = term,
      ame       = estimate,
      se_ame    = std.error,
      z_ame     = statistic,
      p_ame     = p.value,
      ame_ic_lo = conf.low,
      ame_ic_hi = conf.high
    ) %>%
    mutate(
      sig_ame = case_when(
        p_ame < 0.001 ~ "***",
        p_ame < 0.01  ~ "**",
        p_ame < 0.05  ~ "*",
        p_ame < 0.10  ~ ".",
        TRUE          ~ ""
      )
    ) %>%
    arrange(desc(abs(ame)))

  cat(sprintf("  [✅] AMEs calculados: %d variables\n", nrow(ames_07b_df)))
  cat("\n  Top 10 AMEs por |efecto|:\n")
  print(head(ames_07b_df %>% select(variable, ame, se_ame, p_ame, sig_ame), 10),
        row.names = FALSE)

  # θ AMEs
  for (th in c("theta_A_mA", "theta_B_mA")) {
    fila_ame <- ames_07b_df %>% filter(variable == th)
    if (nrow(fila_ame) > 0) {
      cat(sprintf("  %s: AME=%.4f | SE=%.4f | p=%s %s\n",
                  th, fila_ame$ame, fila_ame$se_ame,
                  format.pval(fila_ame$p_ame, digits = 3), fila_ame$sig_ame))
    }
  }
} else {
  ames_07b_df <- NULL
}

# 🪫 5. Clasificación en test set ----------------------------------------------
cat("\n── FASE 5: Clasificación en test set ──────────────────────────────\n")

# Predicciones GLM: type = "response" → probabilidades en [0,1]
df_07b_test_pred <- X_test_df %>%
  select(all_of(vars_disponibles))

pred_07b_raw  <- predict(m_07b_glm, newdata = df_07b_test_pred, type = "response")
pred_07b_clip <- pmax(0, pmin(1, pred_07b_raw))   # por construcción ~0%

pct_07b_fuera <- mean(pred_07b_raw < 0 | pred_07b_raw > 1) * 100
cat(sprintf("  Predicciones fuera [0,1]: %.2f%% %s\n",
            pct_07b_fuera,
            ifelse(pct_07b_fuera < 0.1,
                   "[✅] acotadas por construcción (GLM binomial)",
                   "[⚠️] inesperado — revisar")))

# 5a. AUC-ROC con IC DeLong
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
cat(sprintf("  ΔAUC vs LASSO 07a: %+.4f\n",
            auc_07b_val - contrato_07a$auc_test))

# 5b. AUC-PR (Precision-Recall)
pr_07b_obj <- pr.curve(
  scores.class0 = pred_07b_clip[y_test == 1],
  scores.class1 = pred_07b_clip[y_test == 0],
  curve = TRUE
)
pr_07b_auc <- pr_07b_obj$auc.integral

pr_07b_df <- as.data.frame(pr_07b_obj$curve) %>%
  setNames(c("recall", "precision", "umbral_grid"))

cat(sprintf("  AUC-PR: %.4f\n", pr_07b_auc))

# 5c. Umbral Youden → métricas derivadas
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

# 5d. Hosmer-Lemeshow (calibración — 10 grupos)
# Más relevante para GLM que para LPM por la escala de probabilidades
n_hl   <- length(y_test)
grp_hl <- cut(pred_07b_clip,
              breaks         = quantile(pred_07b_clip, probs = seq(0, 1, 0.1)),
              include.lowest = TRUE,
              labels         = FALSE)
hl_07b_df <- tibble(
  grupo    = grp_hl,
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

cat(sprintf("  Hosmer-Lemeshow: χ²=%.3f, df=8, p=%s %s\n",
            hl_07b_stat,
            format.pval(hl_07b_pval, digits = 3),
            ifelse(hl_07b_pval > 0.05, "[✅] calibración aceptable",
                   "[⚠️] calibración cuestionable — documentar")))

# 🪫 6. Tabla comparativa LASSO vs GLM -----------------------------------------
cat("\n── FASE 6: Tabla comparativa LASSO vs GLM ──────────────────────────\n")

# Extraer log-odds LASSO de cv_fit para λ.1se
coefs_07b_lasso_raw <- coef(cv_fit, s = "lambda.1se")
coefs_07b_lasso <- tibble(
  variable   = rownames(coefs_07b_lasso_raw)[-1],
  coef_lasso = as.numeric(coefs_07b_lasso_raw)[-1]
) %>% filter(coef_lasso != 0)

# Tabla comparativa: LASSO vs GLM (log-odds) + AME
comp_07b_coefs <- tabla_07b_glm %>%
  rename(variable = term) %>%
  filter(variable != "(Intercept)") %>%
  select(variable, estimate, std.error, p.value, significancia) %>%
  rename(
    coef_glm  = estimate,
    se_glm_cl = std.error,
    p_glm     = p.value,
    sig_glm   = significancia
  ) %>%
  left_join(coefs_07b_lasso, by = "variable") %>%
  mutate(
    delta_coef = coef_glm - coef_lasso,
    en_lasso   = !is.na(coef_lasso)
  ) %>%
  # Agregar AME si está disponible
  { if (!is.null(ames_07b_df))
      left_join(., ames_07b_df %>% select(variable, ame, se_ame, p_ame, sig_ame),
                by = "variable")
    else . } %>%
  arrange(desc(abs(coef_glm)))

cat(sprintf("  [✅] Tabla comparativa: %d variables\n", nrow(comp_07b_coefs)))

# Tabla comparativa de performance (λ.min vs λ.1se vs GLM post-LASSO)
comp_07b_lambda <- tibble(
  especificacion = c("LASSO λ.min", "LASSO λ.1se", "GLM post-LASSO"),
  n_vars         = c(contrato_07a$n_vars_sel_min,
                     contrato_07a$n_vars_sel_1se,
                     length(vars_disponibles)),
  auc_test       = c(NA_real_, contrato_07a$auc_test, auc_07b_val),
  pseudo_r2_mf   = c(NA_real_, NA_real_, pseudo_r2_mf),
  aic            = c(NA_real_, NA_real_, aic_07b)
)

print(comp_07b_lambda)

# 🪫 7. Outputs — CSV, TXT, contrato ------------------------------------------
cat("\n── FASE 7: Guardando outputs ───────────────────────────────────────\n")

# 7a. CSV comparativo LASSO vs GLM (+ AMEs)
ruta_07b_csv <- file.path(DIR_REPORTES,
                          paste0("07b_comp_coefs_", SUFIJO_MODELO_GLM, ".csv"))
write_csv(comp_07b_coefs, ruta_07b_csv)
cat(sprintf("  [✅] %s\n", basename(ruta_07b_csv)))

# 7b. TXT tabla GLM completa con encabezado
tabla_07b_glm_txt <- tabla_07b_glm %>%
  select(term, estimate, std.error, statistic, p.value, significancia) %>%
  rename(
    variable  = term,
    log_odds  = estimate,
    se_cl     = std.error,
    z_stat    = statistic,
    p_valor   = p.value,
    sig       = significancia
  )

encabezado_07b <- c(
  paste0("GLM binomial post-LASSO | Modelo ", SUFIJO_MODELO_GLM),
  paste0("Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("SE clusterizados por codusu (HC1) — Cameron & Miller (2015)"),
  paste0("N=", format(n_obs_07b, big.mark = ","),
         " | Pseudo-R² McFadden=", round(pseudo_r2_mf, 4),
         " | AIC=", round(aic_07b, 2),
         " | BIC=", round(bic_07b, 2)),
  paste0("Coeficientes en log-odds. Ver CSV adjunto para AMEs."),
  "Sig: *** p<0.001 | ** p<0.01 | * p<0.05 | . p<0.10",
  "================================================================",
  ""
)

ruta_07b_txt <- file.path(DIR_REPORTES,
                          paste0("07b_tabla_glm_", SUFIJO_MODELO_GLM, ".txt"))
writeLines(encabezado_07b, ruta_07b_txt)
write.table(tabla_07b_glm_txt, ruta_07b_txt,
            sep = "\t", row.names = FALSE,
            quote = FALSE, na = "", append = TRUE, col.names = FALSE)
cat(sprintf("  [✅] %s\n", basename(ruta_07b_txt)))

# 7c. Contrato 07b GLM
contrato_07b <- list(
  # Identificación
  script         = "07b_postlasso_GLM.R",
  sufijo_modelo  = SUFIJO_MODELO_GLM,
  fecha          = Sys.time(),
  version_tag    = paste0("v1_postlasso_", SUFIJO_MODELO_GLM),

  # Universo
  n_train        = nrow(df_train),
  n_test         = nrow(df_test),
  n_vars_glm     = length(vars_disponibles),
  vars_glm       = vars_disponibles,

  # GLM — bondad de ajuste
  glm_pseudo_r2  = pseudo_r2_mf,
  glm_aic        = aic_07b,
  glm_bic        = bic_07b,
  glm_deviance   = glm_res_dev,
  glm_null_dev   = glm_null_dev,
  glm_df_res     = df_res_07b,
  glm_n          = n_obs_07b,
  vcov_type      = "vcovCL",
  cluster_var    = "codusu",

  # Tests de especificación
  bp_stat        = NA_real_,    # N/A para GLM binomial
  bp_pval        = NA_real_,
  reset_stat     = NA_real_,    # N/A para GLM binomial
  reset_pval     = NA_real_,
  n_vif_alto     = n_07b_vif_alto,

  # AMEs
  ames              = ames_07b_df,
  n_ame_subsample   = N_AME_SUBSAMPLE,   # submuestra usada para avg_slopes()

  # Clasificación
  auc_roc        = auc_07b_val,
  auc_roc_ci     = auc_07b_ci,
  auc_pr         = pr_07b_auc,
  metricas_clf   = metricas_07b_clf,
  umbral_youden  = umbral_07b,
  hl_stat        = hl_07b_stat,
  hl_pval        = hl_07b_pval,
  pct_fuera_01   = pct_07b_fuera,

  # Tablas
  tabla_glm      = tabla_07b_glm,
  vif_tabla      = vif_07b_vals,
  comp_coefs     = comp_07b_coefs,
  comp_lambda    = comp_07b_lambda,
  hl_df          = hl_07b_df,

  # Elementos para comparación downstream (07e, 09a)
  pred_test      = list(raw = pred_07b_raw, clip = pred_07b_clip),
  roc_df         = roc_07b_df,
  pr_df          = pr_07b_df,

  # Fila plana para bind_rows(c_lpm$resumen_comparacion, c_glm$resumen_comparacion)
  # r2 = pseudo-R² McFadden (documentado). Columnas compatibles con LPM.
  resumen_comparacion = tibble(
    version        = paste0("postlasso_", SUFIJO_MODELO_GLM),
    fecha          = format(Sys.time(), "%Y-%m-%d %H:%M"),
    n_vars_ols     = length(vars_disponibles),   # nombre heredado para bind_rows
    r2             = pseudo_r2_mf,               # pseudo-R² McFadden
    r2_adj         = NA_real_,                   # N/A para GLM
    f_stat         = NA_real_,                   # N/A para GLM
    bp_pval        = NA_real_,                   # N/A para GLM
    reset_pval     = NA_real_,                   # N/A para GLM
    n_vif_alto     = n_07b_vif_alto,
    auc_roc        = auc_07b_val,
    auc_roc_ci_lo  = auc_07b_ci[1],
    auc_roc_ci_hi  = auc_07b_ci[3],
    auc_pr         = pr_07b_auc,
    f1             = f1_07b,
    mcc            = mcc_07b,
    kappa          = kappa_07b,
    hl_pval        = hl_07b_pval,
    umbral_youden  = umbral_07b
  )
)

saveRDS(contrato_07b, PATH_07B_CONTRATO_GLM)
cat(sprintf("  [✅] %s\n", basename(PATH_07B_CONTRATO_GLM)))

saveRDS(m_07b_glm, PATH_07_POSTLASSO_GLM)   # D36: objeto glm — necesario para 08b/08c y backcasting
cat(sprintf("  [✅] %s\n", basename(PATH_07_POSTLASSO_GLM)))

# 📑 Checklist -----------------------------------------------------------------
cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  RESUMEN 07b_postlasso_GLM.R\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  GLM: N=%d vars | Pseudo-R² McFadden=%.4f\n",
            length(vars_disponibles), pseudo_r2_mf))
cat(sprintf("  AIC: %.2f | BIC: %.2f\n", aic_07b, bic_07b))
cat(sprintf("  Breusch-Pagan:  N/A (GLM binomial — varianza por construcción)\n"))
cat(sprintf("  Ramsey RESET:   N/A (GLM binomial)\n"))
cat(sprintf("  VIF/GVIF>10:    %s\n",
            ifelse(is.na(n_07b_vif_alto), "N/A", as.character(n_07b_vif_alto))))
cat(sprintf("  AMEs calculados: %s\n",
            ifelse(!is.null(ames_07b_df), paste0(nrow(ames_07b_df), " vars"), "no disponible")))
cat(sprintf("  AUC-ROC:        %.4f [%.4f – %.4f]\n",
            auc_07b_val, auc_07b_ci[1], auc_07b_ci[3]))
cat(sprintf("  ΔAUC vs 07a:    %+.4f\n", auc_07b_val - contrato_07a$auc_test))
cat(sprintf("  AUC-PR:         %.4f\n", pr_07b_auc))
cat(sprintf("  F1: %.4f | MCC: %.4f | Kappa: %.4f\n",
            f1_07b, mcc_07b, kappa_07b))
cat(sprintf("  H-L calibración: p=%s\n",
            format.pval(hl_07b_pval, digits = 3)))
cat(sprintf("  Pred fuera [0,1]: %.2f%% (esperado ~0%%)\n", pct_07b_fuera))
cat("\n  Outputs:\n")
cat(sprintf("    [✅] %s\n", basename(PATH_07B_CONTRATO_GLM)))
cat(sprintf("    [✅] %s\n", basename(ruta_07b_csv)))
cat(sprintf("    [✅] %s\n", basename(ruta_07b_txt)))
cat("\n")

end_time <- Sys.time()
toc()
cat(sprintf("  Tiempo total: %.1f min\n",
            as.numeric(difftime(end_time, start_time, units = "mins"))))
cat("\n✅ Script 07b GLM completado\n")
