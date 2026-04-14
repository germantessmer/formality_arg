# =============================================================================
# [EN] 07b_postlasso_LPM.R -- Post-LASSO OLS with clustered SEs, specification tests, and classification metrics (LPM)
# INPUTS:  Contracts/models from 07a LPM, rdos/datos/06_theta_predichos.rds
# OUTPUTS: rdos/contratos/07b_contrato_postlasso_LPM*.rds
# =============================================================================
# 🌟 07b_postlasso_LPM.R 🌟 ####
# OBJETIVO : OLS post-LASSO con SE clusterizados + tests de especificación
#            + métricas de clasificación sobre test set
# INPUTS   : PATH_07_CONTRATO      → contrato 07a (vars seleccionadas, split info)
#            PATH_07_MODELO_LASSO  → cv.glmnet object
#            PATH_07_RECIPE_LASSO  → recipe preparado (bake)
#            PATH_06_THETA         → panel con θ predichos
#            PATH_06_MODELO_HETERO → para extraer theta_data Modelo A
# OUTPUTS  : PATH_07B_CONTRATO     → rdos/contratos/07b_contrato_postlasso_LPM3T.rds
#            07b_comp_coefs_LPM3T.csv   → tabla LASSO vs OLS
#            07b_tabla_ols_LPM3T.txt    → tabla OLS completa con encabezado

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
tic("07b completo")
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  🌟 07b_postlasso_LPM.R 🌟\n")
cat("  OLS post-LASSO | SE clusterizados | Tests | Métricas\n")
cat("  Inicio:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# 🪫 1. Carga de inputs + reconstruccion split ---------------------------------
cat("── FASE 1: Carga de inputs + reconstrucción split ──────────────────\n")

# 1a. Contratos y modelos de 07a
cat("  Cargando contrato_07a...\n")
contrato_07a   <- readRDS(PATH_07_CONTRATO)
cv_fit         <- readRDS(PATH_07_MODELO_LASSO)
recipe_prepped <- readRDS(PATH_07_RECIPE_LASSO)

cat(sprintf("  [✅] Contrato 07a: %d vars seleccionadas (λ.1se)\n",
            length(contrato_07a$vars_seleccionadas)))

# 1b. Panel con θ + join Modelo A
cat("  Cargando panel θ...\n")
panel_raw      <- readRDS(PATH_06_THETA)
modelo_hetero  <- readRDS(PATH_06_MODELO_HETERO)

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
cat("  [✅] Split consistente con 07a\n")

# 1d. Bake del recipe → matrices de features
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

# 🪫 2. OLS post-LASSO ---------------------------------------------------------
cat("\n── FASE 2: OLS post-LASSO ──────────────────────────────────────────\n")

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

# Data frame para lm() — features + target + codusu (para vcovCL)
df_07b_ols <- X_train_df %>%
  select(all_of(vars_disponibles)) %>%
  mutate(
    formalidad_bin = y_train,
    codusu         = df_train$codusu   # identificador hogar para clustering SE
  )

formula_07b_ols <- as.formula(
  paste("formalidad_bin ~", paste(vars_disponibles, collapse = " + "))
)

cat("  Estimando OLS...\n")
m_07b_ols <- lm(formula_07b_ols, data = df_07b_ols)

# SE clusterizados por hogar (HC1) — Cameron & Miller (2015)
# Corrige heterocedasticidad + correlación intra-hogar (panel rotante EPH)
cat("  Aplicando vcovCL (cluster = codusu, HC1)...\n")
vcov_07b_cl      <- vcovCL(m_07b_ols, cluster = ~codusu, type = "HC1")
test_07b_robusto <- coeftest(m_07b_ols, vcov = vcov_07b_cl)

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

glance_07b_ols <- glance(m_07b_ols)

cat(sprintf("  [✅] R²: %.4f | R² ajustado: %.4f | N: %s\n",
            glance_07b_ols$r.squared,
            glance_07b_ols$adj.r.squared,
            format(glance_07b_ols$nobs, big.mark = ",")))
cat(sprintf("       F-stat: %.2f (df1=%d, df2=%d) | p=%s\n",
            glance_07b_ols$statistic,
            glance_07b_ols$df,
            glance_07b_ols$df.residual,
            format.pval(glance_07b_ols$p.value, digits = 3)))

# θ en el OLS — mostrar coeficientes sin restricción
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

# 🪫 3. Tests de especificacion ------------------------------------------------
cat("\n── FASE 3: Tests de especificación ────────────────────────────────\n")

# 3a. Breusch-Pagan (heterocedasticidad)
bp_07b_test <- bptest(m_07b_ols)
cat(sprintf("  Breusch-Pagan: χ²=%.2f, df=%d, p=%s %s\n",
            as.numeric(bp_07b_test$statistic),
            as.integer(bp_07b_test$parameter),
            format.pval(bp_07b_test$p.value, digits = 3),
            ifelse(bp_07b_test$p.value < 0.05,
                   "→ heterocedasticidad confirmada [HC1 aplicados ✅]",
                   "[✅] homocedasticidad no rechazada")))

# 3b. Ramsey RESET (forma funcional)
reset_07b_test <- resettest(m_07b_ols, power = 2:3, type = "fitted")
cat(sprintf("  Ramsey RESET:  F=%.3f, df1=%d, df2=%d, p=%s %s\n",
            as.numeric(reset_07b_test$statistic),
            as.integer(reset_07b_test$parameter[1]),
            as.integer(reset_07b_test$parameter[2]),
            format.pval(reset_07b_test$p.value, digits = 3),
            ifelse(reset_07b_test$p.value < 0.05,
                   "[⚠️] no-linealidad sugerida — documentar en paper",
                   "[✅] forma funcional no rechazada")))

# 3c. VIF / GVIF (multicolinealidad)
# vif() retorna GVIF para variables categóricas multiclase
cat("  Calculando VIF/GVIF...\n")
vif_07b_vals <- tryCatch({
  vif_raw <- vif(m_07b_ols)
  # Para variables binarias: VIF escalar. Para multilevel: GVIF^(1/(2*Df))
  if (is.matrix(vif_raw)) {
    # GVIF — usar columna GVIF^(1/(2*Df)) como indicador comparable
    vif_df <- as.data.frame(vif_raw) %>%
      rownames_to_column("variable") %>%
      rename(
        GVIF    = GVIF,
        Df      = Df,
        GVIF_adj = `GVIF^(1/(2*Df))`
      ) %>%
      mutate(vif_comparable = GVIF_adj^2)   # equivalente a VIF para Df=1
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

# Predicciones OLS sobre test
df_07b_test_pred <- X_test_df %>%
  select(all_of(vars_disponibles))

pred_07b_raw  <- predict(m_07b_ols, newdata = df_07b_test_pred)
pred_07b_clip <- pmax(0, pmin(1, pred_07b_raw))

pct_07b_fuera <- mean(pred_07b_raw < 0 | pred_07b_raw > 1) * 100
cat(sprintf("  Predicciones fuera [0,1]: %.2f%% %s\n",
            pct_07b_fuera,
            ifelse(pct_07b_fuera < 10, "[✅]", "[⚠️] documentar")))

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

# Comparar con LASSO (07a)
cat(sprintf("  ΔAUC vs LASSO 07a: %+.4f\n",
            auc_07b_val - contrato_07a$auc_test))

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

precision_07b  <- tp / (tp + fp)
recall_07b     <- tp / (tp + fn)
f1_07b         <- 2 * precision_07b * recall_07b / (precision_07b + recall_07b)
mcc_07b        <- (as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)) /
                   sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) *
                        as.numeric(tn + fp) * as.numeric(tn + fn))
accuracy_07b   <- (tp + tn) / (tp + tn + fp + fn)
# Kappa de Cohen
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
  umbral       = umbral_07b,
  accuracy     = accuracy_07b,
  sensibilidad = recall_07b,
  especificidad = coords_07b$specificity[1],
  precision    = precision_07b,
  f1           = f1_07b,
  mcc          = mcc_07b,
  kappa        = kappa_07b
)

# 4d. Hosmer-Lemeshow (calibración — 10 grupos)
n_hl   <- length(y_test)
grp_hl <- cut(pred_07b_clip,
              breaks    = quantile(pred_07b_clip, probs = seq(0, 1, 0.1)),
              include.lowest = TRUE,
              labels    = FALSE)
hl_07b_df <- tibble(
  grupo          = grp_hl,
  observado      = y_test,
  predicho       = pred_07b_clip
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

# 🪫 5. Tabla comparativa LASSO vs OLS -----------------------------------------
cat("\n── FASE 5: Tabla comparativa LASSO vs OLS ─────────────────────────\n")

# Extraer coefs LASSO de cv_fit para λ.1se
coefs_07b_lasso_raw <- coef(cv_fit, s = "lambda.1se")
coefs_07b_lasso <- tibble(
  variable     = rownames(coefs_07b_lasso_raw)[-1],   # excluir intercept
  coef_lasso   = as.numeric(coefs_07b_lasso_raw)[-1]
) %>% filter(coef_lasso != 0)

# Tabla comparativa: LASSO vs OLS con SE
comp_07b_coefs <- tabla_07b_ols %>%
  rename(variable = term) %>%
  filter(variable != "(Intercept)") %>%
  select(variable, estimate, std.error, p.value, significancia) %>%
  rename(
    coef_ols    = estimate,
    se_ols_cl   = std.error,
    p_ols       = p.value,
    sig_ols     = significancia
  ) %>%
  left_join(coefs_07b_lasso, by = "variable") %>%
  mutate(
    delta_coef  = coef_ols - coef_lasso,
    en_lasso    = !is.na(coef_lasso)
  ) %>%
  arrange(desc(abs(coef_ols)))

cat(sprintf("  [✅] Tabla comparativa: %d variables\n", nrow(comp_07b_coefs)))

# Tabla comparativa de performance (λ.min vs λ.1se vs OLS)
comp_07b_lambda <- tibble(
  especificacion = c("LASSO λ.min", "LASSO λ.1se", "OLS post-LASSO"),
  n_vars         = c(contrato_07a$n_vars_sel_min,
                     contrato_07a$n_vars_sel_1se,
                     length(vars_disponibles)),
  auc_test       = c(NA_real_, contrato_07a$auc_test, auc_07b_val),
  r2             = c(NA_real_, NA_real_, glance_07b_ols$r.squared),
  r2_adj         = c(NA_real_, NA_real_, glance_07b_ols$adj.r.squared)
)

print(comp_07b_lambda)

# 🪫 6. Outputs — CSV, TXT, contrato ------------------------------------------
cat("\n── FASE 6: Guardando outputs ───────────────────────────────────────\n")

# 6a. CSV comparativo LASSO vs OLS
ruta_07b_csv <- file.path(DIR_REPORTES,
                          paste0("07b_comp_coefs_", SUFIJO_MODELO_LPM, ".csv"))
write_csv(comp_07b_coefs, ruta_07b_csv)
cat(sprintf("  [✅] %s\n", basename(ruta_07b_csv)))

# 6b. TXT tabla OLS completa con encabezado
tabla_07b_ols_txt <- tabla_07b_ols %>%
  select(term, estimate, std.error, statistic, p.value, significancia) %>%
  rename(
    variable  = term,
    coef_ols  = estimate,
    se_cl     = std.error,
    t_stat    = statistic,
    p_valor   = p.value,
    sig       = significancia
  )

encabezado_07b <- c(
  paste0("OLS post-LASSO | Modelo ", SUFIJO_MODELO_LPM),
  paste0("Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("SE clusterizados por codusu (HC1) — Cameron & Miller (2015)"),
  paste0("N=", format(glance_07b_ols$nobs, big.mark = ","),
         " | R²=", round(glance_07b_ols$r.squared, 4),
         " | R²adj=", round(glance_07b_ols$adj.r.squared, 4),
         " | F=", round(glance_07b_ols$statistic, 2)),
  "Sig: *** p<0.001 | ** p<0.01 | * p<0.05 | . p<0.10",
  "================================================================",
  ""
)

ruta_07b_txt <- file.path(DIR_REPORTES,
                          paste0("07b_tabla_ols_", SUFIJO_MODELO_LPM, ".txt"))
writeLines(encabezado_07b, ruta_07b_txt)
write.table(tabla_07b_ols_txt, ruta_07b_txt,
            sep = "\t", row.names = FALSE,
            quote = FALSE, na = "", append = TRUE, col.names = FALSE)
cat(sprintf("  [✅] %s\n", basename(ruta_07b_txt)))

# 6c. Contrato 07b — incluye pred_test, roc_df, pr_df, residuos_ols,
#     resumen_comparacion para bind_rows con GLM/SLS downstream
contrato_07b <- list(
  # Identificación
  script         = "07b_postlasso_LPM.R",
  sufijo_modelo  = SUFIJO_MODELO_LPM,
  fecha          = Sys.time(),
  version_tag    = paste0("v1_postlasso_", SUFIJO_MODELO_LPM),

  # Universo
  n_train        = nrow(df_train),
  n_test         = nrow(df_test),
  n_vars_ols     = length(vars_disponibles),
  vars_ols       = vars_disponibles,

  # OLS
  ols_r2         = glance_07b_ols$r.squared,
  ols_r2_adj     = glance_07b_ols$adj.r.squared,
  ols_n          = glance_07b_ols$nobs,
  ols_f_stat     = glance_07b_ols$statistic,
  ols_f_pval     = glance_07b_ols$p.value,
  ols_df         = glance_07b_ols$df,
  ols_df_res     = glance_07b_ols$df.residual,
  vcov_type      = "vcovCL",
  cluster_var    = "codusu",

  # Tests de especificación
  bp_stat        = as.numeric(bp_07b_test$statistic),
  bp_pval        = bp_07b_test$p.value,
  reset_stat     = as.numeric(reset_07b_test$statistic),
  reset_pval     = reset_07b_test$p.value,
  n_vif_alto     = n_07b_vif_alto,

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
  tabla_ols      = tabla_07b_ols,
  vif_tabla      = vif_07b_vals,
  comp_coefs     = comp_07b_coefs,
  comp_lambda    = comp_07b_lambda,
  hl_df          = hl_07b_df,

  # Elementos para comparación downstream (07e, 09a, GLM, SLS)
  pred_test      = list(raw = pred_07b_raw, clip = pred_07b_clip),
  roc_df         = roc_07b_df,
  pr_df          = pr_07b_df,
  residuos_ols   = residuals(m_07b_ols),

  # Fila plana para bind_rows(c1$resumen_comparacion, c2$resumen_comparacion)
  resumen_comparacion = tibble(
    version        = paste0("postlasso_", SUFIJO_MODELO_LPM),
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
    umbral_youden  = umbral_07b
  )
)

saveRDS(contrato_07b, PATH_07B_CONTRATO)
cat(sprintf("  [✅] %s\n", basename(PATH_07B_CONTRATO)))

saveRDS(m_07b_ols, PATH_07_POSTLASSO_LPM)   # D36: objeto lm — necesario para 08b/08c y backcasting
cat(sprintf("  [✅] %s\n", basename(PATH_07_POSTLASSO_LPM)))

# 📑 Resumen final -------------------------------------------------------------
cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  RESUMEN 07b_postlasso_LPM.R\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  OLS: N=%d vars | R²=%.4f | R²adj=%.4f\n",
            length(vars_disponibles),
            glance_07b_ols$r.squared,
            glance_07b_ols$adj.r.squared))
cat(sprintf("  Breusch-Pagan:  p=%s\n",
            format.pval(bp_07b_test$p.value, digits = 3)))
cat(sprintf("  Ramsey RESET:   p=%s\n",
            format.pval(reset_07b_test$p.value, digits = 3)))
cat(sprintf("  VIF/GVIF>10:    %s\n",
            ifelse(is.na(n_07b_vif_alto), "N/A", as.character(n_07b_vif_alto))))
cat(sprintf("  AUC-ROC:        %.4f [%.4f – %.4f]\n",
            auc_07b_val, auc_07b_ci[1], auc_07b_ci[3]))
cat(sprintf("  ΔAUC vs 07a:    %+.4f\n", auc_07b_val - contrato_07a$auc_test))
cat(sprintf("  AUC-PR:         %.4f\n", pr_07b_auc))
cat(sprintf("  F1: %.4f | MCC: %.4f | Kappa: %.4f\n",
            f1_07b, mcc_07b, kappa_07b))
cat(sprintf("  H-L calibración: p=%s\n",
            format.pval(hl_07b_pval, digits = 3)))
cat(sprintf("  Pred fuera [0,1]: %.2f%%\n", pct_07b_fuera))
cat("\n  Outputs:\n")
cat(sprintf("    [✅] %s\n", basename(PATH_07B_CONTRATO)))
cat(sprintf("    [✅] %s\n", basename(ruta_07b_csv)))
cat(sprintf("    [✅] %s\n", basename(ruta_07b_txt)))
cat("\n")

end_time <- Sys.time()
toc()
cat(sprintf("  Tiempo total: %.1f min\n",
            as.numeric(difftime(end_time, start_time, units = "mins"))))
cat("\n✅ Script 07b completado\n")
