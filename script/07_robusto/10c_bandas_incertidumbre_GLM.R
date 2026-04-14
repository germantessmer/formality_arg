# =============================================================================
# [EN] 10c_bandas_incertidumbre_GLM.R -- Bootstrap confidence intervals (95% CI) for hybrid GLM formality series
# INPUTS:  rdos/modelos/07a_coef_boot_GLM*.rds, panel and recipe from 07-08 GLM
# OUTPUTS: rdos/reportes/00_bandas_incertidumbre_GLM.html, rdos/contratos/10c_*.rds
# =============================================================================
# 🌟 10c_bandas_incertidumbre_GLM.R 🌟 ####
# OBJETIVO: Generar bandas de incertidumbre (IC 95%) para la serie hibrida GLM
#   Usa la matriz completa de coeficientes bootstrap (coef_boot) guardada por
#   07a_lasso_GLM.R. Para cada draw, re-predice el bloque predicho, reconstruye
#   la variable hibrida y agrega tasas trimestrales ponderadas.
# INPUTS:  rdos/modelos/07a_coef_boot_{SUFIJO_MODELO_GLM}.rds (matriz bootstrap real)
#          rdos/modelos/07_modelo_lasso_{SUFIJO_MODELO_GLM}.rds (cv_fit para lambda/intercept)
#          rdos/modelos/07_recipe_lasso_{SUFIJO_MODELO_GLM}.rds (recipe preprocesamiento)
#          rdos/datos/08_panel_formalidad_{SUFIJO_MODELO_GLM}.rds (panel con backcasting)
# OUTPUTS: rdos/reportes/00_bandas_incertidumbre_GLM.csv
#          rdos/reportes/00_bandas_incertidumbre_GLM.html (reporte diagnostico)
#          rdos/contratos/10c_contrato_ci_bootstrap.rds
# TIEMPO ESTIMADO: ~3-5 minutos (500 draws parametricos, sin re-LASSO)

# ⌛ Inicio contador de tiempo -------------------------------------------------

t_inicio <- proc.time()

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(glmnet)
  library(recipes)
  library(Matrix)
})

# 🔧 Cargar configuración y funciones ------------------------------------------

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# Funciones defensivas (no exportadas por funciones_comunes.R)
safe_load <- function(path) {
  if (!file.exists(path)) stop("Archivo no encontrado: ", path)
  readRDS(path)
}
safe_get <- function(obj, ...) {
  keys <- c(...)
  result <- obj
  for (k in keys) {
    if (is.null(result) || !k %in% names(result)) return(NA_real_)
    result <- result[[k]]
  }
  result
}

# Directorio de figuras para este script
DIR_FIGURAS_10C <- file.path(DIR_FIGURAS, "10c_bandas_incertidumbre")
dir.create(DIR_FIGURAS_10C, recursive = TRUE, showWarnings = FALSE)

cat("=" , rep("=", 69), "\n", sep = "")
cat("  Bandas de incertidumbre — Serie hibrida GLM (parametrico)\n")
cat("=" , rep("=", 69), "\n", sep = "")

set.seed(SEED_GLOBAL)

# 🪫 1. Cargar insumos -------------------------------------------------------

path_coef_boot <- file.path(DIR_MODELOS, paste0("07a_coef_boot_", SUFIJO_MODELO_GLM, ".rds"))
coef_boot    <- safe_load(path_coef_boot)
cv_fit       <- safe_load(PATH_07_MODELO_GLM)
recipe_lasso <- safe_load(PATH_07_RECIPE_GLM)

# Panel con backcasting
path_panel_glm <- file.path(DIR_DATOS, paste0("08_panel_formalidad_", SUFIJO_MODELO_GLM, ".rds"))
panel <- safe_load(path_panel_glm)

N_DRAWS <- nrow(coef_boot)

cat("Panel cargado:", nrow(panel), "obs\n")
cat("Bootstrap matrix:", N_DRAWS, "draws x", ncol(coef_boot), "coefs\n")

# 🪫 2. Coeficientes punto del modelo final ----------------------------------

coef_punto <- as.vector(coef(cv_fit, s = "lambda.1se"))
nombres_coef <- rownames(coef(cv_fit, s = "lambda.1se"))

cat("Coeficientes del modelo:", length(coef_punto), "(incl. intercept)\n")

# Verificar que coef_boot tiene las mismas columnas que el modelo
cat("Columnas coef_boot:", ncol(coef_boot), "| Modelo (sin intercept):", length(coef_punto) - 1, "\n")

# 🪫 3. Preparar matriz de prediccion ----------------------------------------

# Identificar universo elegible y columnas
col_tipo       <- "tipo_estimacion_pea"
col_clase_cal  <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_GLM, "_pea")
col_formalidad <- "formalidad_valida"

cat("Columna tipo estimacion:", col_tipo, "\n")
cat("Columna clasificacion calibrada:", col_clase_cal, "\n")

# Construir variable hibrida inline:
#   - Observado (overlap con formalidad_valida) => usar formalidad_valida
#   - Backcasting (historico predicho) => usar clasificacion calibrada
#   - Resto (No_aplica, etc.) => NA
panel <- panel %>%
  mutate(
    hibrida = case_when(
      .data[[col_tipo]] == "Observado"  ~ as.character(.data[[col_formalidad]]),
      .data[[col_tipo]] == "Backcasting" ~ as.character(.data[[col_clase_cal]]),
      TRUE ~ NA_character_
    ),
    # Flag: es parte del bloque predicho (Backcasting)?
    es_predicho = (.data[[col_tipo]] == "Backcasting")
  )

# Filtrar al universo donde la hibrida tiene valor
panel_elegible <- panel %>%
  filter(!is.na(hibrida))

cat("Universo elegible (hibrida no-NA):", nrow(panel_elegible), "obs\n")

# Distinguir observados vs predichos
es_observado <- !panel_elegible$es_predicho

cat("  Observados:", sum(es_observado), "| Backcasting:", sum(!es_observado), "\n")

# Preparar features para prediccion (solo los backcasting necesitan re-prediccion)
# Renombrar theta columns para compatibilidad con recipe (07a usa theta_A_mA/theta_B_mA)
if ("theta_A" %in% names(panel_elegible) && !"theta_A_mA" %in% names(panel_elegible)) {
  panel_elegible <- panel_elegible %>%
    rename(theta_A_mA = theta_A, theta_B_mA = theta_B)
  cat("  Renombrado theta_A -> theta_A_mA, theta_B -> theta_B_mA\n")
}

panel_pred <- panel_elegible %>% filter(es_predicho)

if (nrow(panel_pred) == 0) {
  stop("No hay observaciones predichas en el panel. Verificar columna tipo_estimacion.")
}

# Aplicar recipe al bloque predicho
vars_recipe <- recipe_lasso$var_info$variable[recipe_lasso$var_info$role == "predictor"]
vars_disponibles <- intersect(vars_recipe, names(panel_pred))

X_pred <- tryCatch({
  bake(recipe_lasso, new_data = panel_pred %>% select(any_of(c(vars_recipe)))) %>%
    as.matrix()
}, error = function(e) {
  cat("Error en bake:", e$message, "\n")
  cat("Intentando con variables disponibles...\n")
  bake(recipe_lasso, new_data = panel_pred) %>%
    select(-any_of(c("formalidad_bin"))) %>%
    as.matrix()
})

cat("Matriz de prediccion:", nrow(X_pred), "x", ncol(X_pred), "\n")

# 🪫 4. Cargar umbral de calibracion -----------------------------------------

c08 <- safe_load(file.path(DIR_CONTRATOS,
                           paste0("08_contrato_backcasting_", SUFIJO_MODELO_GLM, ".rds")))

umbral_cal <- tryCatch(
  safe_get(c08, "umbral_calibracion"),
  error = function(e) {
    # Fallback: usar Youden del contrato 07b
    c07b <- safe_load(PATH_07B_CONTRATO_GLM)
    safe_get(c07b, "umbral_youden")
  }
)

if (is.na(umbral_cal) || is.null(umbral_cal)) {
  umbral_cal <- 0.51  # HC documentado: fallback defensivo si ningun contrato tiene umbral
  cat("AVISO: usando umbral fallback =", umbral_cal, "\n")
}
cat("Umbral de calibracion:", round(umbral_cal, 4), "\n")

# 🪫 5. Generar draws parametricos -------------------------------------------

cat("\nGenerando predicciones con", N_DRAWS, "draws bootstrap reales...\n")

# Pre-computar estructura
draws_tasas <- matrix(NA_real_, nrow = length(unique(panel_elegible$periodo_id)), ncol = N_DRAWS)
periodos <- sort(unique(panel_elegible$periodo_id))
rownames(draws_tasas) <- periodos

pesos_elegible   <- panel_elegible$pondera
periodo_elegible <- panel_elegible$periodo_id

# Formalidad hibrida punto (bloque observado no cambia entre draws)
formal_obs <- as.integer(panel_elegible$hibrida %in% c("Formal", "Formal oficial"))
cat("  Tasa formal punto (ponderada):", round(weighted.mean(formal_obs, pesos_elegible) * 100, 2), "%\n")

# Indices del bloque predicho dentro de panel_elegible
idx_pred_en_elegible <- which(!es_observado)

# coef_boot tiene columnas = nombres de coefs (incl. intercept)
# Verificar si intercept esta incluido
tiene_intercept <- "(Intercept)" %in% colnames(coef_boot)
cat("  coef_boot incluye intercept:", tiene_intercept, "\n")

for (d in seq_len(N_DRAWS)) {
  if (d %% 50 == 0) cat("  Draw", d, "/", N_DRAWS, "\n")

  # Extraer vector de coeficientes del draw d
  beta_d <- coef_boot[d, ]

  if (tiene_intercept) {
    intercept_d <- beta_d["(Intercept)"]
    coefs_d     <- beta_d[names(beta_d) != "(Intercept)"]
  } else {
    intercept_d <- coef_punto[1]  # usar intercept punto si no esta en boot
    coefs_d     <- beta_d
  }

  # Alinear coefs con columnas de X_pred
  coefs_aligned <- rep(0, ncol(X_pred))
  names(coefs_aligned) <- colnames(X_pred)
  shared <- intersect(names(coefs_d), colnames(X_pred))
  coefs_aligned[shared] <- coefs_d[shared]

  # Prediccion: logit inverso
  eta_d  <- X_pred %*% coefs_aligned + intercept_d
  prob_d <- as.vector(1 / (1 + exp(-eta_d)))

  # Clasificar con umbral
  clase_d <- as.integer(prob_d >= umbral_cal)

  # Construir hibrido: observado donde hay, predicho donde no
  formal_draw <- formal_obs
  formal_draw[idx_pred_en_elegible] <- clase_d

  # Agregar tasas por periodo
  for (p_idx in seq_along(periodos)) {
    mask_p <- periodo_elegible == periodos[p_idx]
    if (sum(mask_p) > 0) {
      draws_tasas[p_idx, d] <- weighted.mean(formal_draw[mask_p], pesos_elegible[mask_p])
    }
  }
}

cat("Draws completados.\n")

# 🪫 6. Calcular bandas -----------------------------------------------------

df_bandas <- tibble(
  periodo_id  = periodos,
  tasa_punto  = NA_real_,
  tasa_media  = apply(draws_tasas, 1, mean, na.rm = TRUE),
  tasa_sd     = apply(draws_tasas, 1, sd,   na.rm = TRUE),
  ic_025      = apply(draws_tasas, 1, quantile, probs = 0.025, na.rm = TRUE),
  ic_975      = apply(draws_tasas, 1, quantile, probs = 0.975, na.rm = TRUE),
  ic_050      = apply(draws_tasas, 1, quantile, probs = 0.050, na.rm = TRUE),
  ic_950      = apply(draws_tasas, 1, quantile, probs = 0.950, na.rm = TRUE)
)

# Tasa punto (del hibrido original, no perturbado)
for (p_idx in seq_along(periodos)) {
  mask_p <- periodo_elegible == periodos[p_idx]
  if (sum(mask_p) > 0) {
    df_bandas$tasa_punto[p_idx] <- weighted.mean(formal_obs[mask_p], pesos_elegible[mask_p])
  }
}

# Convertir a porcentaje
df_bandas <- df_bandas %>%
  mutate(across(tasa_punto:ic_950, ~ round(. * 100, 2)))

cat("\n", rep("=", 60), "\n")
cat("  RESULTADOS — Bandas de incertidumbre (GLM hibrida)\n")
cat(rep("=", 60), "\n\n")

print(df_bandas %>% select(periodo_id, tasa_punto, ic_025, ic_975), n = 40)

cat("\nAncho medio IC 95%:", round(mean(df_bandas$ic_975 - df_bandas$ic_025), 2), "pp\n")
cat("Ancho maximo IC 95%:", round(max(df_bandas$ic_975 - df_bandas$ic_025), 2), "pp\n")

# 🪫 7. Guardar CSV ----------------------------------------------------------

path_csv  <- file.path(DIR_REPORTES, "00_bandas_incertidumbre_GLM.csv")
path_html <- file.path(DIR_REPORTES, "00_bandas_incertidumbre_GLM.html")

write_csv(df_bandas, path_csv)
cat("CSV guardado:", path_csv, "\n")

# 🪫 8. Generar reporte HTML -------------------------------------------------

cat("-- Construyendo reporte HTML ------------------------------------------\n")

rmd_temp <- tempfile(fileext = ".Rmd")
con      <- file(rmd_temp, open = "wt", encoding = "UTF-8")

writeLines('---
title: "Bandas de Incertidumbre -- Serie Hibrida GLM"
subtitle: "Diagnostico auxiliar -- Review 1, Observacion A2"
date: "Generado: `r format(Sys.time(), \'%d/%m/%Y %H:%M\')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float:
      collapsed: false
    toc_depth: 3
    number_sections: true
    df_print: kable
---', con)

writeLines('
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 10, fig.height = 5.5, dpi = 150)
library(tidyverse); library(knitr); library(kableExtra); library(scales)
source(here::here("script", "config", "funciones_comunes.R"))
```
', con)

# ── Seccion 1: Proposito ──
writeLines(sprintf('
# Proposito y contexto

Este reporte responde a la observacion **A2** del Review 1:

> *"Uncertainty quantification for the bridged series is limited; confidence bands
> incorporating survey design and prediction uncertainty are not reported."*

## Metodo

Se genera un **IC 95%%** para la tasa de formalidad hibrida trimestral usando los
**%d vectores de coeficientes bootstrap reales** estimados en 07a_lasso_GLM.R:

1. Se cargan los %d draws de coeficientes (cada uno es un re-muestreo con reemplazo
   del training set, re-estimado con LASSO binomial)
2. Para cada draw, se re-predicen las probabilidades del **bloque predicho** (el bloque
   observado permanece fijo)
3. Se clasifican con el umbral de calibracion (%.4f) y se reconstruye la variable hibrida
4. Se agregan tasas trimestrales ponderadas (PONDERA)
5. Se calculan percentiles 2.5%% y 97.5%% sobre los %d draws

**Ventaja vs. metodo parametrico:** Los draws reales capturan las correlaciones entre
coeficientes (multicolinealidad), produciendo bandas mas estrechas y realistas.

## Donde incorporar estos resultados en el paper

| Resultado | Seccion LaTeX | Uso |
|-----------|---------------|-----|
| Grafico de serie con bandas | Seccion 5 (Empirical Illustration), nueva figura o panel en Fig. 5 | Evidencia visual de incertidumbre |
| Ancho medio del IC | Seccion 5.2 (Substantive differences), parrafo sobre uncertainty | Cuantificar la incertidumbre de prediccion |
| Tabla trimestral con IC | Appendix I (Hybrid Variable), nueva tabla | Detalle numerico |
| Nota metodologica | Seccion 4.4 o nota al pie | Documentar metodo parametrico |

', N_DRAWS, N_DRAWS, umbral_cal, N_DRAWS), con)

# ── Seccion 2: Serie con bandas ──
writeLines(sprintf('
# Serie hibrida con bandas de incertidumbre

```{r serie-bandas, fig.height=6, fig.width=10}
df <- read.csv("%s")

# Ordenar por periodo
df <- df %%>%%
  mutate(periodo_id = factor(periodo_id, levels = periodo_id))

ggplot(df, aes(x = periodo_id)) +
  geom_ribbon(aes(ymin = ic_025, ymax = ic_975, group = 1),
              fill = COL_BANDA, alpha = 0.35) +
  geom_ribbon(aes(ymin = ic_050, ymax = ic_950, group = 1),
              fill = COL_BANDA, alpha = 0.50) +
  geom_line(aes(y = tasa_punto, group = 1), color = COL_GLM, linewidth = 1) +
  geom_point(aes(y = tasa_punto), color = COL_GLM, size = 1.5) +
  labs(title = "Tasa de formalidad hibrida (GLM) con bandas de incertidumbre",
       subtitle = "Banda clara: IC 95%% | Banda oscura: IC 90%% | Linea: estimacion puntual",
       x = NULL, y = "Tasa de formalidad (%%%%)") +
  scale_y_continuous(labels = function(x) paste0(x, "%%%%")) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
```
', gsub("\\\\", "/", path_csv)), con)

# ── Seccion 3: Tabla ──
writeLines(sprintf('
# Tabla de resultados trimestrales

```{r tabla-bandas}
df <- read.csv("%s")
df %%>%%
  select(periodo_id, tasa_punto, ic_025, ic_975, tasa_sd) %%>%%
  mutate(ancho_ic = ic_975 - ic_025) %%>%%
  kable(col.names = c("Periodo", "Tasa puntual (%%%%)", "IC 2.5%%%%", "IC 97.5%%%%",
                       "SD", "Ancho IC (pp)"),
        digits = 2, align = "lccccc",
        caption = "Tasa de formalidad hibrida GLM con IC 95%%%% parametrico") %%>%%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 11) %%>%%
  row_spec(0, bold = TRUE)
```
', gsub("\\\\", "/", path_csv)), con)

# ── Seccion 4: Diagnostico ──
writeLines(sprintf('
# Diagnostico de las bandas

```{r diagnostico}
df <- read.csv("%s")
ancho_medio <- mean(df$ic_975 - df$ic_025)
ancho_max   <- max(df$ic_975 - df$ic_025)
ancho_min   <- min(df$ic_975 - df$ic_025)

cat(sprintf("Ancho medio IC 95%%%%:  %%.2f pp\\n", ancho_medio))
cat(sprintf("Ancho minimo IC 95%%%%: %%.2f pp\\n", ancho_min))
cat(sprintf("Ancho maximo IC 95%%%%: %%.2f pp\\n", ancho_max))
cat(sprintf("SD media:             %%.2f pp\\n", mean(df$tasa_sd)))
```

## Interpretacion

- Si el ancho medio del IC es **< 2 pp**, la incertidumbre de prediccion es modesta
  y la serie puntual es altamente informativa
- Si el ancho es **> 5 pp**, la incertidumbre es sustantiva y debe reportarse
  prominentemente
- Bandas mas anchas en trimestres con mayor proporcion de bloque predicho
  son esperables y coherentes con el diseno hibrido

## Limitaciones

- Los draws usan lambda fijo (lambda.1se del modelo completo); no capturan
  la incertidumbre de *seleccion del penalizador*
- No incorporan incertidumbre del diseno muestral (PSU/estratos)
- El umbral de clasificacion es fijo (Youden calibrado); no se perturba entre draws

', gsub("\\\\", "/", path_csv)), con)

close(con)
cat("   [OK] Rmd escrito:", rmd_temp, "\n")

# Render
rmarkdown::render(
  input       = rmd_temp,
  output_file = path_html,
  quiet       = TRUE,
  envir       = new.env(parent = globalenv())
)
unlink(rmd_temp)
cat("   [OK] HTML generado:", path_html, "\n")

# 📦 Contrato ----------------------------------------------------------------

ancho_95 <- df_bandas$ic_975 - df_bandas$ic_025
max_quarter_idx <- which.max(ancho_95)

contrato_10c <- list(
  script             = "10c_bandas_incertidumbre_GLM.R",
  fecha              = Sys.time(),
  n_quarters         = nrow(df_bandas),
  n_draws            = ncol(draws_tasas),
  avg_ci95_width_pp  = round(mean(ancho_95), 1),
  max_ci95_width_pp  = round(max(ancho_95), 1),
  max_ci95_quarter   = df_bandas$periodo_id[max_quarter_idx],
  min_ci95_width_pp  = round(min(ancho_95), 1),
  avg_ci90_width_pp  = round(mean(df_bandas$ic_950 - df_bandas$ic_050), 1),
  max_ci90_width_pp  = round(max(df_bandas$ic_950 - df_bandas$ic_050), 1)
)

path_contrato_10c <- file.path(DIR_CONTRATOS, "10c_contrato_ci_bootstrap.rds")
saveRDS(contrato_10c, path_contrato_10c)
cat("\n📦 Contrato:", path_contrato_10c, "\n")

# 📑 9. Checklist y timer ----------------------------------------------------

cat("\nChecklist de outputs:\n")
cat("  CSV:     ", ifelse(file.exists(path_csv),  "OK", "FALTA"), "\n")
cat("  HTML:    ", ifelse(file.exists(path_html), "OK", "FALTA"), "\n")
cat("  Contrato:", ifelse(file.exists(path_contrato_10c), "OK", "FALTA"), "\n")

rm(panel, panel_elegible, panel_pred, X_pred, draws_tasas, c07a, cv_fit, c08, contrato_10c)
gc(verbose = FALSE)

cat("\nTiempo:", round((proc.time() - t_inicio)["elapsed"] / 60, 1), "minutos\n")
