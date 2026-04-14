# =============================================================================
# [EN] 10i_backcast_factores_restringidos.R -- Backcast with restricted factors (A1+ thetas) vs baseline: series stability check
# INPUTS:  rdos/datos/00_robustness_theta_restringido.rds, models from 07-08 GLM
# OUTPUTS: rdos/reportes/10i_backcast_factores_restringidos.html
# =============================================================================
# 🌟 10i_backcast_factores_restringidos.R 🌟 ####
# Backcast con factores del modelo restringido (A1+) vs factores originales
# Proyecto: formalidad_rev  |  Capa 7 -- Robustez  |  B2-Q3 pendiente
#
# OBJETIVO:
#   Re-predecir la serie de formalidad usando los thetas restringidos (estimados
#   sin 2024Q4-2025Q3 en el A1+) y comparar con la serie baseline. Si la serie
#   no cambia materialmente, confirma que el backcast no depende de la
#   disponibilidad de formalidad observada en el periodo de overlap.
#
# INPUTS:
#   rdos/datos/00_robustness_theta_restringido.rds  (thetas del A1+)
#   rdos/datos/08_panel_formalidad_{SUFIJO_MODELO_SLS}.rds        (panel consolidado)
#   rdos/modelos/07_modelo_lasso_{SUFIJO_MODELO_GLM}.rds          (cv.glmnet)
#   rdos/modelos/07_recipe_lasso_{SUFIJO_MODELO_GLM}.rds          (recipe)
#   rdos/contratos/08_contrato_backcasting_{SUFIJO_MODELO_GLM}.rds
#
# OUTPUTS:
#   rdos/reportes/10i_backcast_factores_restringidos.html
#   rdos/figuras/10i_backcast_factores_restringidos/*.pdf
#   rdos/reportes/10i_backcast_factores_restringidos_notas.txt
#
# TIEMPO ESTIMADO: ~5 minutos (bake + predict sobre ~800k obs)

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(recipes)
  library(Matrix)
  library(glmnet)
  library(knitr)
  library(kableExtra)
  library(ggplot2)
  library(patchwork)
  library(rmarkdown)
  library(scales)
})

# 🔧 Cargar configuración y funciones ------------------------------------------

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------

t_inicio <- proc.time()
cat("===================================================================\n")
cat("SCRIPT 10i - BACKCAST CON FACTORES RESTRINGIDOS (B2-Q3)\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")

# 🔑 1. PATHS ----------------------------------------------------------------
cat("-- 1. Paths ------------------------------------------------------------\n")

PATH_THETA_RESTR <- file.path(DIR_DATOS, "00_robustness_theta_restringido.rds")
PATH_HTML_OUT    <- file.path(DIR_REPORTES, "10i_backcast_factores_restringidos.html")
PATH_NOTAS_OUT   <- file.path(DIR_REPORTES, "10i_backcast_factores_restringidos_notas.txt")
DIR_FIG_RESTR    <- file.path(DIR_FIGURAS, "10i_backcast_factores_restringidos")
dir.create(DIR_FIG_RESTR, showWarnings = FALSE, recursive = TRUE)

for (p in c(PATH_THETA_RESTR, PATH_08_PANEL_CONSOLIDADO,
            PATH_07_MODELO_GLM, PATH_07_RECIPE_GLM)) {
  hard_stop(file.exists(p), paste("No existe:", basename(p)))
}
cat("   [OK] Inputs verificados\n")

# 🪫 2. CARGAR MODELO Y RECIPE -----------------------------------------------
cat("-- 2. Cargar modelo y recipe -------------------------------------------\n")

cv_fit <- readRDS(PATH_07_MODELO_GLM)
recipe_lasso <- readRDS(PATH_07_RECIPE_GLM)
c08_glm <- readRDS(PATH_08_CONTRATO_GLM)
umbral <- c08_glm$umbral_calibracion

cat(sprintf("   [OK] cv.glmnet: lambda.1se = %.6f\n", cv_fit$lambda.1se))
cat(sprintf("   [OK] Umbral calibración: %.4f\n", umbral))

# 🪫 3. CARGAR PANEL Y THETAS RESTRINGIDOS -----------------------------------
cat("-- 3. Cargar panel y thetas restringidos --------------------------------\n")

panel <- readRDS(PATH_08_PANEL_CONSOLIDADO)
theta_restr <- readRDS(PATH_THETA_RESTR)

cat(sprintf("   Panel: %s obs\n", format(nrow(panel), big.mark = ".")))
cat(sprintf("   Thetas restringidos: %s obs\n", format(nrow(theta_restr), big.mark = ".")))

# Filtrar solo ocupados que tienen match con thetas restringidos (inner_join)
# Esto evita NAs espurios por falta de match
panel_ocu <- panel %>%
  filter(condicion_actividad == "Ocupado") %>%
  inner_join(
    theta_restr %>% rename(theta_A_restr = theta_A, theta_B_restr = theta_B),
    by = c("id_individuo_hist", "periodo_id")
  ) %>%
  select(-starts_with("prob_formal_"), -starts_with("formalidad_clase_"),
         -starts_with("flag_pred_"))

cat(sprintf("   Ocupados con match: %s obs (de %s totales)\n",
            format(nrow(panel_ocu), big.mark = "."),
            format(sum(panel$condicion_actividad == "Ocupado"), big.mark = ".")))

rm(panel, theta_restr); gc(verbose = FALSE)

# 🪫 4. FUNCIÓN AUXILIAR: bake + predict -------------------------------------

predict_serie <- function(panel_df, recipe, model, umbral,
                          theta_A_col = "theta_A", theta_B_col = "theta_B",
                          zero_theta_A = FALSE) {
  # Preparar features
  df <- panel_df %>%
    rename(theta_A_mA = !!theta_A_col, theta_B_mA = !!theta_B_col) %>%
    mutate(formalidad_bin = NA_real_)

  LABELS_NO_APLICA <- c("No aplica (No PEA)", "Sin Experiencia Previa",
                         "Sin Calificacion", "No aplica", "No corresponde")
  df <- df %>%
    mutate(across(where(is.character), function(x) {
      if_else(x %in% LABELS_NO_APLICA, NA_character_, x)
    }))

  vars_recipe <- recipe$var_info$variable
  features <- df %>% select(any_of(vars_recipe))

  # Bake
  baked <- bake(recipe, new_data = features)
  if ("formalidad_bin" %in% names(baked)) baked <- baked %>% select(-formalidad_bin)

  # Opción B: zerout theta_A (set normalized column to 0 = training mean)
  if (zero_theta_A) {
    theta_col_idx <- which(names(baked) == "theta_A_mA")
    if (length(theta_col_idx) == 1) {
      baked[[theta_col_idx]] <- 0
      cat("   [theta_A_mA zeroed out in baked matrix]\n")
    }
  }

  # Sparse + predict
  X <- Matrix(as.matrix(baked), sparse = TRUE)
  pred_raw <- as.vector(predict(model, newx = X, s = "lambda.1se", type = "response"))
  pred_clase <- if_else(pred_raw >= umbral, 1L, 0L)

  rm(baked, X, features, df); gc(verbose = FALSE)
  list(prob = pred_raw, clase = pred_clase)
}

# 🪫 5. SERIE BASELINE (con thetas originales, misma submuestra) -------------
cat("-- 5. Predicción baseline (thetas originales) --------------------------\n")

pred_base <- predict_serie(panel_ocu, recipe_lasso, cv_fit, umbral,
                           theta_A_col = "theta_A", theta_B_col = "theta_B")
cat(sprintf("   Media prob: %.4f | Tasa formal: %.2f%%\n",
            mean(pred_base$prob), mean(pred_base$clase) * 100))

# 🪫 6. SERIE CON FACTORES RESTRINGIDOS (Opción A) ---------------------------
cat("-- 6. Predicción con factores restringidos (Opción A) ------------------\n")

pred_restr <- predict_serie(panel_ocu, recipe_lasso, cv_fit, umbral,
                            theta_A_col = "theta_A_restr", theta_B_col = "theta_B_restr")
cat(sprintf("   Media prob: %.4f | Tasa formal: %.2f%%\n",
            mean(pred_restr$prob), mean(pred_restr$clase) * 100))

# 🪫 7. SERIE SIN θ_A (Opción B) ---------------------------------------------
cat("-- 7. Predicción sin theta_A (Opción B) --------------------------------\n")

pred_no_thetaA <- predict_serie(panel_ocu, recipe_lasso, cv_fit, umbral,
                                theta_A_col = "theta_A", theta_B_col = "theta_B",
                                zero_theta_A = TRUE)
cat(sprintf("   Media prob: %.4f | Tasa formal: %.2f%%\n",
            mean(pred_no_thetaA$prob), mean(pred_no_thetaA$clase) * 100))

# 🪫 8. CONSTRUIR SERIES TRIMESTRALES Y COMPARAR -----------------------------
cat("-- 8. Series trimestrales y comparación --------------------------------\n")

# Agregar predicciones al panel
panel_ocu$prob_base     <- pred_base$prob
panel_ocu$clase_base    <- pred_base$clase
panel_ocu$prob_restr    <- pred_restr$prob
panel_ocu$clase_restr   <- pred_restr$clase
panel_ocu$prob_no_tA    <- pred_no_thetaA$prob
panel_ocu$clase_no_tA   <- pred_no_thetaA$clase
rm(pred_base, pred_restr, pred_no_thetaA); gc(verbose = FALSE)

# Construir series híbridas (observada donde existe, predicha donde no)
build_serie <- function(df, clase_col) {
  df %>%
    mutate(
      formal_hib = case_when(
        formalidad_empleo == "Formal oficial" ~ 1L,
        formalidad_empleo == "Informal oficial" ~ 0L,
        TRUE ~ .data[[clase_col]]
      )
    ) %>%
    filter(!is.na(formal_hib)) %>%
    group_by(periodo) %>%
    summarise(
      n = n(),
      tasa = weighted.mean(formal_hib, pondera, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      anio = as.integer(sub("T[0-9]+_", "", periodo)),
      trim = as.integer(sub("T([0-9]+)_.*", "\\1", periodo))
    ) %>%
    arrange(anio, trim)
}

s_base  <- build_serie(panel_ocu, "clase_base")
s_restr <- build_serie(panel_ocu, "clase_restr")
s_no_tA <- build_serie(panel_ocu, "clase_no_tA")

# Merge
comp <- s_base %>%
  select(periodo, anio, trim, n, tasa_base = tasa) %>%
  inner_join(s_restr %>% select(periodo, tasa_restr = tasa), by = "periodo") %>%
  inner_join(s_no_tA %>% select(periodo, tasa_no_tA = tasa), by = "periodo") %>%
  mutate(
    delta_restr_pp = (tasa_restr - tasa_base) * 100,
    delta_no_tA_pp = (tasa_no_tA - tasa_base) * 100,
    fecha_q = as.Date(paste0(anio, "-", (trim - 1) * 3 + 2, "-15"))
  )

# Métricas
cor_restr <- cor(comp$tasa_base, comp$tasa_restr)
cor_no_tA <- cor(comp$tasa_base, comp$tasa_no_tA)
rmse_restr <- sqrt(mean(comp$delta_restr_pp^2))
rmse_no_tA <- sqrt(mean(comp$delta_no_tA_pp^2))
max_delta_restr <- max(abs(comp$delta_restr_pp))
max_delta_no_tA <- max(abs(comp$delta_no_tA_pp))

cat(sprintf("\n   === Opción A (factores restringidos) ===\n"))
cat(sprintf("   Correlación: %.4f | RMSE: %.3f pp | Delta máx: %.3f pp\n",
            cor_restr, rmse_restr, max_delta_restr))
cat(sprintf("\n   === Opción B (sin theta_A) ===\n"))
cat(sprintf("   Correlación: %.4f | RMSE: %.3f pp | Delta máx: %.3f pp\n",
            cor_no_tA, rmse_no_tA, max_delta_no_tA))

# 🪫 9. GRÁFICOS -------------------------------------------------------------
cat("-- 9. Gráficos ---------------------------------------------------------\n")

# Gráfico 1: 3 series superpuestas
comp_long <- comp %>%
  select(fecha_q, periodo, tasa_base, tasa_restr, tasa_no_tA) %>%
  pivot_longer(cols = starts_with("tasa_"),
               names_to = "serie", values_to = "tasa") %>%
  mutate(serie = case_when(
    serie == "tasa_base" ~ tr("Baseline (thetas originales)"),
    serie == "tasa_restr" ~ tr("Factores restringidos (A1+)"),
    serie == "tasa_no_tA" ~ tr("Sin theta_A (solo theta_B)")
  ),
  serie = factor(serie, levels = tr(c("Baseline (thetas originales)",
                                    "Factores restringidos (A1+)",
                                    "Sin theta_A (solo theta_B)"))))

p_series <- ggplot(comp_long,
                   aes(x = fecha_q, y = tasa * 100, color = serie, linetype = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  scale_color_manual(values = setNames(c(COL_GLM, PAL_DESCRIPTIVO[2], PAL_DESCRIPTIVO[3]),
                     tr(c("Baseline (thetas originales)", "Factores restringidos (A1+)",
                          "Sin theta_A (solo theta_B)")))) +
  scale_linetype_manual(values = setNames(c("solid", "dashed", "dotted"),
                        tr(c("Baseline (thetas originales)", "Factores restringidos (A1+)",
                             "Sin theta_A (solo theta_B)")))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tr_labs(title = "Serie de formalidad: baseline vs factores restringidos vs sin theta_A",
       subtitle = sprintf("r(restr)=%.4f, RMSE=%.2fpp | r(sin tA)=%.4f, RMSE=%.2fpp",
                           cor_restr, rmse_restr, cor_no_tA, rmse_no_tA),
       x = NULL, y = "Tasa de formalidad (%)") +
  theme_paper() +
  theme(legend.position = "top")

# Gráfico 2: Deltas de ambas opciones
delta_long <- comp %>%
  select(fecha_q, delta_restr_pp, delta_no_tA_pp) %>%
  pivot_longer(-fecha_q, names_to = "tipo", values_to = "delta") %>%
  mutate(tipo = case_when(
    tipo == "delta_restr_pp" ~ tr("Factores restringidos (A)"),
    tipo == "delta_no_tA_pp" ~ tr("Sin theta_A (B)")
  ))

p_delta <- ggplot(delta_long, aes(x = fecha_q, y = delta, fill = tipo)) +
  geom_col(position = position_dodge(width = 25), width = 20, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = setNames(c(PAL_DESCRIPTIVO[2], PAL_DESCRIPTIVO[3]),
                     tr(c("Factores restringidos (A)", "Sin theta_A (B)")))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tr_labs(title = "Diferencia vs baseline por escenario",
       x = NULL, y = "Delta (pp)") +
  theme_paper() +
  theme(legend.position = "top")

guardar_figura(p_series, DIR_FIG_RESTR, "series",  1)
guardar_figura(p_delta,  DIR_FIG_RESTR, "barras",  1)
cat("   [OK] 2 figuras exportadas\n")

# 🪫 10. NOTAS PAPER ---------------------------------------------------------
cat("-- 10. Notas -----------------------------------------------------------\n")

notas_con <- file(PATH_NOTAS_OUT, open = "wt", encoding = "UTF-8")
cat(sprintf("BACKCAST CON FACTORES RESTRINGIDOS — %s\n",
            format(Sys.time(), "%Y-%m-%d")), file = notas_con)
cat("========================================\n\n", file = notas_con)
cat("OPCIÓN A (factores restringidos):\n", file = notas_con)
cat(sprintf("  Correlación: %.4f | RMSE: %.3f pp | Delta máx: %.3f pp\n",
            cor_restr, rmse_restr, max_delta_restr), file = notas_con)
cat("\nOPCIÓN B (sin theta_A):\n", file = notas_con)
cat(sprintf("  Correlación: %.4f | RMSE: %.3f pp | Delta máx: %.3f pp\n",
            cor_no_tA, rmse_no_tA, max_delta_no_tA), file = notas_con)
cat(sprintf("\nTrimestres: %d\n\n", nrow(comp)), file = notas_con)
cat("INTERPRETACIÓN:\n", file = notas_con)
cat("El delta en la Opción A proviene de la inestabilidad numérica de\n", file = notas_con)
cat("theta_A (artefacto de cuadratura GH, documentado en Appendix D).\n", file = notas_con)
cat("La Opción B muestra que remover theta_A produce [ver resultado].\n", file = notas_con)
cat("theta_B (r=0.998) es estable y [ver impacto].\n", file = notas_con)
close(notas_con)
cat(sprintf("   [OK] Notas: %s\n", basename(PATH_NOTAS_OUT)))

# 🪫 11. REPORTE HTML --------------------------------------------------------
cat("-- 11. Reporte HTML ----------------------------------------------------\n")

rds_path <- gsub("\\\\", "/", tempfile(fileext = ".rds"))
save(comp, comp_long, delta_long,
     cor_restr, cor_no_tA, rmse_restr, rmse_no_tA,
     max_delta_restr, max_delta_no_tA,
     p_series, p_delta,
     file = rds_path)

rmd_temp <- tempfile(fileext = ".Rmd")
con <- file(rmd_temp, open = "wt", encoding = "UTF-8")

cat('---
title: "Backcast con Factores Restringidos (A1+)"
subtitle: "Proyecto EPH Argentina -- Formalidad Laboral | Capa 7 | B2-Q3"
date: "Generado: `r format(Sys.time(), \'%d/%m/%Y %H:%M\')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float:
      collapsed: false
    toc_depth: 3
    number_sections: true
    code_folding: hide
    df_print: kable
---

', file = con)

cat(sprintf('```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,
                      fig.width=11, fig.height=5.5, dpi=150)
suppressPackageStartupMessages({
  library(tidyverse); library(knitr); library(kableExtra)
  library(ggplot2); library(scales)
})
load("%s")
source(here::here("script", "config", "funciones_comunes.R"))
```

', rds_path), file = con)

cat('# Resumen {.unnumbered}

```{r resumen}
kpi <- tibble(
  Escenario = c("A: Factores restringidos", "A: Factores restringidos", "A: Factores restringidos",
                "B: Sin theta_A", "B: Sin theta_A", "B: Sin theta_A"),
  Metrica = rep(c("Correlacion", "RMSE (pp)", "Delta max (pp)"), 2),
  Valor = c(sprintf("%.4f", cor_restr), sprintf("%.3f", rmse_restr), sprintf("%.3f", max_delta_restr),
            sprintf("%.4f", cor_no_tA), sprintf("%.3f", rmse_no_tA), sprintf("%.3f", max_delta_no_tA))
)
kable(kpi, align = c("l","l","r"), caption = "KPIs de estabilidad por escenario") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  pack_rows("Opcion A: factores restringidos", 1, 3, background = "#fef9e7") %>%
  pack_rows("Opcion B: sin theta_A", 4, 6, background = "#eaf4fc")
```

# Series superpuestas

Tres series: (1) baseline con thetas originales, (2) con thetas del modelo
restringido (sin 2024Q4-2025Q3), (3) con theta_A eliminado (set a media del training).

```{r fig-series, fig.height=5}
p_series
```

# Deltas por escenario

```{r fig-delta, fig.height=4.5}
p_delta
```

# Tabla trimestral

```{r tabla}
comp %>%
  select(periodo, tasa_base, tasa_restr, delta_restr_pp, tasa_no_tA, delta_no_tA_pp) %>%
  mutate(across(starts_with("tasa_"), ~ sprintf("%.2f%%", .x * 100)),
         across(starts_with("delta_"), ~ sprintf("%.2f", .x))) %>%
  kable(col.names = c("Periodo", "Baseline", "Restringida", "Delta A (pp)",
                       "Sin tA", "Delta B (pp)"),
        align = "lrrrrr") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  add_header_above(c(" " = 1, "Opcion A" = 2, " " = 1, "Opcion B" = 1, " " = 1))
```

# Interpretacion

**Opcion A (factores restringidos):** La diferencia proviene de la inestabilidad
numerica de theta_A (factor cognitivo) entre la estimacion completa y la restringida.
Esta inestabilidad es un artefacto de la cuadratura Gauss-Hermite documentado en
Appendix D, no sensibilidad sustantiva del modelo.

**Opcion B (sin theta_A):** Muestra el impacto marginal de theta_A en la prediccion.
Si la serie sin theta_A es similar a la baseline, confirma que theta_A no es
load-bearing para el backcast y su inestabilidad numerica es irrelevante.

---

<small>Script: 10i_backcast_factores_restringidos.R | B2-Q3</small>
', file = con)

close(con)

rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_HTML_OUT,
  quiet       = TRUE,
  envir       = new.env(parent = globalenv())
)
unlink(c(rmd_temp, rds_path))
cat(sprintf("   [OK] Reporte: %s\n", basename(PATH_HTML_OUT)))

# 📑 12. CHECKLIST -----------------------------------------------------------
cat("\n-- 12. Checklist -------------------------------------------------------\n")
for (f in c(PATH_HTML_OUT, PATH_NOTAS_OUT)) {
  cat(sprintf("   %s %s\n", ifelse(file.exists(f), "OK", "!!"), basename(f)))
}
figs <- list.files(DIR_FIG_RESTR, pattern = "\\.pdf$")
cat(sprintf("   OK %d figuras PDF\n", length(figs)))

# 📑 13. CONTRATO -----------------------------------------------------------
cat("\n-- 13. Contrato --------------------------------------------------------\n")
contrato_10i <- list(
  script            = "10i_backcast_factores_restringidos.R",
  fecha             = Sys.time(),
  n_matched_occupied = nrow(panel_ocu),
  n_trimestres      = nrow(comp),
  # Scenario A: restricted factors
  cor_restr         = round(cor_restr, 4),
  rmse_restr        = round(rmse_restr, 2),
  max_delta_restr   = round(max_delta_restr, 2),
  # Scenario B: without theta_A
  cor_no_tA         = round(cor_no_tA, 4),
  rmse_no_tA        = round(rmse_no_tA, 2),
  max_delta_no_tA   = round(max_delta_no_tA, 2)
)
saveRDS(contrato_10i, file.path(DIR_CONTRATOS, "10i_contrato_backcast_restr.rds"))
cat("   [OK] 10i_contrato_backcast_restr.rds\n")

rm(panel_ocu, comp, comp_long, delta_long); gc(verbose = FALSE)
cat(sprintf("\nTiempo: %.1f minutos\n", (proc.time() - t_inicio)["elapsed"] / 60))
