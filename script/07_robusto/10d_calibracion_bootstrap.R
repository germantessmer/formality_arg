# =============================================================================
# [EN] 10d_calibracion_bootstrap.R -- Brier score, calibration slope/intercept, reliability plots, PP07H missingness analysis
# INPUTS:  rdos/datos/06_theta_predichos.rds, 07b contracts x3, bootstrap CI CSV
# OUTPUTS: rdos/figuras/00_reliability_plot_3modelos.pdf, rdos/figuras/00_bootstrap_ci_glm.pdf
# =============================================================================
# 🌟 10d_calibracion_bootstrap.R 🌟 ####
# OBJETIVO : Fase 2 Review 2 — B2-T4 + B2-Q6 + B2-Q1
#   B2-T4: Brier score + calibration slope/intercept + reliability plot (3 modelos)
#   B2-Q6: Cuantificar missingness PP07H en asalariados + impacto en tasa
#   B2-Q1: Figura de bootstrap CI para incorporar al paper
# INPUTS   : PATH_06_THETA, PATH_06_MODELO_HETERO
#            07b_contrato_postlasso_*.rds
#            rdos/reportes/00_bandas_incertidumbre_GLM.csv
# OUTPUTS  : rdos/reportes/00_fase2_brier_calibracion.csv
#            rdos/reportes/00_fase2_pp07h_missingness.csv
#            rdos/figuras/00_bootstrap_ci_glm.pdf
#            rdos/figuras/00_reliability_plot_3modelos.pdf
# TIEMPO   : ~5-10 min

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(recipes)
})

# 🔧 Cargar configuración y funciones ------------------------------------------

source(here::here("script/config/parametros.R"))
source(here::here("script/config/funciones_comunes.R"))

# Directorio de figuras para este script
DIR_FIGURAS_10D <- file.path(DIR_FIGURAS, "10d_calibracion_bootstrap")
dir.create(DIR_FIGURAS_10D, recursive = TRUE, showWarnings = FALSE)

# ⌛ Inicio contador de tiempo -------------------------------------------------

t_inicio <- proc.time()
cat("══════════════════════════════════════════════════════\n")
cat("  Fase 2 R2: Brier + Calibración + PP07H + Bootstrap CI\n")
cat("══════════════════════════════════════════════════════\n\n")

# ── TEMA GRÁFICO ── (usa theme_paper() de theme_paper.R via funciones_comunes.R)

# 🪫 1. BLOQUE 1: B2-T4 -- Brier score + calibración slope/intercept ---------
cat("── BLOQUE 1: Brier + Calibration slope/intercept ──────────────────\n")

# 1a. Cargar contratos c07b
cat("  Cargando contratos c07b...\n")
c07b_glm <- readRDS(file.path(DIR_CONTRATOS, paste0("07b_contrato_postlasso_", SUFIJO_MODELO_GLM, ".rds")))
c07b_lpm <- readRDS(file.path(DIR_CONTRATOS, paste0("07b_contrato_postlasso_", SUFIJO_MODELO_LPM, ".rds")))
c07b_sls <- readRDS(file.path(DIR_CONTRATOS, paste0("07b_contrato_postlasso_", SUFIJO_MODELO_SLS, ".rds")))

# 1b. Reconstruir y_test (mismo split que 07b — SEED_GLOBAL garantiza reproducibilidad)
cat("  Cargando panel con θ para reconstruir y_test...\n")
panel_raw     <- readRDS(PATH_06_THETA)
modelo_hetero <- readRDS(PATH_06_MODELO_HETERO)

theta_data_mA <- modelo_hetero$modelo_A$theta_data %>%
  rename(theta_A_mA = theta_A, theta_B_mA = theta_B) %>%
  select(id_individuo_hist, periodo_id, theta_A_mA, theta_B_mA)

panel <- panel_raw %>%
  left_join(theta_data_mA, by = c("id_individuo_hist", "periodo_id"),
            relationship = "many-to-one")

# Filtro idéntico al de 07b
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
df_test <- train_raw[-idx_train, ]
y_test  <- df_test$formalidad_bin
w_test  <- df_test$pondera

stopifnot(
  "y_test inconsistente con c07b" = length(y_test) == c07b_glm$n_test
)
cat(sprintf("  [✅] y_test reconstruido: %d obs | Formal rate: %.1f%%\n",
            length(y_test), mean(y_test) * 100))

# Liberar memoria grande
rm(panel_raw, modelo_hetero, theta_data_mA, panel, train_raw, idx_train)
gc(verbose = FALSE)

# 1c. Función: métricas de calibración
compute_calibration <- function(y, p, w = NULL, modelo = "modelo") {
  # Brier score (weighted si hay pesos)
  if (is.null(w)) w <- rep(1, length(y))
  w_norm <- w / mean(w)
  brier  <- weighted.mean((y - p)^2, w_norm)

  # Brier Score nulo (predicción = media ponderada)
  p_null  <- weighted.mean(y, w_norm)
  brier0  <- weighted.mean((y - p_null)^2, w_norm)
  brier_skill <- 1 - brier / brier0  # Brier Skill Score (>0 = mejor que nulo)

  # Calibration slope e intercept (regresión de y sobre logit(p))
  # Intercepto solo (slope fijo = 1): calibration-in-the-large
  p_clip  <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  logit_p <- log(p_clip / (1 - p_clip))

  cal_mod <- suppressWarnings(
    glm(y ~ logit_p, family = binomial, weights = w_norm)
  )
  cal_intercept <- coef(cal_mod)[1]
  cal_slope     <- coef(cal_mod)[2]

  cat(sprintf("  %s → Brier=%.4f | BSS=%.4f | Cal intercept=%.4f | Cal slope=%.4f\n",
              modelo, brier, brier_skill, cal_intercept, cal_slope))

  list(
    modelo          = modelo,
    n_test          = length(y),
    brier           = round(brier, 4),
    brier_null      = round(brier0, 4),
    brier_skill     = round(brier_skill, 4),
    cal_intercept   = round(cal_intercept, 4),
    cal_slope       = round(cal_slope, 4)
  )
}

# 1d. Calcular para los 3 modelos
cat("  Calculando métricas de calibración:\n")
res_glm <- compute_calibration(y_test, c07b_glm$pred_test$raw, w_test, "GLM")
res_lpm <- compute_calibration(y_test, c07b_lpm$pred_test$raw, w_test, "LPM")
# SLS: clipped predictions
res_sls <- compute_calibration(y_test, c07b_sls$pred_test$clip, w_test, "SLS")

# 1e. Tabla resumen
tabla_cal <- bind_rows(
  as.data.frame(res_glm),
  as.data.frame(res_lpm),
  as.data.frame(res_sls)
)
cat("\n  Tabla calibración:\n")
print(tabla_cal)

out_csv_brier <- file.path(DIR_REPORTES, "00_fase2_brier_calibracion.csv")
write.csv(tabla_cal, out_csv_brier, row.names = FALSE)
cat(sprintf("  [✅] Guardado: %s\n", out_csv_brier))

# 1f. Reliability plot (con hl_df de cada modelo)
# hl_df tiene: grupo, n, obs_formal, pred_media
cat("\n  Construyendo reliability plot...\n")

make_rl_df <- function(hl_df, modelo) {
  as.data.frame(hl_df) %>%
    mutate(
      obs_rate  = obs_formal / n,
      pred_rate = pred_media,
      modelo    = modelo
    )
}

rl_glm <- make_rl_df(c07b_glm$hl_df, "GLM")
rl_lpm <- make_rl_df(c07b_lpm$hl_df, "LPM")
rl_sls <- make_rl_df(c07b_sls$hl_df, "SLS")
rl_all <- bind_rows(rl_glm, rl_lpm, rl_sls) %>%
  mutate(modelo = factor(modelo, levels = c("LPM","GLM","SLS")))

p_reliability <- ggplot(rl_all, aes(x = pred_rate, y = obs_rate, color = modelo, shape = modelo)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 3, alpha = 0.85) +
  geom_line(alpha = 0.5) +
  scale_color_modelos() +
  scale_shape_modelos() +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(0, 1)) +
  tr_labs(
    title   = NULL,
    subtitle = NULL,
    x       = "Mean predicted probability",
    y       = "Observed formality rate",
    color   = "Model", shape = "Model"
  ) +
  theme_paper() +
  theme(legend.position = "bottom")

guardar_figura(p_reliability, DIR_FIGURAS_10D, "cal", 1, width = 6, height = 5)

# 🪫 2. BLOQUE 2: B2-Q6 -- PP07H missingness en asalariados ------------------
cat("\n── BLOQUE 2: PP07H missingness ─────────────────────────────────────\n")

# PP07H está en los datos del overlap. Filtramos el df_test + df_train (= train_raw completo)
# Necesitamos el panel completo de los 4 trimestres de overlap para asalariados
# Usamos el objeto df_test que ya tenemos (es una fracción), pero necesitamos todos
# Recargamos solo lo necesario — panel_raw ya fue rm(), recargo del disco

panel_overlap <- readRDS(PATH_06_THETA) %>%
  filter(
    periodo_id %in% TRIMESTRES_FORMALIDAD,
    condicion_actividad == "Ocupado"
  )

cat(sprintf("  Ocupados en overlap: %s obs\n",
            format(nrow(panel_overlap), big.mark = ",")))

# Identificar asalariados (categoria_ocupacional o similar)
# Buscar columna relevante
cat("  Columnas disponibles (muestra):", paste(names(panel_overlap)[1:min(20,ncol(panel_overlap))], collapse=", "), "\n")

# Buscar columna de categoria ocupacional
col_categ <- names(panel_overlap)[grepl("categ|cat_ocup|tipo_empleo|asalar|independ", names(panel_overlap), ignore.case=TRUE)]
cat("  Columnas de categoría:", paste(col_categ, collapse=", "), "\n")

# PP07H
if ("PP07H" %in% names(panel_overlap)) {
  cat("  PP07H encontrado directamente\n")
  pp07h_col <- "PP07H"
} else {
  pp07h_candidates <- names(panel_overlap)[grepl("PP07H|pp07h|pension|jubil|descuento", names(panel_overlap), ignore.case=TRUE)]
  cat("  PP07H candidatos:", paste(pp07h_candidates, collapse=", "), "\n")
  pp07h_col <- if (length(pp07h_candidates) > 0) pp07h_candidates[1] else NULL
}

# Asalariados = empleados/obreros (no independientes)
if (!is.null(col_categ) && length(col_categ) > 0) {
  col_use <- col_categ[1]
  cat("  Usando columna:", col_use, "| Valores únicos:\n")
  print(table(panel_overlap[[col_use]], useNA = "ifany"))

  # Filtrar asalariados
  vals_asalar <- unique(panel_overlap[[col_use]])
  vals_asalar_sel <- vals_asalar[grepl("asalar|obreros?|empleados?|patron|patron",
                                       tolower(as.character(vals_asalar)))]
  cat("  Valores seleccionados como asalariados:", paste(vals_asalar_sel, collapse=", "), "\n")

  if (length(vals_asalar_sel) > 0) {
    asalariados <- panel_overlap %>% filter(.data[[col_use]] %in% vals_asalar_sel)
  } else {
    # fallback: todos los ocupados
    cat("  AVISO: No se detectaron asalariados automáticamente. Usando todos los ocupados.\n")
    asalariados <- panel_overlap
  }
} else {
  cat("  AVISO: Sin columna de categoría. Usando todos los ocupados.\n")
  asalariados <- panel_overlap
}

cat(sprintf("  N asalariados (overlap): %s\n", format(nrow(asalariados), big.mark=",")))

# Contar PP07H missing
if (!is.null(pp07h_col)) {
  n_total_asal  <- nrow(asalariados)
  n_miss_pp07h  <- sum(is.na(asalariados[[pp07h_col]]))
  pct_miss      <- n_miss_pp07h / n_total_asal * 100

  cat(sprintf("  PP07H (%s): N total asalariados = %s | Missing = %d (%.2f%%)\n",
              pp07h_col,
              format(n_total_asal, big.mark=","),
              n_miss_pp07h, pct_miss))

  # Impacto en tasa: worst-case (tratar todos los missing como informales)
  # Calcular tasa actual (con imputación RF)
  if ("formalidad_empleo" %in% names(asalariados)) {
    tasa_actual <- mean(asalariados$formalidad_empleo == "Formal oficial", na.rm = TRUE) * 100
    # Worst-case: los que tienen PP07H missing y fueron clasificados formales → informales
    # Esto requiere saber cuántos de los missing fueron clasificados como formales
    miss_formales <- asalariados %>%
      filter(is.na(.data[[pp07h_col]]),
             formalidad_empleo == "Formal oficial") %>%
      nrow()
    miss_informales <- n_miss_pp07h - miss_formales

    tasa_worstcase <- (sum(asalariados$formalidad_empleo == "Formal oficial", na.rm=TRUE) - miss_formales) /
      n_total_asal * 100

    delta_tasa_asal <- tasa_actual - tasa_worstcase

    # Impacto en tasa TOTAL (no solo asalariados)
    n_total_overlap <- nrow(panel_overlap)
    delta_tasa_total <- delta_tasa_asal * (n_total_asal / n_total_overlap)

    cat(sprintf("  Tasa formalidad asalariados: %.2f%%\n", tasa_actual))
    cat(sprintf("  Worst-case (missing → informal): %.2f%%\n", tasa_worstcase))
    cat(sprintf("  Delta en asalariados: %.3f pp\n", delta_tasa_asal))
    cat(sprintf("  Delta en tasa total (ponderado por N): %.3f pp\n", delta_tasa_total))
    cat(sprintf("  Missing formales: %d | Missing informales: %d\n", miss_formales, miss_informales))

    res_pp07h <- data.frame(
      n_asalariados      = n_total_asal,
      n_pp07h_missing    = n_miss_pp07h,
      pct_missing        = round(pct_miss, 3),
      miss_clasificados_formales = miss_formales,
      tasa_asalariados_actual    = round(tasa_actual, 3),
      tasa_asalariados_worstcase = round(tasa_worstcase, 3),
      delta_asalariados_pp       = round(delta_tasa_asal, 4),
      delta_total_pp             = round(delta_tasa_total, 4)
    )
    out_csv_pp07h <- file.path(DIR_REPORTES, "00_fase2_pp07h_missingness.csv")
    write.csv(res_pp07h, out_csv_pp07h, row.names = FALSE)
    cat(sprintf("  [✅] Guardado: %s\n", out_csv_pp07h))
  }
} else {
  cat("  AVISO: PP07H no encontrado en panel_overlap. Revisar nombre de columna.\n")
}

rm(panel_overlap, asalariados)
gc(verbose = FALSE)

# 🪫 3. BLOQUE 3: B2-Q1 -- Figura bootstrap CI -------------------------------
cat("\n── BLOQUE 3: Bootstrap CI figura ──────────────────────────────────\n")

boot <- read.csv(file.path(DIR_REPORTES, "00_bandas_incertidumbre_GLM.csv"))
cat(sprintf("  Cargado: %d trimestres | IC95 medio: %.2f pp | IC95 máx: %.2f pp (%s)\n",
            nrow(boot),
            mean(boot$ic_975 - boot$ic_025),
            max(boot$ic_975 - boot$ic_025),
            boot$periodo_id[which.max(boot$ic_975 - boot$ic_025)]))

# Ordenar por tiempo
boot <- boot %>%
  mutate(
    anio  = as.integer(sub("_T.*", "", periodo_id)),
    trim  = as.integer(sub(".*_T", "", periodo_id)),
    t_seq = (anio - 2016) * 4 + trim - 3,  # secuencia 1..35
    label = paste0(anio, "Q", trim)
  ) %>%
  arrange(t_seq)

# Etiquetas anuales para el eje X
labels_anuales <- boot %>%
  filter(trim == 1) %>%
  select(t_seq, label = anio)

# Pandemia: 2020Q1–2021Q2
pandemia_ini <- boot %>% filter(periodo_id == "2020_T1") %>% pull(t_seq)
pandemia_fin <- boot %>% filter(periodo_id == "2021_T2") %>% pull(t_seq)

p_boot <- ggplot(boot, aes(x = t_seq)) +
  # Banda pandemia
  annotate("rect",
           xmin = pandemia_ini - 0.5, xmax = pandemia_fin + 0.5,
           ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.5) +
  # IC 95%
  geom_ribbon(aes(ymin = ic_025, ymax = ic_975),
              fill = COL_BANDA, alpha = 0.35) +
  # IC 90%
  geom_ribbon(aes(ymin = ic_050, ymax = ic_950),
              fill = COL_BANDA, alpha = 0.50) +
  # Pandemic label (after ribbons so it's in foreground)
  annotate("label",
           x = (pandemia_ini + pandemia_fin) / 2, y = 64,
           label = "Pandemic\nexclusion", size = 2.8, color = "grey30",
           fill = "white", alpha = 0.85, label.r = unit(0, "pt")) +
  # Serie puntual
  geom_line(aes(y = tasa_punto), color = COL_GLM, linewidth = 0.85) +
  geom_point(aes(y = tasa_punto), color = COL_GLM, size = 1.5) +
  # Media bootstrap
  geom_line(aes(y = tasa_media), color = COL_OBSERVADO, linewidth = 0.5,
            linetype = "dashed", alpha = 0.7) +
  # Ejes
  scale_x_continuous(
    breaks = labels_anuales$t_seq,
    labels = labels_anuales$label
  ) +
  scale_y_continuous(
    limits = c(52, 65),
    breaks = seq(52, 65, by = 2),
    labels = function(x) paste0(x, "%")
  ) +
  tr_labs(
    title    = NULL,
    subtitle = NULL,
    x        = NULL,
    y        = "Formality rate (hybrid series, occupied workers)"
  ) +
  theme_paper() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )

guardar_figura(p_boot, DIR_FIGURAS_10D, "series", 2, width = 8, height = 4.5)

# 📦 CONTRATO ----------------------------------------------------------------
cat("\n── Generando contrato 10d ─────────────────────────────────────────\n")

contrato_10d <- list(
  script              = "10d_calibracion_bootstrap.R",
  fecha               = format(Sys.time(), "%Y-%m-%d %H:%M"),
  brier_glm           = res_glm$brier,
  brier_lpm           = res_lpm$brier,
  brier_sls           = res_sls$brier,
  bss_glm             = res_glm$brier_skill,
  bss_lpm             = res_lpm$brier_skill,
  bss_sls             = res_sls$brier_skill,
  cal_slope_glm       = res_glm$cal_slope,
  cal_slope_lpm       = res_lpm$cal_slope,
  cal_slope_sls       = res_sls$cal_slope,
  cal_intercept_glm   = res_glm$cal_intercept,
  cal_intercept_lpm   = res_lpm$cal_intercept,
  cal_intercept_sls   = res_sls$cal_intercept
)

path_contrato_10d <- file.path(DIR_CONTRATOS, "10d_contrato_calibracion.rds")
saveRDS(contrato_10d, path_contrato_10d)
cat(sprintf("  [✅] Contrato guardado: %s\n", path_contrato_10d))

# 📑 RESUMEN -----------------------------------------------------------------
cat("\n══════════════════════════════════════════════════════\n")
cat("  OUTPUTS GENERADOS:\n")
cat(sprintf("  [B2-T4] %s\n", file.path(DIR_REPORTES, "00_fase2_brier_calibracion.csv")))
cat(sprintf("  [B2-T4] %s\n", DIR_FIGURAS_10D))
cat(sprintf("  [B2-Q6] %s\n", file.path(DIR_REPORTES, "00_fase2_pp07h_missingness.csv")))
cat(sprintf("  [B2-Q1] %s\n", DIR_FIGURAS_10D))
cat("══════════════════════════════════════════════════════\n")

cat("Tiempo total:", round((proc.time() - t_inicio)["elapsed"] / 60, 1), "minutos\n")
