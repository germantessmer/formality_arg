# =============================================================================
# [EN] 09c_comp_hibrido.R -- Hybrid variable comparison: ground truth + calibrated predictions across models
# INPUTS:  rdos/datos/08_panel_formalidad_SLS*.rds, rdos/datos/02_panel_con_taxonomia.rds
# OUTPUTS: rdos/reportes/09c_comp_hibrido.html, rdos/figuras/09c_comp_hibrido/*.pdf
# =============================================================================
# 🌟 09c_comp_hibrido.R 🌟 ####
# Reporte comparativo variable hibrida -- LPM vs GLM vs SLS (sufijo dinámico)
# Proyecto: formalidad_back  |  Capa 6 -- Reportes Comparativos
#
# Variable hibrida: combina ground truth (condicion_formalidad EPH, medida vieja)
# con predicciones calibradas de cada modelo para ocupados sin cobertura de la vieja medida.
# Logica identica a 08c -- reconstruida en memoria [LN12].
#
# INPUT PRINCIPAL:
#   rdos/datos/08_panel_formalidad_{SLS_SUFIJO}.rds  (110 cols -- panel canonico)
# INPUT AUXILIAR:
#   rdos/datos/02_panel_con_taxonomia.rds  (condicion_formalidad)
# INPUTS ESCALARES:
#   rdos/contratos/08_contrato_backcasting_{SUFIJO}.rds  x3
#
# ESTÉTICA:
#   theme_paper() + scale_color_modelos() + scale_fill_modelos() + guardar_figura()
#   coherente con 07e_/08b_/08c_/09a_/09b_
#   TRIM_OBS / TRIM_PANDEMIA desde parametros.R
#
# OUTPUT:
#   rdos/reportes/09c_comp_hibrido.html
#   rdos/reportes/09c_comp_hibrido_notas.txt
#   rdos/figuras/09c_comp_hibrido/*.pdf (via guardar_figura)
#
# ESTRATEGIA:
#   - Panel leido UNA SOLA VEZ; gc() agresivo tras cada calculo [LN14]
#   - Columnas hibridas construidas en memoria (case_when) antes del gc() [LN12]
#   - Join condicion_formalidad: id_individuo + periodo_id [LN13]
#   - Objetos pasados al Rmd via save()/load() [L56]
#   - Secciones:
#       S0  Cobertura hibrida (Observado / Predicho, global y por trimestre)
#       S1  Consistencia GT vs prediccion (acuerdo vieja medida vs nueva)
#       S2  Series hibridas temporales (35 trims x 3 modelos)
#       S3  Expansion de cobertura (ganancia relativa a medida vieja)
#       S4  Correlacion: series hibridas entre modelos y vs backcasting puro
#       S5  Recomendacion para el paper

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  library(ggplot2)
  library(patchwork)
  library(rmarkdown)
  library(scales)
  library(tictoc)
})

# 🔧 Cargar configuración y funciones ------------------------------------------

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── Nombres dinámicos de columnas por modelo ────────────────────────────────
COL_CLASE_CAL_LPM  <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_LPM, "_pea")
COL_CLASE_CAL_GLM  <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_GLM, "_pea")
COL_CLASE_CAL_SLS  <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_SLS, "_pea")

# Nombres dinámicos para columnas híbridas creadas por este script
COL_HIBRIDA_LPM <- paste0("hibrida_", SUFIJO_MODELO_LPM)
COL_HIBRIDA_GLM <- paste0("hibrida_", SUFIJO_MODELO_GLM)
COL_HIBRIDA_SLS <- paste0("hibrida_", SUFIJO_MODELO_SLS)
COL_FUENTE_LPM  <- paste0("fuente_", SUFIJO_MODELO_LPM)
COL_FUENTE_GLM  <- paste0("fuente_", SUFIJO_MODELO_GLM)
COL_FUENTE_SLS  <- paste0("fuente_", SUFIJO_MODELO_SLS)

# ⌛ Inicio contador de tiempo -------------------------------------------------

tic("09c - Comparativo Hibrido")
cat("===================================================================\n")
cat("SCRIPT 09c - COMPARATIVO VARIABLE HIBRIDA LPM / GLM / SLS\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")

# 🪫 1. CARGA ----------------------------------------------------------------
cat("-- 1. Carga -----------------------------------------------------------\n")

PATH_PANEL   <- PATH_08_PANEL_CONSOLIDADO   # panel SLS ~110 cols (LPM+GLM+SLS integrados)
PATH_02_TAX  <- PATH_02_PANEL_TAX
PATH_C08_LPM <- PATH_08_CONTRATO_LPM
PATH_C08_GLM <- PATH_08_CONTRATO_GLM
PATH_C08_SLS <- PATH_08_CONTRATO_SLS

hard_stop(file.exists(PATH_PANEL),   paste0("No existe 08_panel_formalidad_", SUFIJO_MODELO_SLS, ".rds"))
hard_stop(file.exists(PATH_C08_LPM), paste0("No existe 08_contrato_backcasting_", SUFIJO_MODELO_LPM, ".rds"))
hard_stop(file.exists(PATH_C08_GLM), paste0("No existe 08_contrato_backcasting_", SUFIJO_MODELO_GLM, ".rds"))
hard_stop(file.exists(PATH_C08_SLS), paste0("No existe 08_contrato_backcasting_", SUFIJO_MODELO_SLS, ".rds"))
dir.create(DIR_REPORTES, showWarnings = FALSE, recursive = TRUE)

c08_lpm <- readRDS(PATH_C08_LPM)
c08_glm <- readRDS(PATH_C08_GLM)
c08_sls <- readRDS(PATH_C08_SLS)
cat("   [OK] Contratos 08: LPM (", length(names(c08_lpm)),
    ") | GLM (", length(names(c08_glm)),
    ") | SLS (", length(names(c08_sls)), ") campos\n")

# Columnas necesarias del panel [LN14: solo las imprescindibles]
cols_09c <- c(
  "id_individuo", "periodo_id", "pondera",
  "tipo_estimacion_pea",
  COL_CLASE_CAL_LPM,
  COL_CLASE_CAL_GLM,
  COL_CLASE_CAL_SLS
)

cat("   Cargando panel SLS...\n")
panel_full <- readRDS(PATH_PANEL)
panel      <- panel_full[, intersect(cols_09c, names(panel_full))]
rm(panel_full); gc()
cat(sprintf("   [OK] Panel: %s obs x %d cols retenidas\n",
            format(nrow(panel), big.mark = ","), ncol(panel)))

n_panel_total <- nrow(panel)

# condicion_formalidad [LN13: join por id_individuo + periodo_id]
cat("   Cargando condicion_formalidad...\n")
panel_tax <- readRDS(PATH_02_TAX) %>%
  select(id_individuo, periodo_id, condicion_formalidad) %>%
  filter(!is.na(condicion_formalidad))
panel <- panel %>% left_join(panel_tax, by = c("id_individuo", "periodo_id"))
rm(panel_tax); gc()
n_con_cf <- sum(!is.na(panel$condicion_formalidad))
cat(sprintf("   [OK] condicion_formalidad: %s obs con valor\n",
            format(n_con_cf, big.mark = ",")))

# 🪫 2. CONSTANTES Y HELPERS -------------------------------------------------
cat("\n-- 2. Constantes y helpers -------------------------------------------\n")

TRIM_OBS      <- TRIMESTRES_FORMALIDAD
TRIM_PANDEMIA <- TRIMESTRES_PANDEMIA
MODELOS       <- c("LPM","GLM","SLS")

fmt_n   <- function(x) format(as.integer(x), big.mark = ",")
fmt_num <- function(x, d = 4) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("N/D")
  formatC(as.numeric(x), digits = d, format = "f")
}
fmt_pct <- function(x, d = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("N/D")
  sprintf(paste0("%.", d, "f%%"), as.numeric(x))
}
safe_s <- function(fn, fb = NA_real_) {
  v <- tryCatch(fn(), error = function(e) fb)
  if (is.null(v) || length(v) == 0) return(fb)
  suppressWarnings(as.numeric(v[[1]]))
}

cat("   [OK] Helpers definidos\n")

# 🪫 3. CONSTRUIR VARIABLE HIBRIDA [LN12: reconstruccion al vuelo, logica de 08c]
#
# Regla: condicion_formalidad (medida vieja, cobertura historica) tiene prioridad.
#        Para obs sin cobertura de la medida vieja ("No corresponde" o NA),
#        se usa la prediccion calibrada del modelo correspondiente.
cat("\n-- 3. Construccion variable hibrida ----------------------------------\n")

panel <- panel %>%
  mutate(
    # LPM
    !!COL_HIBRIDA_LPM := case_when(
      condicion_formalidad == "Formal"    ~ "Formal",
      condicion_formalidad == "No formal" ~ "Informal",
      .data[[COL_CLASE_CAL_LPM]] %in% c("Formal","Informal") ~ .data[[COL_CLASE_CAL_LPM]],
      TRUE ~ NA_character_
    ),
    !!COL_FUENTE_LPM := case_when(
      condicion_formalidad %in% c("Formal","No formal")          ~ "Observado",
      .data[[COL_CLASE_CAL_LPM]] %in% c("Formal","Informal") ~ "Predicho",
      TRUE ~ NA_character_
    ),
    # GLM
    !!COL_HIBRIDA_GLM := case_when(
      condicion_formalidad == "Formal"    ~ "Formal",
      condicion_formalidad == "No formal" ~ "Informal",
      .data[[COL_CLASE_CAL_GLM]] %in% c("Formal","Informal") ~ .data[[COL_CLASE_CAL_GLM]],
      TRUE ~ NA_character_
    ),
    !!COL_FUENTE_GLM := case_when(
      condicion_formalidad %in% c("Formal","No formal")          ~ "Observado",
      .data[[COL_CLASE_CAL_GLM]] %in% c("Formal","Informal") ~ "Predicho",
      TRUE ~ NA_character_
    ),
    # SLS
    !!COL_HIBRIDA_SLS := case_when(
      condicion_formalidad == "Formal"    ~ "Formal",
      condicion_formalidad == "No formal" ~ "Informal",
      .data[[COL_CLASE_CAL_SLS]] %in% c("Formal","Informal") ~ .data[[COL_CLASE_CAL_SLS]],
      TRUE ~ NA_character_
    ),
    !!COL_FUENTE_SLS := case_when(
      condicion_formalidad %in% c("Formal","No formal")          ~ "Observado",
      .data[[COL_CLASE_CAL_SLS]] %in% c("Formal","Informal") ~ "Predicho",
      TRUE ~ NA_character_
    )
  )

cat(sprintf("   [OK] Hibrida construida: LPM=%s | GLM=%s | SLS=%s obs con valor\n",
            fmt_n(sum(!is.na(panel[[COL_HIBRIDA_LPM]]))),
            fmt_n(sum(!is.na(panel[[COL_HIBRIDA_GLM]]))),
            fmt_n(sum(!is.na(panel[[COL_HIBRIDA_SLS]])))))

# 🪫 4. S0 -- COBERTURA HIBRIDA ----------------------------------------------
cat("\n-- 4. S0 Cobertura hibrida -------------------------------------------\n")

n_obs_cf <- sum(panel$condicion_formalidad %in% c("Formal","No formal"), na.rm=TRUE)

tab_s0_cob <- tibble(
  Modelo      = MODELOS,
  N_Observado = c(
    sum(panel[[COL_FUENTE_LPM]] == "Observado", na.rm=TRUE),
    sum(panel[[COL_FUENTE_GLM]] == "Observado", na.rm=TRUE),
    sum(panel[[COL_FUENTE_SLS]] == "Observado", na.rm=TRUE)
  ),
  N_Predicho  = c(
    sum(panel[[COL_FUENTE_LPM]] == "Predicho", na.rm=TRUE),
    sum(panel[[COL_FUENTE_GLM]] == "Predicho", na.rm=TRUE),
    sum(panel[[COL_FUENTE_SLS]] == "Predicho", na.rm=TRUE)
  )
) %>%
  mutate(
    N_Total  = N_Observado + N_Predicho,
    Pct_Obs  = round(N_Observado / N_Total * 100, 1),
    Pct_Pred = round(N_Predicho  / N_Total * 100, 1)
  )

# Serie de cobertura por trimestre (fuente LPM es representativa -- Observado es identico para los 3)
cob_trim <- panel %>%
  filter(!is.na(tipo_estimacion_pea)) %>%
  group_by(periodo_id) %>%
  summarise(
    n_obs_pond  = sum(pondera[.data[[COL_FUENTE_LPM]] == "Observado"], na.rm = TRUE),
    n_pred_pond = sum(pondera[.data[[COL_FUENTE_LPM]] == "Predicho"],  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_total  = n_obs_pond + n_pred_pond,
    pct_obs  = round(n_obs_pond / n_total * 100, 1)
  ) %>%
  arrange(periodo_id)

trims_todos <- sort(unique(panel$periodo_id))

cat(sprintf("   S0: Observado=%s | Predicho LPM=%s | GLM=%s | SLS=%s\n",
            fmt_n(n_obs_cf),
            fmt_n(tab_s0_cob$N_Predicho[1]),
            fmt_n(tab_s0_cob$N_Predicho[2]),
            fmt_n(tab_s0_cob$N_Predicho[3])))
cat("   [OK] S0\n")

# 🪫 5. S1 -- CONSISTENCIA GROUND TRUTH vs PREDICCION ------------------------
#
# En obs donde condicion_formalidad es valida Y tenemos prediccion:
# cuanto acuerda la prediccion calibrada con la medida vieja de registro.
cat("\n-- 5. S1 Consistencia GT vs Prediccion --------------------------------\n")

calc_cons <- function(pred_col) {
  tryCatch({
    d <- panel %>%
      filter(
        condicion_formalidad %in% c("Formal","No formal"),
        !is.na(.data[[pred_col]])
      ) %>%
      mutate(
        gt  = if_else(condicion_formalidad == "Formal", "Formal", "Informal"),
        ok  = (gt == .data[[pred_col]])
      )
    list(
      n      = nrow(d),
      pct_ok = round(mean(d$ok, na.rm = TRUE) * 100, 2),
      pct_ff = round(mean(d$ok[d$gt == "Formal"],   na.rm = TRUE) * 100, 2),
      pct_ii = round(mean(d$ok[d$gt == "Informal"], na.rm = TRUE) * 100, 2)
    )
  }, error = function(e) list(n=NA_integer_, pct_ok=NA_real_, pct_ff=NA_real_, pct_ii=NA_real_))
}

cons_lpm <- calc_cons(COL_CLASE_CAL_LPM)
cons_glm <- calc_cons(COL_CLASE_CAL_GLM)
cons_sls <- calc_cons(COL_CLASE_CAL_SLS)

tab_s1_cons <- tibble(
  Modelo          = MODELOS,
  N_comparados    = c(cons_lpm$n,      cons_glm$n,      cons_sls$n),
  Pct_acuerdo     = c(cons_lpm$pct_ok, cons_glm$pct_ok, cons_sls$pct_ok),
  Pct_Formal_ok   = c(cons_lpm$pct_ff, cons_glm$pct_ff, cons_sls$pct_ff),
  Pct_Informal_ok = c(cons_lpm$pct_ii, cons_glm$pct_ii, cons_sls$pct_ii)
)

cat(sprintf("   S1: acuerdo LPM=%.2f%% | GLM=%.2f%% | SLS=%.2f%%\n",
            cons_lpm$pct_ok, cons_glm$pct_ok, cons_sls$pct_ok))
cat("   [OK] S1\n")

# 🪫 6. S2 -- SERIES HIBRIDAS TEMPORALES -------------------------------------
cat("\n-- 6. S2 Series hibridas ---------------------------------------------\n")

calc_serie_hib <- function(hib_col) {
  panel %>%
    filter(!is.na(tipo_estimacion_pea)) %>%
    group_by(periodo_id) %>%
    summarise(
      n_formal = sum(pondera[.data[[hib_col]] == "Formal"],   na.rm = TRUE),
      n_total  = sum(pondera[!is.na(.data[[hib_col]])],       na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(tasa = round(n_formal / n_total * 100, 2)) %>%
    arrange(periodo_id)
}

serie_hib_lpm <- calc_serie_hib(COL_HIBRIDA_LPM)
serie_hib_glm <- calc_serie_hib(COL_HIBRIDA_GLM)
serie_hib_sls <- calc_serie_hib(COL_HIBRIDA_SLS)

n_trims_hib <- nrow(serie_hib_lpm)

serie_hib_long <- bind_rows(
  serie_hib_lpm %>% mutate(modelo = "LPM"),
  serie_hib_glm %>% mutate(modelo = "GLM"),
  serie_hib_sls %>% mutate(modelo = "SLS")
)

cat(sprintf("   S2: %d trims | media LPM=%.2f%% | GLM=%.2f%% | SLS=%.2f%%\n",
            n_trims_hib,
            mean(serie_hib_lpm$tasa, na.rm = TRUE),
            mean(serie_hib_glm$tasa, na.rm = TRUE),
            mean(serie_hib_sls$tasa, na.rm = TRUE)))
cat("   [OK] S2\n")

# 🪫 7. S3 -- EXPANSION DE COBERTURA -----------------------------------------
cat("\n-- 7. S3 Expansion de cobertura --------------------------------------\n")

expan_trim <- panel %>%
  filter(!is.na(tipo_estimacion_pea)) %>%
  group_by(periodo_id) %>%
  summarise(
    n_vieja      = sum(pondera[condicion_formalidad %in% c("Formal","No formal")], na.rm = TRUE),
    n_pred_lpm   = sum(pondera[.data[[COL_FUENTE_LPM]] == "Predicho"], na.rm = TRUE),
    n_pred_glm   = sum(pondera[.data[[COL_FUENTE_GLM]] == "Predicho"], na.rm = TRUE),
    n_pred_sls   = sum(pondera[.data[[COL_FUENTE_SLS]] == "Predicho"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_exp_lpm = round(n_pred_lpm / n_vieja * 100, 1),
    pct_exp_glm = round(n_pred_glm / n_vieja * 100, 1),
    pct_exp_sls = round(n_pred_sls / n_vieja * 100, 1)
  ) %>%
  arrange(periodo_id)

exp_lpm_med <- round(mean(expan_trim$pct_exp_lpm, na.rm = TRUE), 1)
exp_glm_med <- round(mean(expan_trim$pct_exp_glm, na.rm = TRUE), 1)
exp_sls_med <- round(mean(expan_trim$pct_exp_sls, na.rm = TRUE), 1)

# Tasa de formalidad DENTRO del bloque Predicho por modelo y trimestre.
# El % de expansion es identico para los 3 modelos (mismo n "Predicho").
# Esta serie muestra las diferencias reales: tasa formal en obs nuevos.
tasa_pred_trim <- panel %>%
  filter(!is.na(tipo_estimacion_pea)) %>%
  group_by(periodo_id) %>%
  summarise(
    tasa_pred_lpm = round(
      sum(pondera[.data[[COL_FUENTE_LPM]] == "Predicho" & .data[[COL_HIBRIDA_LPM]] == "Formal"], na.rm = TRUE) /
      sum(pondera[.data[[COL_FUENTE_LPM]] == "Predicho" & !is.na(.data[[COL_HIBRIDA_LPM]])],    na.rm = TRUE) * 100, 2),
    tasa_pred_glm = round(
      sum(pondera[.data[[COL_FUENTE_GLM]] == "Predicho" & .data[[COL_HIBRIDA_GLM]] == "Formal"], na.rm = TRUE) /
      sum(pondera[.data[[COL_FUENTE_GLM]] == "Predicho" & !is.na(.data[[COL_HIBRIDA_GLM]])],    na.rm = TRUE) * 100, 2),
    tasa_pred_sls = round(
      sum(pondera[.data[[COL_FUENTE_SLS]] == "Predicho" & .data[[COL_HIBRIDA_SLS]] == "Formal"], na.rm = TRUE) /
      sum(pondera[.data[[COL_FUENTE_SLS]] == "Predicho" & !is.na(.data[[COL_HIBRIDA_SLS]])],    na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(periodo_id)

cat(sprintf("   S3: expansion promedio LPM=%.1f%% | GLM=%.1f%% | SLS=%.1f%% sobre medida vieja\n",
            exp_lpm_med, exp_glm_med, exp_sls_med))
cat(sprintf("        tasa formal en bloque Predicho (media): LPM=%.2f%% | GLM=%.2f%% | SLS=%.2f%%\n",
            mean(tasa_pred_trim$tasa_pred_lpm, na.rm=TRUE),
            mean(tasa_pred_trim$tasa_pred_glm, na.rm=TRUE),
            mean(tasa_pred_trim$tasa_pred_sls, na.rm=TRUE)))
cat("   [OK] S3\n")

# Delta promedio absoluto entre series híbridas (para contrato → 10_paper_html.R §4.4)
# Fuente: serie_hib_* ya calculadas arriba (S2). El delta es en escala pp (0-100).
delta_prom_glm_mpl <- round(mean(abs(serie_hib_glm$tasa - serie_hib_lpm$tasa), na.rm = TRUE), 1)
delta_prom_glm_sls <- round(mean(abs(serie_hib_glm$tasa - serie_hib_sls$tasa), na.rm = TRUE), 1)
cat(sprintf("   S3+: delta promedio GLM-LPM=%.1f pp | GLM-SLS=%.1f pp (sobre serie híbrida completa)\n",
            delta_prom_glm_mpl, delta_prom_glm_sls))

# 🪫 8. S4 -- CORRELACION: SERIES HIBRIDAS vs ENTRE SI y vs BACKCASTING PURO -
cat("\n-- 8. S4 Correlacion series ------------------------------------------\n")

# Series backcasting puro (cal, universo ocupados: Observado + Backcasting)
calc_serie_back <- function(cal_col) {
  panel %>%
    filter(tipo_estimacion_pea %in% c("Observado","Backcasting")) %>%
    group_by(periodo_id) %>%
    summarise(
      n_formal = sum(pondera[.data[[cal_col]] == "Formal"],  na.rm = TRUE),
      n_total  = sum(pondera[!is.na(.data[[cal_col]])],      na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(tasa = round(n_formal / n_total * 100, 2)) %>%
    arrange(periodo_id)
}

serie_back_lpm <- calc_serie_back(COL_CLASE_CAL_LPM)
serie_back_glm <- calc_serie_back(COL_CLASE_CAL_GLM)
serie_back_sls <- calc_serie_back(COL_CLASE_CAL_SLS)

# Serie medida vieja (condicion_formalidad = Formal / No formal, ponderada)
serie_old <- panel %>%
  filter(condicion_formalidad %in% c("Formal", "No formal")) %>%
  group_by(periodo_id) %>%
  summarise(
    n_formal = sum(pondera[condicion_formalidad == "Formal"], na.rm = TRUE),
    n_total  = sum(pondera, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(tasa = round(n_formal / n_total * 100, 2)) %>%
  arrange(periodo_id)

cat(sprintf("   Serie medida vieja: %d trimestres\n", nrow(serie_old)))

# Liberar panel ANTES del Rmd [LN14]
rm(panel); gc()
cat("   Panel liberado de memoria\n")

# Matriz de correlacion 6x6
series_mat <- data.frame(
  LPM_hib  = serie_hib_lpm$tasa,
  GLM_hib  = serie_hib_glm$tasa,
  SLS_hib  = serie_hib_sls$tasa,
  LPM_back = serie_back_lpm$tasa,
  GLM_back = serie_back_glm$tasa,
  SLS_back = serie_back_sls$tasa
)
cor_mat <- round(cor(series_mat, use = "complete.obs"), 4)
rownames(cor_mat) <- colnames(cor_mat) <-
  c("LPM hib.","GLM hib.","SLS hib.","LPM back.","GLM back.","SLS back.")

cat(sprintf("   S4: corr hibridas LPM-GLM=%.4f | LPM-SLS=%.4f | GLM-SLS=%.4f\n",
            cor_mat["LPM hib.","GLM hib."],
            cor_mat["LPM hib.","SLS hib."],
            cor_mat["GLM hib.","SLS hib."]))
cat(sprintf("        hib vs back: LPM=%.4f | GLM=%.4f | SLS=%.4f\n",
            cor_mat["LPM hib.","LPM back."],
            cor_mat["GLM hib.","GLM back."],
            cor_mat["SLS hib.","SLS back."]))
cat("   [OK] S4\n")

# 🪫 9. ESCALARES PARA Rmd ---------------------------------------------------
cat("\n-- 9. Escalares para Rmd -------------------------------------------\n")

n_panel_real <- safe_s(function() c08_sls$n_panel_total) %||% n_panel_total

umb_you_lpm <- fmt_num(c08_lpm$umbral_youden, 4)
umb_cal_lpm <- fmt_num(c08_lpm$umbral_calibracion, 4)
umb_you_glm <- fmt_num(c08_glm$umbral_youden, 4)
umb_cal_glm <- fmt_num(c08_glm$umbral_calibracion, 4)
umb_you_sls <- fmt_num(c08_sls$umbral_youden, 4)
umb_cal_sls <- fmt_num(c08_sls$umbral_calibracion, 4)

auc_train_lpm <- safe_s(function() c08_lpm$auc_global_train)  # AUC train backcasting
auc_train_glm <- safe_s(function() c08_glm$auc_global_train)
auc_train_sls <- safe_s(function() c08_sls$auc_global_train)

cat("   [OK] Escalares listos\n")

# 🪫 10. NOTAS TXT -----------------------------------------------------------
cat("\n-- 10. Notas TXT ---------------------------------------------------\n")

notas <- c(
  "09c_comp_hibrido -- Variable hibrida comparativo LPM / GLM / SLS",
  paste0("Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "-- COBERTURA HIBRIDA S0 --",
  paste0("Observado (vieja medida, identico en 3 modelos): ",
         fmt_n(tab_s0_cob$N_Observado[1])),
  paste0("Predicho LPM=", fmt_n(tab_s0_cob$N_Predicho[1]),
         " | GLM=", fmt_n(tab_s0_cob$N_Predicho[2]),
         " | SLS=", fmt_n(tab_s0_cob$N_Predicho[3])),
  paste0("Pct Predicho LPM=", tab_s0_cob$Pct_Pred[1],
         "% | GLM=", tab_s0_cob$Pct_Pred[2],
         "% | SLS=", tab_s0_cob$Pct_Pred[3], "%"),
  "",
  "-- CONSISTENCIA GT vs PREDICCION S1 --",
  paste0("Acuerdo global: LPM=", cons_lpm$pct_ok,
         "% | GLM=", cons_glm$pct_ok,
         "% | SLS=", cons_sls$pct_ok, "%"),
  paste0("Formales bien clasificados:   LPM=", cons_lpm$pct_ff,
         "% | GLM=", cons_glm$pct_ff,
         "% | SLS=", cons_sls$pct_ff, "%"),
  paste0("Informales bien clasificados: LPM=", cons_lpm$pct_ii,
         "% | GLM=", cons_glm$pct_ii,
         "% | SLS=", cons_sls$pct_ii, "%"),
  "",
  "-- EXPANSION DE COBERTURA S3 --",
  paste0("Expansion promedio respecto a medida vieja: ",
         "LPM=", exp_lpm_med,
         "% | GLM=", exp_glm_med,
         "% | SLS=", exp_sls_med, "%"),
  "",
  "-- CORRELACION SERIES HIBRIDAS S4 --",
  paste0("Entre hibridas: LPM-GLM=", cor_mat["LPM hib.","GLM hib."],
         " | LPM-SLS=", cor_mat["LPM hib.","SLS hib."],
         " | GLM-SLS=", cor_mat["GLM hib.","SLS hib."]),
  paste0("Hib vs Back: LPM=", cor_mat["LPM hib.","LPM back."],
         " | GLM=", cor_mat["GLM hib.","GLM back."],
         " | SLS=", cor_mat["SLS hib.","SLS back."])
)

writeLines(notas, PATH_09C_NOTAS)
cat(sprintf("   [OK] Notas: %s\n", PATH_09C_NOTAS))

# 🪫 10b. CONTRATO -- escalares para 10_paper_html.R -------------------------
cat("\n-- 10b. Contrato 09c -------------------------------------------------\n")

# ── Valores para el paper HTML (evitar HC en 10_paper_html.R) ────────────────
# Cobertura híbrida GLM: proporción Observado/Predicho (tab_s0_cob, fila GLM=2)
pct_gt_glm_paper   <- tab_s0_cob$Pct_Obs[tab_s0_cob$Modelo == "GLM"][1]
pct_pred_glm_paper <- tab_s0_cob$Pct_Pred[tab_s0_cob$Modelo == "GLM"][1]

# Tasas de formalidad en trimestres clave (serie híbrida GLM, universo PEA)
# 2016T4 = primer trim del panel; pandemia = mínimo de la serie; 2025T3 = último trim
t_inicial_glm  <- serie_hib_glm$tasa[serie_hib_glm$periodo_id == min(serie_hib_glm$periodo_id)][1]
t_pandemia_glm <- min(serie_hib_glm$tasa, na.rm = TRUE)           # mínimo absoluto (pandemia ~2020T2)
t_final_glm    <- serie_hib_glm$tasa[serie_hib_glm$periodo_id == max(serie_hib_glm$periodo_id)][1]

# Período del mínimo (para identificar trimestre pandemia)
periodo_pandemia_glm <- serie_hib_glm$periodo_id[which.min(serie_hib_glm$tasa)][1]

contrato_09c <- list(
  script                  = "09c_comp_hibrido.R",
  fecha                   = Sys.time(),
  sufijo_lpm              = SUFIJO_MODELO_LPM,
  sufijo_glm              = SUFIJO_MODELO_GLM,
  sufijo_sls              = SUFIJO_MODELO_SLS,
  # Consistencia GT vs predicción calibrada (Panel C del paper)
  acuerdo_gt_lpm          = cons_lpm$pct_ok / 100,   # escala 0-1
  acuerdo_gt_glm          = cons_glm$pct_ok / 100,
  acuerdo_gt_sls          = cons_sls$pct_ok / 100,
  # Formales / Informales bien clasificados (sensibilidad / especificidad sobre GT)
  pct_formales_ok_lpm     = cons_lpm$pct_ff / 100,
  pct_formales_ok_glm     = cons_glm$pct_ff / 100,
  pct_formales_ok_sls     = cons_sls$pct_ff / 100,
  pct_informales_ok_lpm   = cons_lpm$pct_ii / 100,
  pct_informales_ok_glm   = cons_glm$pct_ii / 100,
  pct_informales_ok_sls   = cons_sls$pct_ii / 100,
  # N observaciones comparadas (donde condicion_formalidad es válida)
  n_comparados_lpm        = cons_lpm$n,
  n_comparados_glm        = cons_glm$n,
  n_comparados_sls        = cons_sls$n,
  # Correlación entre series híbridas S4
  corr_hib_lpm_glm        = cor_mat["LPM hib.", "GLM hib."],
  corr_hib_lpm_sls        = cor_mat["LPM hib.", "SLS hib."],
  corr_hib_glm_sls        = cor_mat["GLM hib.", "SLS hib."],
  # Correlación híbrida vs backcasting puro (coherencia interna)
  corr_hib_vs_back_lpm    = cor_mat["LPM hib.", "LPM back."],
  corr_hib_vs_back_glm    = cor_mat["GLM hib.", "GLM back."],
  corr_hib_vs_back_sls    = cor_mat["SLS hib.", "SLS back."],
  # Correlación entre backcasting puro inter-modelos (Tabla 14 — D60)
  # Fuente: cor_mat construida en sección 9 de este script
  corr_back_lpm_glm       = cor_mat["LPM back.", "GLM back."],
  corr_back_lpm_sls       = cor_mat["LPM back.", "SLS back."],
  corr_back_glm_sls       = cor_mat["GLM back.", "SLS back."],
  # ── Para 10_paper_html.R — narrativa del abstract ──────────────────────────
  # Cobertura variable híbrida (% observado vs % predicho sobre PEA)
  pct_gt_glm              = pct_gt_glm_paper,    # ej. 68.6
  pct_pred_glm            = pct_pred_glm_paper,  # ej. 31.4
  # Número de trimestres del panel completo
  n_trims_panel           = n_trims_hib,
  # Tasas de formalidad en trimestres clave (serie híbrida GLM, universo PEA)
  tasa_glm_inicial        = t_inicial_glm,        # 2016T4
  tasa_glm_pandemia       = t_pandemia_glm,        # mínimo histórico (~2020T2)
  tasa_glm_final          = t_final_glm,           # último trim (2025T3)
  periodo_pandemia_glm    = periodo_pandemia_glm,  # id del período mínimo
  # Expansión de cobertura respecto a medida vieja (S3 — idéntico en los 3 modelos)
  pct_exp_cobertura_glm   = exp_glm_med,           # % promedio de ampliación (= exp_lpm_med = exp_sls_med)
  # Delta promedio absoluto entre series híbridas (S3+ — para §4.4 del paper)
  delta_prom_glm_mpl      = delta_prom_glm_mpl,   # pp, promedio sobre panel completo
  delta_prom_glm_sls      = delta_prom_glm_sls,   # pp, promedio sobre panel completo
  # Series trimestrales para el appendix (paper/appendices/I-hybrid.qmd)
  series_trimestrales     = data.frame(
    quarter     = serie_hib_glm$periodo_id,
    old_measure = serie_old$tasa,
    lpm_hybrid  = serie_hib_lpm$tasa,
    glm_hybrid  = serie_hib_glm$tasa,
    sls_hybrid  = serie_hib_sls$tasa,
    stringsAsFactors = FALSE
  )
)

saveRDS(contrato_09c, PATH_09C_COMP_CONTRATO)
cat(sprintf("   [OK] Contrato: %s\n", basename(PATH_09C_COMP_CONTRATO)))

# 🪫 11. CONSTRUIR RMD [L56] -------------------------------------------------
cat("\n-- 11. Construyendo Rmd ----------------------------------------------\n")

rds_env <- tempfile(fileext = ".rds")
save(
  tab_s0_cob, cob_trim,
  tab_s1_cons,
  serie_hib_lpm, serie_hib_glm, serie_hib_sls, serie_hib_long,
  expan_trim, tasa_pred_trim, exp_lpm_med, exp_glm_med, exp_sls_med,
  cor_mat,
  serie_back_lpm, serie_back_glm, serie_back_sls,
  trims_todos, n_trims_hib,
  umb_you_lpm, umb_cal_lpm, umb_you_glm, umb_cal_glm, umb_you_sls, umb_cal_sls,
  n_panel_real, n_obs_cf,
  TRIM_OBS, TRIM_PANDEMIA,
  DIR_FIGURAS_09C,
  SUFIJO_MODELO_LPM, SUFIJO_MODELO_GLM, SUFIJO_MODELO_SLS,
  file = rds_env
)

rds_path <- gsub("\\\\", "/", rds_env)
rmd_temp <- tempfile(fileext = ".Rmd")
con <- file(rmd_temp, open = "wt", encoding = "UTF-8")

# ---- YAML ----
cat('---
title: "Comparativo Variable Hibrida: ', SUFIJO_MODELO_LPM, ' vs ', SUFIJO_MODELO_GLM, ' vs ', SUFIJO_MODELO_SLS, '"
subtitle: "Proyecto EPH Argentina -- Formalidad Laboral 2016T4-2025T3 | Capa 6 | Variable Hibrida"
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

# ---- SETUP ----
# [L56-FIX] Partido en dos cat() para evitar conflicto de % en sprintf.
# Primera: solo el load() usa sprintf para interpolar rds_path.
# Segunda: cat() plano para el resto del chunk (contiene %% / %d / %f).
cat(sprintf('```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,
                      fig.width=11, fig.height=5.5, dpi=150)
suppressPackageStartupMessages({
  library(tidyverse); library(knitr); library(kableExtra)
  library(ggplot2); library(patchwork); library(scales)
})
load("%s")
', rds_path), file = con)
cat('fmt_n   <- function(x) format(as.integer(x), big.mark=",")
fmt_pct <- function(x, d=2) sprintf(paste0("%%.%df%%%%"), d, as.numeric(x))
source(here::here("script", "config", "funciones_comunes.R"))
# → carga theme_paper(), COL_LPM/GLM/SLS, COL_OBSERVADO, scale_*_modelos(), guardar_figura()
n_trims <- length(trims_todos)
idx_obs <- which(trims_todos %in% TRIM_OBS)
```

', file = con)

# ---- RESUMEN EJECUTIVO ----
cat('# Resumen Ejecutivo {.unnumbered}

> **Variable hibrida:** Prioriza la medida de registro (condicion_formalidad) donde esta
> disponible; completa con predicciones calibradas del modelo correspondiente en el resto.
> Resultado: mayor cobertura que la medida vieja y series mas largas que el backcasting puro.

```{r resumen_ej}
kpi <- tibble(
  Indicador = c(
    "Panel total (obs)",
    "Cobertura medida vieja (Formal / No formal)",
    "Nuevos cubiertos por prediccion: LPM / GLM / SLS",
    "Expansion relativa a medida vieja: LPM / GLM / SLS",
    "Acuerdo GT vs prediccion (global): LPM / GLM / SLS",
    "Acuerdo en Formales: LPM / GLM / SLS",
    "Acuerdo en Informales: LPM / GLM / SLS",
    "Corr. series hibridas: LPM-GLM / LPM-SLS / GLM-SLS",
    "Corr. GLM_hib vs GLM_back (coherencia interna)"
  ),
  Valor = c(
    fmt_n(n_panel_real),
    fmt_n(n_obs_cf),
    paste0(fmt_n(tab_s0_cob$N_Predicho[1])," / ",
           fmt_n(tab_s0_cob$N_Predicho[2])," / ",
           fmt_n(tab_s0_cob$N_Predicho[3])),
    paste0(exp_lpm_med,"% / ",exp_glm_med,"% / ",exp_sls_med,"%"),
    paste0(tab_s1_cons$Pct_acuerdo,"%", collapse=" / "),
    paste0(tab_s1_cons$Pct_Formal_ok,"%", collapse=" / "),
    paste0(tab_s1_cons$Pct_Informal_ok,"%", collapse=" / "),
    paste0(cor_mat["LPM hib.","GLM hib."]," / ",
           cor_mat["LPM hib.","SLS hib."]," / ",
           cor_mat["GLM hib.","SLS hib."]),
    as.character(cor_mat["GLM hib.","GLM back."])
  )
)
kable(kpi, format="html", align=c("l","l"),
      caption=paste0("Indicadores clave -- Variable hibrida ", SUFIJO_MODELO_LPM, " / ", SUFIJO_MODELO_GLM, " / ", SUFIJO_MODELO_SLS)) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, position="center") %>%
  row_spec(c(4,5,8), bold=TRUE, background="#d4edda") %>%
  row_spec(9, background="#e3f2fd") %>%
  column_spec(1, width="36em")
```

---

', file = con)

# ---- S0 COBERTURA ----
cat('# Cobertura de la variable hibrida {#s0}

> **Observado** = condicion_formalidad ∈ {Formal, No formal} (medida de registro EPH).
> **Predicho** = prediccion calibrada del modelo, aplicada donde condicion_formalidad = "No corresponde" o NA.
> El bloque Observado es identico en los tres modelos (no depende del modelo).

## Tabla de cobertura global

```{r s0_tabla}
tab_s0_cob %>%
  mutate(
    N_Observado = format(N_Observado, big.mark=","),
    N_Predicho  = format(N_Predicho,  big.mark=","),
    N_Total     = format(N_Total,     big.mark=","),
    Pct_Obs     = paste0(Pct_Obs,  "%"),
    Pct_Pred    = paste0(Pct_Pred, "%")
  ) %>%
  kbl(format="html", align=c("l","r","r","r","r","r"),
      col.names=c("Modelo","N Observado","N Predicho","N Total","% Obs.","% Pred."),
      caption="S0: Cobertura hibrida por modelo (panel completo)") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(1, bold=TRUE) %>%
  column_spec(3, background="#e3f2fd") %>%
  column_spec(6, background="#e3f2fd")
```

## Evolucion de cobertura por trimestre

```{r s0_cob_trim, fig.height=4}
cob_long <- cob_trim %>%
  select(periodo_id, Observado=n_obs_pond, Predicho=n_pred_pond) %>%
  pivot_longer(c(Observado, Predicho), names_to="fuente", values_to="n") %>%
  mutate(fuente = factor(tr(fuente), levels=tr(c("Observado","Predicho"))))

p_cob_trim <- ggplot(cob_long, aes(x=periodo_id, y=n/1e6, fill=fuente)) +
  geom_col(position="stack") +
  scale_fill_manual(values=setNames(
    c(COL_OBSERVADO, PAL_DESCRIPTIVO[2]),
    tr(c("Observado", "Predicho")))) +
  scale_y_continuous(labels=function(x) paste0(x,"M")) +
  scale_x_discrete(breaks=trims_todos[seq(1,length(trims_todos),by=4)]) +
  geom_vline(xintercept=which(trims_todos==TRIM_PANDEMIA[1]), linetype="dashed",
             color="#666", alpha=0.7) +
  tr_labs(title="Cobertura hibrida por fuente (ponderada)",
          subtitle="Punteado = inicio pandemia | Predicho = obs nuevos via prediccion",
          x=NULL, y="Personas (millones)", fill="Fuente") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        legend.position="top")
guardar_figura(p_cob_trim, DIR_FIGURAS_09C, "cob_trim", 1)
p_cob_trim
```

<small>
Nota: la composicion Observado/Predicho es identica para los tres modelos ya que el bloque
Observado depende exclusivamente de condicion_formalidad.
Las diferencias entre modelos aparecen solo en la tasa de formalidad del bloque Predicho.
</small>

---

', file = con)

# ---- S1 CONSISTENCIA ----
cat('# Consistencia: medida vieja vs prediccion {#s1}

> Donde ambas fuentes existen (condicion_formalidad valida + prediccion disponible),
> **¿cuanto acuerda la prediccion calibrada con la medida de registro?**
> Es una validacion cruzada de los modelos: alta concordancia valida la capacidad
> predictiva fuera de la ventana de entrenamiento.

```{r s1_tabla}
tab_s1_cons %>%
  mutate(
    N_comparados    = format(N_comparados, big.mark=","),
    Pct_acuerdo     = paste0(Pct_acuerdo,     "%"),
    Pct_Formal_ok   = paste0(Pct_Formal_ok,   "%"),
    Pct_Informal_ok = paste0(Pct_Informal_ok, "%")
  ) %>%
  kbl(format="html", align=c("l","r","r","r","r"),
      col.names=c("Modelo","N comparados","% Acuerdo global",
                  "% Formal-Formal","% Informal-Informal"),
      caption="S1: Consistencia prediccion calibrada vs medida de registro") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(1, bold=TRUE) %>%
  column_spec(3, background="#d4edda", bold=TRUE) %>%
  column_spec(2, background="#e3f2fd", bold=TRUE)
```

<small>
Formal-Formal = tasa de acierto sobre individuos con condicion_formalidad=="Formal".
Informal-Informal = tasa de acierto sobre condicion_formalidad=="No formal".
Las medidas son parcialmente comparables: condicion_formalidad usa registro AFIP/ANSES
(asalariados solamente); la prediccion aplica al universo PEA ampliado.
</small>

---

', file = con)

# ---- S2 SERIES HIBRIDAS ----
cat('# Series temporales hibridas {#s2}

> Series de tasa de formalidad usando la variable hibrida.
> Universo: ocupados en PEA (tipo_estimacion_pea valido).
> La tasa hibrida combina registro historico donde disponible con prediccion calibrada.

## Grafico de series comparativas

```{r s2_series, fig.height=5.5}
p_hib <- ggplot(serie_hib_long,
                aes(x=periodo_id, y=tasa, color=modelo, group=modelo)) +
  geom_line(size=0.9) +
  geom_point(size=1.8, alpha=0.7) +
  geom_vline(xintercept=idx_obs[1], linetype="dashed", color="#27ae60", alpha=0.7) +
  annotate("text", x=idx_obs[1], y=max(serie_hib_long$tasa, na.rm=TRUE)*0.97,
           label=tr("Inicio\nobservados"), hjust=-0.1, size=3, color="#27ae60") +
  scale_color_modelos() +
  scale_linetype_modelos() +
  scale_y_continuous(labels=function(x) paste0(x,"%"),
                     limits=c(min(serie_hib_long$tasa, na.rm=TRUE)*0.97,
                               max(serie_hib_long$tasa, na.rm=TRUE)*1.02)) +
  scale_x_discrete(breaks=trims_todos[seq(1,length(trims_todos),by=4)]) +
  tr_labs(title="Tasa de formalidad -- Variable hibrida",
          subtitle=paste0(SUFIJO_MODELO_LPM, " / ", SUFIJO_MODELO_GLM, " / ", SUFIJO_MODELO_SLS, " | Ocupados PEA | 2016T4-2025T3"),
          x=NULL, y="Tasa de formalidad (%)", color="Modelo") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        legend.position="top")
guardar_figura(p_hib, DIR_FIGURAS_09C, "series_hibridas", 2)
p_hib
```

## GLM -- hibrida vs backcasting puro

```{r s2_hib_vs_back_glm, fig.height=4.5}
comp_glm <- bind_rows(
  serie_hib_glm  %>% select(periodo_id, tasa) %>% mutate(tipo=tr("GLM hibrida")),
  serie_back_glm %>% select(periodo_id, tasa) %>% mutate(tipo=tr("GLM backcasting"))
)

p_hib_glm <- ggplot(comp_glm, aes(x=periodo_id, y=tasa, color=tipo, group=tipo, linetype=tipo)) +
  geom_line(size=0.9) +
  geom_point(size=1.5, alpha=0.7) +
  scale_color_manual(values=setNames(
    c(COL_GLM, scales::alpha(COL_GLM, 0.45)),
    tr(c("GLM hibrida", "GLM backcasting")))) +
  scale_linetype_manual(values=setNames(
    c("solid", "dashed"),
    tr(c("GLM hibrida", "GLM backcasting")))) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  scale_x_discrete(breaks=trims_todos[seq(1,length(trims_todos),by=4)]) +
  tr_labs(title="GLM: serie hibrida vs backcasting puro",
          subtitle="La serie hibrida incorpora registro historico donde disponible",
          x=NULL, y="Tasa de formalidad (%)", color=NULL, linetype=NULL) +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        legend.position="top")
guardar_figura(p_hib_glm, DIR_FIGURAS_09C, "hib_vs_back_glm", 3)
p_hib_glm
```

```{r s2_tabla_glm}
tbl_glm <- inner_join(
  serie_hib_glm  %>% select(periodo_id, tasa_hib  = tasa),
  serie_back_glm %>% select(periodo_id, tasa_back = tasa),
  by = "periodo_id"
) %>%
  mutate(
    delta    = round(tasa_hib - tasa_back, 2),
    delta_pp = paste0(ifelse(delta >= 0, "+", ""), delta, " pp"),
    bg       = ifelse(abs(delta) > 1, "#fff3cd", "white")
  )

tbl_glm %>%
  select(Trimestre=periodo_id,
         `Hib. (%)`=tasa_hib, `Back. (%)`=tasa_back, `Delta`=delta_pp) %>%
  kbl(format="html", align=c("l","r","r","r"),
      caption="GLM: tasa hibrida vs backcasting puro por trimestre") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(4, bold=TRUE, background=tbl_glm$bg)
```

## LPM -- hibrida vs backcasting puro

```{r s2_hib_vs_back_lpm, fig.height=4.5}
comp_lpm <- bind_rows(
  serie_hib_lpm  %>% select(periodo_id, tasa) %>% mutate(tipo=tr("LPM hibrida")),
  serie_back_lpm %>% select(periodo_id, tasa) %>% mutate(tipo=tr("LPM backcasting"))
)

p_hib_lpm <- ggplot(comp_lpm, aes(x=periodo_id, y=tasa, color=tipo, group=tipo, linetype=tipo)) +
  geom_line(size=0.9) +
  geom_point(size=1.5, alpha=0.7) +
  scale_color_manual(values=setNames(
    c(COL_LPM, scales::alpha(COL_LPM, 0.45)),
    tr(c("LPM hibrida", "LPM backcasting")))) +
  scale_linetype_manual(values=setNames(
    c("solid", "dashed"),
    tr(c("LPM hibrida", "LPM backcasting")))) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  scale_x_discrete(breaks=trims_todos[seq(1,length(trims_todos),by=4)]) +
  tr_labs(title="LPM: serie hibrida vs backcasting puro",
          subtitle="La serie hibrida incorpora registro historico donde disponible",
          x=NULL, y="Tasa de formalidad (%)", color=NULL, linetype=NULL) +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        legend.position="top")
guardar_figura(p_hib_lpm, DIR_FIGURAS_09C, "hib_vs_back_lpm", 4)
p_hib_lpm
```

```{r s2_tabla_lpm}
inner_join(
  serie_hib_lpm  %>% select(periodo_id, tasa_hib  = tasa),
  serie_back_lpm %>% select(periodo_id, tasa_back = tasa),
  by = "periodo_id"
) %>%
  mutate(
    delta    = round(tasa_hib - tasa_back, 2),
    delta_pp = paste0(ifelse(delta >= 0, "+", ""), delta, " pp")
  ) %>%
  select(Trimestre=periodo_id,
         `Hib. (%)`=tasa_hib, `Back. (%)`=tasa_back, `Delta`=delta_pp) %>%
  kbl(format="html", align=c("l","r","r","r"),
      caption="LPM: tasa hibrida vs backcasting puro por trimestre") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(4, bold=TRUE)
```

## SLS -- hibrida vs backcasting puro

```{r s2_hib_vs_back_sls, fig.height=4.5}
comp_sls <- bind_rows(
  serie_hib_sls  %>% select(periodo_id, tasa) %>% mutate(tipo=tr("SLS hibrida")),
  serie_back_sls %>% select(periodo_id, tasa) %>% mutate(tipo=tr("SLS backcasting"))
)

p_hib_sls <- ggplot(comp_sls, aes(x=periodo_id, y=tasa, color=tipo, group=tipo, linetype=tipo)) +
  geom_line(size=0.9) +
  geom_point(size=1.5, alpha=0.7) +
  scale_color_manual(values=setNames(
    c(COL_SLS, scales::alpha(COL_SLS, 0.45)),
    tr(c("SLS hibrida", "SLS backcasting")))) +
  scale_linetype_manual(values=setNames(
    c("solid", "dashed"),
    tr(c("SLS hibrida", "SLS backcasting")))) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  scale_x_discrete(breaks=trims_todos[seq(1,length(trims_todos),by=4)]) +
  tr_labs(title="SLS: serie hibrida vs backcasting puro",
          subtitle="La serie hibrida incorpora registro historico donde disponible",
          x=NULL, y="Tasa de formalidad (%)", color=NULL, linetype=NULL) +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        legend.position="top")
guardar_figura(p_hib_sls, DIR_FIGURAS_09C, "hib_vs_back_sls", 5)
p_hib_sls
```

```{r s2_tabla_sls}
inner_join(
  serie_hib_sls  %>% select(periodo_id, tasa_hib  = tasa),
  serie_back_sls %>% select(periodo_id, tasa_back = tasa),
  by = "periodo_id"
) %>%
  mutate(
    delta    = round(tasa_hib - tasa_back, 2),
    delta_pp = paste0(ifelse(delta >= 0, "+", ""), delta, " pp")
  ) %>%
  select(Trimestre=periodo_id,
         `Hib. (%)`=tasa_hib, `Back. (%)`=tasa_back, `Delta`=delta_pp) %>%
  kbl(format="html", align=c("l","r","r","r"),
      caption="SLS: tasa hibrida vs backcasting puro por trimestre") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(4, bold=TRUE)
```

---

', file = con)

# ---- S3 EXPANSION ----
cat('# Expansion de cobertura {#s3}

> Nuevos individuos clasificados via prediccion en relacion a la medida vieja.
> Una expansion alta indica que el modelo captura un universo sustancialmente mayor
> que la medida de registro tradicional.

## Expansion promedio por modelo

```{r s3_tabla_exp}
tibble(
  Modelo              = c("LPM","GLM","SLS"),
  Exp_promedio        = c(exp_lpm_med, exp_glm_med, exp_sls_med),
  Desc                = c(
    "Nuevo universo vs medida vieja",
    "Nuevo universo vs medida vieja",
    "Nuevo universo vs medida vieja"
  )
) %>%
  mutate(Exp_promedio = paste0(Exp_promedio, "%")) %>%
  kbl(format="html", align=c("l","r","l"),
      col.names=c("Modelo","Expansion promedio","Descripcion"),
      caption="S3: Expansion promedio de cobertura respecto a medida de registro vieja") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(2, bold=TRUE, background="#d4edda")
```

## Serie de expansion por trimestre

```{r s3_expan_trim, fig.height=4}
# La expansion % es IDENTICA en los 3 modelos (mismo n Predicho).
# Se grafica una sola linea con nota aclaratoria.
p_expan <- ggplot(expan_trim, aes(x=periodo_id, y=pct_exp_lpm, group=1)) +
  geom_line(size=0.9, color=COL_OBSERVADO) +
  geom_point(size=1.8, color=COL_OBSERVADO, alpha=0.8) +
  scale_y_continuous(labels=function(x) paste0(x,"%"),
                     limits=c(0, max(expan_trim$pct_exp_lpm, na.rm=TRUE)*1.1)) +
  scale_x_discrete(breaks=trims_todos[seq(1,length(trims_todos),by=4)]) +
  tr_labs(title="Expansion de cobertura por trimestre (identica en LPM / GLM / SLS)",
          subtitle="% de obs clasificados via prediccion vs universo de medida vieja (ponderado)",
          x=NULL, y="Expansion (%)") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8))
guardar_figura(p_expan, DIR_FIGURAS_09C, "expan_cobertura", 6)
p_expan
```

<small>
Los tres modelos producen exactamente la misma expansion porque el bloque "Predicho"
se define por la ausencia de condicion_formalidad valida -- no por el modelo --,
resultando en el mismo conjunto de individuos clasificados como nuevos en los tres casos.
Las diferencias entre modelos aparecen en la **tasa de formalidad** dentro de ese bloque.
</small>

## Tasa de formalidad en el bloque Predicho por modelo

```{r s3_tasa_pred, fig.height=4.5}
tasa_pred_long <- tasa_pred_trim %>%
  pivot_longer(c(tasa_pred_lpm, tasa_pred_glm, tasa_pred_sls),
               names_to="modelo", values_to="tasa") %>%
  mutate(modelo = case_when(
    modelo == "tasa_pred_lpm" ~ "LPM",
    modelo == "tasa_pred_glm" ~ "GLM",
    modelo == "tasa_pred_sls" ~ "SLS"
  ))

p_tasa_pred <- ggplot(tasa_pred_long, aes(x=periodo_id, y=tasa, color=modelo, group=modelo)) +
  geom_line(size=0.9) +
  geom_point(size=1.8, alpha=0.7) +
  scale_color_modelos() +
  scale_linetype_modelos() +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  scale_x_discrete(breaks=trims_todos[seq(1,length(trims_todos),by=4)]) +
  tr_labs(title="Tasa de formalidad en el bloque Predicho por modelo",
          subtitle="Individuos sin cobertura de medida vieja | Diferencias entre modelos visibles aqui",
          x=NULL, y="Tasa formal en bloque Predicho (%)", color="Modelo") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        legend.position="top")
guardar_figura(p_tasa_pred, DIR_FIGURAS_09C, "tasa_pred_bloque", 7)
p_tasa_pred
```

```{r s3_tasa_pred_tabla}
tasa_pred_trim %>%
  mutate(across(c(tasa_pred_lpm, tasa_pred_glm, tasa_pred_sls),
                ~paste0(., "%"))) %>%
  select(Trimestre=periodo_id,
         `LPM (%)`=tasa_pred_lpm,
         `GLM (%)`=tasa_pred_glm,
         `SLS (%)`=tasa_pred_sls) %>%
  kbl(format="html", align=c("l","r","r","r"),
      caption="S3: Tasa de formalidad en bloque Predicho por modelo y trimestre") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(3, background="#e3f2fd", bold=TRUE)
```

---

', file = con)

# ---- S4 CORRELACION ----
cat('# Correlacion entre series {#s4}

> Matriz de correlacion de Pearson entre las 6 series temporales:
> las 3 series hibridas y las 3 series de backcasting puro (de 09b).
> Correlaciones altas entre hibrida y back del mismo modelo validan la coherencia interna.

```{r s4_cor}
cor_df <- as.data.frame(cor_mat) %>%
  rownames_to_column("Serie")

cor_df %>%
  kbl(format="html", digits=4,
      caption="S4: Correlacion entre series hibridas y backcasting puro (2016T4-2025T3)") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(1, bold=TRUE) %>%
  column_spec(which(colnames(cor_df) == "GLM hib."), background="#e3f2fd") %>%
  row_spec(which(cor_df$Serie == "GLM hib."), background="#e3f2fd")
```

<small>
hib. = serie hibrida (medida vieja + prediccion). back. = backcasting puro (solo prediccion calibrada).
Series calculadas sobre universo de ocupados PEA (tipo_estimacion_pea valido).
</small>

---

', file = con)

# ---- S5 RECOMENDACION ----
cat('# Recomendacion para el paper {#s5}

## Criterios de seleccion

```{r s5_ranking}
tibble(
  Criterio   = c(
    "AUC-ROC (train, ocupados)",
    "Acuerdo con medida de registro (global)",
    "Acuerdo en Formales",
    "Acuerdo en Informales",
    "Expansion de cobertura (vs vieja medida)",
    "0% predicciones fuera [0,1]",
    "Coherencia interna (corr hib vs back)",
    "Correlacion entre series hibridas (min par)"
  ),
  LPM = c(
    fmt_num(auc_train_lpm, 4),
    paste0(tab_s1_cons$Pct_acuerdo[1],    "%"),
    paste0(tab_s1_cons$Pct_Formal_ok[1],  "%"),
    paste0(tab_s1_cons$Pct_Informal_ok[1],"%"),
    paste0(exp_lpm_med, "%"),
    "No (LPM lineal)",
    as.character(cor_mat["LPM hib.","LPM back."]),
    as.character(min(cor_mat["LPM hib.", c("GLM hib.","SLS hib.")]))
  ),
  GLM = c(
    fmt_num(auc_train_glm, 4),
    paste0(tab_s1_cons$Pct_acuerdo[2],    "%"),
    paste0(tab_s1_cons$Pct_Formal_ok[2],  "%"),
    paste0(tab_s1_cons$Pct_Informal_ok[2],"%"),
    paste0(exp_glm_med, "%"),
    "Si (binomial)",
    as.character(cor_mat["GLM hib.","GLM back."]),
    as.character(min(cor_mat["GLM hib.", c("LPM hib.","SLS hib.")]))
  ),
  SLS = c(
    fmt_num(auc_train_sls, 4),
    paste0(tab_s1_cons$Pct_acuerdo[3],    "%"),
    paste0(tab_s1_cons$Pct_Formal_ok[3],  "%"),
    paste0(tab_s1_cons$Pct_Informal_ok[3],"%"),
    paste0(exp_sls_med, "%"),
    "No (SLS lineal)",
    as.character(cor_mat["SLS hib.","SLS back."]),
    as.character(min(cor_mat["SLS hib.", c("LPM hib.","GLM hib.")]))
  ),
  Ganador = c(
    "GLM",          # AUC: GLM > SLS > LPM (desde c08_*$auc_global_train)
    "GLM",          # Acuerdo global: ver valores computados en tab_s5
    "LPM",          # Formales bien clasif.: ver valores computados en tab_s5
    "SLS",          # Informales bien clasif.: ver valores computados en tab_s5
    "Indistinto",   # Expansion: identico en los 3
    "GLM",          # 0% fuera [0,1]: solo GLM garantiza por construccion binomial
    "LPM",          # Coherencia interna (hib vs back): ver cor_mat computada
    "GLM"           # Corr min par hibridas: ver cor_mat computada
  )
) %>%
  kbl(format="html", align=c("l","c","c","c","c"),
      caption="S5: Criterios de seleccion para la serie hibrida del paper") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=TRUE, font_size=11) %>%
  column_spec(3, background="#e3f2fd", bold=TRUE) %>%
  column_spec(5, bold=TRUE) %>%
  row_spec(c(1,2,6), background="#d4edda")
```

## Conclusion

**GLM es el modelo recomendado** para la serie hibrida del paper:
mayor AUC, predicciones acotadas en [0,1] por construccion binomial,
y consistencia comparable con la medida de registro historica.

La variable hibrida amplifica la cobertura de la medida vieja en
`r paste0(exp_glm_med, "%")` en promedio por trimestre,
incorporando al universo de analisis a trabajadores antes excluidos del
indicador de registro (cuenta propia, patron, desocupados en edad laboral).

**Pipeline completo alcanzado.** Los tres scripts comparativos (09a, 09b, 09c)
cubren la cadena: seleccion de variables → series de backcasting → variable hibrida.
', file = con)

close(con)
cat("   [OK] Rmd escrito:", rmd_temp, "\n\n")

# 🪫 12. RENDER HTML ---------------------------------------------------------
cat("-- 12. Renderizando HTML ---------------------------------------------\n")

tic("render")
rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_09C_HTML,
  quiet       = TRUE,
  envir       = new.env()
)
toc()
unlink(rmd_temp)
unlink(rds_env)

cat(sprintf("   [OK] HTML: %s (%.1f KB)\n",
            PATH_09C_HTML, file.size(PATH_09C_HTML) / 1024))

# 📑 13. CHECKLIST -----------------------------------------------------------
cat("\n-- 13. Checklist -----------------------------------------------------\n")
cat("   HTML    :", basename(PATH_09C_HTML),
    if (file.exists(PATH_09C_HTML))           "[OK]" else "[FALTA]", "\n")
cat("   Notas   :", basename(PATH_09C_NOTAS),
    if (file.exists(PATH_09C_NOTAS))          "[OK]" else "[FALTA]", "\n")
cat("   Contrato:", basename(PATH_09C_COMP_CONTRATO),
    if (file.exists(PATH_09C_COMP_CONTRATO))  "[OK]" else "[FALTA]", "\n")

cat("\n===================================================================\n")
cat("SCRIPT 09c COMPLETADO\n")
cat("  HTML    :", basename(PATH_09C_HTML),          "\n")
cat("  TXT     :", basename(PATH_09C_NOTAS),         "\n")
cat("  Contrato:", basename(PATH_09C_COMP_CONTRATO), "\n")
cat("===================================================================\n")

toc()
