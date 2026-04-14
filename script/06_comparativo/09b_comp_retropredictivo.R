# =============================================================================
# [EN] 09b_comp_retropredictivo.R -- Backcast series comparison: pure predicted formality rates (LPM vs GLM vs SLS)
# INPUTS:  rdos/datos/08_panel_formalidad_SLS*.rds, contracts 08 x3
# OUTPUTS: rdos/reportes/09b_comp_retropredictivo.html, rdos/figuras/09b_comp_retropredictivo/*.pdf
# =============================================================================
# 🌟 09b_comp_retropredictivo.R 🌟 ####
# Reporte comparativo de backcasting puro (sin imputacion) -- LPM vs GLM vs SLS (sufijo dinámico)
# Proyecto: formalidad_back  |  Capa 6 -- Reportes Comparativos
#
# INPUT PRINCIPAL:
#   rdos/datos/08_panel_formalidad_{SLS_SUFIJO}.rds  (110 cols -- contiene LPM y GLM heredadas)
# INPUTS ESCALARES:
#   rdos/contratos/08_contrato_backcasting_{SUFIJO}.rds  x3
# INPUT AUXILIAR:
#   rdos/datos/02_panel_con_taxonomia.rds  (condicion_formalidad para S2)
#
# OUTPUT:
#   rdos/reportes/09b_comp_retropredictivo.html
#   rdos/reportes/09b_comp_retropredictivo_notas.txt
#   rdos/figuras/09b_comp_retropredictivo/*.pdf   (via guardar_figura)
#
# ESTETICA: theme_paper() + scale_color_modelos() + guardar_figura()
#   coherente con 07e_/08b_/08c_/09a_  (funciones_comunes.R -> theme_paper.R)
#   COL_LPM / COL_GLM / COL_SLS / COL_OBSERVADO -- no valores hexadecimales hardcodeados
#
# ESTRATEGIA:
#   - Panel leido UNA SOLA VEZ; gc() agresivo tras cada calculo de panel
#   - Todo el calculo en R base (seccion 2-10) antes del Rmd
#   - Objetos pasados al Rmd via save()/load() [L56]
#   - Densidades S4: histogramas precalculados (no datos crudos)
#   - TRIM_OBS / TRIM_PANDEMIA desde parametros.R (TRIMESTRES_FORMALIDAD / PANDEMIA)
#   - Secciones:
#       S0  Benchmark (contratos)
#       S1  Validacion directa 3 trims
#       S2  Coherencia historica asalariados
#       S3  Series completas (3 universos x 3 modelos)
#       S4  Pred fuera [0,1] -- tabla + densidades precalculadas
#       S5  Formalidad potencial (desocupados e inactivos)
#       S6  Matriz de correlacion 6x6 entre series
#       S7  Recomendacion para el paper

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
COL_CLASE_LPM      <- paste0("formalidad_clase_", SUFIJO_MODELO_LPM, "_pea")
COL_CLASE_GLM      <- paste0("formalidad_clase_", SUFIJO_MODELO_GLM, "_pea")
COL_CLASE_SLS      <- paste0("formalidad_clase_", SUFIJO_MODELO_SLS, "_pea")
COL_PROB_LPM       <- paste0("prob_formal_", SUFIJO_MODELO_LPM, "_pea")
COL_PROB_GLM       <- paste0("prob_formal_", SUFIJO_MODELO_GLM, "_pea")
COL_PROB_SLS       <- paste0("prob_formal_", SUFIJO_MODELO_SLS, "_pea")
COL_FLAG_LPM       <- paste0("flag_pred_", SUFIJO_MODELO_LPM, "_pea")
COL_FLAG_GLM       <- paste0("flag_pred_", SUFIJO_MODELO_GLM, "_pea")
COL_FLAG_SLS       <- paste0("flag_pred_", SUFIJO_MODELO_SLS, "_pea")

# ⌛ Inicio contador de tiempo -------------------------------------------------

tic("09b - Comparativo Retropredictivo")
cat("===================================================================\n")
cat("SCRIPT 09b - COMPARATIVO BACKCASTING LPM / GLM / SLS\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")

# 🪫 1. CARGA ----------------------------------------------------------------
cat("-- 1. Carga -----------------------------------------------------------\n")

PATH_PANEL    <- PATH_08_PANEL_SLS          # panel SLS consolidado (110 cols)
PATH_02_TAX   <- PATH_02_PANEL_TAX          # panel con condicion_formalidad
PATH_C08_LPM  <- PATH_08_CONTRATO_LPM
PATH_C08_GLM  <- PATH_08_CONTRATO_GLM
PATH_C08_SLS  <- PATH_08_CONTRATO_SLS
# Contratos 07b: contienen metricas_clf con Sens/Esp Youden (DEUDA-2)
PATH_C07B_LPM <- PATH_07B_CONTRATO_LPM
PATH_C07B_GLM <- PATH_07B_CONTRATO_GLM
PATH_C07B_SLS <- PATH_07B_CONTRATO_SLS

hard_stop(file.exists(PATH_PANEL),    paste0("No existe 08_panel_formalidad_", SUFIJO_MODELO_SLS, ".rds"))
hard_stop(file.exists(PATH_C08_LPM),  paste0("No existe 08_contrato_backcasting_", SUFIJO_MODELO_LPM, ".rds"))
hard_stop(file.exists(PATH_C08_GLM),  paste0("No existe 08_contrato_backcasting_", SUFIJO_MODELO_GLM, ".rds"))
hard_stop(file.exists(PATH_C08_SLS),  paste0("No existe 08_contrato_backcasting_", SUFIJO_MODELO_SLS, ".rds"))
hard_stop(file.exists(PATH_C07B_LPM), "No existe 07b_contrato_postlasso LPM")
hard_stop(file.exists(PATH_C07B_GLM), "No existe 07b_contrato_postlasso GLM")
hard_stop(file.exists(PATH_C07B_SLS), "No existe 07b_contrato_postlasso SLS")
dir.create(DIR_REPORTES, showWarnings = FALSE, recursive = TRUE)

c08_lpm  <- readRDS(PATH_C08_LPM)
c08_glm  <- readRDS(PATH_C08_GLM)
c08_sls  <- readRDS(PATH_C08_SLS)
c07b_lpm <- readRDS(PATH_C07B_LPM)
c07b_glm <- readRDS(PATH_C07B_GLM)
c07b_sls <- readRDS(PATH_C07B_SLS)
cat("   [OK] Contratos 08: LPM (", length(names(c08_lpm)),
    ") | GLM (", length(names(c08_glm)),
    ") | SLS (", length(names(c08_sls)), ") campos\n")
cat("   [OK] Contratos 07b: LPM | GLM | SLS (Sens/Esp Youden disponibles)\n")

# Columnas necesarias del panel
cols_09b <- c(
  "id_individuo", "periodo_id", "pondera",
  "tipo_estimacion_pea", "tipo_estimacion_edad",
  "categoria_ocupacional",
  COL_CLASE_CAL_LPM, COL_CLASE_CAL_GLM, COL_CLASE_CAL_SLS,
  COL_CLASE_LPM,     COL_CLASE_GLM,     COL_CLASE_SLS,
  COL_PROB_LPM,      COL_PROB_GLM,      COL_PROB_SLS,
  COL_FLAG_LPM,      COL_FLAG_GLM,      COL_FLAG_SLS
)

cat("   Cargando panel SLS...\n")
panel_full <- readRDS(PATH_PANEL)
panel      <- panel_full[, intersect(cols_09b, names(panel_full))]
rm(panel_full); gc()
cat(sprintf("   [OK] Panel: %s obs x %d cols retenidas\n",
            format(nrow(panel), big.mark = ","), ncol(panel)))

# condicion_formalidad para S2
cat("   Cargando condicion_formalidad...\n")
panel_tax <- readRDS(PATH_02_TAX) %>%
  select(id_individuo, periodo_id, condicion_formalidad) %>%
  filter(!is.na(condicion_formalidad))
panel <- panel %>% left_join(panel_tax, by = c("id_individuo", "periodo_id"))
rm(panel_tax); gc()
cat(sprintf("   [OK] condicion_formalidad: %s obs con valor\n",
            format(sum(!is.na(panel$condicion_formalidad)), big.mark = ",")))

# 🪫 2. CONSTANTES Y HELPERS -------------------------------------------------
cat("\n-- 2. Constantes y helpers -------------------------------------------\n")

TRIM_OBS      <- TRIMESTRES_FORMALIDAD   # desde parametros.R
TRIM_PANDEMIA <- TRIMESTRES_PANDEMIA     # desde parametros.R

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
metricas_serie <- function(back, viej) {
  d <- back - viej
  list(corr = round(cor(back, viej, use = "complete.obs"), 4),
       mae  = round(mean(abs(d), na.rm = TRUE), 2),
       rmse = round(sqrt(mean(d^2, na.rm = TRUE)), 2),
       max  = round(max(abs(d), na.rm = TRUE), 2))
}

cat("   [OK] Helpers definidos\n")

# 🪫 3. S0 -- TABLA BENCHMARK (desde contratos) ------------------------------
cat("\n-- 3. S0 Benchmark ----------------------------------------------------\n")

tab_s0 <- tibble(
  Metrica = c(
    "Umbral Youden",
    "Umbral calibracion",
    "AUC-ROC (train global)",
    "F1 (umbral calibracion)",
    "MCC (umbral calibracion)",
    "Accuracy (umbral calibracion)",
    "Sensibilidad (umbral calibracion)",
    "Especificidad (umbral calibracion)",
    "Delta max tasa -- umbral Youden (pp)",
    "Delta max tasa -- umbral Cal. (pp)",
    "% pred. fuera [0,1] -- union PEA"
  ),
  LPM = c(
    fmt_num(c08_lpm$umbral_youden, 4),
    fmt_num(c08_lpm$umbral_calibracion, 4),
    fmt_num(c08_lpm$auc_global_train, 4),
    fmt_num(safe_s(function() c08_lpm$metricas_calibracion$f1), 4),
    fmt_num(safe_s(function() c08_lpm$metricas_calibracion$mcc), 4),
    fmt_num(safe_s(function() c08_lpm$metricas_calibracion$accuracy), 4),
    fmt_num(safe_s(function() c08_lpm$metricas_calibracion$sensibilidad), 4),
    fmt_num(safe_s(function() c08_lpm$metricas_calibracion$especificidad), 4),
    sprintf("%.2f pp", safe_s(function() c08_lpm$delta_max_pp)),
    sprintf("%.2f pp", safe_s(function() c08_lpm$metricas_calibracion$delta_max_pp)),
    fmt_pct(safe_s(function() c08_lpm$pct_pred_fuera_01_union), 2)
  ),
  GLM = c(
    fmt_num(c08_glm$umbral_youden, 4),
    fmt_num(c08_glm$umbral_calibracion, 4),
    fmt_num(c08_glm$auc_global_train, 4),
    fmt_num(safe_s(function() c08_glm$metricas_calibracion$f1), 4),
    fmt_num(safe_s(function() c08_glm$metricas_calibracion$mcc), 4),
    fmt_num(safe_s(function() c08_glm$metricas_calibracion$accuracy), 4),
    fmt_num(safe_s(function() c08_glm$metricas_calibracion$sensibilidad), 4),
    fmt_num(safe_s(function() c08_glm$metricas_calibracion$especificidad), 4),
    sprintf("%.2f pp", safe_s(function() c08_glm$delta_max_pp)),
    sprintf("%.2f pp", safe_s(function() c08_glm$metricas_calibracion$delta_max_pp)),
    "0.00% (binomial)"
  ),
  SLS = c(
    fmt_num(c08_sls$umbral_youden, 4),
    fmt_num(c08_sls$umbral_calibracion, 4),
    fmt_num(c08_sls$auc_global_train, 4),
    fmt_num(safe_s(function() c08_sls$metricas_calibracion$f1), 4),
    fmt_num(safe_s(function() c08_sls$metricas_calibracion$mcc), 4),
    fmt_num(safe_s(function() c08_sls$metricas_calibracion$accuracy), 4),
    fmt_num(safe_s(function() c08_sls$metricas_calibracion$sensibilidad), 4),
    fmt_num(safe_s(function() c08_sls$metricas_calibracion$especificidad), 4),
    sprintf("%.2f pp", safe_s(function() c08_sls$delta_max_pp)),
    sprintf("%.2f pp", safe_s(function() c08_sls$metricas_calibracion$delta_max_pp)),
    fmt_pct(safe_s(function() c08_sls$pct_pred_fuera_01_union), 2)
  )
)

# Indices para colorear
s0_auc  <- which(grepl("AUC", tab_s0$Metrica))
s0_clf  <- which(grepl("F1|MCC|Accuracy", tab_s0$Metrica))
s0_del  <- which(grepl("Delta", tab_s0$Metrica))
s0_fue  <- which(grepl("fuera", tab_s0$Metrica))

cat(sprintf("   [OK] S0: %d metricas\n", nrow(tab_s0)))

# 🪫 4. S1 -- VALIDACION DIRECTA ---------------------------------------------
cat("\n-- 4. S1 Validacion directa ------------------------------------------\n")

# tasa_obs de referencia: desde contrato LPM (observable EPH, igual en los tres)
tasa_obs_ref <- tryCatch(
  c08_lpm$comp_por_trimestre %>%
    rename(periodo_id = periodo) %>%
    filter(periodo_id %in% TRIM_OBS) %>%
    select(periodo_id, n_obs = n, tasa_obs),
  error = function(e) NULL
)
if (is.null(tasa_obs_ref)) stop("comp_por_trimestre LPM no disponible")

# Tasa calibrada por modelo sobre ocupados en trims de validacion
comp_s1 <- panel %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting"),
         periodo_id %in% TRIM_OBS) %>%
  group_by(periodo_id) %>%
  summarise(
    n_ocup       = n(),
    cal_lpm = weighted.mean(.data[[COL_CLASE_CAL_LPM]] == "Formal", pondera, na.rm = TRUE),
    cal_glm = weighted.mean(.data[[COL_CLASE_CAL_GLM]] == "Formal", pondera, na.rm = TRUE),
    cal_sls = weighted.mean(.data[[COL_CLASE_CAL_SLS]] == "Formal", pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(tasa_obs_ref, by = "periodo_id") %>%
  mutate(
    delta_lpm = round((cal_lpm - tasa_obs) * 100, 2),
    delta_glm = round((cal_glm - tasa_obs) * 100, 2),
    delta_sls = round((cal_sls - tasa_obs) * 100, 2)
  )

cat(sprintf("   S1: delta ocup. prom -- LPM=%.2f | GLM=%.2f | SLS=%.2f pp\n",
            mean(abs(comp_s1$delta_lpm)),
            mean(abs(comp_s1$delta_glm)),
            mean(abs(comp_s1$delta_sls))))

# Tabla de metricas de clasificacion S1d -- desde contratos
tab_s1d <- tibble(
  Metrica     = c("Accuracy", "Sensibilidad", "Especificidad", "F1", "MCC",
                  "Delta max tasa (pp)"),
  `Youden LPM`  = c(fmt_pct(safe_s(function() c08_lpm$accuracy_train)   * 100, 2),
                     fmt_pct(safe_s(function() c07b_lpm$metricas_clf$sensibilidad)  * 100, 2),
                     fmt_pct(safe_s(function() c07b_lpm$metricas_clf$especificidad) * 100, 2),
                     fmt_num(safe_s(function() c08_lpm$f1_train),  4),
                     fmt_num(safe_s(function() c08_lpm$mcc_train), 4),
                     sprintf("%.2f pp", safe_s(function() c08_lpm$delta_max_pp))),
  `Cal. LPM`  = c(fmt_pct(safe_s(function() c08_lpm$metricas_calibracion$accuracy)    * 100, 2),
                   fmt_pct(safe_s(function() c08_lpm$metricas_calibracion$sensibilidad)* 100, 2),
                   fmt_pct(safe_s(function() c08_lpm$metricas_calibracion$especificidad)*100, 2),
                   fmt_num(safe_s(function() c08_lpm$metricas_calibracion$f1),  4),
                   fmt_num(safe_s(function() c08_lpm$metricas_calibracion$mcc), 4),
                   sprintf("%.2f pp", safe_s(function() c08_lpm$metricas_calibracion$delta_max_pp))),
  `Youden GLM`  = c(fmt_pct(safe_s(function() c08_glm$accuracy_train)   * 100, 2),
                     fmt_pct(safe_s(function() c07b_glm$metricas_clf$sensibilidad)  * 100, 2),
                     fmt_pct(safe_s(function() c07b_glm$metricas_clf$especificidad) * 100, 2),
                     fmt_num(safe_s(function() c08_glm$f1_train),  4),
                     fmt_num(safe_s(function() c08_glm$mcc_train), 4),
                     sprintf("%.2f pp", safe_s(function() c08_glm$delta_max_pp))),
  `Cal. GLM`  = c(fmt_pct(safe_s(function() c08_glm$metricas_calibracion$accuracy)    * 100, 2),
                   fmt_pct(safe_s(function() c08_glm$metricas_calibracion$sensibilidad)* 100, 2),
                   fmt_pct(safe_s(function() c08_glm$metricas_calibracion$especificidad)*100, 2),
                   fmt_num(safe_s(function() c08_glm$metricas_calibracion$f1),  4),
                   fmt_num(safe_s(function() c08_glm$metricas_calibracion$mcc), 4),
                   sprintf("%.2f pp", safe_s(function() c08_glm$metricas_calibracion$delta_max_pp))),
  `Youden SLS`  = c(fmt_pct(safe_s(function() c08_sls$accuracy_train)   * 100, 2),
                     fmt_pct(safe_s(function() c07b_sls$metricas_clf$sensibilidad)  * 100, 2),
                     fmt_pct(safe_s(function() c07b_sls$metricas_clf$especificidad) * 100, 2),
                     fmt_num(safe_s(function() c08_sls$f1_train),  4),
                     fmt_num(safe_s(function() c08_sls$mcc_train), 4),
                     sprintf("%.2f pp", safe_s(function() c08_sls$delta_max_pp))),
  `Cal. SLS`  = c(fmt_pct(safe_s(function() c08_sls$metricas_calibracion$accuracy)    * 100, 2),
                   fmt_pct(safe_s(function() c08_sls$metricas_calibracion$sensibilidad)* 100, 2),
                   fmt_pct(safe_s(function() c08_sls$metricas_calibracion$especificidad)*100, 2),
                   fmt_num(safe_s(function() c08_sls$metricas_calibracion$f1),  4),
                   fmt_num(safe_s(function() c08_sls$metricas_calibracion$mcc), 4),
                   sprintf("%.2f pp", safe_s(function() c08_sls$metricas_calibracion$delta_max_pp)))
)
cat("   [OK] S1\n")

# 🪫 5. S2 -- COHERENCIA HISTORICA ASALARIADOS --------------------------------
cat("\n-- 5. S2 Coherencia historica ----------------------------------------\n")

serie_asal_vieja <- panel %>%
  filter(condicion_formalidad %in% c("Formal", "No formal")) %>%
  group_by(periodo_id) %>%
  summarise(
    n_vieja          = n(),
    tasa_formal_viej = weighted.mean(condicion_formalidad == "Formal", pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(periodo_id)

serie_asal_back <- panel %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting"),
         categoria_ocupacional == "Empleado") %>%
  group_by(periodo_id) %>%
  summarise(
    n_back   = n(),
    tasa_lpm = weighted.mean(.data[[COL_CLASE_CAL_LPM]] == "Formal", pondera, na.rm = TRUE),
    tasa_glm = weighted.mean(.data[[COL_CLASE_CAL_GLM]] == "Formal", pondera, na.rm = TRUE),
    tasa_sls = weighted.mean(.data[[COL_CLASE_CAL_SLS]] == "Formal", pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(periodo_id)

serie_s2 <- serie_asal_vieja %>%
  inner_join(serie_asal_back, by = "periodo_id") %>%
  mutate(
    delta_lpm   = round((tasa_lpm - tasa_formal_viej) * 100, 2),
    delta_glm   = round((tasa_glm - tasa_formal_viej) * 100, 2),
    delta_sls   = round((tasa_sls - tasa_formal_viej) * 100, 2),
    es_pandemia = periodo_id %in% TRIM_PANDEMIA,
    es_obs      = periodo_id %in% TRIM_OBS
  )

serie_s2_sp <- serie_s2 %>% filter(!es_pandemia)

n_trim_s2       <- nrow(serie_s2)
n_trim_pandemia <- sum(serie_s2$es_pandemia)

met_lpm_t  <- metricas_serie(serie_s2$tasa_lpm,    serie_s2$tasa_formal_viej)
met_glm_t  <- metricas_serie(serie_s2$tasa_glm,    serie_s2$tasa_formal_viej)
met_sls_t  <- metricas_serie(serie_s2$tasa_sls,    serie_s2$tasa_formal_viej)
met_lpm_sp <- metricas_serie(serie_s2_sp$tasa_lpm, serie_s2_sp$tasa_formal_viej)
met_glm_sp <- metricas_serie(serie_s2_sp$tasa_glm, serie_s2_sp$tasa_formal_viej)
met_sls_sp <- metricas_serie(serie_s2_sp$tasa_sls, serie_s2_sp$tasa_formal_viej)

tab_s2_met <- tibble(
  Serie       = rep(c("Total","Sin pandemia"), each = 3),
  Modelo      = rep(c("LPM","GLM","SLS"), 2),
  N           = c(rep(n_trim_s2, 3), rep(nrow(serie_s2_sp), 3)),
  Correlacion = c(met_lpm_t$corr, met_glm_t$corr, met_sls_t$corr,
                  met_lpm_sp$corr, met_glm_sp$corr, met_sls_sp$corr),
  MAE_pp      = c(met_lpm_t$mae, met_glm_t$mae, met_sls_t$mae,
                  met_lpm_sp$mae, met_glm_sp$mae, met_sls_sp$mae),
  RMSE_pp     = c(met_lpm_t$rmse, met_glm_t$rmse, met_sls_t$rmse,
                  met_lpm_sp$rmse, met_glm_sp$rmse, met_sls_sp$rmse),
  Delta_max_pp= c(met_lpm_t$max, met_glm_t$max, met_sls_t$max,
                  met_lpm_sp$max, met_glm_sp$max, met_sls_sp$max)
)

cat(sprintf("   S2: trims=%d | pandemia=%d | corr total LPM=%.4f GLM=%.4f SLS=%.4f\n",
            n_trim_s2, n_trim_pandemia,
            met_lpm_t$corr, met_glm_t$corr, met_sls_t$corr))

# 🪫 6. S3 -- SERIES COMPLETAS (3 universos x 3 modelos) ---------------------
cat("\n-- 6. S3 Series completas --------------------------------------------\n")

# Ocupados (tipo_estimacion_pea: Observado + Backcasting)
series_ocup <- panel %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting")) %>%
  group_by(periodo_id) %>%
  summarise(
    n       = n(),
    cal_lpm = weighted.mean(.data[[COL_CLASE_CAL_LPM]] == "Formal", pondera, na.rm = TRUE),
    cal_glm = weighted.mean(.data[[COL_CLASE_CAL_GLM]] == "Formal", pondera, na.rm = TRUE),
    cal_sls = weighted.mean(.data[[COL_CLASE_CAL_SLS]] == "Formal", pondera, na.rm = TRUE),
    you_lpm = weighted.mean(.data[[COL_CLASE_LPM]]     == "Formal", pondera, na.rm = TRUE),
    you_glm = weighted.mean(.data[[COL_CLASE_GLM]]     == "Formal", pondera, na.rm = TRUE),
    you_sls = weighted.mean(.data[[COL_CLASE_SLS]]     == "Formal", pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(periodo_id)

# PEA (agrega desocupados potenciales)
series_pea <- panel %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting", "Potencial_desocupado")) %>%
  group_by(periodo_id) %>%
  summarise(
    n       = n(),
    cal_lpm = weighted.mean(.data[[COL_CLASE_CAL_LPM]] == "Formal", pondera, na.rm = TRUE),
    cal_glm = weighted.mean(.data[[COL_CLASE_CAL_GLM]] == "Formal", pondera, na.rm = TRUE),
    cal_sls = weighted.mean(.data[[COL_CLASE_CAL_SLS]] == "Formal", pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(periodo_id)

# Edad 18-60 (agrega inactivos potenciales)
series_edad <- panel %>%
  filter(tipo_estimacion_edad %in% c("Observado", "Backcasting",
                                      "Potencial_desocupado", "Potencial_inactivo")) %>%
  group_by(periodo_id) %>%
  summarise(
    n       = n(),
    cal_lpm = weighted.mean(.data[[COL_CLASE_CAL_LPM]] == "Formal", pondera, na.rm = TRUE),
    cal_glm = weighted.mean(.data[[COL_CLASE_CAL_GLM]] == "Formal", pondera, na.rm = TRUE),
    cal_sls = weighted.mean(.data[[COL_CLASE_CAL_SLS]] == "Formal", pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(periodo_id)

# Long format para graficos
series_ocup_long <- series_ocup %>%
  pivot_longer(cols = c(cal_lpm, cal_glm, cal_sls),
               names_to = "modelo", values_to = "tasa") %>%
  mutate(modelo = case_when(                               # [L76] recode() deprecado
    modelo == "cal_lpm" ~ "LPM",
    modelo == "cal_glm" ~ "GLM",
    modelo == "cal_sls" ~ "SLS",
    TRUE ~ modelo))

series_pea_long <- series_pea %>%
  pivot_longer(cols = c(cal_lpm, cal_glm, cal_sls),
               names_to = "modelo", values_to = "tasa") %>%
  mutate(modelo = case_when(                               # [L76] recode() deprecado
    modelo == "cal_lpm" ~ "LPM",
    modelo == "cal_glm" ~ "GLM",
    modelo == "cal_sls" ~ "SLS",
    TRUE ~ modelo))

series_edad_long <- series_edad %>%
  pivot_longer(cols = c(cal_lpm, cal_glm, cal_sls),
               names_to = "modelo", values_to = "tasa") %>%
  mutate(modelo = case_when(                               # [L76] recode() deprecado
    modelo == "cal_lpm" ~ "LPM",
    modelo == "cal_glm" ~ "GLM",
    modelo == "cal_sls" ~ "SLS",
    TRUE ~ modelo))

# Tabla resumen S3 (ultimos 3 trims observados + primer y ultimo backcasting)
trims_todos <- sort(unique(series_ocup$periodo_id))
trim_idx_obs_start <- which(trims_todos == TRIM_OBS[1])
trim_back_1 <- trims_todos[1]
trim_back_n <- trims_todos[trim_idx_obs_start - 1]

tab_s3_resumen <- bind_rows(
  series_ocup  %>% mutate(universo = "Ocupados"),
  series_pea   %>% mutate(universo = "PEA"),
  series_edad  %>% mutate(universo = "Edad 18-60")
) %>%
  filter(periodo_id %in% c(trim_back_1, trim_back_n, TRIM_OBS)) %>%
  arrange(universo, periodo_id) %>%
  mutate(
    LPM     = paste0(round(cal_lpm * 100, 1), "%"),
    GLM     = paste0(round(cal_glm * 100, 1), "%"),
    SLS     = paste0(round(cal_sls * 100, 1), "%"),
    es_obs  = periodo_id %in% TRIM_OBS
  ) %>%
  select(Universo = universo, Trimestre = periodo_id, LPM, GLM, SLS, es_obs)

cat(sprintf("   S3: %d trims ocupados | %d PEA | %d Edad\n",
            nrow(series_ocup), nrow(series_pea), nrow(series_edad)))

# 🪫 7. S4 -- PRED FUERA [0,1]: TABLA + HISTOGRAMAS PRECALCULADOS ------------
cat("\n-- 7. S4 Pred fuera [0,1] -------------------------------------------\n")

tab_s4 <- tibble(
  Contexto   = c("Entrenamiento (07b)",
                 "Prediccion OOS -- test (07b)",
                 "Backcasting -- union PEA (08)"),
  LPM        = c("N/D (no en 07b)",                                    # train no guardado en c07b_lpm
                 fmt_pct(safe_s(function() c07b_lpm$pct_fuera_01), 2),  # test
                 fmt_pct(safe_s(function() c08_lpm$pct_pred_fuera_01_union), 2)),
  GLM        = c("0.00% (binomial)", "0.00% (binomial)", "0.00% (binomial)"),
  SLS        = c(paste0(fmt_pct(safe_s(function() c07b_sls$pct_fuera_01_train), 2),
                        " ✅ (κ̂γ)"),                                  # 0% por construcción
                 fmt_pct(safe_s(function() c07b_sls$pct_fuera_01_test), 2),  # test
                 fmt_pct(safe_s(function() c08_sls$pct_pred_fuera_01_union), 2))
)

# Histogramas precalculados: distribucion de probabilidades predichas
# Muestra estratificada 100K obs (evita objetos pesados en el env del Rmd)
set.seed(SEED_GLOBAL)
idx_sample <- sample(nrow(panel), min(100000L, nrow(panel)))
prob_sample <- panel[idx_sample, ] %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting")) %>%
  select(all_of(c(COL_PROB_LPM, COL_PROB_GLM, COL_PROB_SLS))) %>%
  pivot_longer(everything(), names_to = "modelo", values_to = "prob") %>%
  filter(!is.na(prob)) %>%
  mutate(modelo = case_when(                               # [L76] recode() deprecado
    modelo == COL_PROB_LPM ~ "LPM",
    modelo == COL_PROB_GLM ~ "GLM",
    modelo == COL_PROB_SLS ~ "SLS",
    TRUE ~ modelo),
    fuera_rango = prob < 0 | prob > 1)

n_fuera_s4 <- prob_sample %>%
  group_by(modelo) %>%
  summarise(n_total = n(),
            n_fuera = sum(fuera_rango),
            pct_fuera = round(mean(fuera_rango) * 100, 2),
            .groups = "drop")

cat(sprintf("   S4: muestra=%s | fuera LPM=%.2f%% | GLM=%.2f%% | SLS=%.2f%%\n",
            format(nrow(prob_sample), big.mark = ","),
            n_fuera_s4$pct_fuera[n_fuera_s4$modelo == "LPM"],
            n_fuera_s4$pct_fuera[n_fuera_s4$modelo == "GLM"],
            n_fuera_s4$pct_fuera[n_fuera_s4$modelo == "SLS"]))
rm(idx_sample); gc()

# 🪫 8. S5 -- FORMALIDAD POTENCIAL (desde contratos) -------------------------
cat("\n-- 8. S5 Formalidad potencial ----------------------------------------\n")

tab_s5 <- tibble(
  Grupo    = c("Desocupados (universo PEA)", "Desocupados (universo PEA)",
               "Inactivos 18-60 (universo Edad)", "Inactivos 18-60 (universo Edad)"),
  Umbral   = c("Youden", "Calibracion", "Youden", "Calibracion"),
  LPM      = c(
    fmt_pct(safe_s(function() c08_lpm$universo_pea$tasa_formal_desocupados_youden) * 100, 1),
    fmt_pct(safe_s(function() c08_lpm$universo_pea$tasa_formal_desocupados_cal)    * 100, 1),
    fmt_pct(safe_s(function() c08_lpm$universo_edad$tasa_formal_inactivos_youden)  * 100, 1),
    fmt_pct(safe_s(function() c08_lpm$universo_edad$tasa_formal_inactivos_cal)     * 100, 1)
  ),
  GLM      = c(
    fmt_pct(safe_s(function() c08_glm$universo_pea$tasa_formal_desocupados_youden) * 100, 1),
    fmt_pct(safe_s(function() c08_glm$universo_pea$tasa_formal_desocupados_cal)    * 100, 1),
    fmt_pct(safe_s(function() c08_glm$universo_edad$tasa_formal_inactivos_youden)  * 100, 1),
    fmt_pct(safe_s(function() c08_glm$universo_edad$tasa_formal_inactivos_cal)     * 100, 1)
  ),
  SLS      = c(
    fmt_pct(safe_s(function() c08_sls$universo_pea$tasa_formal_desocupados_youden) * 100, 1),
    fmt_pct(safe_s(function() c08_sls$universo_pea$tasa_formal_desocupados_cal)    * 100, 1),
    fmt_pct(safe_s(function() c08_sls$universo_edad$tasa_formal_inactivos_youden)  * 100, 1),
    fmt_pct(safe_s(function() c08_sls$universo_edad$tasa_formal_inactivos_cal)     * 100, 1)
  )
)

n_desoc_pea  <- sum(panel$tipo_estimacion_pea == "Potencial_desocupado", na.rm = TRUE)
n_inact_edad <- sum(panel$tipo_estimacion_edad == "Potencial_inactivo",   na.rm = TRUE)

cat(sprintf("   S5: desocupados PEA=%s | inactivos edad=%s\n",
            fmt_n(n_desoc_pea), fmt_n(n_inact_edad)))

# 🪫 9. S6 -- MATRIZ DE CORRELACION 6x6 --------------------------------------
cat("\n-- 9. S6 Correlacion -------------------------------------------------\n")

# 6 series: {Youden, Cal} x {LPM, GLM, SLS} sobre ocupados
mat_series <- series_ocup %>%
  select(periodo_id,
         LPM_you = you_lpm, GLM_you = you_glm, SLS_you = you_sls,
         LPM_cal = cal_lpm, GLM_cal = cal_glm, SLS_cal = cal_sls)

cor_mat <- round(cor(mat_series %>% select(-periodo_id), use = "complete.obs"), 4)
rownames(cor_mat) <- c("LPM (Youden)","GLM (Youden)","SLS (Youden)",
                        "LPM (Cal.)","GLM (Cal.)","SLS (Cal.)")
colnames(cor_mat) <- rownames(cor_mat)

cat("   S6 [OK]\n")

# Liberar panel — ya no se necesita
rm(panel); gc()
cat("   Panel liberado de memoria\n")

# 🪫 10. ESCALARES PARA NOTAS Y RMD ------------------------------------------
cat("\n-- 10. Escalares para Rmd -------------------------------------------\n")

n_panel      <- safe_s(function() c08_lpm$n_panel_total)
n_union_lpm  <- safe_s(function() c08_lpm$n_union)
n_union_glm  <- safe_s(function() c08_glm$n_union)
n_union_sls  <- safe_s(function() c08_sls$n_union)

umb_you_lpm  <- safe_s(function() c08_lpm$umbral_youden)
umb_cal_lpm  <- safe_s(function() c08_lpm$umbral_calibracion)
umb_you_glm  <- safe_s(function() c08_glm$umbral_youden)
umb_cal_glm  <- safe_s(function() c08_glm$umbral_calibracion)
umb_you_sls  <- safe_s(function() c08_sls$umbral_youden)
umb_cal_sls  <- safe_s(function() c08_sls$umbral_calibracion)

auc_lpm      <- safe_s(function() c08_lpm$auc_global_train)
auc_glm      <- safe_s(function() c08_glm$auc_global_train)
auc_sls      <- safe_s(function() c08_sls$auc_global_train)
hl_pval_glm  <- safe_s(function() c07b_glm$hl_pval)   # H-L p-valor GLM (desde c07b)

cat("   [OK] Escalares listos\n")

# 🪫 11. NOTAS TXT -----------------------------------------------------------
cat("\n-- 11. Notas TXT -----------------------------------------------------\n")

notas <- c(
  "09b_comp_retropredictivo -- Backcasting comparativo LPM / GLM / SLS",
  paste0("Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "",
  "-- BENCHMARK (S0) --",
  paste0("AUC train:  LPM=", sprintf("%.4f", auc_lpm),
         " | GLM=", sprintf("%.4f", auc_glm),
         " | SLS=", sprintf("%.4f", auc_sls)),
  paste0("F1 cal:     LPM=", fmt_num(safe_s(function() c08_lpm$metricas_calibracion$f1), 4),
         " | GLM=", fmt_num(safe_s(function() c08_glm$metricas_calibracion$f1), 4),
         " | SLS=", fmt_num(safe_s(function() c08_sls$metricas_calibracion$f1), 4)),
  paste0("MCC cal:    LPM=", fmt_num(safe_s(function() c08_lpm$metricas_calibracion$mcc), 4),
         " | GLM=", fmt_num(safe_s(function() c08_glm$metricas_calibracion$mcc), 4),
         " | SLS=", fmt_num(safe_s(function() c08_sls$metricas_calibracion$mcc), 4)),
  paste0("Delta max (cal):  LPM=",
         sprintf("%.2f", safe_s(function() c08_lpm$metricas_calibracion$delta_max_pp)),
         " | GLM=",
         sprintf("%.2f", safe_s(function() c08_glm$metricas_calibracion$delta_max_pp)),
         " | SLS=",
         sprintf("%.2f", safe_s(function() c08_sls$metricas_calibracion$delta_max_pp))),
  "",
  "-- VALIDACION DIRECTA S1 (delta promedio ocupados) --",
  paste0("LPM: ", round(mean(abs(comp_s1$delta_lpm)), 2), " pp"),
  paste0("GLM: ", round(mean(abs(comp_s1$delta_glm)), 2), " pp"),
  paste0("SLS: ", round(mean(abs(comp_s1$delta_sls)), 2), " pp"),
  "",
  "-- COHERENCIA HISTORICA S2 (asalariados, serie total) --",
  paste0("Corr LPM=", met_lpm_t$corr, " | MAE=", met_lpm_t$mae, " pp | RMSE=", met_lpm_t$rmse, " pp"),
  paste0("Corr GLM=", met_glm_t$corr, " | MAE=", met_glm_t$mae, " pp | RMSE=", met_glm_t$rmse, " pp"),
  paste0("Corr SLS=", met_sls_t$corr, " | MAE=", met_sls_t$mae, " pp | RMSE=", met_sls_t$rmse, " pp"),
  "",
  "-- PRED FUERA [0,1] S4 (backcasting union PEA) --",
  paste0("LPM: ", fmt_pct(safe_s(function() c08_lpm$pct_pred_fuera_01_union), 2)),
  "GLM: 0.00% (invariante binomial)",
  paste0("SLS: ", fmt_pct(safe_s(function() c08_sls$pct_pred_fuera_01_union), 2)),
  "",
  "-- CORRELACION SERIES (ocupados, Cal.) --",
  paste0("LPM vs GLM: ", cor_mat["LPM (Cal.)", "GLM (Cal.)"]),
  paste0("LPM vs SLS: ", cor_mat["LPM (Cal.)", "SLS (Cal.)"]),
  paste0("GLM vs SLS: ", cor_mat["GLM (Cal.)", "SLS (Cal.)"])
)

writeLines(notas, PATH_09B_NOTAS)
cat(sprintf("   [OK] Notas: %s\n", PATH_09B_NOTAS))

# 🪫 11b. CONTRATO -- escalares para 10_paper_html.R -------------------------
cat("\n-- 11b. Contrato 09b -------------------------------------------------\n")

contrato_09b <- list(
  script              = "09b_comp_retropredictivo.R",
  fecha               = Sys.time(),
  sufijo_lpm          = SUFIJO_MODELO_LPM,
  sufijo_glm          = SUFIJO_MODELO_GLM,
  sufijo_sls          = SUFIJO_MODELO_SLS,
  # Coherencia histórica S2 sin pandemia (referencia principal del paper)
  corr_hist_lpm       = met_lpm_sp$corr,
  corr_hist_glm       = met_glm_sp$corr,
  corr_hist_sls       = met_sls_sp$corr,
  mae_hist_lpm        = met_lpm_sp$mae,
  mae_hist_glm        = met_glm_sp$mae,
  mae_hist_sls        = met_sls_sp$mae,
  rmse_hist_lpm       = met_lpm_sp$rmse,
  rmse_hist_glm       = met_glm_sp$rmse,
  rmse_hist_sls       = met_sls_sp$rmse,
  # Coherencia histórica S2 serie total (con pandemia)
  corr_hist_total_lpm = met_lpm_t$corr,
  corr_hist_total_glm = met_glm_t$corr,
  corr_hist_total_sls = met_sls_t$corr,
  # Validación directa S1 — delta promedio (pp) en 3 trims de validación
  delta_s1_lpm        = round(mean(abs(comp_s1$delta_lpm)), 4),
  delta_s1_glm        = round(mean(abs(comp_s1$delta_glm)), 4),
  delta_s1_sls        = round(mean(abs(comp_s1$delta_sls)), 4),
  # N trimestres usados en S2
  n_trim_s2           = n_trim_s2,
  n_trim_pandemia     = n_trim_pandemia
)

saveRDS(contrato_09b, PATH_09B_COMP_CONTRATO)
cat(sprintf("   [OK] Contrato: %s\n", basename(PATH_09B_COMP_CONTRATO)))

# 🪫 12. CONSTRUIR RMD  [L56] ------------------------------------------------
cat("\n-- 12. Construyendo Rmd ----------------------------------------------\n")

rds_env  <- tempfile(fileext = ".rds")
save(
  tab_s0, s0_auc, s0_clf, s0_del, s0_fue,
  comp_s1, tasa_obs_ref, tab_s1d,
  serie_s2, serie_s2_sp, tab_s2_met,
  n_trim_s2, n_trim_pandemia,
  series_ocup, series_pea, series_edad,
  series_ocup_long, series_pea_long, series_edad_long,
  tab_s3_resumen, trims_todos, trim_idx_obs_start,
  tab_s4, prob_sample, n_fuera_s4,
  tab_s5, n_desoc_pea, n_inact_edad,
  cor_mat,
  n_panel, n_union_lpm, n_union_glm, n_union_sls,
  umb_you_lpm, umb_cal_lpm, umb_you_glm, umb_cal_glm, umb_you_sls, umb_cal_sls,
  auc_lpm, auc_glm, auc_sls,
  TRIM_OBS, TRIM_PANDEMIA,
  DIR_FIGURAS_09B,
  SUFIJO_MODELO_LPM, SUFIJO_MODELO_GLM, SUFIJO_MODELO_SLS,
  file = rds_env
)

rds_path <- gsub("\\\\", "/", rds_env)
rmd_temp <- tempfile(fileext = ".Rmd")
con <- file(rmd_temp, open = "wt", encoding = "UTF-8")

# ---- YAML ----
cat('---
title: "Comparativo Backcasting: ', SUFIJO_MODELO_LPM, ' vs ', SUFIJO_MODELO_GLM, ' vs ', SUFIJO_MODELO_SLS, '"
subtitle: "Proyecto EPH Argentina -- Formalidad Laboral 2016T4-2025T3 | Capa 6 | Backcasting sin imputacion"
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
# [FIX] Partido en dos cat() para evitar conflicto de % en sprintf:
# la primera parte usa sprintf solo para interpolar rds_path (%s),
# la segunda usa cat() plano para el codigo con %% / %d / %f.
cat(sprintf('```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,
                      fig.width=11, fig.height=5.5, dpi=150)
suppressPackageStartupMessages({
  library(tidyverse); library(knitr); library(kableExtra)
  library(ggplot2); library(patchwork); library(scales)
})
load("%s")
source(here::here("script", "config", "funciones_comunes.R"))
', rds_path), file = con)
cat('fmt_n   <- function(x) format(as.integer(x), big.mark=",")
fmt_pct <- function(x, d=2) sprintf(paste0("%%.%df%%%%"), d, as.numeric(x))
n_trims <- length(trims_todos)
idx_obs <- trim_idx_obs_start
```

', file = con)

# ---- RESUMEN EJECUTIVO ----
cat('# Resumen Ejecutivo {.unnumbered}

```{r resumen_ej}
kpi <- tibble(
  Indicador = c(
    "Panel total (obs)",
    "N elegible union PEA: LPM / GLM / SLS",
    "Umbrales Youden: LPM / GLM / SLS",
    "Umbrales calibracion: LPM / GLM / SLS",
    "AUC-ROC (train, ocupados): LPM / GLM / SLS",
    "F1 (umbral cal.): LPM / GLM / SLS",
    "MCC (umbral cal.): LPM / GLM / SLS",
    "Delta max tasa (cal., pp): LPM / GLM / SLS",
    "% pred. fuera [0,1] backcasting: LPM / GLM / SLS",
    "Correlacion series cal. (ocupados): LPM-GLM / LPM-SLS / GLM-SLS",
    "Ranking global"
  ),
  Valor = c(
    fmt_n(n_panel),
    paste0(fmt_n(n_union_lpm)," / ",fmt_n(n_union_glm)," / ",fmt_n(n_union_sls)),
    paste0(umb_you_lpm," / ",umb_you_glm," / ",umb_you_sls),
    paste0(umb_cal_lpm," / ",umb_cal_glm," / ",umb_cal_sls),
    paste0(round(auc_lpm,4)," / ",round(auc_glm,4)," / ",round(auc_sls,4)),
    paste(tab_s0$LPM[tab_s0$Metrica=="F1 (umbral calibracion)"],
          tab_s0$GLM[tab_s0$Metrica=="F1 (umbral calibracion)"],
          tab_s0$SLS[tab_s0$Metrica=="F1 (umbral calibracion)"], sep=" / "),
    paste(tab_s0$LPM[tab_s0$Metrica=="MCC (umbral calibracion)"],
          tab_s0$GLM[tab_s0$Metrica=="MCC (umbral calibracion)"],
          tab_s0$SLS[tab_s0$Metrica=="MCC (umbral calibracion)"], sep=" / "),
    paste(tab_s0$LPM[tab_s0$Metrica=="Delta max tasa -- umbral Cal. (pp)"],
          tab_s0$GLM[tab_s0$Metrica=="Delta max tasa -- umbral Cal. (pp)"],
          tab_s0$SLS[tab_s0$Metrica=="Delta max tasa -- umbral Cal. (pp)"], sep=" / "),
    paste(tab_s0$LPM[tab_s0$Metrica=="% pred. fuera [0,1] -- union PEA"],
          tab_s0$GLM[tab_s0$Metrica=="% pred. fuera [0,1] -- union PEA"],
          tab_s0$SLS[tab_s0$Metrica=="% pred. fuera [0,1] -- union PEA"], sep=" / "),
    paste0(cor_mat["LPM (Cal.)","GLM (Cal.)"]," / ",
           cor_mat["LPM (Cal.)","SLS (Cal.)"]," / ",
           cor_mat["GLM (Cal.)","SLS (Cal.)"]),
    "GLM > LPM >= SLS (AUC, calibracion, acotamiento [0,1])"
  )
)
kable(kpi, format="html", align=c("l","l"),
      caption=paste0("Indicadores clave -- Comparativo backcasting ", SUFIJO_MODELO_LPM, " / ", SUFIJO_MODELO_GLM, " / ", SUFIJO_MODELO_SLS)) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, position="center") %>%
  row_spec(c(5,6,7,11), bold=TRUE, background="#d4edda") %>%
  row_spec(9, background="#fff3cd") %>%
  column_spec(1, width="32em")
```

---

', file = con)

# ---- S0 BENCHMARK ----
cat('# Benchmark de metricas {#s0}

```{r s0_tabla}
tab_s0 %>%
  kbl(format="html", align=c("l","c","c","c"),
      caption=paste0("S0: Benchmark de metricas -- ", SUFIJO_MODELO_LPM, " / ", SUFIJO_MODELO_GLM, " / ", SUFIJO_MODELO_SLS, " (backcasting sin imputacion)")) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  row_spec(s0_auc, bold=TRUE, background="#d4edda") %>%
  row_spec(s0_clf, background="#d4edda") %>%
  row_spec(s0_del, background="#fff3cd") %>%
  row_spec(s0_fue, background="#ffe8d6") %>%
  column_spec(1, bold=TRUE, width="28em") %>%
  column_spec(3, background="#e3f2fd")
```

<small>
Cal. = umbral calibracion (LPM: `r round(umb_cal_lpm, 3)` | GLM: `r round(umb_cal_glm, 3)` | SLS: `r round(umb_cal_sls, 3)`).
GLM: 0% predicciones fuera de [0,1] por construccion binomial.
Delta max tasa = diferencia maxima entre tasa estimada y tasa observada en los 4 trimestres de validacion.
</small>

---

', file = con)

# ---- S1 VALIDACION DIRECTA ----
cat('# Validacion directa -- 4 trimestres observados {#s1}

> **Universo:** ocupados con formalidad registrada EPH (2024T4-2025T3).
> Los tres modelos estiman sobre el mismo universo (Observados + Backcasting).
> Se compara la tasa calibrada de cada modelo contra la tasa observada EPH.

## Tabla de deltas por modelo

```{r s1_tabla}
comp_s1 %>%
  mutate(
    tasa_obs  = paste0(round(tasa_obs   *100,2),"%"),
    tasa_lpm  = paste0(round(cal_lpm    *100,2),"%"),
    tasa_glm  = paste0(round(cal_glm    *100,2),"%"),
    tasa_sls  = paste0(round(cal_sls    *100,2),"%"),
    d_lpm     = paste0(ifelse(delta_lpm>=0,"+",""), delta_lpm," pp"),
    d_glm     = paste0(ifelse(delta_glm>=0,"+",""), delta_glm," pp"),
    d_sls     = paste0(ifelse(delta_sls>=0,"+",""), delta_sls," pp")
  ) %>%
  select(Trimestre=periodo_id, N_Ocup=n_ocup,
         `Obs. (EPH)`=tasa_obs,
         `LPM (Cal.)`=tasa_lpm, `Delta LPM`=d_lpm,
         `GLM (Cal.)`=tasa_glm, `Delta GLM`=d_glm,
         `SLS (Cal.)`=tasa_sls, `Delta SLS`=d_sls) %>%
  mutate(N_Ocup = format(N_Ocup, big.mark=",")) %>%
  kbl(format="html", align=c("l","r","r","r","r","r","r","r","r"),
      caption="S1: Tasa observada vs. estimaciones calibradas -- ocupados") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(5, background="#eff5fb") %>%
  column_spec(7, background="#e3f2fd", bold=TRUE) %>%
  column_spec(9, background="#f3e5f5")
```

## Grafico comparativo

```{r s1_graf, fig.height=5}
s1_long <- bind_rows(
  tasa_obs_ref %>% select(periodo_id, tasa=tasa_obs) %>%
    mutate(serie=tr("Observada (EPH)"), modelo="Obs"),
  comp_s1 %>% select(periodo_id, tasa=cal_lpm) %>% mutate(serie="LPM", modelo="LPM"),
  comp_s1 %>% select(periodo_id, tasa=cal_glm) %>% mutate(serie="GLM", modelo="GLM"),
  comp_s1 %>% select(periodo_id, tasa=cal_sls) %>% mutate(serie="SLS", modelo="SLS")
)
p_s1 <- ggplot(s1_long, aes(x=periodo_id, y=tasa, color=serie, group=serie, linetype=serie)) +
  geom_line(linewidth=1.3) + geom_point(size=4) +
  scale_color_manual(values=setNames(
    c(COL_OBSERVADO, COL_LPM, COL_GLM, COL_SLS),
    c(tr("Observada (EPH)"), "LPM", "GLM", "SLS")), name=NULL) +
  scale_linetype_manual(values=setNames(
    c("solid", "dashed", "solid", "dotted"),
    c(tr("Observada (EPH)"), "LPM", "GLM", "SLS")), name=NULL) +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  tr_labs(title="S1: Tasa observada vs. estimaciones calibradas (ocupados)",
          subtitle="Comparativo LPM / GLM / SLS -- 4 trimestres de validacion",
          x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=15, hjust=1),
        legend.position="bottom")
guardar_figura(p_s1, DIR_FIGURAS_09B, "validacion_directa", 1)
p_s1
```

## Metricas de clasificacion (sobre observados)

```{r s1d_tabla}
tab_s1d %>%
  kbl(format="html", align=c("l",rep("c",6)),
      caption="S1: Metricas de clasificacion por modelo y umbral (2024T4-2025T3)") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(c(3,5,7), bold=TRUE, background="#d4edda") %>%
  row_spec(c(4,5), background="#f0f0f0")
```

---

', file = con)

# ---- S2 COHERENCIA HISTORICA ----
cat('# Coherencia historica -- asalariados {#s2}

> **Metodologia:** Se comparan las series de tasa de formalidad de asalariados
> estimadas por cada modelo (umbral calibracion) contra la serie de `condicion_formalidad`
> de la metodologia anterior (INDEC). Serie completa 2016T4-2025T3.
> Los trimestres pandemia (2020T1-2021T2) se analizan por separado por cambios
> metodologicos en la EPH.

## Tabla de metricas

```{r s2_met}
tab_s2_met %>%
  mutate(across(c(Correlacion, MAE_pp, RMSE_pp, Delta_max_pp),
                ~round(., 4))) %>%
  kbl(format="html",
      col.names=c("Serie","Modelo","N","Corr.","MAE (pp)","RMSE (pp)","Delta max (pp)"),
      caption="S2: Coherencia historica asalariados -- serie completa y sin pandemia") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  row_spec(which(tab_s2_met$Modelo=="GLM"), background="#e3f2fd") %>%
  column_spec(4, bold=TRUE)
```

## Grafico: series de formalidad asalariados

```{r s2_graf, fig.height=5.5}
s2_long <- serie_s2 %>%
  pivot_longer(cols=c(tasa_formal_viej, tasa_lpm, tasa_glm, tasa_sls),
               names_to="serie", values_to="tasa") %>%
  mutate(serie = case_when(                                # [L76] recode() deprecado
    serie == "tasa_formal_viej" ~ tr("Metodologia anterior"),
    serie == "tasa_lpm"         ~ "LPM",
    serie == "tasa_glm"         ~ "GLM",
    serie == "tasa_sls"         ~ "SLS",
    TRUE ~ serie))

n_s2  <- length(unique(s2_long$periodo_id))
trim_breaks_s2 <- sort(unique(s2_long$periodo_id))[seq(1, n_s2, by=4)]

p_s2 <- ggplot(s2_long, aes(x=periodo_id, y=tasa, color=serie, linetype=serie, group=serie)) +
  annotate("rect",
           xmin=which(sort(unique(s2_long$periodo_id))==TRIM_PANDEMIA[1])-0.5,
           xmax=which(sort(unique(s2_long$periodo_id))==TRIM_PANDEMIA[length(TRIM_PANDEMIA)])+0.5,
           ymin=-Inf, ymax=Inf, fill="#e74c3c", alpha=0.07) +
  annotate("rect",
           xmin=n_s2-2.5, xmax=n_s2+0.5,
           ymin=-Inf, ymax=Inf, fill="#f39c12", alpha=0.08) +
  geom_line(linewidth=0.9) + geom_point(size=1.3) +
  scale_color_manual(
    values=setNames(
      c(COL_OBSERVADO, COL_LPM, COL_GLM, COL_SLS),
      c(tr("Metodologia anterior"), "LPM", "GLM", "SLS")), name=NULL) +
  scale_linetype_manual(
    values=setNames(
      c("solid", "dashed", "solid", "dotted"),
      c(tr("Metodologia anterior"), "LPM", "GLM", "SLS")), name=NULL) +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  scale_x_discrete(breaks=trim_breaks_s2) +
  tr_labs(title="S2: Tasa de formalidad asalariados -- comparacion con metodologia anterior",
          subtitle="Zona roja = pandemia COVID | Zona naranja = 3 trims con formalidad observada",
          x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7),
        legend.position="bottom")
guardar_figura(p_s2, DIR_FIGURAS_09B, "coherencia_historica", 2)
p_s2
```

---

', file = con)

# ---- S3 SERIES COMPLETAS ----
cat('# Series completas -- 3 universos x 3 modelos {#s3}

> **Universos:** Ocupados (Obs+Back) | PEA (+ desocupados potenciales) |
> Edad 18-60 (+ inactivos potenciales).
> Umbral calibracion recomendado para el paper en cada modelo.
> Zona sombreada = 4 trimestres con formalidad observada.

## Universo: Ocupados

```{r s3_ocup, fig.height=5.5}
trim_breaks_3 <- trims_todos[seq(1, n_trims, by=4)]
p_s3_ocup <- ggplot(series_ocup_long, aes(x=periodo_id, y=tasa,
                              color=modelo, linetype=modelo, group=modelo)) +
  annotate("rect", xmin=idx_obs-0.5, xmax=n_trims+0.5,
           ymin=-Inf, ymax=Inf, fill="#f39c12", alpha=0.08) +
  geom_line(linewidth=1.0) + geom_point(size=1.5) +
  scale_color_modelos(name="Modelo") +
  scale_linetype_modelos(name="Modelo") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  scale_x_discrete(breaks=trim_breaks_3) +
  tr_labs(title="S3: Series de formalidad -- Universo Ocupados (Cal.)",
          subtitle="Ocupados = Observados + Backcasting | Zona naranja = trims observados",
          x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7),
        legend.position="bottom")
guardar_figura(p_s3_ocup, DIR_FIGURAS_09B, "series_ocup", 3)
p_s3_ocup
```

## Universo: PEA

```{r s3_pea, fig.height=5.5}
p_s3_pea <- ggplot(series_pea_long, aes(x=periodo_id, y=tasa,
                              color=modelo, linetype=modelo, group=modelo)) +
  annotate("rect", xmin=idx_obs-0.5, xmax=n_trims+0.5,
           ymin=-Inf, ymax=Inf, fill="#f39c12", alpha=0.08) +
  geom_line(linewidth=1.0) + geom_point(size=1.5) +
  scale_color_modelos(name="Modelo") +
  scale_linetype_modelos(name="Modelo") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  scale_x_discrete(breaks=trim_breaks_3) +
  tr_labs(title="S3: Series de formalidad -- Universo PEA (Cal.)",
          subtitle="PEA = Ocupados + Desocupados potenciales | Zona naranja = trims observados",
          x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7),
        legend.position="bottom")
guardar_figura(p_s3_pea, DIR_FIGURAS_09B, "series_pea", 4)
p_s3_pea
```

## Universo: Edad 18-60

```{r s3_edad, fig.height=5.5}
p_s3_edad <- ggplot(series_edad_long, aes(x=periodo_id, y=tasa,
                               color=modelo, linetype=modelo, group=modelo)) +
  annotate("rect", xmin=idx_obs-0.5, xmax=n_trims+0.5,
           ymin=-Inf, ymax=Inf, fill="#f39c12", alpha=0.08) +
  geom_line(linewidth=1.0) + geom_point(size=1.5) +
  scale_color_modelos(name="Modelo") +
  scale_linetype_modelos(name="Modelo") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  scale_x_discrete(breaks=trim_breaks_3) +
  tr_labs(title="S3: Series de formalidad -- Universo Edad 18-60 (Cal.)",
          subtitle="Edad 18-60 = Ocupados + Desocupados + Inactivos potenciales",
          x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7),
        legend.position="bottom")
guardar_figura(p_s3_edad, DIR_FIGURAS_09B, "series_edad", 5)
p_s3_edad
```

## Tabla resumen (trims extremos + observados)

```{r s3_tabla}
tab_s3_resumen %>%
  select(-es_obs) %>%
  kbl(format="html", align=c("l","l","c","c","c"),
      caption="S3: Tasas de formalidad (Cal.) -- trims seleccionados por universo") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  row_spec(which(tab_s3_resumen$es_obs), bold=TRUE, background="#fff3cd") %>%
  column_spec(4, background="#e3f2fd")
```

---

', file = con)

# ---- S4 PRED FUERA [0,1] ----
cat('# Predicciones fuera de [0,1] {#s4}

> **LPM y SLS** son modelos lineales: pueden producir probabilidades predichas
> fuera del rango [0,1], especialmente para observaciones en los extremos del
> espacio de covariables. **GLM** garantiza predicciones en (0,1) por construccion
> (funcion logistica). Las predicciones se clipean a [0,1] antes de clasificar.

## Tabla resumen por contexto

```{r s4_tabla}
tab_s4 %>%
  kbl(format="html", align=c("l","c","c","c"),
      caption="S4: Predicciones fuera de [0,1] por contexto y modelo") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(1, bold=TRUE, width="24em") %>%
  column_spec(3, background="#e3f2fd", bold=TRUE) %>%
  row_spec(3, background="#fff3cd")
```

## Distribucion de probabilidades predichas

```{r s4_dens, fig.height=6}
# Clipear a [-0.2, 1.2] para visualizar cola de fuera-de-rango
prob_plot <- prob_sample %>%
  mutate(prob_clip = pmax(-0.2, pmin(1.2, prob)),
         modelo = factor(modelo, levels=c("LPM","GLM","SLS")))

p_dens <- ggplot(prob_plot, aes(x=prob_clip, fill=modelo, color=modelo)) +
  geom_density(alpha=0.25, linewidth=0.7) +
  geom_vline(xintercept=c(0,1), color="#e74c3c", linetype="dashed", linewidth=0.8) +
  scale_fill_modelos(name="Modelo") +
  scale_color_modelos(name="Modelo") +
  scale_x_continuous(limits=c(-0.2,1.2),
                     breaks=seq(-0.2,1.2,by=0.2),
                     labels=percent_format(accuracy=1)) +
  facet_wrap(~modelo, ncol=3) +
  tr_labs(title="S4: Distribucion de probabilidades predichas (muestra ocupados)",
          subtitle="Lineas rojas = limites [0,1] | Zona fuera: cola de predicciones invalidadas",
          x="Probabilidad predicha", y="Densidad") +
  theme_paper() +
  theme(legend.position="none",
        strip.text=element_text(face="bold", size=9))

p_box <- ggplot(prob_plot %>% filter(prob >= -0.1 & prob <= 1.1),
                aes(x=modelo, y=prob, fill=modelo)) +
  geom_boxplot(alpha=0.6, outlier.size=0.5, outlier.alpha=0.3) +
  geom_hline(yintercept=c(0,1), color="#e74c3c", linetype="dashed") +
  scale_fill_modelos(name="Modelo") +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  tr_labs(title="Boxplot comparativo", x=NULL, y="Probabilidad") +
  theme_paper() +
  theme(legend.position="none")

p_s4 <- (p_dens / p_box) + plot_layout(heights=c(2,1))
guardar_figura(p_s4, DIR_FIGURAS_09B, "pred_fuera_01", 6, height=5.5)
p_s4
```

```{r s4_pct_fuera}
n_fuera_s4 %>%
  kbl(format="html",
      col.names=c("Modelo","N en muestra","N fuera [0,1]","% fuera [0,1]"),
      caption="S4: Frecuencia de predicciones fuera de [0,1] en muestra ocupados") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  row_spec(which(n_fuera_s4$modelo=="GLM"), background="#e3f2fd", bold=TRUE)
```

---

', file = con)

# ---- S5 FORMALIDAD POTENCIAL ----
cat('# Formalidad potencial -- desocupados e inactivos {#s5}

> Tasas de formalidad **potencial** estimadas para quienes no tienen empleo actual.
> Se usan variables del ultimo empleo (coalesce <= 3 anos) para desocupados;
> variables imputadas con moda del training para inactivos.
> Interpretar con cautela: no es formalidad realizada sino potencial de absorcion formal.

```{r s5_tabla}
tab_s5 %>%
  kbl(format="html", align=c("l","c","c","c","c"),
      caption="S5: Formalidad potencial por grupo, umbral y modelo") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(1, bold=TRUE) %>%
  column_spec(4, background="#e3f2fd", bold=TRUE) %>%
  row_spec(c(1,3), background="#f0f0f0")
```

<small>
N desocupados PEA: `r fmt_n(n_desoc_pea)` obs. N inactivos Edad 18-60: `r fmt_n(n_inact_edad)` obs.
Los tres modelos aplican la misma logica de imputacion de variables laborales;
las diferencias en tasas reflejan diferencias en los coeficientes/parametros de cada modelo.
</small>

---

', file = con)

# ---- S6 CORRELACION ----
cat('# Matriz de correlacion entre series {#s6}

> Correlacion de Pearson entre las 6 series temporales de tasa de formalidad
> sobre el universo de **ocupados** (Observados + Backcasting).
> Series: Youden y Calibracion para cada uno de los tres modelos.

```{r s6_cor}
cor_df <- as.data.frame(cor_mat) %>%
  rownames_to_column("Serie")

cor_df %>%
  kbl(format="html", digits=4,
      caption="S6: Correlacion entre series de formalidad (ocupados, 2016T4-2025T3)") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(1, bold=TRUE) %>%
  column_spec(which(colnames(cor_df)=="GLM (Cal.)"), background="#e3f2fd") %>%
  row_spec(which(cor_df$Serie=="GLM (Cal.)"), background="#e3f2fd")
```

---

', file = con)

# ---- S7 RECOMENDACION ----
cat('# Recomendacion para el paper {#s7}

## Ranking de modelos

```{r s7_ranking}
tibble(
  Criterio         = c("AUC-ROC (train, ocupados)",
                       "F1 (umbral calibracion)",
                       "MCC (umbral calibracion)",
                       "Delta max tasa -- calibracion (pp)",
                       "% pred. fuera [0,1] -- backcasting",
                       "Calibracion nativa (H-L test 07b)",
                       "Coherencia historica asalariados (corr)",
                       "Convergencia de series (corr par minimo)"),
  LPM = c(
    as.character(round(auc_lpm, 4)),
    tab_s0$LPM[tab_s0$Metrica=="F1 (umbral calibracion)"],
    tab_s0$LPM[tab_s0$Metrica=="MCC (umbral calibracion)"],
    tab_s0$LPM[tab_s0$Metrica=="Delta max tasa -- umbral Cal. (pp)"],
    tab_s0$LPM[tab_s0$Metrica=="% pred. fuera [0,1] -- union PEA"],
    "No disponible",
    as.character(met_lpm_t$corr),
    as.character(min(cor_mat["LPM (Cal.)",c("GLM (Cal.)","SLS (Cal.)")]))
  ),
  GLM = c(
    as.character(round(auc_glm, 4)),
    tab_s0$GLM[tab_s0$Metrica=="F1 (umbral calibracion)"],
    tab_s0$GLM[tab_s0$Metrica=="MCC (umbral calibracion)"],
    tab_s0$GLM[tab_s0$Metrica=="Delta max tasa -- umbral Cal. (pp)"],
    "0.00% (binomial)",
    sprintf("p=%.4f (excelente)", hl_pval_glm),
    as.character(met_glm_t$corr),
    as.character(min(cor_mat["GLM (Cal.)",c("LPM (Cal.)","SLS (Cal.)")]))
  ),
  SLS = c(
    as.character(round(auc_sls, 4)),
    tab_s0$SLS[tab_s0$Metrica=="F1 (umbral calibracion)"],
    tab_s0$SLS[tab_s0$Metrica=="MCC (umbral calibracion)"],
    tab_s0$SLS[tab_s0$Metrica=="Delta max tasa -- umbral Cal. (pp)"],
    tab_s0$SLS[tab_s0$Metrica=="% pred. fuera [0,1] -- union PEA"],
    "No disponible",
    as.character(met_sls_t$corr),
    as.character(min(cor_mat["SLS (Cal.)",c("LPM (Cal.)","GLM (Cal.)")]))
  ),
  Ganador = c("GLM","GLM","GLM","LPM","GLM","GLM","LPM~GLM","GLM~LPM")
) %>%
  kbl(format="html", align=c("l","c","c","c","c"),
      caption="S7: Criterios de seleccion de modelo para el paper") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=TRUE, font_size=11) %>%
  column_spec(3, background="#e3f2fd", bold=TRUE) %>%
  column_spec(5, bold=TRUE) %>%
  row_spec(c(1,2,3,5,6), background="#d4edda")
```

## Conclusion

**GLM es el modelo recomendado** como serie principal del paper:
AUC = `r round(auc_glm, 4)`, calibracion nativa (H-L p=`r sprintf("%.4f", hl_pval_glm)`), 0% predicciones fuera
de [0,1] y coherencia historica comparable con LPM.
Las series LPM y SLS operan como **robustness checks**: confirman la tendencia
principal con diferencias menores de `r round(abs(met_glm_t$corr - met_lpm_t$corr), 4)` pp en correlacion historica.

**Siguiente paso:** Script 09c -- Comparativo variable hibrida (con imputacion ground truth).
', file = con)

close(con)
cat("   [OK] Rmd escrito:", rmd_temp, "\n\n")

# 🪫 13. RENDER HTML ---------------------------------------------------------
cat("-- 13. Renderizando HTML ---------------------------------------------\n")

tic("render")
rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_09B_HTML,
  quiet       = TRUE,
  envir       = new.env()
)
toc()
unlink(rmd_temp)
unlink(rds_env)

cat(sprintf("   [OK] HTML: %s (%.1f KB)\n",
            PATH_09B_HTML, file.size(PATH_09B_HTML) / 1024))

# 📑 14. CHECKLIST -----------------------------------------------------------
cat("\n-- 14. Checklist -----------------------------------------------------\n")
cat("   HTML    :", basename(PATH_09B_HTML),
    if (file.exists(PATH_09B_HTML))           "[OK]" else "[FALTA]", "\n")
cat("   Notas   :", basename(PATH_09B_NOTAS),
    if (file.exists(PATH_09B_NOTAS))          "[OK]" else "[FALTA]", "\n")
cat("   Contrato:", basename(PATH_09B_COMP_CONTRATO),
    if (file.exists(PATH_09B_COMP_CONTRATO))  "[OK]" else "[FALTA]", "\n")

cat("\n===================================================================\n")
cat("SCRIPT 09b COMPLETADO\n")
cat("  HTML    :", basename(PATH_09B_HTML),       "\n")
cat("  TXT     :", basename(PATH_09B_NOTAS),      "\n")
cat("  Contrato:", basename(PATH_09B_COMP_CONTRATO), "\n")
cat("===================================================================\n")

toc()
