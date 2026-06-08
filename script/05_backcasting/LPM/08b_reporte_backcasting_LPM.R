# =============================================================================
# [EN] 08b_reporte_backcasting_LPM.R -- Funnel-structured HTML report: backcasting validation, historical coherence, coverage (LPM)
# INPUTS:  rdos/datos/08_panel_formalidad_LPM*.rds, contracts from 07-08 LPM
# OUTPUTS: rdos/reportes/08b_reporte_backcasting_LPM*.html
# =============================================================================
# 🌟 08b_reporte_backcasting_LPM.R 🌟 ####
# OBJETIVO:
#    Reporte HTML del backcasting LPM — estructura de embudo de comparabilidad.
#    Cada seccion expande el universo (de mayor a menor certeza):
#
#    S1 — Validacion directa (4 trimestres observados)
#         1a: tasa obs vs serie ocupados (Obs+Back)     — universo mas comparable
#         1b: tasa obs vs serie PEA (Obs+Back+Desoc)    — universo ampliado
#         1c: descomposicion del delta (contribucion desocupados)
#         1d: metricas de clasificacion por umbral
#    S2 — Coherencia historica vs. metodologia anterior (asalariados 2016-2025)
#         con y sin trimestres pandemia
#    S3 — Extension a otras categorias (cta. propia, patron, otros)
#         3a: solo ocupados  |  3b: + desocupados potenciales (ultimo empleo)
#    S4 — Series temporales completas (tres universos)
#    S5 — Formalidad potencial (desocupados e inactivos)
#    S6 — Cobertura del backcasting
#    S7 — Notas metodologicas
#
# INPUTS:
#    - rdos/datos/08_panel_formalidad_{SUFIJO}.rds         (Script 08)
#    - rdos/contratos/08_contrato_backcasting_{SUFIJO}.rds (Script 08)
#    - rdos/contratos/07a_contrato_lasso_{SUFIJO}.rds      (Script 07a)
#    - rdos/contratos/07b_contrato_postlasso_{SUFIJO}.rds  (Script 07b)
#    - rdos/datos/02_panel_con_taxonomia.rds             (Script 02)
#
# OUTPUTS:
#    - rdos/reportes/08b_reporte_backcasting_{SUFIJO}.html
#    - rdos/reportes/08b_notas_paper_{SUFIJO}.txt
#    - rdos/figuras/08b_reporte_backcasting_LPM/  (10 PDFs)
#
# TIEMPO ESTIMADO: ~3 minutos


# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  library(ggplot2)
  library(patchwork)
  library(rmarkdown)
  library(tictoc)
})


# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# 🔑 Nombres dinámicos de columnas (evitar HC de sufijo) ----------------------
SUFIJO <- SUFIJO_MODELO_LPM
COL_CLASE_PEA      <- paste0("formalidad_clase_", SUFIJO, "_pea")
COL_CLASE_CAL_PEA  <- paste0("formalidad_clase_cal_", SUFIJO, "_pea")
COL_CLASE_EDAD     <- paste0("formalidad_clase_", SUFIJO, "_edad")
COL_CLASE_CAL_EDAD <- paste0("formalidad_clase_cal_", SUFIJO, "_edad")
COL_PROB_PEA       <- paste0("prob_formal_", SUFIJO, "_pea")
COL_FLAG_PEA       <- paste0("flag_pred_", SUFIJO, "_pea")


# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 08b - Reporte Backcasting LPM")
start_time <- Sys.time()
cat("===================================================================\n")
cat("SCRIPT 08b - REPORTE BACKCASTING LPM (embudo de comparabilidad)\n")
cat("Inicio:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")


# 🪫 1. CARGA -------------------------------------------------------------------
cat("-- 1. Carga -----------------------------------------------------------\n")

# Usar paths definidos en parametros.R — sin HC de rutas
PATH_08B_HTML <- PATH_08B_HTML_LPM
PATH_TXT_08B  <- file.path(DIR_REPORTES, paste0("08b_notas_paper_", SUFIJO_MODELO_LPM, ".txt"))

hard_stop(file.exists(PATH_08_PANEL_LPM),    paste0("No existe 08_panel_formalidad_", SUFIJO_MODELO_LPM, ".rds"))
hard_stop(file.exists(PATH_08_CONTRATO_LPM), paste0("No existe 08_contrato_backcasting_", SUFIJO_MODELO_LPM, ".rds"))
hard_stop(file.exists(PATH_07_CONTRATO),     paste0("No existe 07a_contrato_lasso_", SUFIJO_MODELO_LPM, ".rds"))
hard_stop(file.exists(PATH_07B_CONTRATO),    paste0("No existe 07b_contrato_postlasso_", SUFIJO_MODELO_LPM, ".rds"))
dir.create(DIR_REPORTES,          showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_FIGURAS_08B_LPM,   showWarnings = FALSE, recursive = TRUE)

c08  <- readRDS(PATH_08_CONTRATO_LPM)
c07a <- readRDS(PATH_07_CONTRATO)
c07b <- readRDS(PATH_07B_CONTRATO)
cat("   [OK] Contrato c08. Campos:", length(names(c08)), "\n")
cat("   [OK] Contrato c07a. Campos:", length(names(c07a)), "\n")
cat("   [OK] Contrato c07b. Campos:", length(names(c07b)), "\n")

cols_necesarias <- c(
  "id_individuo", "periodo_id", "pondera",
  "tipo_estimacion_pea",  "tipo_estimacion_edad",
  COL_CLASE_PEA,      COL_CLASE_CAL_PEA,
  COL_CLASE_EDAD,     COL_CLASE_CAL_EDAD,
  "formalidad_valida",
  COL_PROB_PEA, COL_FLAG_PEA,
  "categoria_ocupacional"
)

cat("   Cargando panel 08...\n")
panel_full <- readRDS(PATH_08_PANEL_LPM)
cols_ok    <- intersect(cols_necesarias, names(panel_full))
panel      <- panel_full[, cols_ok]
rm(panel_full); gc()
cat("   [OK] Panel:", format(nrow(panel), big.mark = ","), "obs |",
    ncol(panel), "cols retenidas\n")

# --- condicion_formalidad desde panel 02 ---
cat("   Cargando condicion_formalidad desde panel 02...\n")
tiene_tax <- file.exists(PATH_02_PANEL_TAX)
if (tiene_tax) {
  panel_tax <- readRDS(PATH_02_PANEL_TAX) %>%
    select(id_individuo, condicion_formalidad) %>%
    filter(!is.na(condicion_formalidad))
  panel <- panel %>% left_join(panel_tax, by = "id_individuo")
  rm(panel_tax); gc()
  cat("   [OK] Join condicion_formalidad completado:\n")
  print(table(panel$condicion_formalidad, useNA = "ifany"))
} else {
  warning("02_panel_con_taxonomia.rds no encontrado. S2 no disponible.")
  panel$condicion_formalidad <- NA_character_
}
cat("\n")


# 🪫 2. CALCULOS PARA EL REPORTE ------------------------------------------------
cat("-- 2. Calculos --------------------------------------------------------\n")

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE)

# Leer desde parametros.R (evitar HC)
TRIM_OBS      <- TRIMESTRES_FORMALIDAD   # c("2024_T4","2025_T1","2025_T2","2025_T3")
TRIM_PANDEMIA <- TRIMESTRES_PANDEMIA     # c("2020_T1",...,"2021_T2")

tipos_activos <- c("Observado", "Backcasting")

label_tipo_pea <- c(
  "Observado"            = tr("Observado"),
  "Backcasting"          = tr("Backcasting"),
  "Potencial_desocupado" = tr("Desoc. potencial formal"),
  "No_aplica"            = tr("No aplica"),
  "Sin_theta"            = tr("Sin theta")
)

# Helper: categoria ocupacional canonica
cat_simple_fn <- function(x) {
  case_when(
    x == "Empleado"                          ~ tr("Asalariado"),
    startsWith(as.character(x), "Patr")      ~ tr("Patron"),
    x == "Cuenta Propia"                     ~ tr("Cta. propia"),
    x == "Familiar"                          ~ tr("Otros"),
    TRUE                                     ~ NA_character_
  )
}

# Helper: metricas de ajuste de la serie (backcasting vs. vieja)
metricas_serie <- function(df) {
  tibble(
    n_trim    = nrow(df),
    corr      = round(cor(df$tasa_formal_back, df$tasa_formal_viej, use = "complete.obs"), 4),
    mae       = round(mean(abs(df$delta_formal_pp)), 2),
    rmse      = round(sqrt(mean(df$delta_formal_pp^2)), 2),
    max_delta = round(max(abs(df$delta_formal_pp)), 2)
  )
}

# ── 2a. Series base (tres universos) ─────────────────────────────────────────
cat("   2a. Series base...\n")

serie_ocupados <- panel %>%
  filter(tipo_estimacion_pea %in% tipos_activos) %>%
  group_by(periodo_id) %>%
  summarise(
    n                = n(),
    tasa_youden      = weighted.mean(.data[[COL_CLASE_PEA]]     == "Formal", pondera, na.rm = TRUE),
    tasa_calibracion = weighted.mean(.data[[COL_CLASE_CAL_PEA]] == "Formal", pondera, na.rm = TRUE),
    .groups = "drop") %>% arrange(periodo_id)

serie_pea <- panel %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting", "Potencial_desocupado")) %>%
  group_by(periodo_id) %>%
  summarise(
    n                = n(),
    tasa_youden      = weighted.mean(.data[[COL_CLASE_PEA]]     == "Formal", pondera, na.rm = TRUE),
    tasa_calibracion = weighted.mean(.data[[COL_CLASE_CAL_PEA]] == "Formal", pondera, na.rm = TRUE),
    .groups = "drop") %>% arrange(periodo_id)

serie_edad <- panel %>%
  filter(tipo_estimacion_edad %in% c("Observado", "Backcasting",
                                      "Potencial_desocupado", "Potencial_inactivo")) %>%
  group_by(periodo_id) %>%
  summarise(
    n                = n(),
    tasa_youden      = weighted.mean(.data[[COL_CLASE_EDAD]]     == "Formal", pondera, na.rm = TRUE),
    tasa_calibracion = weighted.mean(.data[[COL_CLASE_CAL_EDAD]] == "Formal", pondera, na.rm = TRUE),
    .groups = "drop") %>% arrange(periodo_id)

# ── 2b. S1 — Validacion directa ──────────────────────────────────────────────
cat("   2b. S1 - Validacion directa...\n")

comp_trim <- c08$comp_por_trimestre %>% rename(periodo_id = periodo)

comp_s1 <- serie_ocupados %>%
  filter(periodo_id %in% TRIM_OBS) %>%
  select(periodo_id, n_ocup = n, tasa_ocup_cal = tasa_calibracion) %>%
  left_join(
    serie_pea %>% filter(periodo_id %in% TRIM_OBS) %>%
      select(periodo_id, n_pea = n, tasa_pea_cal = tasa_calibracion),
    by = "periodo_id") %>%
  left_join(
    comp_trim %>% select(periodo_id, n_obs = n, tasa_obs, delta_pr_pp),
    by = "periodo_id") %>%
  mutate(
    n_desoc_pot   = n_pea  - n_ocup,
    pct_desoc_pea = round(n_desoc_pot / n_pea * 100, 1),
    delta_ocup_pp = round((tasa_ocup_cal - tasa_obs) * 100, 2),
    delta_pea_pp  = round((tasa_pea_cal  - tasa_obs) * 100, 2),
    contrib_desoc = round((tasa_pea_cal  - tasa_ocup_cal) * 100, 2)
  )

cat(sprintf("   comp_s1 OK: delta_ocup prom=%.2f pp | delta_pea prom=%.2f pp\n",
            mean(abs(comp_s1$delta_ocup_pp)), mean(abs(comp_s1$delta_pea_pp))))

# ── 2c. S2 — Coherencia historica asalariados ────────────────────────────────
cat("   2c. S2 - Coherencia historica asalariados...\n")

serie_asal_vieja <- panel %>%
  filter(condicion_formalidad %in% c("Formal", "No formal")) %>%
  group_by(periodo_id) %>%
  summarise(
    n_vieja          = n(),
    tasa_formal_viej = weighted.mean(condicion_formalidad == "Formal",    pondera, na.rm = TRUE),
    tasa_infor_viej  = weighted.mean(condicion_formalidad == "No formal", pondera, na.rm = TRUE),
    .groups = "drop") %>% arrange(periodo_id)

serie_asal_back <- panel %>%
  filter(tipo_estimacion_pea %in% tipos_activos,
         !is.na(.data[[COL_CLASE_CAL_PEA]]),
         categoria_ocupacional == "Empleado") %>%
  group_by(periodo_id) %>%
  summarise(
    n_back           = n(),
    tasa_formal_back = weighted.mean(.data[[COL_CLASE_CAL_PEA]] == "Formal",   pondera, na.rm = TRUE),
    tasa_infor_back  = weighted.mean(.data[[COL_CLASE_CAL_PEA]] == "Informal", pondera, na.rm = TRUE),
    .groups = "drop") %>% arrange(periodo_id)

serie_asal_comp <- serie_asal_vieja %>%
  inner_join(serie_asal_back, by = "periodo_id") %>%
  mutate(
    delta_formal_pp = round((tasa_formal_back - tasa_formal_viej) * 100, 2),
    es_observado    = periodo_id %in% TRIM_OBS,
    es_pandemia     = periodo_id %in% TRIM_PANDEMIA
  )

trim_pandemia_en_datos <- TRIM_PANDEMIA[TRIM_PANDEMIA %in% serie_asal_comp$periodo_id]
n_trim_pandemia        <- length(trim_pandemia_en_datos)

met_total <- metricas_serie(serie_asal_comp)
met_sin_p <- metricas_serie(serie_asal_comp %>% filter(!es_pandemia))

cat(sprintf("   Serie asal.: %d trims | %d pandemia | corr total=%.4f | sin pand=%.4f\n",
            met_total$n_trim, n_trim_pandemia, met_total$corr, met_sin_p$corr))

# ── 2d. S3 — Categorias ocupacionales ────────────────────────────────────────
cat("   2d. S3 - Categorias ocupacionales...\n")

panel_cat <- panel %>%
  filter(tipo_estimacion_pea %in% tipos_activos,
         !is.na(.data[[COL_CLASE_CAL_PEA]]),
         !is.na(categoria_ocupacional),
         categoria_ocupacional != "No corresponde",
         !(categoria_ocupacional %in% c("Ns/Nr", ""))) %>%
  mutate(cat_simple = cat_simple_fn(categoria_ocupacional),
         condicion  = .data[[COL_CLASE_CAL_PEA]]) %>%
  filter(!is.na(cat_simple))

panel_cat_pea <- panel %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting", "Potencial_desocupado"),
         !is.na(.data[[COL_CLASE_CAL_PEA]]),
         !is.na(categoria_ocupacional),
         categoria_ocupacional != "No corresponde",
         !(categoria_ocupacional %in% c("Ns/Nr", ""))) %>%
  mutate(cat_simple    = cat_simple_fn(categoria_ocupacional),
         condicion      = .data[[COL_CLASE_CAL_PEA]],
         es_desocupado  = tipo_estimacion_pea == "Potencial_desocupado") %>%
  filter(!is.na(cat_simple))

tab_cat_global <- panel_cat %>%
  group_by(cat_simple) %>%
  summarise(
    n_total    = n(),
    n_formal   = sum(condicion == "Formal",   na.rm = TRUE),
    n_informal = sum(condicion == "Informal", na.rm = TRUE),
    tasa_formal = weighted.mean(condicion == "Formal", pondera, na.rm = TRUE),
    .groups = "drop") %>% arrange(desc(n_total))

tab_cat_global_pea <- panel_cat_pea %>%
  group_by(cat_simple) %>%
  summarise(
    n_total    = n(),
    n_ocup     = sum(!es_desocupado),
    n_desoc    = sum(es_desocupado),
    pct_desoc  = round(mean(es_desocupado) * 100, 1),
    tasa_formal = weighted.mean(condicion == "Formal", pondera, na.rm = TRUE),
    .groups = "drop") %>% arrange(desc(n_total))

comp_cat_trim <- panel_cat %>%
  group_by(periodo_id, cat_simple, condicion) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(periodo_id) %>%
  mutate(pct = n / sum(n)) %>% ungroup()

serie_cat <- panel_cat %>%
  group_by(periodo_id, cat_simple) %>%
  summarise(tasa = weighted.mean(condicion == "Formal", pondera, na.rm = TRUE),
            .groups = "drop")

serie_cat_pea <- panel_cat_pea %>%
  group_by(periodo_id, cat_simple) %>%
  summarise(tasa    = weighted.mean(condicion == "Formal", pondera, na.rm = TRUE),
            n_desoc = sum(es_desocupado),
            n_total = n(),
            .groups = "drop")

# ── 2e. Cobertura PEA ────────────────────────────────────────────────────────
cat("   2e. Cobertura...\n")
cob_pea <- panel %>%
  filter(!is.na(tipo_estimacion_pea)) %>%
  group_by(periodo_id, tipo_estimacion_pea) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(periodo_id) %>%
  mutate(n_total = sum(n), pct = n / n_total) %>%
  ungroup() %>%
  mutate(tipo_label = ifelse(tipo_estimacion_pea %in% names(label_tipo_pea),
                             label_tipo_pea[tipo_estimacion_pea],
                             tipo_estimacion_pea))

n_desoc_pea <- sum(panel$tipo_estimacion_pea == "Potencial_desocupado", na.rm = TRUE)
cat(sprintf("   N desocupados PEA: %s\n", fmt_n(n_desoc_pea)))

# ── 2f. Metricas S1d y variables S7 ──────────────────────────────────────────
cat("   2f. Metricas por umbral...\n")

UMBRAL_YOUDEN <- as.numeric(c08$umbral_youden)
UMBRAL_CAL    <- as.numeric(c08$umbral_calibracion)
hard_stop(!is.na(UMBRAL_YOUDEN) && UMBRAL_YOUDEN > 0 && UMBRAL_YOUDEN < 1,
          paste("[08b LPM] UMBRAL_YOUDEN invalido desde c08:", UMBRAL_YOUDEN))
hard_stop(!is.na(UMBRAL_CAL) && UMBRAL_CAL > 0 && UMBRAL_CAL < 1,
          paste("[08b LPM] UMBRAL_CAL invalido desde c08:", UMBRAL_CAL))
cat(sprintf("   Umbrales desde c08: Youden=%.4f | Cal=%.4f\n", UMBRAL_YOUDEN, UMBRAL_CAL))

# Metricas umbral Youden: leer de c07b$metricas_clf (fila del umbral Youden)
# c07b$metricas_clf es 1×8 con el umbral Youden del post-LASSO
acc_youden   <- round(c07b$metricas_clf$accuracy[1]      * 100, 2)
sens_youden  <- round(c07b$metricas_clf$sensibilidad[1]  * 100, 2)
esp_youden   <- round(c07b$metricas_clf$especificidad[1] * 100, 2)
f1_youden    <- round(c07b$metricas_clf$f1[1],  4)
mcc_youden   <- round(c07b$metricas_clf$mcc[1], 4)
delta_youden <- round(c08$delta_max_pp, 2)

# Metricas umbral calibracion: desde c08$metricas_calibracion
acc_cal   <- round(c08$metricas_calibracion$accuracy      * 100, 2)
sens_cal  <- round(c08$metricas_calibracion$sensibilidad  * 100, 2)
esp_cal   <- round(c08$metricas_calibracion$especificidad * 100, 2)
f1_cal    <- round(c08$metricas_calibracion$f1,  4)
mcc_cal   <- round(c08$metricas_calibracion$mcc, 4)
delta_cal <- round(c08$metricas_calibracion$delta_max_pp, 2)

cat(sprintf("   S1d: Acc Youden=%.2f%% | Sens=%.2f%% | Esp=%.2f%%\n",
            acc_youden, sens_youden, esp_youden))

# S7: % pred fuera [0,1] sobre Observados del backcasting — computar desde panel
# (no guardado en contratos; "17%" era HC incorrecto)
pct_fuera_01_obs_back <- panel %>%
  filter(tipo_estimacion_pea == "Observado",
         !is.na(.data[[COL_PROB_PEA]])) %>%
  summarise(pct = mean(.data[[COL_PROB_PEA]] < 0 | .data[[COL_PROB_PEA]] > 1,
                       na.rm = TRUE) * 100) %>%
  pull(pct) %>% round(1)
cat(sprintf("   Pred fuera [0,1] en Observados: %.1f%%\n", pct_fuera_01_obs_back))

# S7: AUC y pct_fuera_01 test (desde c07a)
auc_test_c07a     <- c07a$auc_test
pct_fuera_01_test <- c07a$pct_pred_fuera_01   # 6.14% (era HC "6.18%")

cat("   Calculos completados.\n\n")


# 🪫 3. CONSTRUIR RMD -----------------------------------------------------------
cat("-- 3. Construyendo Rmd ------------------------------------------------\n")

rmd_temp <- tempfile(fileext = ".Rmd")
con      <- file(rmd_temp, open = "wt", encoding = "UTF-8")

writeLines('---
title: "Backcasting LPM -- Reporte Capa 5"
subtitle: "Proyecto EPH Argentina - Formalidad Laboral 2016T4-2025T3"
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
---', con)

writeLines('
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 10, fig.height = 5.5, dpi = 150)
library(tidyverse); library(knitr); library(kableExtra)
library(ggplot2);   library(patchwork);  library(scales)

# Colores semanticos formalidad — distintos de colores de modelos
# HC documentado: representan concepto formalidad, no un modelo especifico
COL_FORMAL   <- COL_LPM          # verde bosque — formal = LPM (coherencia)
COL_INFORMAL <- PAL_DESCRIPTIVO[3]  # naranja — informal (evita verde/rojo)

TRIM_OBS      <- TRIMESTRES_FORMALIDAD
TRIM_PANDEMIA <- TRIMESTRES_PANDEMIA
cats_orden    <- tr(c("Asalariado","Patron","Cta. propia","Otros"))

# Categorias ocupacionales: PAL_DESCRIPTIVO (no son modelos)
colores_cat <- setNames(
  c(PAL_DESCRIPTIVO[1], PAL_DESCRIPTIVO[4], PAL_DESCRIPTIVO[3], PAL_DESCRIPTIVO[5]),
  tr(c("Asalariado", "Patron", "Cta. propia", "Otros"))
)

# Series temporales S4: mismo color COL_LPM, diferencia por linetype + linewidth
# (Edad 18-60) > (PEA) > (Ocupados) en tamano poblacional
linetypes_series  <- setNames(c("solid","dashed","dotted"), tr(c("Ocupados","PEA","Edad 18-60")))
linewidths_series <- setNames(c(0.6, 0.9, 1.2),            tr(c("Ocupados","PEA","Edad 18-60")))
shapes_series     <- setNames(c(16L, 17L, 15L),             tr(c("Ocupados","PEA","Edad 18-60")))

# Labels dinamicos desde umbrales del contrato (via envir=environment())
LBL_CAL       <- sprintf("Cal.%.3g",  UMBRAL_CAL)
LBL_YOUDEN    <- sprintf("Youden (%.4f)", UMBRAL_YOUDEN)
LBL_BACK_CAL  <- paste0("Backcasting LPM (", LBL_CAL, ")")
LBL_ESTIM_CAL <- paste0(tr("Estimada ocupados"), " (", LBL_CAL, ")")
```
', con)

# ── RESUMEN EJECUTIVO ─────────────────────────────────────────────────────────
writeLines('
# Resumen Ejecutivo {.unnumbered}

```{r resumen_ejecutivo}
kpi <- tibble(
  Indicador = c(
    "N panel total","N elegible PEA","N elegible Edad 18-60",
    "N union (PEA U Edad 18-60)","Modelo base","Lambda utilizado",
    "Umbral Youden","Umbral calibracion",
    "AUC-ROC (Observados, train)","MAE (Observados, train)",
    "F1 (umbral calibracion)","MCC (umbral calibracion)",
    "Delta max tasa (Youden)","Delta max tasa (Calibracion)",
    "Pred. fuera [0,1] (union PEA)"
  ),
  Valor = c(
    fmt_n(c08$n_panel_total), fmt_n(c08$universo_pea$n_elegible),
    fmt_n(c08$universo_edad$n_elegible), fmt_n(c08$n_union),
    c08$modelo_origen, sprintf("%.6f (lambda.1se)", c08$lambda_1se),
    sprintf("%.4f", c08$umbral_youden), sprintf("%.4f", c08$umbral_calibracion),
    sprintf("%.4f", c08$auc_global_train), sprintf("%.4f", c08$mae_global_train),
    sprintf("%.4f", c08$metricas_calibracion$f1),
    sprintf("%.4f", c08$metricas_calibracion$mcc),
    sprintf("%.2f pp", c08$delta_max_pp),
    sprintf("%.2f pp", c08$metricas_calibracion$delta_max_pp),
    sprintf("%.1f%%", c08$pct_pred_fuera_01_union)
  )
)
kable(kpi, format="html", align=c("l","r"),
      caption="Indicadores clave del backcasting LPM -- Capa 5") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, position="center") %>%
  row_spec(c(9,14), bold=TRUE, background="#d4edda") %>%
  row_spec(13, bold=TRUE, background="#fff3cd") %>%
  column_spec(1, width="22em")
```

---

> **Embudo de comparabilidad:** Cada seccion expande el universo y la incertidumbre.
> **S1** valida en los 4 trimestres con medicion directa: primero sobre ocupados,
> luego sobre PEA completa. **S2** verifica coherencia historica vs. metodologia
> anterior (asalariados, con y sin pandemia). **S3** extiende a otras categorias
> con la misma logica de embudo. **S4** presenta las series completas.

---
', con)

# ── S1: VALIDACION DIRECTA ────────────────────────────────────────────────────
writeLines('
# Validacion directa -- 4 trimestres observados

> **Universo de referencia:** `r fmt_n(sum(comp_trim$n))` Observados — ocupados con
> formalidad registrada bajo la nueva metodologia INDEC (2024T4-2025T3).
>
> Esta seccion progresa en tres pasos:
> **1a** compara la tasa observada con la serie estimada para ocupados (universo mas
> comparable, deltas esperados pequenos). **1b** introduce la estimacion para PEA
> completa (incluye desocupados potenciales). **1c** descompone la diferencia entre
> ambas estimaciones, cuantificando la contribucion de los desocupados.

## S1a: Serie estimada ocupados vs. tasa observada

> **Universo de estimacion:** todos los ocupados en esos 4 trimestres =
> Observados + Backcasting (`r fmt_n(min(comp_s1$n_ocup))` a
> `r fmt_n(max(comp_s1$n_ocup))` obs por trimestre).
> La tasa de referencia es la observada sobre los
> `r fmt_n(sum(comp_trim$n))` Observados unicamente.
> Como ambos universos son ocupados, los deltas deberan ser pequenos.

```{r s1a_graf, fig.height=5}
s1a_long <- bind_rows(
  comp_trim %>% select(periodo_id, tasa=tasa_obs) %>%
    mutate(serie=tr("Observada (EPH)"), linea=tr("Observada")),
  comp_s1   %>% select(periodo_id, tasa=tasa_ocup_cal) %>%
    mutate(serie=LBL_ESTIM_CAL, linea=tr("Estimada"))
)
cols_s1a <- setNames(c(PAL_DESCRIPTIVO[2], COL_LPM),
                     c(tr("Observada (EPH)"), LBL_ESTIM_CAL))
p_s1a <- ggplot(s1a_long, aes(x=periodo_id, y=tasa, color=serie, group=serie, linetype=linea)) +
  geom_line(linewidth=1.0) + geom_point(size=3.0) +
  scale_color_manual(values=cols_s1a, name=NULL) +
  scale_linetype_manual(values=setNames(c("solid","dashed"), tr(c("Observada","Estimada"))), guide="none") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  tr_labs(title="S1a: Tasa observada vs. serie estimada -- Ocupados",
       subtitle=paste0("Umbral ", LBL_CAL, " | N estimacion incluye Backcasting adicional"),
       x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=20, hjust=1), legend.position="bottom")
guardar_figura(p_s1a, DIR_FIGURAS_08B_LPM, "s1a", 1)
p_s1a
```

```{r s1a_tabla}
comp_s1 %>%
  mutate(
    n_obs     = format(n_obs,  big.mark=","),
    n_ocup    = format(n_ocup, big.mark=","),
    tasa_obs  = paste0(round(tasa_obs    *100,2), "%"),
    tasa_ocup = paste0(round(tasa_ocup_cal*100,2), "%"),
    delta     = paste0(ifelse(delta_ocup_pp>=0,"+",""), delta_ocup_pp, " pp")
  ) %>%
  select(Trimestre=periodo_id,
         `N Observados`=n_obs, `Tasa obs.`=tasa_obs,
         `N Ocupados (back.)`=n_ocup, `Tasa ocup. (Cal.)`=tasa_ocup,
         `Delta (pp)`=delta) %>%
  kable(format="html", align=c("l","r","r","r","r","r"),
        caption=paste0("S1a: Tasa observada vs. estimacion sobre ocupados (", LBL_CAL, ")")) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  column_spec(6, bold=TRUE, background="#d4edda")
```

## S1b: Serie estimada PEA vs. tasa observada

> **Universo de estimacion:** Ocupados + Desocupados potencialmente formales
> (`r fmt_n(min(comp_s1$n_pea))` a `r fmt_n(max(comp_s1$n_pea))` obs por trimestre).
> Los desocupados tienen tasa formal ~8%, por lo que al incluirlos la tasa agregada
> baja y el delta respecto a la observada sube respecto a S1a.

```{r s1b_graf, fig.height=5}
s1b_long <- bind_rows(
  comp_trim %>% select(periodo_id, tasa=tasa_obs)      %>% mutate(serie=tr("Observada (EPH)")),
  comp_s1   %>% select(periodo_id, tasa=tasa_ocup_cal) %>% mutate(serie=tr("Estimada ocupados")),
  comp_s1   %>% select(periodo_id, tasa=tasa_pea_cal)  %>% mutate(serie=tr("Estimada PEA"))
)
p_s1b <- ggplot(s1b_long, aes(x=periodo_id, y=tasa, color=serie, group=serie, linetype=serie)) +
  geom_line(linewidth=0.9) + geom_point(size=2.5) +
  scale_color_manual(
    values=setNames(c(PAL_DESCRIPTIVO[2], COL_LPM, PAL_DESCRIPTIVO[1]),
                    tr(c("Observada (EPH)", "Estimada ocupados", "Estimada PEA"))),
    name=NULL) +
  scale_linetype_manual(
    values=setNames(c("solid", "dashed", "dotted"),
                    tr(c("Observada (EPH)", "Estimada ocupados", "Estimada PEA"))),
    name=NULL) +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  tr_labs(title="S1b: Tasa observada vs. estimaciones (Ocupados y PEA)",
       subtitle=paste0("Umbral ", LBL_CAL),
       x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=20, hjust=1), legend.position="bottom")
guardar_figura(p_s1b, DIR_FIGURAS_08B_LPM, "s1b", 2)
p_s1b
```

```{r s1b_tabla}
comp_s1 %>%
  mutate(
    tasa_obs  = paste0(round(tasa_obs    *100,2), "%"),
    tasa_ocup = paste0(round(tasa_ocup_cal*100,2), "%"),
    tasa_pea  = paste0(round(tasa_pea_cal *100,2), "%"),
    d_ocup    = paste0(ifelse(delta_ocup_pp>=0,"+",""), delta_ocup_pp, " pp"),
    d_pea     = paste0(ifelse(delta_pea_pp >=0,"+",""), delta_pea_pp,  " pp")
  ) %>%
  select(Trimestre=periodo_id,
         `Tasa obs.`=tasa_obs,
         `Ocup. (Cal.)`=tasa_ocup, `Delta ocup.`=d_ocup,
         `PEA (Cal.)`=tasa_pea,   `Delta PEA`=d_pea) %>%
  kable(format="html", align=c("l","r","r","r","r","r"),
        caption="S1b: Comparacion de deltas -- Ocupados vs. PEA") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  column_spec(4, bold=TRUE, background="#d4edda") %>%
  column_spec(6, bold=TRUE, background="#fff3cd")
```

## S1c: Descomposicion del delta -- contribucion de los desocupados

```{r s1c_tabla}
comp_s1 %>%
  mutate(
    n_ocup      = format(n_ocup,      big.mark=","),
    n_pea       = format(n_pea,       big.mark=","),
    n_desoc_pot = format(n_desoc_pot, big.mark=","),
    pct_desoc   = paste0(pct_desoc_pea, "%"),
    contrib     = paste0(ifelse(contrib_desoc>=0,"+",""), contrib_desoc, " pp")
  ) %>%
  select(Trimestre=periodo_id,
         `N Ocupados`=n_ocup, `N PEA`=n_pea,
         `N Desoc. pot.`=n_desoc_pot, `% desoc. en PEA`=pct_desoc,
         `Contribucion desoc. (pp)`=contrib) %>%
  kable(format="html", align=c("l","r","r","r","r","r"),
        caption="S1c: Contribucion de desocupados potenciales al delta PEA vs. Ocupados") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  column_spec(6, bold=TRUE, background="#fff3cd")
```

```{r s1c_barras, fig.height=4.5}
delta_df <- comp_s1 %>%
  select(periodo_id, delta_ocup_pp, contrib_desoc) %>%
  pivot_longer(-periodo_id, names_to="componente", values_to="pp") %>%
  mutate(componente = case_when(
    componente == "delta_ocup_pp" ~ tr("Delta ocupados vs obs."),
    componente == "contrib_desoc" ~ tr("Contribucion desocupados"),
    TRUE ~ componente
  ))
p_s1c <- ggplot(delta_df, aes(x=periodo_id, y=pp, fill=componente)) +
  geom_col(position="dodge", width=0.6) +
  geom_hline(yintercept=0, linewidth=0.4) +
  scale_fill_manual(
    values=setNames(c(COL_OBSERVADO, PAL_DESCRIPTIVO[3]),
                    tr(c("Delta ocupados vs obs.", "Contribucion desocupados"))),
    name=NULL) +
  scale_y_continuous(labels=function(x) paste0(x," pp")) +
  tr_labs(title="S1c: Descomposicion del delta -- Ocupados y PEA",
       subtitle=paste0("Delta ocupados: diferencia estimacion vs. observada\n",
                       "Contribucion desocupados: diferencia adicional al ampliar a PEA"),
       x=NULL, y="Diferencia (pp)") +
  theme_paper() +
  theme(legend.position="bottom")
guardar_figura(p_s1c, DIR_FIGURAS_08B_LPM, "s1c", 3)
p_s1c
```

## S1d: Metricas de clasificacion por umbral

```{r s1d_metricas}
tibble(
  Metrica  = c("Umbral","Accuracy","Sensibilidad","Especificidad",
               "F1","MCC","Delta max tasa (pp)"),
  v_youden = c(
    sprintf("%.4f", c08$umbral_youden),
    paste0(acc_youden,"%"), paste0(sens_youden,"%"), paste0(esp_youden,"%"),
    sprintf("%.4f", f1_youden), sprintf("%.4f", mcc_youden),
    paste0(delta_youden," pp")),
  v_cal    = c(
    sprintf("%.4f", c08$umbral_calibracion),
    paste0(acc_cal,"%"), paste0(sens_cal,"%"), paste0(esp_cal,"%"),
    sprintf("%.4f", f1_cal), sprintf("%.4f", mcc_cal),
    paste0(delta_cal," pp"))
) %>% {
  tbl <- .
  names(tbl)[2] <- sprintf("Youden (%.4f)", UMBRAL_YOUDEN)
  names(tbl)[3] <- sprintf("Calibracion (%.3g)", UMBRAL_CAL)
  tbl
} %>%
  kable(format="html", align=c("l","r","r"),
        caption=paste0("Metricas de clasificacion por umbral (sobre ",
                       fmt_n(sum(comp_trim$n))," Observados)")) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  row_spec(7, bold=TRUE, background="#d4edda") %>%
  column_spec(3, bold=TRUE)
```

> **Umbral recomendado para el paper:** calibracion (`r sprintf("%.3g", UMBRAL_CAL)`).
> Delta max = `r delta_cal` pp por trimestre.

---
', con)

# ── S2: COHERENCIA HISTORICA ──────────────────────────────────────────────────
writeLines('
# Coherencia historica -- Asalariados (2016T4-2025T3)

> **Logica:** `condicion_formalidad` mide formalidad directa para asalariados en toda
> la serie. Si el backcasting reproduce esa serie con precision, el modelo es confiable
> para extrapolar a categorias sin etiqueta (S3).
>
> **Universo:** `categoria_ocupacional == "Empleado"`, Observado o Backcasting.
> Umbral calibracion (`r sprintf("%.3g", UMBRAL_CAL)`).

## Serie backcasting vs. metodologia anterior

```{r s2_serie, fig.height=5.5}
s2_long <- bind_rows(
  serie_asal_vieja %>%
    select(periodo_id, formal=tasa_formal_viej, informal=tasa_infor_viej) %>%
    pivot_longer(-periodo_id, names_to="condicion", values_to="tasa") %>%
    mutate(fuente=tr("Metodologia anterior")),
  serie_asal_back %>%
    select(periodo_id, formal=tasa_formal_back, informal=tasa_infor_back) %>%
    pivot_longer(-periodo_id, names_to="condicion", values_to="tasa") %>%
    mutate(fuente=LBL_BACK_CAL)
) %>%
  mutate(
    condicion = case_when(
      condicion == "formal"   ~ tr("Formal"),
      condicion == "informal" ~ tr("Informal"),
      TRUE ~ condicion),
    serie = paste0(condicion, " -- ", fuente))

trim_ids_s2 <- sort(unique(s2_long$periodo_id))
n_s2        <- length(trim_ids_s2)
idx_obs_s2  <- which(trim_ids_s2 == TRIMESTRES_FORMALIDAD[1])
idx_pand_s2 <- which(trim_ids_s2 %in% TRIM_PANDEMIA)

# Canon formal/informal (D67):
#   Formal   → color canonico del modelo (solid)
#   Informal → version clara del mismo hue (dashed)
nms_s2    <- c(paste0(tr("Formal"),   " -- ", tr("Metodologia anterior")),
               paste0(tr("Informal"), " -- ", tr("Metodologia anterior")),
               paste0(tr("Formal"),   " -- ", LBL_BACK_CAL),
               paste0(tr("Informal"), " -- ", LBL_BACK_CAL))
cols_s2_4 <- setNames(c(COL_OBSERVADO, COL_OBSERVADO_CLARO, COL_LPM, COL_LPM_CLARO), nms_s2)
lty_s2_4  <- setNames(c("solid",       "dashed",             "solid", "dashed"),       nms_s2)

p_s2 <- ggplot(s2_long,
               aes(x=periodo_id, y=tasa, color=serie, linetype=serie, group=serie)) +
  annotate("rect",
           xmin=min(idx_pand_s2)-0.5, xmax=max(idx_pand_s2)+0.5,
           ymin=-Inf, ymax=Inf, fill=PAL_DESCRIPTIVO[5], alpha=0.10) +
  annotate("rect",
           xmin=idx_obs_s2-0.5, xmax=n_s2+0.5,
           ymin=-Inf, ymax=Inf, fill=PAL_DESCRIPTIVO[3], alpha=0.08) +
  geom_line(linewidth=0.9) + geom_point(size=1.6) +
  scale_color_manual(values=cols_s2_4, name=NULL) +
  scale_linetype_manual(values=lty_s2_4, name=NULL) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_x_discrete(breaks=trim_ids_s2[seq(1,n_s2,by=4)]) +
  tr_labs(title="S2: Backcasting asalariados vs. metodologia anterior",
       subtitle="Zona naranja = observados | Zona gris = trimestres pandemia (COVID)",
       x=NULL, y="Tasa") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7),
        legend.position="bottom")
guardar_figura(p_s2, DIR_FIGURAS_08B_LPM, "s2", 4)
p_s2
```

## Metricas de ajuste: serie completa vs. sin pandemia

```{r s2_metricas}
tibble(
  Metrica          = c("Trimestres comparados","Correlacion (formal)",
                       "MAE (pp)","RMSE (pp)","Delta max (pp)"),
  `Serie completa` = c(as.character(met_total$n_trim),
                       sprintf("%.4f",met_total$corr),
                       sprintf("%.2f",met_total$mae),
                       sprintf("%.2f",met_total$rmse),
                       sprintf("%.2f",met_total$max_delta)),
  `Sin pandemia`   = c(sprintf("%d (-%d)",met_sin_p$n_trim,
                               met_total$n_trim-met_sin_p$n_trim),
                       sprintf("%.4f",met_sin_p$corr),
                       sprintf("%.2f",met_sin_p$mae),
                       sprintf("%.2f",met_sin_p$rmse),
                       sprintf("%.2f",met_sin_p$max_delta)),
  Mejora           = c(sprintf("-%d trims", n_trim_pandemia),
                       sprintf("%+.4f",met_sin_p$corr -met_total$corr),
                       sprintf("%+.2f pp",met_total$mae  -met_sin_p$mae),
                       sprintf("%+.2f pp",met_total$rmse -met_sin_p$rmse),
                       sprintf("%+.2f pp",met_total$max_delta-met_sin_p$max_delta))
) %>%
  kable(format="html", align=c("l","r","r","r"),
        caption=paste0("Metricas de ajuste: serie completa vs. sin pandemia (",
                       n_trim_pandemia," trims COVID excluidos)")) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  row_spec(2, bold=TRUE, background="#d4edda") %>%
  column_spec(4, bold=TRUE)
```

```{r s2_delta_tabla}
serie_asal_comp %>%
  mutate(
    tasa_formal_viej=paste0(round(tasa_formal_viej*100,2),"%"),
    tasa_infor_viej =paste0(round(tasa_infor_viej *100,2),"%"),
    tasa_formal_back=paste0(round(tasa_formal_back*100,2),"%"),
    tasa_infor_back =paste0(round(tasa_infor_back *100,2),"%"),
    delta_formal_pp =paste0(ifelse(delta_formal_pp>=0,"+",""),
                             round(delta_formal_pp,2)," pp"),
    flag=case_when(es_observado~"[OBS]",es_pandemia~"[PND]",TRUE~"")
  ) %>%
  select(Trimestre=periodo_id, Flag=flag,
         `Formal (vieja)`=tasa_formal_viej, `Informal (vieja)`=tasa_infor_viej,
         `Formal (back.)`=tasa_formal_back, `Informal (back.)`=tasa_infor_back,
         `Delta formal`=delta_formal_pp) %>%
  kable(format="html", align=c("l","c","r","r","r","r","r"),
        caption="S2: Tasas por trimestre. [OBS]=observado, [PND]=pandemia COVID.") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  column_spec(7, bold=TRUE)
```

---
', con)

# ── S3: OTRAS CATEGORIAS ─────────────────────────────────────────────────────
writeLines('
# Extension -- Otras categorias ocupacionales

> **Racionalidad:** S1-S2 establecen precision sobre asalariados. La confianza
> en cuenta propistas, patrones y otros se deduce de ese resultado.
> Se sigue la misma logica de embudo: primero solo ocupados (S3a), luego
> incluyendo desocupados potenciales segun ultimo empleo (S3b).

## S3a: Solo ocupados

```{r s3a_global}
tab_cat_global %>%
  mutate(n_total   =format(n_total,   big.mark=","),
         n_formal  =format(n_formal,  big.mark=","),
         n_informal=format(n_informal,big.mark=","),
         tasa_formal=paste0(round(tasa_formal*100,1),"%")) %>%
  rename(Categoria=cat_simple,`N total`=n_total,
         `N formal`=n_formal,`N informal`=n_informal,
         `Tasa formal (Cal.)`=tasa_formal) %>%
  kable(format="html", align=c("l","r","r","r","r"),
        caption=paste0("S3a: Formalidad por categoria -- solo ocupados (", LBL_CAL, ")")) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  row_spec(which(tab_cat_global$cat_simple=="Asalariado"),
           bold=TRUE, background="#d4edda")
```

```{r s3a_serie, fig.height=5}
trim_ids_cat <- sort(unique(serie_cat$periodo_id))
n_trim_cat   <- length(trim_ids_cat)
idx_obs_cat  <- which(trim_ids_cat == TRIMESTRES_FORMALIDAD[1])

p_s3a_serie <- serie_cat %>%
  filter(cat_simple %in% cats_orden) %>%
  mutate(cat_simple=factor(cat_simple, levels=cats_orden)) %>%
  ggplot(aes(x=periodo_id, y=tasa, color=cat_simple, group=cat_simple)) +
  annotate("rect", xmin=idx_obs_cat-0.5, xmax=n_trim_cat+0.5,
           ymin=-Inf, ymax=Inf, fill=PAL_DESCRIPTIVO[3], alpha=0.08) +
  geom_line(linewidth=0.9) + geom_point(size=1.6) +
  scale_color_manual(values=colores_cat, name=tr("Categoria")) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_x_discrete(breaks=trim_ids_cat[seq(1,n_trim_cat,by=4)]) +
  tr_labs(title="S3a: Tasa de formalidad por categoria -- solo ocupados",
       subtitle=paste0("Umbral ", LBL_CAL, " | Zona sombreada = trimestres observados"),
       x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7),
        legend.position="bottom")
guardar_figura(p_s3a_serie, DIR_FIGURAS_08B_LPM, "s3a_serie", 5)
p_s3a_serie
```

```{r s3a_comp, fig.height=5.5}
colores_cond <- setNames(c(COL_LPM, PAL_DESCRIPTIVO[3]), tr(c("Formal","Informal")))
p_s3a_comp <- comp_cat_trim %>%
  filter(cat_simple %in% cats_orden) %>%
  mutate(cat_simple=factor(cat_simple, levels=cats_orden),
         condicion=tr(condicion)) %>%
  ggplot(aes(x=periodo_id, y=pct, fill=condicion)) +
  geom_col(width=0.85) +
  facet_wrap(~cat_simple, ncol=2) +
  scale_fill_manual(values=colores_cond, name=NULL) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  tr_labs(title="S3a: Composicion formal/informal por categoria -- solo ocupados",
       subtitle=paste0("Umbral ", LBL_CAL), x=NULL, y="Proporcion") +
  scale_x_discrete(breaks=trim_ids_cat[seq(1, n_trim_cat, by=4)]) +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=6),
        strip.text=element_text(size=7, lineheight=0.9),
        legend.position="bottom")
guardar_figura(p_s3a_comp, DIR_FIGURAS_08B_LPM, "s3a_comp", 6)
p_s3a_comp
```

## S3b: Incluyendo desocupados potenciales (por ultimo empleo)

> Los desocupados llevan `categoria_ocupacional` de su ultimo empleo (coalesce <=3
> anios para quienes tienen experiencia reciente). Al incluirlos, el universo crece
> y la tasa formal de cada categoria se ve afectada por el perfil de los desocupados
> de ese sector. La tabla de comparacion cuantifica el impacto por categoria.

```{r s3b_global}
tab_cat_global_pea %>%
  mutate(n_total =format(n_total, big.mark=","),
         n_ocup  =format(n_ocup,  big.mark=","),
         n_desoc =format(n_desoc, big.mark=","),
         pct_desoc=paste0(pct_desoc,"%"),
         tasa_formal=paste0(round(tasa_formal*100,1),"%")) %>%
  rename(Categoria=cat_simple, `N total`=n_total,
         `N ocupados`=n_ocup, `N desoc. pot.`=n_desoc,
         `% desoc.`=pct_desoc, `Tasa formal`=tasa_formal) %>%
  kable(format="html", align=c("l","r","r","r","r","r"),
        caption="S3b: Formalidad por categoria -- ocupados + desocupados potenciales") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  row_spec(which(tab_cat_global_pea$cat_simple=="Asalariado"),
           bold=TRUE, background="#d4edda")
```

```{r s3b_delta}
tab_cat_global %>%
  select(cat_simple, tasa_ocup=tasa_formal) %>%
  left_join(tab_cat_global_pea %>% select(cat_simple, tasa_pea=tasa_formal),
            by="cat_simple") %>%
  mutate(delta     =round((tasa_pea-tasa_ocup)*100,2),
         tasa_ocup =paste0(round(tasa_ocup*100,1),"%"),
         tasa_pea  =paste0(round(tasa_pea *100,1),"%"),
         delta_fmt =paste0(ifelse(delta>=0,"+",""),delta," pp")) %>%
  select(-delta) %>%
  rename(Categoria=cat_simple,
         `Tasa solo ocup.`=tasa_ocup,
         `Tasa + desoc. pot.`=tasa_pea,
         `Impacto (pp)`=delta_fmt) %>%
  kable(format="html", align=c("l","r","r","r"),
        caption="S3: Impacto de incluir desocupados potenciales por categoria") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  column_spec(4, bold=TRUE, background="#fff3cd")
```

```{r s3b_serie, fig.height=5.5}
serie_comp_cat <- bind_rows(
  serie_cat     %>% mutate(universo=tr("Solo ocupados")),
  serie_cat_pea %>% select(periodo_id,cat_simple,tasa) %>%
    mutate(universo=tr("Ocupados + Desoc. pot."))
) %>% filter(cat_simple %in% cats_orden) %>%
  mutate(cat_simple=factor(cat_simple, levels=cats_orden))

trim_ids_s3b <- sort(unique(serie_comp_cat$periodo_id))
n_s3b        <- length(trim_ids_s3b)
idx_obs_s3b  <- which(trim_ids_s3b == TRIMESTRES_FORMALIDAD[1])

p_s3b <- ggplot(serie_comp_cat,
       aes(x=periodo_id, y=tasa, color=universo, linetype=universo, group=universo)) +
  annotate("rect", xmin=idx_obs_s3b-0.5, xmax=n_s3b+0.5,
           ymin=-Inf, ymax=Inf, fill=PAL_DESCRIPTIVO[3], alpha=0.08) +
  geom_line(linewidth=0.9) + geom_point(size=1.4) +
  facet_wrap(~cat_simple, ncol=2, scales="free_y") +
  scale_color_manual(
    values=setNames(c(COL_OBSERVADO, PAL_DESCRIPTIVO[3]),
                    tr(c("Solo ocupados", "Ocupados + Desoc. pot."))),
    name=NULL) +
  scale_linetype_manual(
    values=setNames(c("solid","dashed"),
                    tr(c("Solo ocupados", "Ocupados + Desoc. pot."))),
    name=NULL) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_x_discrete(breaks=trim_ids_s3b[seq(1,n_s3b,by=4)]) +
  tr_labs(title="S3b: Tasa de formalidad por categoria -- ocupados vs. PEA",
       subtitle=paste0("Umbral ", LBL_CAL, " | Escala libre por categoria"),
       x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=6),
        strip.text=element_text(size=7, lineheight=0.9),
        legend.position="bottom")
guardar_figura(p_s3b, DIR_FIGURAS_08B_LPM, "s3b", 7)
p_s3b
```

---
', con)

# ── S4: SERIES COMPLETAS ─────────────────────────────────────────────────────
writeLines('
# Series temporales completas

> **Tres universos progresivos:** la confianza en cada expansion descansa en S1-S3.
> Umbral recomendado para el paper: **`r LBL_CAL`**.
> Series diferenciadas por grosor (Edad 18-60 > PEA > Ocupados) y tipo de linea.

## Umbral Youden

```{r s4_youden, fig.height=5.5}
trim_ids    <- sort(unique(serie_ocupados$periodo_id))
n_trim      <- length(trim_ids)
idx_ini_obs <- which(trim_ids == TRIMESTRES_FORMALIDAD[1])

series_youden <- bind_rows(
  serie_ocupados %>% select(periodo_id, tasa=tasa_youden)      %>% mutate(serie=tr("Ocupados")),
  serie_pea      %>% select(periodo_id, tasa=tasa_youden)      %>% mutate(serie=tr("PEA")),
  serie_edad     %>% select(periodo_id, tasa=tasa_youden)      %>% mutate(serie=tr("Edad 18-60"))
) %>% mutate(serie=factor(serie, levels=tr(c("Ocupados","PEA","Edad 18-60"))))

p_s4_you <- ggplot(series_youden,
       aes(x=periodo_id, y=tasa,
           linetype=serie, linewidth=serie, shape=serie, group=serie)) +
  annotate("rect", xmin=idx_ini_obs-0.5, xmax=n_trim+0.5,
           ymin=-Inf, ymax=Inf, fill=PAL_DESCRIPTIVO[3], alpha=0.08) +
  geom_line(color=COL_LPM) +
  geom_point(color=COL_LPM, size=1.8) +
  scale_linetype_manual(values=linetypes_series,  name=tr("Universo")) +
  scale_linewidth_manual(values=linewidths_series, name=tr("Universo"), guide="none") +
  scale_shape_manual(values=shapes_series,         name=tr("Universo")) +
  scale_y_continuous(labels=percent_format(accuracy=0.1), limits=c(0,NA)) +
  scale_x_discrete(breaks=trim_ids[seq(1,n_trim,by=4)]) +
  tr_labs(title=paste0("S4: Series de formalidad -- ", LBL_YOUDEN),
       subtitle="Zona sombreada = 4 trimestres con formalidad observada",
       x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7),
        legend.position="bottom")
guardar_figura(p_s4_you, DIR_FIGURAS_08B_LPM, "s4_youden", 8)
p_s4_you
```

## Umbral Calibracion

```{r s4_cal, fig.height=5.5}
series_cal <- bind_rows(
  serie_ocupados %>% select(periodo_id, tasa=tasa_calibracion) %>% mutate(serie=tr("Ocupados")),
  serie_pea      %>% select(periodo_id, tasa=tasa_calibracion) %>% mutate(serie=tr("PEA")),
  serie_edad     %>% select(periodo_id, tasa=tasa_calibracion) %>% mutate(serie=tr("Edad 18-60"))
) %>% mutate(serie=factor(serie, levels=tr(c("Ocupados","PEA","Edad 18-60"))))

p_s4_cal <- ggplot(series_cal,
       aes(x=periodo_id, y=tasa,
           linetype=serie, linewidth=serie, shape=serie, group=serie)) +
  annotate("rect", xmin=idx_ini_obs-0.5, xmax=n_trim+0.5,
           ymin=-Inf, ymax=Inf, fill=PAL_DESCRIPTIVO[3], alpha=0.08) +
  geom_line(color=COL_LPM) +
  geom_point(color=COL_LPM, size=1.8) +
  scale_linetype_manual(values=linetypes_series,  name=tr("Universo")) +
  scale_linewidth_manual(values=linewidths_series, name=tr("Universo"), guide="none") +
  scale_shape_manual(values=shapes_series,         name=tr("Universo")) +
  scale_y_continuous(labels=percent_format(accuracy=0.1), limits=c(0,NA)) +
  scale_x_discrete(breaks=trim_ids[seq(1,n_trim,by=4)]) +
  tr_labs(title=paste0("S4: Series de formalidad -- Umbral Calibracion (", LBL_CAL, ")"),
       subtitle="Zona sombreada = 4 trimestres con formalidad observada",
       x=NULL, y="Tasa de formalidad") +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7),
        legend.position="bottom")
guardar_figura(p_s4_cal, DIR_FIGURAS_08B_LPM, "s4_cal", 9)
p_s4_cal
```

## Tabla comparativa de universos (`r LBL_CAL`)

```{r s4_tabla}
tab_series <- serie_ocupados %>%
  select(Trimestre=periodo_id, N_ocup=n, Ocupados=tasa_calibracion) %>%
  left_join(serie_pea  %>% select(periodo_id, N_pea=n, PEA=tasa_calibracion),
            by=c("Trimestre"="periodo_id")) %>%
  left_join(serie_edad %>% select(periodo_id, N_edad=n, Edad18_60=tasa_calibracion),
            by=c("Trimestre"="periodo_id")) %>%
  mutate(
    gap_pea  = round((PEA-Ocupados)*100,2),
    gap_edad = round((Edad18_60-Ocupados)*100,2),
    es_obs   = Trimestre %in% TRIM_OBS,
    N_ocup   = format(N_ocup, big.mark=","),
    N_pea    = format(N_pea,  big.mark=","),
    N_edad   = format(N_edad, big.mark=","),
    Ocupados  = paste0(round(Ocupados *100,1),"%"),
    PEA       = paste0(round(PEA      *100,1),"%"),
    Edad18_60 = paste0(round(Edad18_60*100,1),"%"),
    gap_pea   = paste0(ifelse(gap_pea >=0,"+",""),gap_pea, " pp"),
    gap_edad  = paste0(ifelse(gap_edad>=0,"+",""),gap_edad," pp")
  )

tab_series %>%
  select(Trimestre, N_ocup, Ocupados, N_pea, PEA, `Gap PEA`=gap_pea,
         N_edad, `Edad 18-60`=Edad18_60, `Gap Edad`=gap_edad) %>%
  kable(format="html", align=c("l","r","r","r","r","r","r","r","r"),
        caption=paste0("S4: Comparacion de universos -- Umbral calibracion (", LBL_CAL, ")")) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=12) %>%
  row_spec(which(tab_series$es_obs), bold=TRUE, background="#fff3cd") %>%
  column_spec(c(6,9), bold=TRUE)
```

---
', con)

# ── S5: FORMALIDAD POTENCIAL ─────────────────────────────────────────────────
writeLines('
# Formalidad potencial

## Desocupados

```{r s5_desoc}
# HC documentado: theta_A medio desoc./obs. y pct_fuera no guardados en c08.
# Valores provienen de c08$nota_desocupados (texto) + corrida de 08_backcasting_LPM.R.
tibble(
  Metrica=c("N elegible","Tasa formal (Youden)","Tasa formal (Calibracion)",
            "Pred. fuera [0,1]","theta_A medio (desocupados)",
            "theta_A medio (Observados)"),
  `Universo PEA`=c(
    fmt_n(n_desoc_pea),
    paste0(round(c08$universo_pea$tasa_formal_desocupados_youden*100,1),"%"),
    paste0(round(c08$universo_pea$tasa_formal_desocupados_cal   *100,1),"%"),
    "~81%",   # HC documentado: no guardado en c08. Ver nota_desocupados.
    "~0.99",  # HC documentado: no guardado en c08. Ver nota_desocupados.
    "~0.14"   # HC documentado: no guardado en c08. Ver nota_desocupados.
  )
) %>%
  kable(format="html", align=c("l","r"),
        caption="Desocupados: formalidad potencial") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  row_spec(4, background="#fff3cd")
```

> `r c08$nota_desocupados`
>
> **Advertencia:** ~81% de predicciones fuera de [0,1]. Extrapola fuera del soporte
> del training para desocupados con theta_A alto. Resultados con cautela; GLM es mas
> apropiado para este subgrupo.

## Inactivos 18-60

```{r s5_inact}
tibble(
  Metrica=c("N elegible","Tasa formal (Youden)","Tasa formal (Calibracion)"),
  Valor=c(
    fmt_n(c08$n_solo_edad),
    paste0(round(c08$universo_edad$tasa_formal_inactivos_youden*100,1),"%"),
    paste0(round(c08$universo_edad$tasa_formal_inactivos_cal   *100,1),"%")
  )
) %>%
  kable(format="html", align=c("l","r"),
        caption="Inactivos 18-60: formalidad potencial") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE)
```

> `r c08$nota_inactivos`

---
', con)

# ── S6: COBERTURA ────────────────────────────────────────────────────────────
writeLines('
# Cobertura del Backcasting

## Universo PEA

```{r s6_cob_graf, fig.height=5}
tipos_orden <- tr(c("Observado","Backcasting","Desoc. potencial formal","No aplica","Sin theta"))
colores_tipos <- setNames(
  c(COL_OBSERVADO, COL_LPM, PAL_DESCRIPTIVO[3], COL_BANDA, PAL_DESCRIPTIVO[5]),
  tipos_orden
)

p_s6 <- cob_pea %>%
  mutate(tipo_label=tr(tipo_label)) %>%
  mutate(tipo_label=factor(tipo_label, levels=tipos_orden)) %>%
  ggplot(aes(x=periodo_id, y=n/1e3, fill=tipo_label)) +
  geom_col(width=0.8) +
  scale_fill_manual(values=colores_tipos, name=NULL) +
  scale_y_continuous(labels=comma_format(suffix=" k")) +
  tr_labs(title="Cobertura por tipo de estimacion -- Universo PEA",
       subtitle="Observaciones por trimestre (miles)", x=NULL, y="Obs (miles)") +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_paper() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7),
        legend.position="bottom")
guardar_figura(p_s6, DIR_FIGURAS_08B_LPM, "s6_cob", 10)
p_s6
```

```{r s6_tab_pea}
# Computado desde c08$universo_pea$resumen_tipos (no HC)
tipos_lbl_pea <- c(
  "Backcasting"         = "Backcasting",
  "Observado"           = "Observado",
  "Potencial_desocupado"= "Desoc. potencial formal",
  "No_aplica"           = "No aplica",
  "Sin_theta"           = "Sin theta"
)
orden_pea <- c("Backcasting","Observado","Potencial_desocupado","No_aplica","Sin_theta")

tab_pea <- c08$universo_pea$resumen_tipos %>%
  filter(tipo_estimacion_pea %in% names(tipos_lbl_pea)) %>%
  mutate(
    Tipo   = factor(tipos_lbl_pea[tipo_estimacion_pea],
                    levels = tipos_lbl_pea[orden_pea]),
    N      = fmt_n(n),
    pct_fmt= ifelse(n < 50, "~0%", paste0(round(pct, 1), "%")),
    tasa_y = ifelse(is.nan(tasa_formal_youden), "---",
                    paste0(round(tasa_formal_youden, 1), "%")),
    tasa_c = ifelse(is.nan(tasa_formal_cal), "---",
                    paste0(round(tasa_formal_cal, 1), "%"))
  ) %>%
  arrange(Tipo) %>%
  select(Tipo, N, `% panel`=pct_fmt,
         `Tasa formal (Youden)`=tasa_y, `Tasa formal (Cal.)`=tasa_c)

tab_pea %>%
  kable(format="html", align=c("l","r","r","r","r"),
        caption="Distribucion por tipo -- Universo PEA") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  row_spec(1:2, bold=TRUE, background="#d4edda") %>%
  row_spec(3, background="#fff3cd")
```

## Universo Edad 18-60

```{r s6_tab_edad}
# Computado desde c08$universo_edad$resumen_tipos (no HC)
tipos_lbl_edad <- c(
  "Backcasting"         = "Backcasting",
  "Observado"           = "Observado",
  "Potencial_desocupado"= "Desoc. potencial formal",
  "Potencial_inactivo"  = "Inactivo potencial formal",
  "No_aplica"           = "No aplica",
  "Sin_theta"           = "Sin theta"
)
orden_edad <- c("Backcasting","Observado","Potencial_desocupado",
                "Potencial_inactivo","No_aplica","Sin_theta")

tab_edad <- c08$universo_edad$resumen_tipos %>%
  filter(tipo_estimacion_edad %in% names(tipos_lbl_edad)) %>%
  mutate(
    Tipo   = factor(tipos_lbl_edad[tipo_estimacion_edad],
                    levels = tipos_lbl_edad[orden_edad]),
    N      = fmt_n(n),
    pct_fmt= paste0(round(pct, 1), "%"),
    tasa_y = ifelse(is.nan(tasa_formal_youden), "---",
                    paste0(round(tasa_formal_youden, 1), "%")),
    tasa_c = ifelse(is.nan(tasa_formal_cal), "---",
                    paste0(round(tasa_formal_cal, 1), "%"))
  ) %>%
  arrange(Tipo) %>%
  select(Tipo, N, `% panel`=pct_fmt,
         `Tasa formal (Youden)`=tasa_y, `Tasa formal (Cal.)`=tasa_c)

tab_edad %>%
  kable(format="html", align=c("l","r","r","r","r"),
        caption="Distribucion por tipo -- Universo Edad 18-60") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  row_spec(1:2, bold=TRUE, background="#d4edda") %>%
  row_spec(3:4, background="#fff3cd")
```

---
', con)

# ── S7: NOTAS METODOLOGICAS ──────────────────────────────────────────────────
writeLines('
# Notas Metodologicas

## Dos umbrales de clasificacion

> `r c08$nota_calibracion`

## Predicciones fuera de [0,1]

```{r s7_pred}
tibble(
  Contexto=c(
    paste0("Test set 07a (cv.glmnet, ", fmt_n(c07a$n_test), " obs)"),
    paste0("Observados backcasting (", fmt_n(sum(comp_trim$n)), " obs)"),
    "PEA completa -- union"),
  `% fuera [0,1]`=c(
    sprintf("%.2f%%", pct_fuera_01_test),
    sprintf("%.1f%%", pct_fuera_01_obs_back),
    paste0(round(c08$pct_pred_fuera_01_union,1),"%")),
  Referencia=c(
    "c07a$pct_pred_fuera_01",
    "Referencia correcta para el paper (calculado desde panel)",
    "Incluye backcasting historico")
) %>%
  kable(format="html", align=c("l","r","l"),
        caption="Predicciones fuera de [0,1] en distintos contextos") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE) %>%
  row_spec(2, bold=TRUE, background="#d4edda")
```

> Angrist & Pischke (2009, MHE): valores fuera [0,1] no son deficiencia del LPM.
> Clipeados a [0,1] antes de clasificar.

## AUC backcasting vs. AUC test 07a

AUC backcasting (**`r sprintf("%.4f", c08$auc_global_train)`**) < test 07a (**`r sprintf("%.4f", c07a$auc_test)`**).
No es degradacion: (1) pool train+test vs. solo test; (2) cv.glmnet vs. OLS; (3) sin ponderacion normalizada.

## Limitaciones

```{r s7_limit}
tibble(
  ID=paste0("L",1:5),
  Limitacion=c("Hold-out no disponible","Desocupados -- extrapolacion masiva",
               "Inactivos -- imputacion laboral","Pred. fuera [0,1] (LPM estructural)",
               "Clustering EPH"),
  Detalle=c(
    "Formalidad observada solo en 2024T4-2025T3. Validacion temporal restringida al training.",
    "theta_A medio~0.99 (vs ~0.14 en Observados). ~81% pred. fuera [0,1]. GLM mas apropiado.",
    "Variables laborales imputadas con moda del training. Tasa refleja theta+educacion+demografia.",
    "Propiedad estructural LPM. Angrist & Pischke (2009). Clipeadas a [0,1] antes de clasificar.",
    "Mismo hogar (codusu) hasta 2 veces en training. Corregido con foldid (CV) y vcovCL (OLS) en 07b."
  )
) %>%
  kable(format="html", align=c("c","l","l"),
        caption="Limitaciones metodologicas del backcasting LPM") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=TRUE, font_size=12) %>%
  column_spec(3, width="40em")
```

---
', con)

# ── CONCLUSION ────────────────────────────────────────────────────────────────
writeLines('
# Conclusion {.unnumbered}

El backcasting LPM aplica el modelo 07b al panel completo 2016T4-2025T3
(`r fmt_n(c08$n_union)` obs elegibles). El reporte construye confianza en cuatro escalones:

**S1** valida en 4 trimestres observados. La serie de ocupados tiene delta
`r sprintf("%.2f pp", mean(abs(comp_s1$delta_ocup_pp)))` promedio respecto a la tasa
observada; al ampliar a PEA el delta sube a
`r sprintf("%.2f pp", mean(abs(comp_s1$delta_pea_pp)))` por el efecto dilutivo de
los desocupados potenciales
(`r round(mean(comp_s1$pct_desoc_pea),1)`% del universo PEA en training, tasa formal ~8%).

**S2** verifica coherencia historica sobre asalariados con la metodologia anterior:
correlacion `r sprintf("%.4f", met_total$corr)` (serie completa) y
`r sprintf("%.4f", met_sin_p$corr)` (sin pandemia COVID), MAE
`r sprintf("%.2f pp", met_total$mae)` y
`r sprintf("%.2f pp", met_sin_p$mae)` respectivamente.
Las mayores desviaciones se concentran en trimestres con cambios metodologicos COVID.

**S3** extiende a cuenta propia, patron y otros con la misma logica de embudo.
En cada categoria se explicita el impacto de incluir desocupados potenciales.

**S4** presenta las tres series completas. Umbral calibracion (`r LBL_CAL`) recomendado.

**Siguiente paso:** Script 09a -- Comparativo retropredictivo LPM/GLM/SLS.
', con)

close(con)
cat("   [OK] Rmd escrito:", rmd_temp, "\n\n")


# 🪫 4. RENDER HTML -------------------------------------------------------------
cat("-- 4. Renderizando HTML -----------------------------------------------\n")

rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_08B_HTML,
  quiet       = TRUE,
  envir       = environment()
)
unlink(rmd_temp)
cat("   [OK] HTML generado:", PATH_08B_HTML, "\n\n")


# 🪫 5. GENERAR TXT PARA EL PAPER -----------------------------------------------
cat("-- 5. Generando TXT para el paper ------------------------------------\n")

txt_lines <- c(
  "# =================================================================",
  "# NOTAS PARA EL PAPER -- Backcasting LPM",
  paste0("# Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "# Script:   08b_reporte_backcasting_LPM.R (Capa 5)",
  "# =================================================================",
  "",
  "## MODELO APLICADO",
  paste0("Modelo origen:       ", c08$modelo_origen),
  paste0("Lambda (1se):        ", sprintf("%.6f", c08$lambda_1se)),
  paste0("Umbral Youden:       ", sprintf("%.4f", c08$umbral_youden)),
  paste0("Umbral calibracion:  ", sprintf("%.4f", c08$umbral_calibracion)),
  "",
  "## COBERTURA BACKCASTING",
  paste0("N panel total:       ", fmt_n(c08$n_panel_total)),
  paste0("N elegible PEA:      ", fmt_n(c08$universo_pea$n_elegible)),
  paste0("N elegible Edad:     ", fmt_n(c08$universo_edad$n_elegible)),
  paste0("N union:             ", fmt_n(c08$n_union)),
  paste0("N solo PEA:          ", fmt_n(c08$n_solo_pea)),
  paste0("N solo Edad (inact): ", fmt_n(c08$n_solo_edad)),
  paste0("N ambos:             ", fmt_n(c08$n_ambos)),
  "",
  "## VALIDACION S1 -- Ocupados (3 trims)",
  paste0("AUC global:           ", sprintf("%.4f", c08$auc_global_train)),
  paste0("MAE global:           ", sprintf("%.4f", c08$mae_global_train)),
  paste0("F1 (Cal.):            ", sprintf("%.4f", c08$metricas_calibracion$f1)),
  paste0("MCC (Cal.):           ", sprintf("%.4f", c08$metricas_calibracion$mcc)),
  paste0("Delta max (Cal.):     ", sprintf("%.2f pp", c08$metricas_calibracion$delta_max_pp)),
  paste0("Delta ocup. prom.:    ", sprintf("%.2f pp", mean(abs(comp_s1$delta_ocup_pp)))),
  paste0("Delta PEA prom.:      ", sprintf("%.2f pp", mean(abs(comp_s1$delta_pea_pp)))),
  paste0("% desoc. en PEA:      ", sprintf("%.1f%%", mean(comp_s1$pct_desoc_pea))),
  paste0("Pred fuera [0,1]:     ", sprintf("%.1f%%", c08$pct_pred_fuera_01_union)," (union PEA)"),
  "",
  "## S1d METRICAS POR UMBRAL",
  paste0("  [fuente sens/esp Youden: c07b$metricas_clf]"),
  "                     Youden       Calibracion",
  paste0("Umbral:              ",
         sprintf("%.4f", c08$umbral_youden), "       ",
         sprintf("%.4f", c08$umbral_calibracion)),
  paste0("Accuracy:            ", acc_youden, "%       ", acc_cal, "%"),
  paste0("Sensibilidad:        ", sens_youden, "%       ", sens_cal, "%"),
  paste0("Especificidad:       ", esp_youden, "%       ", esp_cal, "%"),
  paste0("F1:                  ", sprintf("%.4f", f1_youden), "       ", sprintf("%.4f", f1_cal)),
  paste0("MCC:                 ", sprintf("%.4f", mcc_youden), "       ", sprintf("%.4f", mcc_cal)),
  paste0("Delta max tasa:      ", delta_youden, " pp      ", delta_cal, " pp"),
  "",
  "## COHERENCIA HISTORICA S2 -- Asalariados",
  paste0("Trims comparados:      ", met_total$n_trim),
  paste0("Trims pandemia:        ", n_trim_pandemia),
  paste0("Correlacion (total):   ", sprintf("%.4f", met_total$corr)),
  paste0("Correlacion (sin p.):  ", sprintf("%.4f", met_sin_p$corr)),
  paste0("MAE (total):           ", sprintf("%.2f pp", met_total$mae)),
  paste0("MAE (sin pand.):       ", sprintf("%.2f pp", met_sin_p$mae)),
  paste0("RMSE (total):          ", sprintf("%.2f pp", met_total$rmse)),
  paste0("RMSE (sin pand.):      ", sprintf("%.2f pp", met_sin_p$rmse)),
  paste0("Delta max (total):     ", sprintf("%.2f pp", met_total$max_delta)),
  paste0("Delta max (sin pand.): ", sprintf("%.2f pp", met_sin_p$max_delta)),
  "",
  "## DESGLOSE CATEGORIA OCUPACIONAL (Cal.)",
  {
    tab_cat_global %>%
      mutate(linea=paste0(cat_simple,": tasa=",round(tasa_formal*100,1),
                          "%, N=",format(n_total, big.mark=","))) %>%
      pull(linea)
  },
  "",
  "## FORMALIDAD POTENCIAL",
  paste0("Desoc (PEA) N:       ", fmt_n(n_desoc_pea)),
  paste0("Desoc tasa (Youden): ",
         sprintf("%.1f%%", c08$universo_pea$tasa_formal_desocupados_youden*100)),
  paste0("Desoc tasa (Cal.):   ",
         sprintf("%.1f%%", c08$universo_pea$tasa_formal_desocupados_cal*100)),
  paste0("Inact (Edad) N:      ", fmt_n(c08$n_solo_edad)),
  paste0("Inact tasa (Youden): ",
         sprintf("%.1f%%", c08$universo_edad$tasa_formal_inactivos_youden*100)),
  paste0("Inact tasa (Cal.):   ",
         sprintf("%.1f%%", c08$universo_edad$tasa_formal_inactivos_cal*100)),
  "",
  "## PRED FUERA [0,1]",
  paste0("Test set 07a:        ", sprintf("%.2f%%", pct_fuera_01_test),
         " (c07a$pct_pred_fuera_01)"),
  paste0("Observados back.:    ", sprintf("%.1f%%", pct_fuera_01_obs_back),
         " (calculado desde panel)"),
  paste0("PEA union:           ", sprintf("%.1f%%", c08$pct_pred_fuera_01_union)),
  "",
  "## AUC",
  paste0("AUC test 07a:        ", sprintf("%.4f", c07a$auc_test),
         " (c07a$auc_test)"),
  paste0("AUC global back.:    ", sprintf("%.4f", c08$auc_global_train)),
  "",
  "## LIMITACIONES",
  "L1: Hold-out no disponible -- formalidad solo en 2024T4-2025T3",
  "L2: Desocupados -- ~81% pred fuera [0,1]. GLM mas apropiado.",
  "L3: Inactivos -- variables laborales imputadas con moda training",
  paste0("L4: Pred fuera [0,1] = ", sprintf("%.1f%%", pct_fuera_01_obs_back),
         " sobre Observados (ref. correcta paper)"),
  "    Ref: Angrist & Pischke (2009): no es deficiencia del modelo",
  "L5: Clustering EPH corregido (foldid codusu CV + vcovCL OLS)",
  "",
  "## FIGURAS EXPORTADAS",
  paste0("Directorio: rdos/figuras/", basename(DIR_FIGURAS_08B_LPM), "/"),
  "01: s1a -- tasa obs vs estimada ocupados",
  "02: s1b -- tasa obs vs ocupados y PEA",
  "03: s1c -- descomposicion delta",
  "04: s2  -- coherencia historica asalariados",
  "05: s3a_serie -- series por categoria",
  "06: s3a_comp  -- composicion por categoria",
  "07: s3b -- categorias con desoc potenciales",
  "08: s4_youden -- series tres universos (Youden)",
  "09: s4_cal    -- series tres universos (Cal.)",
  "10: s6_cob    -- cobertura PEA por tipo",
  "",
  "## SIGUIENTE PASO",
  "Script 09b -- Comparativo retropredictivo LPM/GLM/SLS"
)

writeLines(txt_lines, PATH_TXT_08B)
cat("   [OK] TXT generado:", PATH_TXT_08B, "\n\n")


# 📑 Checklist -----------------------------------------------------------------
cat("-- 6. Checklist de salidas -------------------------------------------\n")
cat("   [OK] HTML:", file.exists(PATH_08B_HTML), "\n")
cat("   [OK] TXT: ", file.exists(PATH_TXT_08B),  "\n")

n_pdfs <- length(list.files(DIR_FIGURAS_08B_LPM, pattern="\\.pdf$"))
cat(sprintf("   [OK] PDFs en figuras: %d/10\n", n_pdfs))

rm(list = c("panel","cob_pea",
            "serie_ocupados","serie_pea","serie_edad",
            "comp_trim","comp_s1",
            "serie_asal_vieja","serie_asal_back","serie_asal_comp",
            "panel_cat","panel_cat_pea",
            "tab_cat_global","tab_cat_global_pea",
            "comp_cat_trim","serie_cat","serie_cat_pea",
            "cols_ok","cols_necesarias"))
gc()

cat("\n===================================================================\n")
cat("SCRIPT 08b COMPLETADO\n")
cat("  HTML:", basename(PATH_08B_HTML), "\n")
cat("  TXT: ", basename(PATH_TXT_08B),  "\n")
cat(sprintf("  PDFs: %d/10 en %s\n", n_pdfs, basename(DIR_FIGURAS_08B_LPM)))
cat("===================================================================\n")

toc()
