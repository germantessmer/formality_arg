# =============================================================================
# [EN] 08c_reporte_hibrido_SLS.R -- HTML report for hybrid variable (ground truth + SLS backcast) with composition analysis
# INPUTS:  rdos/datos/08_panel_formalidad_SLS*.rds, contracts from 07-08 SLS
# OUTPUTS: rdos/reportes/08c_reporte_hibrido_SLS*.html
# =============================================================================
# 🌟 08c_reporte_hibrido_SLS.R 🌟 ####
# OBJETIVO:
#    Reporte HTML de la variable híbrida SLS — combina ground truth
#    (condicion_formalidad) con backcasting SLS (umbral calibración).
#    Estructura de embudo idéntica a 08b, con S2 reemplazada por análisis
#    de composición de la variable híbrida.
#
#    S1 — Validación directa (4 trimestres observados)
#         1a: tasa obs vs híbrida — ocupados
#         1b: tasa obs vs híbrida — PEA completa
#         1c: descomposición delta (contribución desocupados)
#         1d: métricas de clasificación
#    S2 — Composición de la variable híbrida (GT vs backcasting)
#    S3 — Extensión: otras categorías ocupacionales
#         3a: solo ocupados  |  3b: + desocupados potenciales
#    S4 — Series temporales completas (tres versiones)
#    S5 — Formalidad potencial (desocupados e inactivos)
#    S6 — Cobertura del universo
#    S7 — Notas metodológicas
#
# DIFERENCIAS CLAVE vs 08c GLM:
#   - Columnas: formalidad_clase_{SUFIJO}_pea/edad, formalidad_clase_cal_{SUFIJO}_pea/edad
#   - Color: COL_SLS (#6A3D9A) — violeta
#   - Sin H-L test (propiedad del GLM binomial)
#   - S7.3: propiedad estructural SLS — pred fuera [0,1] (Horrace & Oaxaca 2003)
#   - Panel SLS (~110 cols) es el canónico para comparativos 09c
#
# INPUTS:
#    - rdos/datos/08_panel_formalidad_{SUFIJO_MODELO_SLS}.rds         (Script 08)
#    - rdos/contratos/08_contrato_backcasting_{SUFIJO_MODELO_SLS}.rds (Script 08)
#    - rdos/datos/02_panel_con_taxonomia.rds             (Script 02)
#
# OUTPUTS:
#    - rdos/reportes/08c_reporte_hibrido_{SUFIJO_MODELO_SLS}.html
#    - rdos/reportes/08c_notas_paper_hibrido_{SUFIJO_MODELO_SLS}.txt
#    - rdos/figuras/08c_reporte_hibrido_SLS/  (9 PDFs)
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
source(here::here("script", "funciones", "theme_paper.R"))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b


# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 08c - Reporte Variable Híbrida SLS")
start_time <- Sys.time()
cat("===================================================================\n")
cat("SCRIPT 08c - REPORTE VARIABLE HÍBRIDA SLS\n")
cat("Inicio:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")


# 🪫 1. CARGA -------------------------------------------------------------------
cat("-- 1. Carga -----------------------------------------------------------\n")

# Usar paths definidos en parametros.R — sin HC de rutas
PATH_08C_HTML <- PATH_08C_HTML_SLS
PATH_TXT_08C  <- file.path(DIR_REPORTES,
                            paste0("08c_notas_paper_hibrido_", SUFIJO_MODELO_SLS, ".txt"))

hard_stop(file.exists(PATH_08_PANEL_SLS),    paste0("No existe 08_panel_formalidad_", SUFIJO_MODELO_SLS, ".rds"))
hard_stop(file.exists(PATH_08_CONTRATO_SLS), paste0("No existe 08_contrato_backcasting_", SUFIJO_MODELO_SLS, ".rds"))
dir.create(DIR_REPORTES,        showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_FIGURAS_08C_SLS, showWarnings = FALSE, recursive = TRUE)

c08 <- readRDS(PATH_08_CONTRATO_SLS)
cat("   [OK] Contrato c08. Campos:", length(names(c08)), "\n")

# Leer umbrales desde contrato — fuente primaria
UMBRAL_YOUDEN <- as.numeric(c08$umbral_youden)
UMBRAL_CAL    <- as.numeric(c08$umbral_calibracion)
hard_stop(!is.na(UMBRAL_YOUDEN) && UMBRAL_YOUDEN > 0 && UMBRAL_YOUDEN < 1,
          paste("UMBRAL_YOUDEN inválido desde c08:", UMBRAL_YOUDEN))
hard_stop(!is.na(UMBRAL_CAL) && UMBRAL_CAL > 0 && UMBRAL_CAL < 1,
          paste("UMBRAL_CAL inválido desde c08:", UMBRAL_CAL))
cat(sprintf("   Umbrales desde contrato: Youden=%.4f | Cal=%.4f\n",
            UMBRAL_YOUDEN, UMBRAL_CAL))

# Labels dinámicos (evitar HC de umbrales en texto)
LBL_YOUDEN <- sprintf("Youden (%.4f)", UMBRAL_YOUDEN)
LBL_CAL    <- sprintf("Cal. (%.3g)",   UMBRAL_CAL)
LBL_CAL_B  <- sprintf("Cal. %.3g",     UMBRAL_CAL)  # forma corta para leyendas

# Columnas dinámicas basadas en SUFIJO_MODELO_SLS (evitar HC "SLS3T")
SUFIJO <- SUFIJO_MODELO_SLS
COL_CLASE_PEA      <- paste0("formalidad_clase_", SUFIJO, "_pea")
COL_CLASE_CAL_PEA  <- paste0("formalidad_clase_cal_", SUFIJO, "_pea")
COL_CLASE_EDAD     <- paste0("formalidad_clase_", SUFIJO, "_edad")
COL_CLASE_CAL_EDAD <- paste0("formalidad_clase_cal_", SUFIJO, "_edad")
COL_PROB_PEA       <- paste0("prob_formal_", SUFIJO, "_pea")
COL_FLAG_PEA       <- paste0("flag_pred_", SUFIJO, "_pea")
COL_HIBRIDA_PEA    <- paste0("formalidad_hibrida_", SUFIJO, "_pea")
COL_FUENTE_PEA     <- paste0("fuente_hibrida_", SUFIJO, "_pea")

cols_necesarias <- c(
  "id_individuo", "periodo_id", "pondera",
  "tipo_estimacion_pea",  "tipo_estimacion_edad",
  COL_CLASE_PEA,      COL_CLASE_CAL_PEA,
  COL_CLASE_EDAD,     COL_CLASE_CAL_EDAD,
  "formalidad_valida",
  COL_PROB_PEA, COL_FLAG_PEA,
  "categoria_ocupacional"
)

cat("   Cargando panel 08 SLS...\n")
panel_full <- readRDS(PATH_08_PANEL_SLS)
cols_ok    <- intersect(cols_necesarias, names(panel_full))
panel      <- panel_full[, cols_ok]
rm(panel_full); gc()
cat("   [OK] Panel:", format(nrow(panel), big.mark = ","), "obs |",
    ncol(panel), "cols retenidas\n")

# Join lateral: condicion_formalidad (desde taxonomía 02)
# ⚠️ Join por id_individuo + periodo_id — período-específico (L59)
cat("   Cargando condicion_formalidad desde panel 02...\n")
tiene_tax <- file.exists(PATH_02_PANEL_TAX)
if (tiene_tax) {
  panel_tax <- readRDS(PATH_02_PANEL_TAX) %>%
    select(id_individuo, periodo_id, condicion_formalidad) %>%
    filter(!is.na(condicion_formalidad))
  panel <- panel %>% left_join(panel_tax, by = c("id_individuo", "periodo_id"))
  rm(panel_tax); gc()
  cat("   [OK] Join condicion_formalidad completado.\n")
  cat("   Distribución condicion_formalidad:\n")
  print(table(panel$condicion_formalidad, useNA = "ifany"))
} else {
  warning("02_panel_con_taxonomia.rds no encontrado.")
  panel$condicion_formalidad <- NA_character_
}
cat("\n")


# 🪫 2. CÁLCULOS PREPARATORIOS --------------------------------------------------
cat("-- 2. Calculos --------------------------------------------------------\n")

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE)

# Leer desde parametros.R (evitar HC)
TRIM_OBS      <- TRIMESTRES_FORMALIDAD   # c("2024_T4","2025_T1","2025_T2","2025_T3")
TRIM_PANDEMIA <- TRIMESTRES_PANDEMIA     # c("2020_T1",...,"2021_T2")

# ── 2a. Verificación de distribuciones ──────────────────────────────────────
cat("   Verificando cobertura por tipo PEA:\n")
print(table(
  ground_truth = !is.na(panel$condicion_formalidad) &
    panel$condicion_formalidad %in% c("Formal", "No formal"),
  tipo_pea     = panel$tipo_estimacion_pea,
  useNA        = "ifany"
))

# ── 2b. Construcción de la variable híbrida ──────────────────────────────────
cat("   Construyendo variable híbrida...\n")

panel <- panel %>%
  mutate(
    # Variable híbrida PEA
    # Ground truth para asalariados con condicion_formalidad conocida
    # Backcasting SLS (umbral calibración) para el resto
    !!COL_HIBRIDA_PEA := case_when(
      condicion_formalidad == "Formal"    ~ "Formal",
      condicion_formalidad == "No formal" ~ "Informal",   # mapeo explícito [L59]
      TRUE                                ~ .data[[COL_CLASE_CAL_PEA]]
    ),
    # Fuente de la variable híbrida (para S2)
    !!COL_FUENTE_PEA := case_when(
      condicion_formalidad %in% c("Formal", "No formal")          ~ "Ground truth",
      tipo_estimacion_pea %in% c("Observado", "Backcasting",
                                  "Potencial_desocupado")          ~ "Backcasting",
      TRUE                                                         ~ NA_character_
    )
  )

cat(paste0("   ", COL_HIBRIDA_PEA, ":\n"))
print(table(panel[[COL_HIBRIDA_PEA]], useNA = "ifany"))
cat(paste0("   ", COL_FUENTE_PEA, ":\n"))
print(table(panel[[COL_FUENTE_PEA]], useNA = "ifany"))

# ── 2c. Universos ────────────────────────────────────────────────────────────
cat("   Construyendo universos...\n")

n_desoc_pea <- sum(panel$tipo_estimacion_pea == "Potencial_desocupado", na.rm = TRUE)

pea_activos <- panel %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting"))

pea_total <- panel %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting", "Potencial_desocupado"))

obs_trim <- panel %>%
  filter(tipo_estimacion_pea == "Observado", periodo_id %in% TRIM_OBS)

cat(sprintf("   n_desoc_pea: %s | pea_activos: %s | obs_trim: %s\n",
            fmt_n(n_desoc_pea), fmt_n(nrow(pea_activos)), fmt_n(nrow(obs_trim))))


# 🪫 3. S1 — VALIDACIÓN DIRECTA (4 TRIMESTRES OBSERVADOS) -----------------------
cat("-- 3. S1 — Validacion directa ----------------------------------------\n")

# S1a: Tasa observada vs híbrida — OCUPADOS
s1a <- obs_trim %>%
  group_by(periodo_id) %>%
  summarise(
    n_obs        = n(),
    tasa_obs     = mean(formalidad_valida == "Formal oficial", na.rm = TRUE) * 100,
    tasa_hibrida = mean(.data[[COL_HIBRIDA_PEA]] == "Formal", na.rm = TRUE) * 100,
    delta_pp     = tasa_hibrida - tasa_obs,
    .groups = "drop"
  )

cat(sprintf("   S1a — delta promedio ocupados: %.2f pp\n",
            round(mean(abs(s1a$delta_pp), na.rm = TRUE), 2)))

# S1b: Tasa observada vs híbrida — PEA completa (+ desocupados)
s1b_obs <- obs_trim %>%
  group_by(periodo_id) %>%
  summarise(
    n_activos_obs = n(),
    tasa_obs      = mean(formalidad_valida == "Formal oficial", na.rm = TRUE) * 100,
    .groups = "drop"
  )

s1b_pea <- panel %>%
  filter(periodo_id %in% TRIM_OBS,
         tipo_estimacion_pea %in% c("Observado", "Backcasting", "Potencial_desocupado")) %>%
  group_by(periodo_id) %>%
  summarise(
    n_pea_total      = n(),
    tasa_hibrida_pea = mean(.data[[COL_HIBRIDA_PEA]] == "Formal", na.rm = TRUE) * 100,
    .groups = "drop"
  )

s1b <- s1b_obs %>%
  left_join(s1b_pea, by = "periodo_id") %>%
  mutate(delta_pp = tasa_hibrida_pea - tasa_obs)

cat(sprintf("   S1b — delta promedio PEA: %.2f pp\n",
            round(mean(abs(s1b$delta_pp), na.rm = TRUE), 2)))

# S1c: Descomposición del delta
s1c <- s1b %>%
  left_join(
    panel %>%
      filter(periodo_id %in% TRIM_OBS, tipo_estimacion_pea == "Potencial_desocupado") %>%
      group_by(periodo_id) %>%
      summarise(
        n_desoc            = n(),
        tasa_hibrida_desoc = mean(.data[[COL_HIBRIDA_PEA]] == "Formal",
                                  na.rm = TRUE) * 100,
        .groups = "drop"
      ),
    by = "periodo_id"
  ) %>%
  mutate(
    pct_desoc     = n_desoc / n_pea_total * 100,
    contrib_desoc = delta_pp - (tasa_hibrida_pea - tasa_obs)
  )

# S1d: Métricas de clasificación
s1d_df <- obs_trim %>%
  filter(!is.na(formalidad_valida), !is.na(.data[[COL_HIBRIDA_PEA]])) %>%
  mutate(
    real_bin = (formalidad_valida == "Formal oficial"),
    pred_bin = (.data[[COL_HIBRIDA_PEA]] == "Formal")
  )

tp <- sum( s1d_df$real_bin &  s1d_df$pred_bin)
tn <- sum(!s1d_df$real_bin & !s1d_df$pred_bin)
fp <- sum(!s1d_df$real_bin &  s1d_df$pred_bin)
fn <- sum( s1d_df$real_bin & !s1d_df$pred_bin)

s1d <- tibble(
  Metrica = c("Accuracy", "Sensibilidad", "Especificidad",
              "F1", "MCC", "N clasificados"),
  Valor = c(
    round((tp + tn) / (tp + tn + fp + fn), 4),
    round(tp / (tp + fn), 4),
    round(tn / (tn + fp), 4),
    round(2 * tp / (2 * tp + fp + fn), 4),
    round((as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)) /
            sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) *
                 as.numeric(tn + fp) * as.numeric(tn + fn)), 4),
    tp + tn + fp + fn
  )
)
cat("   S1d métricas:\n"); print(s1d)


# 🪫 4. S2 — COMPOSICIÓN DE LA VARIABLE HÍBRIDA ---------------------------------
cat("-- 4. S2 — Composicion variable hibrida ------------------------------\n")

s2a <- pea_total %>%
  group_by(periodo_id) %>%
  summarise(
    n_total  = n(),
    n_gt     = sum(.data[[COL_FUENTE_PEA]] == "Ground truth", na.rm = TRUE),
    n_back   = sum(.data[[COL_FUENTE_PEA]] == "Backcasting",  na.rm = TRUE),
    pct_gt   = round(n_gt   / n_total * 100, 1),
    pct_back = round(n_back / n_total * 100, 1),
    .groups  = "drop"
  ) %>%
  arrange(periodo_id)

cat("   S2a primeras filas:\n"); print(head(s2a, 3))

s2b <- pea_total %>%
  filter(!is.na(.data[[COL_FUENTE_PEA]]), !is.na(.data[[COL_HIBRIDA_PEA]])) %>%
  group_by(periodo_id, .data[[COL_FUENTE_PEA]]) %>%
  summarise(
    n           = n(),
    tasa_formal = mean(.data[[COL_HIBRIDA_PEA]] == "Formal") * 100,
    .groups     = "drop"
  ) %>%
  rename(fuente = !!COL_FUENTE_PEA) %>%
  arrange(periodo_id, fuente)

s2b_global <- pea_total %>%
  filter(!is.na(.data[[COL_HIBRIDA_PEA]])) %>%
  group_by(periodo_id) %>%
  summarise(
    tasa_hibrida = mean(.data[[COL_HIBRIDA_PEA]] == "Formal") * 100,
    n            = n(),
    .groups      = "drop"
  )


# 🪫 5. S3 — EXTENSIÓN: OTRAS CATEGORÍAS OCUPACIONALES --------------------------
cat("-- 5. S3 — Otras categorias ------------------------------------------\n")

# 3a: Solo ocupados — tabla global + composición por trimestre
# categoria_ocupacional es FACTOR → as.character()
s3a_global <- pea_activos %>%
  filter(!is.na(.data[[COL_HIBRIDA_PEA]])) %>%
  mutate(cat_ocup = as.character(categoria_ocupacional)) %>%
  group_by(cat_ocup) %>%
  summarise(
    n           = n(),
    tasa_formal = round(mean(.data[[COL_HIBRIDA_PEA]] == "Formal") * 100, 1),
    .groups     = "drop"
  ) %>%
  arrange(desc(n))

cat("   S3a global:\n"); print(s3a_global)

s3a_comp <- pea_activos %>%
  filter(!is.na(.data[[COL_HIBRIDA_PEA]])) %>%
  mutate(
    cat_ocup = as.character(categoria_ocupacional),
    grupo = case_when(
      startsWith(cat_ocup, "Asalariado") ~ tr("Asalariado"),
      startsWith(cat_ocup, "Patr")       ~ tr("Patrón"),    # tilde → "Patr"
      startsWith(cat_ocup, "Cuenta")     ~ tr("Cta. propia"),
      startsWith(cat_ocup, "Familiar")   ~ tr("Familiar"),
      TRUE                               ~ tr("Otro")
    )
  ) %>%
  group_by(periodo_id, grupo) %>%
  summarise(
    n           = n(),
    tasa_formal = mean(.data[[COL_HIBRIDA_PEA]] == "Formal") * 100,
    .groups     = "drop"
  )

# 3b: + desocupados potenciales
s3b_impacto <- panel %>%
  filter(tipo_estimacion_pea == "Potencial_desocupado",
         !is.na(.data[[COL_HIBRIDA_PEA]])) %>%
  mutate(cat_ocup = as.character(categoria_ocupacional)) %>%
  group_by(cat_ocup) %>%
  summarise(
    n           = n(),
    tasa_formal = round(mean(.data[[COL_HIBRIDA_PEA]] == "Formal") * 100, 1),
    .groups     = "drop"
  ) %>%
  arrange(desc(n))

s3b_serie <- panel %>%
  filter(tipo_estimacion_pea %in% c("Observado", "Backcasting", "Potencial_desocupado"),
         !is.na(.data[[COL_HIBRIDA_PEA]])) %>%
  mutate(
    tipo_grupo = if_else(tipo_estimacion_pea == "Potencial_desocupado",
                         tr("Desocupado potencial"), tr("Ocupado"))
  ) %>%
  group_by(periodo_id, tipo_grupo) %>%
  summarise(
    n           = n(),
    tasa_formal = mean(.data[[COL_HIBRIDA_PEA]] == "Formal") * 100,
    .groups     = "drop"
  )


# 🪫 6. S4 — SERIES TEMPORALES COMPLETAS ----------------------------------------
cat("-- 6. S4 — Series temporales completas -------------------------------\n")

s4_pea_activos <- pea_activos %>%
  filter(!is.na(.data[[COL_CLASE_PEA]]),
         !is.na(.data[[COL_CLASE_CAL_PEA]])) %>%
  group_by(periodo_id) %>%
  summarise(
    n            = n(),
    tasa_youden  = mean(.data[[COL_CLASE_PEA]]     == "Formal") * 100,
    tasa_cal     = mean(.data[[COL_CLASE_CAL_PEA]] == "Formal") * 100,
    tasa_hibrida = mean(.data[[COL_HIBRIDA_PEA]]   == "Formal", na.rm = TRUE) * 100,
    .groups      = "drop"
  )

s4_edad <- panel %>%
  filter(tipo_estimacion_edad %in% c("Observado", "Backcasting",
                                      "Potencial_desocupado", "Potencial_inactivo"),
         !is.na(.data[[COL_CLASE_CAL_EDAD]])) %>%
  group_by(periodo_id) %>%
  summarise(
    n        = n(),
    tasa_cal = mean(.data[[COL_CLASE_CAL_EDAD]] == "Formal") * 100,
    .groups  = "drop"
  )


# 🪫 7. S5 — FORMALIDAD POTENCIAL -----------------------------------------------
cat("-- 7. S5 — Formalidad potencial --------------------------------------\n")

s5_desoc <- panel %>%
  filter(tipo_estimacion_pea == "Potencial_desocupado",
         !is.na(.data[[COL_CLASE_CAL_PEA]])) %>%
  group_by(periodo_id) %>%
  summarise(
    n           = n(),
    tasa_formal = mean(.data[[COL_CLASE_CAL_PEA]] == "Formal") * 100,
    .groups     = "drop"
  )

s5_inact <- panel %>%
  filter(tipo_estimacion_edad == "Potencial_inactivo",
         !is.na(.data[[COL_CLASE_CAL_EDAD]])) %>%
  group_by(periodo_id) %>%
  summarise(
    n           = n(),
    tasa_formal = mean(.data[[COL_CLASE_CAL_EDAD]] == "Formal") * 100,
    .groups     = "drop"
  )


# 🪫 8. S6 — COBERTURA ----------------------------------------------------------
cat("-- 8. S6 — Cobertura -------------------------------------------------\n")

s6_cobertura <- panel %>%
  group_by(periodo_id) %>%
  summarise(
    n_total           = n(),
    n_pea_activos     = sum(tipo_estimacion_pea %in% c("Observado", "Backcasting"),
                            na.rm = TRUE),
    n_pea_total       = sum(tipo_estimacion_pea %in% c("Observado", "Backcasting",
                                                        "Potencial_desocupado"),
                            na.rm = TRUE),
    n_edad            = sum(tipo_estimacion_edad %in% c("Observado", "Backcasting",
                                                         "Potencial_desocupado",
                                                         "Potencial_inactivo"),
                            na.rm = TRUE),
    n_sin_theta       = sum(tipo_estimacion_pea == "Sin_theta", na.rm = TRUE),
    n_hibrida_val     = sum(!is.na(.data[[COL_HIBRIDA_PEA]]) &
                              tipo_estimacion_pea %in% c("Observado", "Backcasting",
                                                          "Potencial_desocupado")),
    pct_cobertura_pea = round(n_pea_activos / n_pea_total * 100, 1),
    .groups           = "drop"
  )


# 🪫 9. KPIs PARA RESUMEN EJECUTIVO ---------------------------------------------
cat("-- 9. KPIs resumen ejecutivo -----------------------------------------\n")

n_gt_total   <- sum(panel[[COL_FUENTE_PEA]] == "Ground truth", na.rm = TRUE)
n_back_total <- sum(panel[[COL_FUENTE_PEA]] == "Backcasting",  na.rm = TRUE)
pct_gt       <- round(n_gt_total  / (n_gt_total + n_back_total) * 100, 1)
pct_back     <- round(n_back_total / (n_gt_total + n_back_total) * 100, 1)

tasa_hibrida_global <- mean(
  pea_activos[[COL_HIBRIDA_PEA]] == "Formal", na.rm = TRUE
) * 100

tasa_cal_global <- mean(
  pea_activos[[COL_CLASE_CAL_PEA]] == "Formal", na.rm = TRUE
) * 100

delta_hibrida_vs_cal <- round(tasa_hibrida_global - tasa_cal_global, 2)

# n_kappa_train: campo que puede no existir en contratos SLS antiguos
# HC documentado: fallback defensivo — Puede no existir en c08 antiguo.
lbl_kappa <- if (!is.null(c08$n_kappa_train) && length(c08$n_kappa_train) > 0) {
  paste0("N kappa (train SLS): ", format(c08$n_kappa_train, big.mark = ","))
} else {
  paste0("N kappa (train SLS): ver 08_contrato_backcasting_", SUFIJO)
}

kpi_df <- tibble(
  Indicador = c(
    "N panel total",
    "N universo PEA activos",
    "N universo PEA + desocupados",
    "N universo Edad 18-60",
    "N obs. ground truth (formalidad híbrida)",
    "N obs. backcasting SLS (formalidad híbrida)",
    "% ground truth en variable híbrida",
    "% backcasting SLS en variable híbrida",
    "Tasa formal híbrida — PEA activos (global)",
    "Tasa formal calibrada — PEA activos (global)",
    "Delta híbrida vs calibrada",
    "Delta validación S1a — ocupados (prom. 3T)",
    "Delta validación S1b — PEA (prom. 3T)",
    "Umbral Youden",
    "Umbral Calibración",
    "Coherencia SLS (κ̂γ)"
  ),
  Valor = c(
    format(c08$n_panel_total, big.mark = ","),
    format(c08$universo_pea$n_elegible - n_desoc_pea, big.mark = ","),
    format(c08$universo_pea$n_elegible, big.mark = ","),
    format(c08$universo_edad$n_elegible, big.mark = ","),
    format(n_gt_total,   big.mark = ","),
    format(n_back_total, big.mark = ","),
    paste0(pct_gt,   "%"),
    paste0(pct_back, "%"),
    paste0(round(tasa_hibrida_global, 2), "%"),
    paste0(round(tasa_cal_global,     2), "%"),
    paste0(delta_hibrida_vs_cal, " pp"),
    paste0(round(mean(abs(s1a$delta_pp)), 2), " pp"),
    paste0(round(mean(abs(s1b$delta_pp)), 2), " pp"),
    as.character(UMBRAL_YOUDEN),
    as.character(UMBRAL_CAL),
    paste0("κ̂γ garantiza coherencia entre selección y resultado. ",
           lbl_kappa)
  )
)

# Tabla composición para resumen ejecutivo
comp_exec <- tibble(
  Fuente      = c("Ground truth (condicion_formalidad)",
                  paste0("Backcasting SLS (umbral ", sprintf("%.3g", UMBRAL_CAL), ")"),
                  "Total"),
  N           = c(n_gt_total, n_back_total, n_gt_total + n_back_total),
  Pct         = c(pct_gt, pct_back, 100.0),
  Descripcion = c(
    "Asalariados con aporte jubilatorio observable",
    "Cuenta propia, patrón, familiar, desocupados potenciales",
    "Universo PEA activos + desocupados"
  )
) %>%
  mutate(N = format(N, big.mark = ","), Pct = paste0(Pct, "%"))

cat(sprintf("   KPIs generados: %d filas | GT=%.1f%% | Back=%.1f%%\n",
            nrow(kpi_df), pct_gt, pct_back))


# 🪫 10. GRÁFICOS PRE-CALCULADOS ------------------------------------------------
cat("-- 10. Graficos pre-calculados ----------------------------------------\n")

# Paleta de trimestres para eje X
trim_levels <- sort(unique(panel$periodo_id))

# Función: añadir zonas pandemia + observados al ggplot
zonas_ggplot <- function(p, trim_vec = trim_levels) {
  idx_obs  <- which(trim_vec %in% TRIM_OBS)
  idx_pand <- which(trim_vec %in% TRIM_PANDEMIA)
  if (length(idx_obs)  > 0) p <- p + annotate("rect",
    xmin = min(idx_obs)  - 0.4, xmax = max(idx_obs)  + 0.4,
    ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.12)
  if (length(idx_pand) > 0) p <- p + annotate("rect",
    xmin = min(idx_pand) - 0.4, xmax = max(idx_pand) + 0.4,
    ymin = -Inf, ymax = Inf, fill = "#9b59b6", alpha = 0.10)
  p
}

# Colores semánticos para el reporte híbrido SLS
# HC documentado: COL_FORMAL y COL_INFORMAL representan concepto formalidad,
# no un modelo específico — coherencia con el modelo SLS del reporte.
COL_FORMAL   <- COL_SLS              # violeta — formal SLS
COL_INFORMAL <- PAL_DESCRIPTIVO[3]   # naranja — informal (constante todos los modelos)
COL_GT       <- COL_OBSERVADO        # gris oscuro — ground truth EPH
COL_BACK_SLS <- COL_SLS             # violeta — backcasting SLS

# Orden de categorías ocupacionales
cats_orden <- tr(c("Asalariado", "Patrón", "Cta. propia", "Familiar", "Otro"))

# Colores por categoría — PAL_DESCRIPTIVO (no son modelos)
colores_cat <- setNames(
  c(PAL_DESCRIPTIVO[1], PAL_DESCRIPTIVO[4], PAL_DESCRIPTIVO[3],
    PAL_DESCRIPTIVO[5], PAL_DESCRIPTIVO[2]),
  tr(c("Asalariado", "Patrón", "Cta. propia", "Familiar", "Otro"))
)

# S1a: barras delta ocupados
g_s1a <- ggplot(s1a, aes(x = periodo_id)) +
  geom_col(aes(y = tasa_obs,     fill = tr("Observada")),
           width = 0.35, position = position_nudge(x = -0.2)) +
  geom_col(aes(y = tasa_hibrida, fill = tr("Híbrida")),
           width = 0.35, position = position_nudge(x =  0.2)) +
  scale_fill_manual(values = setNames(
    c(COL_GT, COL_FORMAL), tr(c("Observada", "Híbrida"))
  )) +
  tr_labs(title    = "S1a: Tasa formal observada vs híbrida SLS — Ocupados",
          subtitle = "4 trimestres de entrenamiento",
          x = NULL, y = "Tasa formal (%)", fill = NULL) +
  theme_paper() +
  theme(legend.position = "bottom")

guardar_figura(g_s1a, DIR_FIGURAS_08C_SLS, "s1a", 1)

# S1b: delta PEA
g_s1b <- ggplot(s1b, aes(x = periodo_id)) +
  geom_col(aes(y = tasa_obs,         fill = tr("Observada")),
           width = 0.35, position = position_nudge(x = -0.2)) +
  geom_col(aes(y = tasa_hibrida_pea, fill = tr("Híbrida PEA")),
           width = 0.35, position = position_nudge(x =  0.2)) +
  scale_fill_manual(values = setNames(
    c(COL_GT, COL_BACK_SLS), tr(c("Observada", "Híbrida PEA"))
  )) +
  tr_labs(title    = "S1b: Tasa formal observada vs híbrida SLS — PEA completa",
          subtitle = "Incluye desocupados potenciales",
          x = NULL, y = "Tasa formal (%)", fill = NULL) +
  theme_paper() +
  theme(legend.position = "bottom")

guardar_figura(g_s1b, DIR_FIGURAS_08C_SLS, "s1b", 2)

# S2a: composición por trimestre (barras apiladas %)
s2a_long <- s2a %>%
  select(periodo_id, `Ground truth` = pct_gt, Backcasting = pct_back) %>%
  pivot_longer(-periodo_id, names_to = "Fuente", values_to = "Pct") %>%
  mutate(Fuente = tr(Fuente))

g_s2a <- ggplot(s2a_long, aes(x = periodo_id, y = Pct, fill = Fuente)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = setNames(
    c(COL_GT, COL_BACK_SLS), tr(c("Ground truth", "Backcasting"))
  )) +
  tr_labs(title    = "S2a: Composición de la variable híbrida SLS por trimestre",
          subtitle = "% de observaciones por fuente (PEA activos + desocupados)",
          x = NULL, y = "%", fill = "Fuente") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "bottom")

guardar_figura(g_s2a, DIR_FIGURAS_08C_SLS, "s2a_comp", 3)

# S2b: serie tasa formal por fuente + global
s2b <- s2b %>% mutate(fuente = tr(fuente))

g_s2b <- ggplot() +
  geom_line(data = s2b,
            aes(x = periodo_id, y = tasa_formal,
                color = fuente, group = fuente),
            linewidth = 0.9) +
  geom_line(data = s2b_global,
            aes(x = periodo_id, y = tasa_hibrida, group = 1),
            color = "black", linewidth = 1.0, linetype = "dashed") +
  scale_color_manual(values = setNames(
    c(COL_GT, COL_BACK_SLS), tr(c("Ground truth", "Backcasting"))
  )) +
  tr_labs(title    = "S2b: Tasa formal por fuente de la variable híbrida SLS",
          subtitle = "Línea negra punteada = total híbrida PEA",
          x = NULL, y = "Tasa formal (%)", color = "Fuente") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "bottom")

guardar_figura(g_s2b, DIR_FIGURAS_08C_SLS, "s2b_serie", 4)

# S3a: composición apilada por categoría y trimestre
g_s3a_comp <- ggplot(s3a_comp, aes(x = periodo_id, y = n / 1000, fill = grupo)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = colores_cat) +
  tr_labs(title    = "S3a: Composición de ocupados por categoría ocupacional",
          x = NULL, y = "Miles de obs.", fill = "Categoría") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "bottom")

guardar_figura(g_s3a_comp, DIR_FIGURAS_08C_SLS, "s3a_comp", 5)

# S3b: ocupados vs desocupados potenciales
g_s3b <- ggplot(s3b_serie,
                aes(x = periodo_id, y = tasa_formal,
                    color = tipo_grupo, group = tipo_grupo)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = setNames(
    c(COL_FORMAL, COL_INFORMAL), tr(c("Ocupado", "Desocupado potencial"))
  )) +
  tr_labs(title    = "S3b: Formalidad híbrida SLS — ocupados vs desocupados potenciales",
          x = NULL, y = "Tasa formal (%)", color = NULL) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "bottom")

guardar_figura(g_s3b, DIR_FIGURAS_08C_SLS, "s3b", 6)

# S4: PEA activos — tres versiones (Youden, Cal., Híbrida)
s4_long <- s4_pea_activos %>%
  select(periodo_id, tasa_youden, tasa_cal, tasa_hibrida) %>%
  rename_with(~ c("periodo_id", LBL_YOUDEN, LBL_CAL, tr("Híbrida"))) %>%
  pivot_longer(-periodo_id, names_to = "Serie", values_to = "Tasa")

g_s4_pea_activos <- ggplot(s4_long,
                            aes(x = periodo_id, y = Tasa,
                                color = Serie, group = Serie)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = setNames(
    c(PAL_DESCRIPTIVO[5], PAL_DESCRIPTIVO[3], COL_FORMAL),
    c(LBL_YOUDEN, LBL_CAL, tr("Híbrida"))
  )) +
  tr_labs(title    = "S4: Series completas — PEA activos (3 versiones SLS)",
          subtitle = "Zona naranja = trimestres observados | Zona violeta = pandemia",
          x = NULL, y = "Tasa formal (%)", color = NULL) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "bottom")

guardar_figura(g_s4_pea_activos, DIR_FIGURAS_08C_SLS, "s4_pea", 7)

# S4: Universo Edad 18-60
g_s4_edad <- ggplot(s4_edad, aes(x = periodo_id, y = tasa_cal, group = 1)) +
  geom_line(color = PAL_DESCRIPTIVO[4], linewidth = 0.9) +
  tr_labs(title    = "S4: Serie calibrada SLS — Universo Edad 18-60",
          subtitle = "Incluye inactivos con θ disponible",
          x = NULL, y = "Tasa formal (%)") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

guardar_figura(g_s4_edad, DIR_FIGURAS_08C_SLS, "s4_edad", 8)

# S5: formalidad potencial
g_s5 <- ggplot() +
  geom_line(data = s5_desoc,
            aes(x = periodo_id, y = tasa_formal, group = 1, color = tr("Desocupados")),
            linewidth = 0.9) +
  geom_line(data = s5_inact,
            aes(x = periodo_id, y = tasa_formal, group = 1, color = tr("Inactivos 18-60")),
            linewidth = 0.9) +
  scale_color_manual(values = setNames(
    c(COL_INFORMAL, PAL_DESCRIPTIVO[3]), tr(c("Desocupados", "Inactivos 18-60"))
  )) +
  tr_labs(title    = "S5: Formalidad potencial SLS — Desocupados e inactivos",
          subtitle = paste0("Umbral calibración (", UMBRAL_CAL,
                            ") | Clipping aplicado en OOS"),
          x = NULL, y = "Tasa formal estimada (%)", color = NULL) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "bottom")

guardar_figura(g_s5, DIR_FIGURAS_08C_SLS, "s5_potencial", 9)

cat("   [OK] Graficos pre-calculados: 9 | PDFs guardados en DIR_FIGURAS_08C_SLS\n\n")


# 🪫 11. NOTAS PAPER (TXT) ------------------------------------------------------
cat("-- 11. Generando TXT para el paper -----------------------------------\n")

notas_txt <- c(
  "# =================================================================",
  "# NOTAS PARA EL PAPER -- Variable Híbrida SLS",
  paste0("# Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "# Script:   08c_reporte_hibrido_SLS.R (Capa 5)",
  "# =================================================================",
  "",
  "## VARIABLE HÍBRIDA",
  paste0("Ground truth (condicion_formalidad): N = ", fmt_n(n_gt_total),
         sprintf(" (%.1f%% del universo PEA + desoc.)", pct_gt)),
  paste0("Backcasting SLS (umbral calibración): N = ", fmt_n(n_back_total),
         sprintf(" (%.1f%% del universo PEA + desoc.)", pct_back)),
  paste0("Umbral Calibración (desde c08):      ", UMBRAL_CAL),
  paste0("Umbral Youden      (desde c08):      ", UMBRAL_YOUDEN),
  "",
  "## VALIDACIÓN S1a — OCUPADOS (4 trimestres observados)",
  paste(capture.output(print(s1a)), collapse = "\n"),
  paste0("Delta promedio abs. ocupados: ",
         round(mean(abs(s1a$delta_pp)), 2), " pp"),
  "",
  "## VALIDACIÓN S1b — PEA (+ desocupados)",
  paste(capture.output(
    print(s1b[, c("periodo_id", "tasa_obs", "tasa_hibrida_pea", "delta_pp")])
  ), collapse = "\n"),
  paste0("Delta promedio abs. PEA: ", round(mean(abs(s1b$delta_pp)), 2), " pp"),
  "",
  "## MÉTRICAS DE CLASIFICACIÓN (S1d)",
  paste(capture.output(print(s1d)), collapse = "\n"),
  "",
  "## TASA FORMAL GLOBAL (PEA activos)",
  paste0("Híbrida:   ", round(tasa_hibrida_global, 2), "%"),
  paste0("Calibrada: ", round(tasa_cal_global,     2), "%"),
  paste0("Delta:     ", delta_hibrida_vs_cal, " pp"),
  "",
  "## [LN4] PRED FUERA [0,1] — SLS",
  "kappa (train SLS): 0.00%  (acotamiento garantizado — Horrace & Oaxaca 2003)",
  paste0("Test SLS (07b): ", round(c08$benchmarks_modelo$pct_fuera_test, 1),
         "%  (benchmark)"),
  paste0("Union PEA:      ", round(c08$pct_pred_fuera_01_union, 1), "%"),
  "Nota: en la variable híbrida el segmento ground truth (asalariados) no depende",
  "del acotamiento SLS; el riesgo de pred fuera [0,1] aplica solo al segmento back.",
  "",
  "## N UNIVERSOS",
  paste0("N desocupados PEA: ", fmt_n(n_desoc_pea)),
  paste0("N inactivos 18-60: ", fmt_n(c08$n_solo_edad)),
  paste0("N unión PEA+Edad:  ", fmt_n(c08$n_union)),
  "",
  "## COBERTURA BACKCASTING (desde c08)",
  paste0("N panel total:     ", fmt_n(c08$n_panel_total)),
  paste0("N elegible PEA:    ", fmt_n(c08$universo_pea$n_elegible)),
  paste0("N elegible Edad:   ", fmt_n(c08$universo_edad$n_elegible)),
  "",
  "## INTERPRETACIÓN PARA EL PAPER",
  "La variable híbrida SLS combina el ground truth observable (asalariados con registro",
  "jubilatorio) con estimaciones backcasting SLS para el resto del universo PEA.",
  "Ventaja del SLS: κ̂γ garantiza coherencia entre la ecuación de selección (quién",
  "trabaja) y la ecuación de resultado (formalidad).",
  paste0("Con umbral Cal. (", UMBRAL_CAL, "): delta max = ",
         c08$metricas_calibracion$delta_max_pp, " pp en ventana training."),
  "Panel SLS (~110 cols) es el canónico para los comparativos 09c.",
  "",
  "## FIGURAS EXPORTADAS",
  paste0("Directorio: rdos/figuras/", basename(DIR_FIGURAS_08C_SLS), "/"),
  "01: s1a          — tasa obs vs híbrida ocupados",
  "02: s1b          — tasa obs vs híbrida PEA",
  "03: s2a_comp     — composición GT vs backcasting por trimestre",
  "04: s2b_serie    — tasa formal por fuente",
  "05: s3a_comp     — composición ocupados por categoría",
  "06: s3b          — ocupados vs desocupados potenciales",
  "07: s4_pea       — series completas PEA activos (3 versiones)",
  "08: s4_edad      — serie Edad 18-60",
  "09: s5_potencial — formalidad potencial desoc. e inactivos",
  "",
  "## SIGUIENTE PASO",
  "Script 09c — Comparativo variable híbrida LPM/GLM/SLS"
)

writeLines(notas_txt, PATH_TXT_08C)
cat("   [OK] TXT generado:", PATH_TXT_08C, "\n\n")


# 🪫 12. GENERACIÓN DEL REPORTE HTML (RMD temporal) -----------------------------
cat("-- 12. Generando reporte HTML -----------------------------------------\n")

rmd_temp <- tempfile(fileext = ".Rmd")
con      <- file(rmd_temp, open = "wt", encoding = "UTF-8")

# Helpers de formato
fmt_pct <- function(x) paste0(round(x, 2), "%")
fmt_pp  <- function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 2), " pp")

# ── YAML ─────────────────────────────────────────────────────────────────────
writeLines(c(
  '---',
  'title: "Reporte Variable Híbrida — SLS Backcasting Formalidad"',
  paste0('subtitle: "EPH Argentina ', ANIO_INI, 'T', TRIM_INI, '\u2013', ANIO_FIN, 'T',
         TRIM_FIN, ' | Modelo ', SUFIJO_MODELO_SLS, ' | Umbral Cal. ', UMBRAL_CAL, '"'),
  'date: "`r format(Sys.time(), \'%Y-%m-%d %H:%M\')`"',
  'output:',
  '  html_document:',
  '    toc: true',
  '    toc_depth: 3',
  '    toc_float:',
  '      collapsed: false',
  '    theme: flatly',
  '    highlight: tango',
  '    number_sections: true',
  '    code_folding: hide',
  '    df_print: kable',
  '---',
  ''
), con)

# ── CSS ──────────────────────────────────────────────────────────────────────
writeLines(c(
  '<style>',
  'body { font-size: 14px; font-family: "Helvetica Neue", Arial, sans-serif; }',
  'h1 { color: #2c3e50; border-bottom: 2px solid #6A3D9A; }',
  'h2 { color: #6A3D9A; }',
  'h3 { color: #555; }',
  '.kable-table { font-size: 12px; }',
  '.nota    { background: #ecf0f1; border-left: 4px solid #3498db; padding: 8px 12px;',
  '           margin: 10px 0; border-radius: 3px; font-size: 13px; }',
  '.alerta  { background: #fef9e7; border-left: 4px solid #f39c12; padding: 8px 12px;',
  '           margin: 10px 0; border-radius: 3px; font-size: 13px; }',
  '.hibrida { background: #f3eef9; border-left: 4px solid #6A3D9A; padding: 8px 12px;',
  '            margin: 10px 0; border-radius: 3px; font-size: 13px; }',
  '.sls-nota { background: #fdebd0; border-left: 4px solid #e67e22; padding: 8px 12px;',
  '             margin: 10px 0; border-radius: 3px; font-size: 13px; }',
  '</style>',
  ''
), con)

# ── SETUP CHUNK ──────────────────────────────────────────────────────────────
writeLines(c(
  '```{r setup, include=FALSE}',
  'knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,',
  '                      fig.width = 6, fig.height = 3.7, dpi = 150)',
  'library(kableExtra)',
  'library(ggplot2)',
  '```',
  ''
), con)

# ── RESUMEN EJECUTIVO ────────────────────────────────────────────────────────
writeLines(c(
  '# Resumen Ejecutivo {.unnumbered}',
  '',
  '<div class="hibrida">',
  '<strong>Variable híbrida SLS:</strong> combina el ground truth administrativo',
  '(<code>condicion_formalidad</code>) para asalariados cubiertos por la metodología',
  sprintf('EPH tradicional, con el backcasting SLS (umbral de calibración %.3g) para', UMBRAL_CAL),
  'cuenta propia, patrón, familiar no remunerado y desocupados potenciales.',
  'El resultado es una variable <strong>Formal / Informal</strong> con cobertura',
  'sobre el universo PEA completo. <strong>Ventaja SLS:</strong> κ̂γ garantiza',
  'coherencia estructural entre la ecuación de selección y la de resultado.',
  '</div>',
  ''
), con)

writeLines(c(
  '```{r kpi-table}',
  'knitr::kable(kpi_df, col.names = c("Indicador","Valor"),',
  paste0('  caption = "Tabla KPI — Variable Híbrida ', SUFIJO, '") |>'),
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 12) |>',
  '  row_spec(c(5,6,7,8), background = "#f3eef9") |>',
  '  row_spec(16, bold = TRUE, background = "#e8d5f5")',
  '```',
  ''
), con)

writeLines(c(
  '```{r comp-exec}',
  'knitr::kable(comp_exec,',
  '  col.names = c("Fuente","N","% del universo","Descripción"),',
  '  caption = "Composición de la variable híbrida SLS — universo PEA activos + desocupados") |>',
  '  kable_styling(bootstrap_options = c("striped","hover"), full_width = TRUE, font_size = 12) |>',
  '  row_spec(1, background = "#d6eaf8") |>',
  '  row_spec(2, background = "#fdebd0") |>',
  '  row_spec(3, bold = TRUE)',
  '```',
  ''
), con)

# ── S1 ───────────────────────────────────────────────────────────────────────
writeLines(c(
  '# S1. Validación directa — 4 trimestres observados',
  '',
  '<div class="nota">',
  'Los 4 trimestres de entrenamiento (2024T4-2025T3) son los únicos con',
  '<code>formalidad_valida</code> observada. Se compara la tasa formal observada',
  'con la variable híbrida en dos universos: ocupados activos (S1a) y PEA completa',
  '(S1b, incluye desocupados potenciales).',
  '</div>',
  ''
), con)

writeLines(c(
  '## S1.1 S1a: Serie estimada vs observada — Ocupados',
  '',
  '```{r s1a-plot}',
  'g_s1a',
  '```',
  '',
  '```{r s1a-tabla}',
  'knitr::kable(s1a |> mutate(across(c(tasa_obs, tasa_hibrida), ~paste0(round(.x,2),"%")),',
  '                            delta_pp = paste0(ifelse(delta_pp>=0,"+",""),round(delta_pp,2)," pp")),',
  '  col.names = c("Trimestre","N obs.","Tasa obs.","Tasa híbrida SLS","Delta"),',
  '  caption = "S1a: Validación ocupados — variable híbrida SLS vs observada") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 12)',
  '```',
  ''
), con)

writeLines(c(
  '## S1.2 S1b: Serie estimada vs observada — PEA',
  '',
  '<div class="alerta">',
  'El delta PEA versus el delta ocupados es mecánico: los desocupados',
  'representan ~6.1% del universo PEA y tienen tasa formal estimada baja,',
  'arrastrando la media hacia abajo. No es un error de calibración. [Lección L57]',
  '</div>',
  '',
  '```{r s1b-plot}',
  'g_s1b',
  '```',
  '',
  '```{r s1b-tabla}',
  'knitr::kable(',
  '  s1b |> select(periodo_id, n_activos_obs, tasa_obs, tasa_hibrida_pea, n_pea_total, delta_pp) |>',
  '         mutate(across(c(tasa_obs, tasa_hibrida_pea), ~paste0(round(.x,2),"%")),',
  '                delta_pp = paste0(ifelse(delta_pp>=0,"+",""),round(delta_pp,2)," pp"),',
  '                across(c(n_activos_obs, n_pea_total), ~format(.x, big.mark=","))),',
  '  col.names = c("Trimestre","N activos obs.","Tasa obs.",',
  '                "Tasa híbrida SLS PEA","N PEA total","Delta PEA"),',
  '  caption = "S1b: Validación PEA — variable híbrida SLS vs observada") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 12)',
  '```',
  ''
), con)

writeLines(c(
  '## S1.3 S1c: Descomposición del delta PEA',
  '',
  '```{r s1c-tabla}',
  'knitr::kable(',
  '  s1c |> select(periodo_id, n_pea_total, n_desoc, pct_desoc, tasa_hibrida_desoc, delta_pp) |>',
  '         mutate(across(c(tasa_hibrida_desoc), ~paste0(round(.x,2),"%")),',
  '                pct_desoc = paste0(round(pct_desoc,1),"%"),',
  '                delta_pp  = paste0(ifelse(delta_pp>=0,"+",""),round(delta_pp,2)," pp"),',
  '                n_pea_total = format(n_pea_total, big.mark=","),',
  '                n_desoc     = format(n_desoc, big.mark=",")),',
  '  col.names = c("Trimestre","N PEA total","N desoc.","% desoc.","Tasa formal desoc. est.","Delta PEA"),',
  '  caption = "S1c: Descomposición delta PEA — contribución desocupados") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 12)',
  '```',
  ''
), con)

writeLines(c(
  '## S1.4 S1d: Métricas de clasificación (variable híbrida SLS)',
  '',
  '```{r s1d-tabla}',
  'knitr::kable(s1d,',
  '  col.names = c("Métrica","Valor"),',
  '  caption = "S1d: Métricas de clasificación — variable híbrida SLS vs ground truth") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 12)',
  '```',
  ''
), con)

# ── S2 ───────────────────────────────────────────────────────────────────────
writeLines(c(
  '# S2. Composición de la variable híbrida',
  '',
  '<div class="nota">',
  'S2 descompone la variable híbrida por fuente (ground truth vs backcasting)',
  'para cada trimestre del panel (35T). Permite identificar qué fracción',
  'del universo PEA tiene información administrativa vs estimada.',
  '</div>',
  ''
), con)

writeLines(c(
  '## S2.1 S2a: Composición por trimestre',
  '',
  '```{r s2a-plot}',
  'g_s2a',
  '```',
  '',
  '```{r s2a-tabla}',
  'knitr::kable(',
  '  s2a |> mutate(pct_gt = paste0(pct_gt,"%"), pct_back = paste0(pct_back,"%"),',
  '                n_total = format(n_total, big.mark=",")),',
  '  col.names = c("Trimestre","N total","N GT","N Back.","%  GT","% Back."),',
  '  caption = "S2a: Composición GT vs Backcasting SLS por trimestre") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 11)',
  '```',
  ''
), con)

writeLines(c(
  '## S2.2 S2b: Tasa formal por fuente',
  '',
  '```{r s2b-plot}',
  'g_s2b',
  '```',
  ''
), con)

# ── S3 ───────────────────────────────────────────────────────────────────────
writeLines(c(
  '# S3. Extensión: otras categorías ocupacionales',
  ''
), con)

writeLines(c(
  '## S3.1 S3a: Composición de ocupados por categoría',
  '',
  '```{r s3a-comp-plot}',
  'g_s3a_comp',
  '```',
  '',
  '```{r s3a-global-tabla}',
  'knitr::kable(s3a_global,',
  '  col.names = c("Categoría","N","Tasa formal (%)"),',
  '  caption = "S3a: Tasa formal híbrida SLS por categoría ocupacional (global)") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 12)',
  '```',
  ''
), con)

writeLines(c(
  '## S3.2 S3b: Ocupados vs desocupados potenciales',
  '',
  '```{r s3b-plot}',
  'g_s3b',
  '```',
  '',
  '```{r s3b-impacto-tabla}',
  'knitr::kable(s3b_impacto,',
  '  col.names = c("Categoría","N desocupados","Tasa formal est. (%)"),',
  '  caption = "S3b: Tasa formal SLS — desocupados potenciales por categoría ocupacional") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 12)',
  '```',
  ''
), con)

# ── S4 ───────────────────────────────────────────────────────────────────────
writeLines(c(
  '# S4. Series temporales completas',
  '',
  '<div class="nota">',
  'S4 presenta las series de formalidad sobre el universo PEA activos (35 trimestres).',
  'Tres versiones: umbral Youden, umbral calibración y variable híbrida.',
  'Las zonas coloreadas marcan pandemia (violeta) y trimestres observados (naranja).',
  '</div>',
  ''
), con)

writeLines(c(
  '## S4.1 PEA activos — 3 versiones',
  '',
  '```{r s4-pea-plot}',
  'g_s4_pea_activos',
  '```',
  '',
  '```{r s4-pea-tabla}',
  'knitr::kable(',
  '  s4_pea_activos |>',
  '    mutate(across(c(tasa_youden, tasa_cal, tasa_hibrida), ~paste0(round(.x,2),"%")),',
  '           n = format(n, big.mark=",")),',
  paste0('  col.names = c("Trimestre","N","Tasa ', LBL_YOUDEN, '","Tasa ', LBL_CAL_B, '","Tasa híbrida"),'),
  '  caption = "S4.1: Series completas SLS — PEA activos") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 11)',
  '```',
  ''
), con)

writeLines(c(
  '## S4.2 Universo Edad 18-60',
  '',
  '```{r s4-edad-plot}',
  'g_s4_edad',
  '```',
  ''
), con)

# ── S5 ───────────────────────────────────────────────────────────────────────
writeLines(c(
  '# S5. Formalidad potencial',
  '',
  '<div class="nota">',
  'S5 presenta la tasa de formalidad estimada para los universos sin ground truth:',
  'desocupados potenciales (PEA) e inactivos (Edad 18-60).',
  '</div>',
  ''
), con)

writeLines(c(
  '## S5.1 Desocupados e inactivos',
  '',
  '```{r s5-plot}',
  'g_s5',
  '```',
  '',
  '```{r s5-tabla-desoc}',
  'knitr::kable(',
  '  s5_desoc |> mutate(n = format(n, big.mark=","),',
  '                     tasa_formal = paste0(round(tasa_formal,1),"%")),',
  '  col.names = c("Trimestre","N desocupados","Tasa formal est."),',
  paste0('  caption = paste0("S5.1: Formalidad potencial — Desocupados PEA (Cal. ", UMBRAL_CAL, ")")) |>'),
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 12)',
  '```',
  '',
  '```{r s5-tabla-inact}',
  'knitr::kable(',
  '  s5_inact |> mutate(n = format(n, big.mark=","),',
  '                     tasa_formal = paste0(round(tasa_formal,1),"%")),',
  '  col.names = c("Trimestre","N inactivos 18-60","Tasa formal est."),',
  paste0('  caption = paste0("S5.2: Formalidad potencial — Inactivos Edad 18-60 (Cal. ", UMBRAL_CAL, ")")) |>'),
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 12)',
  '```',
  ''
), con)

# ── S6 ───────────────────────────────────────────────────────────────────────
writeLines(c(
  '# S6. Cobertura del universo',
  '',
  '## S6.1 Universo PEA',
  '',
  '```{r s6-tabla-pea}',
  'knitr::kable(',
  '  s6_cobertura |>',
  '    select(periodo_id, n_total, n_pea_activos, n_pea_total,',
  '           n_sin_theta, n_hibrida_val, pct_cobertura_pea) |>',
  '    mutate(across(c(n_total, n_pea_activos, n_pea_total, n_sin_theta, n_hibrida_val),',
  '                  ~format(.x, big.mark=",")),',
  '           pct_cobertura_pea = paste0(pct_cobertura_pea, "%")),',
  '  col.names = c("Trimestre","N panel","N PEA activos","N PEA+desoc.",',
  '                "N sin theta","N híbrida válida","% cob. PEA activos"),',
  '  caption = "S6.1: Cobertura — Universo PEA por trimestre") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = TRUE, font_size = 11)',
  '```',
  '',
  '## S6.2 Universo Edad',
  '',
  '```{r s6-tabla-edad}',
  's6_edad_tab <- s6_cobertura |>',
  '  select(periodo_id, n_total, n_edad) |>',
  '  mutate(pct_edad = round(n_edad / n_total * 100, 1),',
  '         n_total  = format(n_total, big.mark = ","),',
  '         n_edad   = format(n_edad,  big.mark = ","),',
  '         pct_edad = paste0(pct_edad, "%"))',
  'knitr::kable(s6_edad_tab,',
  '  col.names = c("Trimestre","N panel","N Edad 18-60","% cobertura Edad"),',
  '  caption = "S6.2: Cobertura — Universo Edad 18-60 por trimestre") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = FALSE, font_size = 11)',
  '```',
  ''
), con)

# ── S7 ───────────────────────────────────────────────────────────────────────
# S7.2 no incluye H-L test (específico del GLM binomial).
# S7.3 adaptada para SLS: clipping en OOS (Horrace & Oaxaca 2003).
writeLines(c(
  '# S7. Notas metodológicas',
  '',
  '## S7.1 Variable híbrida — construcción y justificación',
  '',
  '<div class="hibrida">',
  '<strong>¿Qué es la variable híbrida SLS?</strong>',
  paste0('<p>La variable <code>', COL_HIBRIDA_PEA, '</code> combina dos fuentes de información'),
  'para clasificar a cada individuo del universo PEA como <em>Formal</em> o <em>Informal</em>:</p>',
  '<ol>',
  '  <li><strong>Ground truth administrativo</strong> (<code>condicion_formalidad</code>):',
  '  para asalariados con <code>condicion_formalidad</code> en {"Formal", "No formal"}.</li>',
  paste0('  <li><strong>Backcasting SLS</strong> (<code>', COL_CLASE_CAL_PEA, '</code>,'),
  sprintf('  umbral %.3g): para individuos con <code>condicion_formalidad == "No corresponde"</code>', UMBRAL_CAL),
  '  (cuenta propia, patrón, familiar) o sin información de formalidad (desocupados potenciales).',
  '  El κ̂γ del SLS garantiza coherencia entre la ecuación de selección al empleo',
  '  y la ecuación de resultado de formalidad.</li>',
  '</ol>',
  '</div>',
  ''
), con)

writeLines(c(
  '## S7.2 Dos umbrales de clasificación',
  '',
  paste0('- **Umbral Youden:** ', UMBRAL_YOUDEN,
         ' (maximiza sensibilidad + especificidad en training)'),
  paste0('- **Umbral Calibración:** ', UMBRAL_CAL,
         ' (minimiza delta vs tasa observada; delta max = ',
         c08$metricas_calibracion$delta_max_pp, ' pp)'),
  sprintf('- **La variable híbrida usa umbral de calibración (%.3g)** para la parte backcasting,', UMBRAL_CAL),
  '  garantizando máxima calibración de la tasa global.',
  ''
), con)

writeLines(c(
  '## S7.3 [LN4] Predicciones fuera de [0,1] — propiedad estructural SLS',
  '',
  '<div class="sls-nota">',
  '<strong>Nota SLS:</strong> el algoritmo Sequential Least Squares (Horrace &amp; Oaxaca 2003)',
  'garantiza predicciones en [0,1] solo dentro del subconjunto de entrenamiento kappa.',
  'Fuera de muestra (backcasting hacia 2016T4–2024T3) no existe garantía de acotamiento.',
  'Las predicciones son clipeadas con <code>pmax(0, pmin(1, pred))</code> antes de clasificar.',
  '',
  paste0('En la variable híbrida, el segmento <em>ground truth</em> (~', pct_gt, '% de observaciones)',
         ' no depende del acotamiento SLS.'),
  'El riesgo de predicciones fuera de [0,1] aplica únicamente al segmento backcasting',
  paste0('(~', pct_back, '%): cuenta propia, patrón, familiar y desocupados potenciales.'),
  '</div>',
  '',
  '```{r s7-pred-fuera}',
  'knitr::kable(',
  '  tibble(',
  '    Contexto = c(',
  '      "kappa (train SLS) — acotado por construcción",',
  '      "Test set 07b (SLS) — benchmark",',
  '      "Unión PEA (backcasting completo)"',
  '    ),',
  '    `% fuera [0,1]` = c(',
  '      "0.00%",',
  paste0('      paste0(round(c08$benchmarks_modelo$pct_fuera_test, 1), "%"),'),
  '      paste0(round(c08$pct_pred_fuera_01_union, 1), "%")',
  '    ),',
  '    Nota = c(',
  '      "Garantía algoritmo SLS — Horrace & Oaxaca (2003)",',
  '      "Referencia benchmark 07b",',
  '      "Incl. desocupados (alta frecuencia fuera) y backcasting histórico"',
  '    )',
  '  ),',
  '  caption = "[LN4] Predicciones fuera de [0,1] — contextos SLS") |>',
  '  kable_styling(bootstrap_options = c("striped","hover","condensed"),',
  '                full_width = TRUE, font_size = 12) |>',
  '  row_spec(1, background = "#d4edda") |>',
  '  row_spec(3, background = "#fdebd0")',
  '```',
  ''
), con)

writeLines(c(
  '## S7.4 Predicciones fuera de muestra',
  '',
  '<div class="nota">',
  'Las predicciones del modelo SLS para períodos anteriores a 2024T4 son',
  '<em>out-of-sample</em> (backcasting). El modelo fue entrenado en los 4 trimestres',
  'observados y aplicado hacia atrás. La estabilidad de los coeficientes',
  'fue verificada en 07c_lasso_tiempo_SLS.R. El κ̂γ garantiza coherencia entre',
  'selección y resultado, a diferencia del LPM. Las predicciones fuera del subconjunto',
  'kappa se clipean a [0,1] antes de aplicar el umbral de clasificación.',
  '</div>',
  ''
), con)

writeLines(c(
  '## S7.5 Limitaciones',
  '',
  paste0('- **Pandemia (', paste(TRIM_PANDEMIA[c(1,6)], collapse="–"), '):**',
         ' Patrones de formalidad atípicos. Reportar MAE.'),
  paste0('- **Delta PEA vs ocupados (~', round(mean(abs(s1b$delta_pp)), 1), ' pp):',
         ' Causa mecánica (desocupados ~6.1% del universo, tasa formal baja).'),
  '  No es error de calibración. [Lección L57]',
  '- **[LN4] SLS fuera de [0,1]:** Garantizado en kappa, no en OOS.',
  '  Segmento ground truth no afectado. Clipping aplicado en segmento backcasting.',
  '  Ref: Horrace & Oaxaca (2003).',
  '- **condicion_formalidad:** Join período-específico (id_individuo + periodo_id)',
  '  para evitar asignar el valor de un período a otro. [L59]',
  '- **Panel SLS canónico:** el panel SLS (~110 cols) contiene columnas LPM + GLM + SLS',
  '  y es el input recomendado para los comparativos 09c.',
  ''
), con)

# ── CONCLUSIÓN ───────────────────────────────────────────────────────────────
writeLines(c(
  '# Conclusión {.unnumbered}',
  '',
  '<div class="hibrida">',
  paste0(
    'La **variable híbrida SLS** integra ground truth administrativo (',
    pct_gt, '% de las observaciones PEA) con backcasting SLS (', pct_back, '%), ',
    'logrando cobertura completa sobre el universo PEA activo y desocupados potenciales.'
  ),
  '',
  paste0(
    'La validación directa sobre los 4 trimestres observados muestra un delta promedio ',
    'de **', round(mean(abs(s1a$delta_pp)), 2), ' pp** en el universo de ocupados. ',
    'El delta sobre PEA completa (**', round(mean(abs(s1b$delta_pp)), 2), ' pp**) ',
    'es mecánico: refleja la baja tasa formal de los desocupados potenciales, ',
    'no un problema de calibración del modelo.'
  ),
  '',
  paste0(
    'La variable híbrida SLS es el input recomendado para los análisis del paper que ',
    'requieran cobertura PEA + coherencia estructural entre selección y resultado. ',
    'Ventaja sobre la versión LPM: κ̂γ garantiza que la clasificación respeta la ',
    'estructura de selección al empleo (Horrace & Oaxaca 2003). ',
    'Ventaja sobre GLM: alineación con el modelo de selección. ',
    'El backcasting puro (08b) es preferible cuando se necesita comparabilidad ',
    'metodológica homogénea entre categorías. ',
    '**El panel SLS (~110 cols) es el canónico para los comparativos 09c.**'
  ),
  '</div>',
  '',
  '---',
  paste0('*Script: 08c_reporte_hibrido_SLS.R | Modelo: ', SUFIJO_MODELO_SLS,
         ' | Generado: `r format(Sys.time(), "%Y-%m-%d %H:%M")`*'),
  ''
), con)

close(con)
cat("   [OK] Rmd temporal escrito:", rmd_temp, "\n\n")


# 🪫 13. RENDER HTML ------------------------------------------------------------
cat("-- 13. Renderizando HTML ----------------------------------------------\n")

rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_08C_HTML,
  quiet       = TRUE,
  envir       = environment()
)
unlink(rmd_temp)
cat("   [OK] HTML generado:", PATH_08C_HTML, "\n\n")


# 📑 Checklist -----------------------------------------------------------------
cat("-- 14. Checklist de salidas ------------------------------------------\n")
cat("   [OK] HTML:", file.exists(PATH_08C_HTML), "\n")
cat("   [OK] TXT: ", file.exists(PATH_TXT_08C),  "\n")

n_pdfs <- length(list.files(DIR_FIGURAS_08C_SLS, pattern = "\\.pdf$"))
cat(sprintf("   [OK] PDFs en figuras: %d/9\n", n_pdfs))

rm(list = c("panel", "pea_activos", "pea_total", "obs_trim",
            "s1a", "s1b", "s1b_obs", "s1b_pea", "s1c", "s1d", "s1d_df",
            "s2a", "s2a_long", "s2b", "s2b_global",
            "s3a_global", "s3a_comp", "s3b_impacto", "s3b_serie",
            "s4_pea_activos", "s4_long", "s4_edad",
            "s5_desoc", "s5_inact",
            "s6_cobertura",
            "kpi_df", "comp_exec",
            "cols_ok", "cols_necesarias"))
gc()

cat("\n===================================================================\n")
cat("SCRIPT 08c COMPLETADO\n")
cat("  HTML:", basename(PATH_08C_HTML), "\n")
cat("  TXT: ", basename(PATH_TXT_08C),  "\n")
cat(sprintf("  PDFs: %d/9 en %s\n", n_pdfs, basename(DIR_FIGURAS_08C_SLS)))
cat("===================================================================\n")

toc()
