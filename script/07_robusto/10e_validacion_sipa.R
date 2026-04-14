# =============================================================================
# [EN] 10e_validacion_sipa.R -- External validation: EPH hybrid formality vs SIPA registered employment (admin data)
# INPUTS:  proyecto/sipa/trabajoregistrado_*.xlsx, rdos/datos/08_panel_formalidad_SLS*.rds
# OUTPUTS: rdos/reportes/00_validacion_sipa.html, rdos/figuras/00_validacion_sipa/*.pdf
# =============================================================================
# 🌟 10e_validacion_sipa.R 🌟 ####
# Validación externa: EPH formalidad híbrida vs SIPA empleo registrado
# Proyecto: formalidad_rev  |  Auxiliar (no-pipeline, bajo demanda)
#
# OBJETIVO:
#   Comparar la serie trimestral de formalidad EPH (híbrida: observada + predicha)
#   contra la serie de empleo registrado SIPA (administrativa), desagregada por
#   modalidad ocupacional. Validación macro de co-movimiento (no niveles).
#
# INPUTS:
#   proyecto/sipa/trabajoregistrado_2512_estadisticas.xlsx  (hoja T.2.1)
#   rdos/datos/08_panel_formalidad_{SUFIJO_MODELO_SLS}.rds   (panel consolidado)
#   rdos/contratos/08_contrato_backcasting_{SUFIJO_MODELO_GLM}.rds
#
# OUTPUTS:
#   rdos/reportes/00_validacion_sipa.html
#   rdos/figuras/00_validacion_sipa/*.pdf   (via guardar_figura)
#   rdos/reportes/00_validacion_sipa_notas.txt
#
# TIEMPO ESTIMADO: ~3 minutos (lectura panel + render)

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
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

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ⌛ Inicio contador de tiempo -------------------------------------------------

t_inicio <- proc.time()
cat("===================================================================\n")
cat("SCRIPT 00 - VALIDACIÓN EXTERNA EPH vs SIPA\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")

# 🔑 1. PATHS Y DIRECTORIOS --------------------------------------------------
cat("-- 1. Paths y directorios ---------------------------------------------\n")

PATH_SIPA_XLSX <- here::here("proyecto", "sipa",
                              "trabajoregistrado_2512_estadisticas.xlsx")
PATH_HTML_OUT  <- file.path(DIR_REPORTES, "00_validacion_sipa.html")
PATH_NOTAS_OUT <- file.path(DIR_REPORTES, "00_validacion_sipa_notas.txt")
DIR_FIG_SIPA   <- file.path(DIR_FIGURAS, "00_validacion_sipa")
dir.create(DIR_FIG_SIPA, showWarnings = FALSE, recursive = TRUE)

hard_stop(file.exists(PATH_SIPA_XLSX),  "No existe XLSX del SIPA")
hard_stop(file.exists(PATH_08_PANEL_CONSOLIDADO), "No existe panel consolidado SLS")
hard_stop(file.exists(PATH_08_CONTRATO_GLM), "No existe contrato backcasting GLM")
cat("   [OK] Inputs verificados\n")

# 🪫 2. LECTURA Y PROCESAMIENTO SIPA -----------------------------------------
cat("-- 2. Lectura SIPA (hoja T.2.1) ---------------------------------------\n")

sipa_raw <- read_excel(PATH_SIPA_XLSX, sheet = "T.2.1", col_names = FALSE)

# Columnas: Periodo | Asal.Privado | Asal.Publico | Casas.Part | Autonomos | Monotributo | ...
# Filas 1-2: título + encabezado; datos desde fila 3.
# Fechas tienen formato mixto: serial Excel (2012-2017) o texto "ene-18", "jul-25*"
nombres_sipa <- c("fecha_raw", "asal_privado", "asal_publico",
                   "casas_particulares", "autonomos", "monotributo")

# Meses en español para parseo
meses_es <- c("ene" = 1, "feb" = 2, "mar" = 3, "abr" = 4, "may" = 5, "jun" = 6,
              "jul" = 7, "ago" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dic" = 12)

# Limpiar: tomar solo las 6 primeras columnas con datos
sipa <- sipa_raw %>%
  select(1:6) %>%
  set_names(nombres_sipa) %>%
  filter(!is.na(fecha_raw)) %>%
  # Descartar filas de encabezado/notas

  filter(!grepl("^T\\.", fecha_raw),
         !grepl("^Per", fecha_raw, ignore.case = TRUE),
         !grepl("^Nota", fecha_raw, ignore.case = TRUE),
         !grepl("^Fuente", fecha_raw, ignore.case = TRUE),
         !grepl("^\\*", fecha_raw),
         !grepl("asalariados", fecha_raw, ignore.case = TRUE)) %>%
  mutate(
    # Quitar asteriscos de datos provisorios
    fecha_clean = gsub("\\*", "", fecha_raw),
    # Intentar parsear como numérico (serial Excel)
    fecha_num = suppressWarnings(as.numeric(fecha_clean))
  )

# Parsear fechas en dos pasos para evitar que case_when evalúe todo
# Paso A: filas con serial Excel
sipa$fecha <- as.Date(NA)
idx_num <- !is.na(sipa$fecha_num)
sipa$fecha[idx_num] <- as.Date(sipa$fecha_num[idx_num], origin = "1899-12-30")

# Paso B: filas con texto español "ene-18", "dic-25", etc.
idx_txt <- is.na(sipa$fecha) & grepl("^[a-z]{3}-\\d{2,4}$", sipa$fecha_clean)
if (sum(idx_txt) > 0) {
  partes <- strsplit(sipa$fecha_clean[idx_txt], "-")
  mes_v <- sapply(partes, `[`, 1)
  anio_v <- sapply(partes, `[`, 2)
  anio_v <- ifelse(nchar(anio_v) == 2, paste0("20", anio_v), anio_v)
  mes_num <- meses_es[mes_v]
  sipa$fecha[idx_txt] <- as.Date(paste0(anio_v, "-", sprintf("%02d", mes_num), "-01"))
}

sipa <- sipa %>%
  filter(!is.na(fecha)) %>%
  mutate(
    anio  = year(fecha),
    mes   = month(fecha),
    trim  = ceiling(mes / 3),
    periodo_q = paste0("T", trim, "_", anio),
    across(all_of(nombres_sipa[-1]), as.numeric)
  ) %>%
  select(-fecha_raw, -fecha_clean, -fecha_num) %>%
  # Crear agregados comparables con EPH
  mutate(
    sipa_asalariados    = asal_privado + asal_publico,
    sipa_independientes = autonomos + monotributo,
    sipa_total          = sipa_asalariados + casas_particulares + sipa_independientes
  )

cat(sprintf("   [OK] SIPA: %d meses, rango %s a %s\n",
            nrow(sipa), min(sipa$fecha), max(sipa$fecha)))

# Agregar a trimestral (promedio de los 3 meses)
sipa_q <- sipa %>%
  group_by(anio, trim, periodo_q) %>%
  summarise(
    across(c(sipa_asalariados, sipa_independientes, sipa_total,
             asal_privado, asal_publico, autonomos, monotributo,
             casas_particulares),
           ~ mean(.x, na.rm = TRUE)),
    n_meses = n(),
    .groups = "drop"
  ) %>%
  filter(n_meses == 3) %>%  # solo trimestres completos
  arrange(anio, trim)

cat(sprintf("   [OK] SIPA trimestral: %d trimestres completos\n", nrow(sipa_q)))

# 🪫 3. LECTURA Y PROCESAMIENTO EPH ------------------------------------------
cat("-- 3. Lectura panel EPH -----------------------------------------------\n")

cols_necesarias <- c(
  "periodo_id", "periodo", "pondera",
  "condicion_actividad", "categoria_ocupacional",
  "formalidad_empleo", "formalidad_valida",
  paste0("formalidad_clase_cal_", SUFIJO_MODELO_GLM, "_pea")
)

panel_full <- readRDS(PATH_08_PANEL_CONSOLIDADO)
panel <- panel_full[, intersect(cols_necesarias, names(panel_full))]
rm(panel_full); gc()

cat(sprintf("   [OK] Panel: %s obs x %d cols\n",
            format(nrow(panel), big.mark = "."), ncol(panel)))

# Extraer anio/trim del periodo (formato "T1_2017")
panel <- panel %>%
  mutate(
    anio = as.integer(sub("T\\d+_", "", periodo)),
    trim = as.integer(sub("T(\\d+)_.*", "\\1", periodo)),
    periodo_q = periodo
  )

# Nombre dinámico de columna de clasificación calibrada GLM
col_clase_cal_glm <- paste0("formalidad_clase_cal_", SUFIJO_MODELO_GLM, "_pea")

# Variable híbrida de formalidad (observada donde existe, predicha donde no)
panel <- panel %>%
  filter(condicion_actividad == "Ocupado") %>%
  mutate(
    es_formal = case_when(
      # Overlap (2024Q4+): usar variable observada
      formalidad_empleo == "Formal oficial" ~ 1L,
      formalidad_empleo == "Informal oficial" ~ 0L,
      # Backcast: usar predicción calibrada GLM
      .data[[col_clase_cal_glm]] == "Formal" ~ 1L,
      .data[[col_clase_cal_glm]] == "Informal" ~ 0L,
      TRUE ~ NA_integer_
    ),
    es_asalariado = categoria_ocupacional == "Empleado",
    es_independiente = categoria_ocupacional %in% c("Cuenta Propia", "Patrón")
  )

# Serie trimestral EPH: total, asalariados, independientes
# Ponderada por pondera (expansor poblacional)
eph_q <- panel %>%
  filter(!is.na(es_formal)) %>%
  group_by(anio, trim, periodo_q) %>%
  summarise(
    n_obs           = n(),
    eph_tasa_total  = weighted.mean(es_formal, pondera, na.rm = TRUE),
    eph_n_formal    = sum(es_formal * pondera, na.rm = TRUE),
    eph_n_total     = sum(pondera, na.rm = TRUE),
    # Asalariados
    eph_tasa_asal   = weighted.mean(es_formal[es_asalariado],
                                     pondera[es_asalariado], na.rm = TRUE),
    eph_n_formal_asal = sum(es_formal[es_asalariado] * pondera[es_asalariado],
                            na.rm = TRUE),
    eph_n_asal      = sum(pondera[es_asalariado], na.rm = TRUE),
    # Independientes
    eph_tasa_indep  = weighted.mean(es_formal[es_independiente],
                                     pondera[es_independiente], na.rm = TRUE),
    eph_n_formal_indep = sum(es_formal[es_independiente] * pondera[es_independiente],
                             na.rm = TRUE),
    eph_n_indep     = sum(pondera[es_independiente], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(anio, trim)

cat(sprintf("   [OK] EPH trimestral: %d trimestres, rango %s a %s\n",
            nrow(eph_q), min(eph_q$periodo_q), max(eph_q$periodo_q)))

rm(panel); gc()

# 🪫 4. MERGE Y CONSTRUCCIÓN DE SERIES COMPARABLES ---------------------------
cat("-- 4. Merge EPH-SIPA --------------------------------------------------\n")

# Período común
comp <- inner_join(eph_q, sipa_q, by = c("anio", "trim", "periodo_q")) %>%
  arrange(anio, trim)

comp_sorted <- comp %>% arrange(anio, trim)
cat(sprintf("   [OK] Trimestres comunes: %d (%s a %s)\n",
            nrow(comp), comp_sorted$periodo_q[1], comp_sorted$periodo_q[nrow(comp_sorted)]))

# Calcular magnitudes primero, luego índices
comp <- comp %>%
  mutate(
    # EPH: número de formales expandido (en miles, para comparar magnitudes)
    eph_formales_miles       = eph_n_formal / 1000,
    eph_formales_asal_miles  = eph_n_formal_asal / 1000,
    eph_formales_indep_miles = eph_n_formal_indep / 1000,
    # Fecha para eje X
    fecha_q = as.Date(paste0(anio, "-", (trim - 1) * 3 + 2, "-15"))
  )

# Índices base 100 = primer trimestre común
base_q <- comp %>% slice(1)

comp <- comp %>%
  mutate(
    idx_eph_total  = eph_formales_miles / base_q$eph_formales_miles[1] * 100,
    idx_sipa_total = sipa_total / base_q$sipa_total[1] * 100,
    idx_eph_asal   = eph_formales_asal_miles / base_q$eph_formales_asal_miles[1] * 100,
    idx_sipa_asal  = sipa_asalariados / base_q$sipa_asalariados[1] * 100,
    idx_eph_indep  = eph_formales_indep_miles / base_q$eph_formales_indep_miles[1] * 100,
    idx_sipa_indep = sipa_independientes / base_q$sipa_independientes[1] * 100
  )

# 🪫 5. CORRELACIONES --------------------------------------------------------
cat("-- 5. Correlaciones ----------------------------------------------------\n")

# Correlación en niveles (índices)
cor_total_nivel  <- cor(comp$idx_eph_total, comp$idx_sipa_total, use = "complete.obs")
cor_asal_nivel   <- cor(comp$idx_eph_asal, comp$idx_sipa_asal, use = "complete.obs")
cor_indep_nivel  <- cor(comp$idx_eph_indep, comp$idx_sipa_indep, use = "complete.obs")

# Correlación en variaciones intertrimestrales (Δ)
calc_delta_cor <- function(x, y) {
  dx <- diff(x)
  dy <- diff(y)
  if (length(dx) < 3) return(NA_real_)
  cor(dx, dy, use = "complete.obs")
}

cor_total_delta  <- calc_delta_cor(comp$idx_eph_total, comp$idx_sipa_total)
cor_asal_delta   <- calc_delta_cor(comp$idx_eph_asal, comp$idx_sipa_asal)
cor_indep_delta  <- calc_delta_cor(comp$idx_eph_indep, comp$idx_sipa_indep)

cat(sprintf("   Correlaciones niveles:  Total=%.3f | Asal=%.3f | Indep=%.3f\n",
            cor_total_nivel, cor_asal_nivel, cor_indep_nivel))
cat(sprintf("   Correlaciones deltas:   Total=%.3f | Asal=%.3f | Indep=%.3f\n",
            cor_total_delta, cor_asal_delta, cor_indep_delta))

# 🪫 6. MAGNITUDES COMPARADAS ------------------------------------------------
cat("-- 6. Magnitudes -------------------------------------------------------\n")

# Último trimestre con datos
ultimo <- comp %>% slice(n())
cat(sprintf("   Último trimestre: %s\n", ultimo$periodo_q))
cat(sprintf("   EPH formales (expandido): %.0f mil | SIPA registrados: %.0f mil\n",
            ultimo$eph_formales_miles, ultimo$sipa_total))
cat(sprintf("   EPH asal formales: %.0f mil | SIPA asalariados: %.0f mil\n",
            ultimo$eph_formales_asal_miles, ultimo$sipa_asalariados))
cat(sprintf("   EPH indep formales: %.0f mil | SIPA indep (aut+mono): %.0f mil\n",
            ultimo$eph_formales_indep_miles, ultimo$sipa_independientes))

# Ratio EPH/SIPA por segmento
ratio_asal  <- mean(comp$eph_formales_asal_miles / comp$sipa_asalariados, na.rm = TRUE)
ratio_indep <- mean(comp$eph_formales_indep_miles / comp$sipa_independientes, na.rm = TRUE)
ratio_total <- mean(comp$eph_formales_miles / comp$sipa_total, na.rm = TRUE)

cat(sprintf("   Ratio medio EPH/SIPA: Total=%.2f | Asal=%.2f | Indep=%.2f\n",
            ratio_total, ratio_asal, ratio_indep))

# 🪫 7. NOTAS PARA EL PAPER --------------------------------------------------
cat("-- 7. Notas para el paper ---------------------------------------------\n")

notas_con <- file(PATH_NOTAS_OUT, open = "wt", encoding = "UTF-8")
cat(sprintf("VALIDACIÓN EXTERNA EPH vs SIPA — %s\n", format(Sys.time(), "%Y-%m-%d")),
    file = notas_con)
cat("========================================\n\n", file = notas_con)

cat(sprintf("Período comparado: %s a %s (%d trimestres)\n\n",
            min(comp$periodo_q), max(comp$periodo_q), nrow(comp)),
    file = notas_con)

cat("CORRELACIONES DE ÍNDICES (base 100 = primer trimestre común):\n", file = notas_con)
cat(sprintf("  Total:          Pearson r = %.3f (niveles), r = %.3f (Δ trimestrales)\n",
            cor_total_nivel, cor_total_delta), file = notas_con)
cat(sprintf("  Asalariados:    Pearson r = %.3f (niveles), r = %.3f (Δ trimestrales)\n",
            cor_asal_nivel, cor_asal_delta), file = notas_con)
cat(sprintf("  Independientes: Pearson r = %.3f (niveles), r = %.3f (Δ trimestrales)\n\n",
            cor_indep_nivel, cor_indep_delta), file = notas_con)

cat("MAGNITUDES (último trimestre):\n", file = notas_con)
cat(sprintf("  EPH formales expandidos: %.0f mil\n", ultimo$eph_formales_miles),
    file = notas_con)
cat(sprintf("  SIPA registrados total:  %.0f mil\n", ultimo$sipa_total),
    file = notas_con)
cat(sprintf("  Ratio EPH/SIPA (promedio): %.2f\n\n", ratio_total), file = notas_con)

cat("NOTA METODOLÓGICA:\n", file = notas_con)
cat("  - EPH: 31 aglomerados urbanos (91%% pob. urbana). Formalidad = variable\n",
    file = notas_con)
cat("    híbrida (observada 2024Q4+ / predicha GLM calibrado pre-2024Q4).\n",
    file = notas_con)
cat("  - SIPA: cobertura nacional, registro administrativo (F.931 + monotributo).\n",
    file = notas_con)
cat("  - Diferencia de universo explica gap de niveles; la comparación es de\n",
    file = notas_con)
cat("    co-movimiento (tendencias), no de magnitudes absolutas.\n",
    file = notas_con)
cat("  - SIPA desestacionalizado no usado; se comparan series con estacionalidad\n",
    file = notas_con)
cat("    (T.2.1) ya que la EPH también tiene estacionalidad.\n", file = notas_con)
close(notas_con)
cat(sprintf("   [OK] Notas: %s\n", PATH_NOTAS_OUT))

# 🪫 8. GRÁFICOS (precalculados, pasados al Rmd) -----------------------------
cat("-- 8. Gráficos ---------------------------------------------------------\n")

# Colores para fuentes: EPH usa PAL_DESCRIPTIVO[1], SIPA usa PAL_DESCRIPTIVO[2]
COL_EPH  <- PAL_DESCRIPTIVO[1]   # azul medio
COL_SIPA <- PAL_DESCRIPTIVO[2]   # rojo

# Datos long para gráficos de índices
comp_long_total <- comp %>%
  select(fecha_q, periodo_q, idx_eph_total, idx_sipa_total) %>%
  pivot_longer(cols = starts_with("idx_"),
               names_to = "fuente", values_to = "indice") %>%
  mutate(fuente = case_when(
    fuente == "idx_eph_total" ~ tr("EPH (hybrid formality)"),
    fuente == "idx_sipa_total" ~ tr("SIPA (registered employment)")
  ))

comp_long_asal <- comp %>%
  select(fecha_q, periodo_q, idx_eph_asal, idx_sipa_asal) %>%
  pivot_longer(cols = starts_with("idx_"),
               names_to = "fuente", values_to = "indice") %>%
  mutate(fuente = case_when(
    fuente == "idx_eph_asal" ~ tr("EPH formal employees"),
    fuente == "idx_sipa_asal" ~ tr("SIPA registered employees")
  ))

comp_long_indep <- comp %>%
  select(fecha_q, periodo_q, idx_eph_indep, idx_sipa_indep) %>%
  pivot_longer(cols = starts_with("idx_"),
               names_to = "fuente", values_to = "indice") %>%
  mutate(fuente = case_when(
    fuente == "idx_eph_indep" ~ tr("EPH formal self-employed"),
    fuente == "idx_sipa_indep" ~ tr("SIPA registered self-employed")
  ))

# Gráfico de co-movimiento: Total
p_total <- ggplot(comp_long_total,
                  aes(x = fecha_q, y = indice, color = fuente, linetype = fuente)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_color_manual(values = setNames(c(COL_EPH, COL_SIPA),
                     tr(c("EPH (hybrid formality)", "SIPA (registered employment)")))) +
  scale_linetype_manual(values = setNames(c("solid", "dashed"),
                        tr(c("EPH (hybrid formality)", "SIPA (registered employment)")))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tr_labs(title = "Co-movement EPH vs SIPA: Total formal employment",
       subtitle = sprintf("Index base 100 = %s | Pearson r = %.3f (levels)",
                           min(comp$periodo_q), cor_total_nivel),
       x = NULL, y = "Index (base 100)") +
  theme_paper() +
  theme(legend.position = "bottom")

# Gráfico de co-movimiento: Asalariados
p_asal <- ggplot(comp_long_asal,
                 aes(x = fecha_q, y = indice, color = fuente, linetype = fuente)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_color_manual(values = setNames(c(COL_EPH, COL_SIPA),
                     tr(c("EPH formal employees", "SIPA registered employees")))) +
  scale_linetype_manual(values = setNames(c("solid", "dashed"),
                        tr(c("EPH formal employees", "SIPA registered employees")))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tr_labs(title = "Co-movement EPH vs SIPA: Employees",
       subtitle = sprintf("Pearson r = %.3f (levels) | r = %.3f (quarterly Δ)",
                           cor_asal_nivel, cor_asal_delta),
       x = NULL, y = "Index (base 100)") +
  theme_paper() +
  theme(legend.position = "bottom")

# Gráfico de co-movimiento: Independientes
p_indep <- ggplot(comp_long_indep,
                  aes(x = fecha_q, y = indice, color = fuente, linetype = fuente)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_color_manual(values = setNames(c(COL_EPH, COL_SIPA),
                     tr(c("EPH formal self-employed", "SIPA registered self-employed")))) +
  scale_linetype_manual(values = setNames(c("solid", "dashed"),
                        tr(c("EPH formal self-employed", "SIPA registered self-employed")))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tr_labs(title = "Co-movement EPH vs SIPA: Self-employed",
       subtitle = sprintf("Pearson r = %.3f (levels) | r = %.3f (quarterly Δ)",
                           cor_indep_nivel, cor_indep_delta),
       x = NULL, y = "Index (base 100)") +
  theme_paper() +
  theme(legend.position = "bottom")

# Scatter: EPH vs SIPA (Δ trimestrales)
delta_df <- tibble(
  segmento = rep(tr(c("Total", "Employees", "Self-employed")),
                 each = nrow(comp) - 1),
  delta_eph = c(diff(comp$idx_eph_total), diff(comp$idx_eph_asal),
                diff(comp$idx_eph_indep)),
  delta_sipa = c(diff(comp$idx_sipa_total), diff(comp$idx_sipa_asal),
                 diff(comp$idx_sipa_indep))
)

p_scatter <- ggplot(delta_df, aes(x = delta_sipa, y = delta_eph)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = COL_EPH, linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  facet_wrap(~ segmento, scales = "free") +
  tr_labs(title = "Quarter-to-quarter changes: EPH vs SIPA",
       subtitle = "Each point = one quarter. Line = linear regression.",
       x = "Δ SIPA (index pp)", y = "Δ EPH (index pp)") +
  theme_paper()

# Gráfico de magnitudes: niveles absolutos (miles)
comp_mag <- comp %>%
  select(fecha_q, eph_formales_asal_miles, sipa_asalariados,
         eph_formales_indep_miles, sipa_independientes) %>%
  pivot_longer(-fecha_q, names_to = "serie", values_to = "miles") %>%
  mutate(
    fuente = case_when(
      grepl("eph", serie) ~ "EPH",
      TRUE ~ "SIPA"
    ),
    segmento = case_when(
      grepl("asal", serie) ~ tr("Asalariados"),
      TRUE ~ tr("Independientes")
    )
  )

p_magnitudes <- ggplot(comp_mag, aes(x = fecha_q, y = miles,
                                      color = fuente, linetype = fuente)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = setNames(c(COL_EPH, COL_SIPA), c("EPH", "SIPA"))) +
  scale_linetype_manual(values = setNames(c("solid", "dashed"), c("EPH", "SIPA"))) +
  scale_y_continuous(labels = label_comma(big.mark = ".")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~ segmento, scales = "free_y") +
  tr_labs(title = "Magnitudes absolutas: EPH (expandido) vs SIPA",
       subtitle = "EPH en miles (expandido por pondera) | SIPA en miles (registro administrativo)",
       x = NULL, y = "Miles de personas") +
  theme_paper() +
  theme(legend.position = "top")

# Guardar figuras PDF
guardar_figura(p_total,      DIR_FIG_SIPA, "series",  1)
guardar_figura(p_asal,       DIR_FIG_SIPA, "series",  2)
guardar_figura(p_indep,      DIR_FIG_SIPA, "series",  3)
guardar_figura(p_scatter,    DIR_FIG_SIPA, "scatter", 1, width = ANCHO_FIG * 1.5)
guardar_figura(p_magnitudes, DIR_FIG_SIPA, "series",  4, width = ANCHO_FIG * 1.5)

cat("   [OK] 5 figuras exportadas a PDF\n")

# 🪫 9. DATOS PARA Rmd -------------------------------------------------------
cat("-- 9. Preparando datos para Rmd ---------------------------------------\n")

rds_path <- gsub("\\\\", "/", tempfile(fileext = ".rds"))
save(comp, comp_long_total, comp_long_asal, comp_long_indep, delta_df, comp_mag,
     cor_total_nivel, cor_asal_nivel, cor_indep_nivel,
     cor_total_delta, cor_asal_delta, cor_indep_delta,
     ratio_total, ratio_asal, ratio_indep,
     ultimo,
     p_total, p_asal, p_indep, p_scatter, p_magnitudes,
     COL_EPH, COL_SIPA,
     file = rds_path)
cat(sprintf("   [OK] Datos salvados: %s\n", rds_path))

# 🪫 10. GENERAR REPORTE HTML ------------------------------------------------
cat("-- 10. Generando reporte HTML -----------------------------------------\n")

rmd_temp <- tempfile(fileext = ".Rmd")
con <- file(rmd_temp, open = "wt", encoding = "UTF-8")

# ---- YAML ----
cat('---
title: "Validación Externa: EPH Formalidad vs SIPA Empleo Registrado"
subtitle: "Proyecto EPH Argentina -- Formalidad Laboral 2016T4-2025T3 | Auxiliar | B2-Q4"
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
cat('fmt_n   <- function(x) format(as.integer(x), big.mark = ".")
fmt_pct <- function(x, d = 2) sprintf(paste0("%.", d, "f%%"), as.numeric(x) * 100)
fmt_r   <- function(x, d = 3) sprintf(paste0("%.", d, "f"), as.numeric(x))
```

', file = con)

# ---- RESUMEN EJECUTIVO ----
cat('# Resumen Ejecutivo {.unnumbered}

```{r resumen_ej}
kpi <- tibble(
  Indicador = c(
    "Trimestres comparados",
    "Corr. niveles (total)",
    "Corr. niveles (asalariados)",
    "Corr. niveles (independientes)",
    "Corr. Δ trimestrales (total)",
    "Corr. Δ trimestrales (asalariados)",
    "Corr. Δ trimestrales (independientes)",
    "Ratio medio EPH/SIPA (total)"
  ),
  Valor = c(
    as.character(nrow(comp)),
    fmt_r(cor_total_nivel),
    fmt_r(cor_asal_nivel),
    fmt_r(cor_indep_nivel),
    fmt_r(cor_total_delta),
    fmt_r(cor_asal_delta),
    fmt_r(cor_indep_delta),
    fmt_r(ratio_total)
  )
)

kable(kpi, align = c("l", "r"), caption = "KPIs de Validación EPH vs SIPA") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  row_spec(1, bold = TRUE, background = "#eaf4fc") %>%
  row_spec(2:4, background = "#d4edda") %>%
  row_spec(5:7, background = "#fef9e7") %>%
  row_spec(8, background = "#eaf4fc")
```

', file = con)

# ---- SECCIÓN 1: CONTEXTO ----
cat('# Contexto Metodológico

## Fuentes comparadas

| Dimensión | EPH | SIPA |
|-----------|-----|------|
| **Fuente** | Encuesta de hogares (INDEC) | Registro administrativo (ARCA/AFIP) |
| **Cobertura** | 31 aglomerados urbanos (91% pob. urbana) | Nacional (todas las provincias) |
| **Periodicidad** | Trimestral (microdatos) | Mensual (agregados) |
| **Concepto** | Formalidad laboral (dual-path: PP07H + indicadores independientes) | Empleo registrado en seguridad social |
| **Formalidad** | Híbrida: observada (2024Q4+) + predicha GLM calibrado (pre-2024) | Definición administrativa: inscripción activa |

## Estrategia de comparación

La comparación es de **co-movimiento** (tendencias), no de niveles absolutos:

- **Diferencia de universo**: EPH cubre 31 aglomerados urbanos; SIPA es nacional.
  El ratio EPH/SIPA refleja esta diferencia de cobertura, no error de estimación.
- **Diferencia conceptual**: La formalidad EPH incluye descuento jubilatorio (asalariados)
  e indicadores de registro (independientes). El SIPA mide inscripción activa en la
  seguridad social. Ambas capturan la formalización, pero con instrumentos distintos.
- **Agregación temporal**: SIPA mensual → promedio trimestral (3 meses completos).
  EPH es trimestral nativa.

', file = con)

# ---- SECCIÓN 2: CO-MOVIMIENTO ----
cat('# Co-movimiento de Índices

Ambas series normalizadas a base 100 en el primer trimestre común.
La correlación de Pearson mide el grado de co-movimiento lineal.

## Total: empleo formal EPH vs registrado SIPA

```{r fig_total, fig.height=4.5}
p_total
```

## Asalariados

```{r fig_asal, fig.height=4.5}
p_asal
```

## Independientes (cuentapropistas + patrones vs autónomos + monotributistas)

```{r fig_indep, fig.height=4.5}
p_indep
```

', file = con)

# ---- SECCIÓN 3: CORRELACIONES ----
cat('# Tabla de Correlaciones

```{r tabla_cor}
cor_df <- tibble(
  Segmento = c("Total", "Asalariados", "Independientes"),
  `r niveles` = c(cor_total_nivel, cor_asal_nivel, cor_indep_nivel),
  `r Δ trimestrales` = c(cor_total_delta, cor_asal_delta, cor_indep_delta)
)

kable(cor_df, digits = 3, align = c("l", "r", "r"),
      caption = "Correlación de Pearson entre series EPH y SIPA") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  column_spec(1, bold = TRUE, width = "15em") %>%
  add_header_above(c(" " = 1, "Pearson r" = 2))
```

**Interpretación:**

- **Correlación de niveles**: captura si ambas series se mueven en la misma dirección
  a lo largo del tiempo (tendencia compartida).
- **Correlación de Δ trimestrales**: captura si los *cambios* trimestre-a-trimestre
  coinciden (co-movimiento de corto plazo). Es más exigente que la de niveles.

', file = con)

# ---- SECCIÓN 4: SCATTER DELTAS ----
cat('# Variaciones Intertrimestrales

```{r fig_scatter, fig.width=12, fig.height=4.5}
p_scatter
```

Cada punto representa un trimestre. La pendiente de la regresión indica
cuánto cambia la EPH por cada unidad de cambio en el SIPA.

', file = con)

# ---- SECCIÓN 5: MAGNITUDES ----
cat('# Magnitudes Absolutas

```{r fig_magnitudes, fig.width=12, fig.height=5}
p_magnitudes
```

```{r tabla_magnitudes}
mag_df <- comp %>%
  select(periodo_q, eph_formales_asal_miles, sipa_asalariados,
         eph_formales_indep_miles, sipa_independientes) %>%
  mutate(
    ratio_asal = eph_formales_asal_miles / sipa_asalariados,
    ratio_indep = eph_formales_indep_miles / sipa_independientes
  ) %>%
  filter(row_number() %in% c(1, ceiling(n()/2), n()))

kable(mag_df, digits = c(0, 0, 0, 0, 0, 2, 2), align = "lrrrrrr",
      col.names = c("Período", "EPH Asal.", "SIPA Asal.",
                     "EPH Indep.", "SIPA Indep.",
                     "Ratio Asal.", "Ratio Indep."),
      caption = "Magnitudes en miles — períodos seleccionados") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  add_header_above(c(" " = 1, "Asalariados (miles)" = 2,
                      "Independientes (miles)" = 2, "Ratio EPH/SIPA" = 2))
```

**Nota sobre el ratio EPH/SIPA:**
El ratio < 1 es esperable: la EPH cubre solo 31 aglomerados urbanos (~91% de la
población urbana, ~60-65% de la población total), mientras que el SIPA tiene
cobertura nacional. El ratio se mantiene relativamente estable en el tiempo,
lo que indica consistencia entre ambas fuentes.

', file = con)

# ---- SECCIÓN 6: COMPOSICIÓN SIPA ----
cat('# Composición del SIPA por Modalidad

```{r fig_composicion, fig.width=11, fig.height=5}
sipa_comp <- comp %>%
  select(fecha_q, asal_privado, asal_publico, autonomos, monotributo, casas_particulares) %>%
  pivot_longer(-fecha_q, names_to = "modalidad", values_to = "miles") %>%
  mutate(modalidad = case_when(
    modalidad == "asal_privado" ~ tr("Asal. privado"),
    modalidad == "asal_publico" ~ tr("Asal. público"),
    modalidad == "autonomos" ~ tr("Autónomos"),
    modalidad == "monotributo" ~ tr("Monotributistas"),
    modalidad == "casas_particulares" ~ tr("Casas particulares")
  ),
  modalidad = factor(modalidad, levels = tr(c("Asal. privado", "Asal. público",
                                            "Monotributistas", "Autónomos",
                                            "Casas particulares"))))

ggplot(sipa_comp, aes(x = fecha_q, y = miles, fill = modalidad)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = setNames(c("#1F4E79", "#5B8EC0", "#2E6F40", "#7DB892", "#7B7B7B"),
                     tr(c("Asal. privado", "Asal. público", "Monotributistas",
                          "Autónomos", "Casas particulares")))) +
  scale_y_continuous(labels = label_comma(big.mark = ".")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tr_labs(title = "Composición del empleo registrado SIPA por modalidad",
       subtitle = "Miles de personas. Fuente: Sec. de Trabajo (hoja T.2.1)",
       x = NULL, y = "Miles de personas") +
  theme_paper() +
  theme(legend.position = "top")
```

', file = con)

# ---- SECCIÓN 7: NOTA METODOLÓGICA ----
cat('# Nota Metodológica para el Paper

## Texto sugerido (Appendix)

> *"As external validation, we compare our bridged formality series with
> administrative records from SIPA (Sistema Integrado Previsional Argentino),
> which provides a census of registered employment. Despite differences in
> coverage (EPH covers 31 urban agglomerates while SIPA is national) and
> concept (survey-based formality vs. administrative registration), the two
> series exhibit [strong/moderate] co-movement at the quarterly frequency
> (Pearson r = [X.XX] for wage workers, r = [X.XX] for self-employed).
> This concordance provides independent confirmation that the bridged series
> captures meaningful variation in labor formalization over the 2016-2025 period."*

## Limitaciones explícitas

1. **No es micro-linkage**: comparación de series agregadas, no de individuos.
2. **Diferencia de universo**: EPH urbana vs SIPA nacional.
3. **Diferencia conceptual**: la formalidad EPH incluye dimensiones no captadas
   por el registro administrativo (e.g., descuento jubilatorio ≠ inscripción activa).
4. **Período pre-2024**: la serie EPH es *predicha* (backcasting), no observada.
   La validación mide co-movimiento de la predicción con un benchmark externo.

', file = con)

close(con)
cat(sprintf("   [OK] Rmd escrito: %s\n\n", rmd_temp))

# ---- RENDER ----
cat("   Renderizando HTML...\n")
rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_HTML_OUT,
  quiet       = TRUE,
  envir       = new.env(parent = globalenv())
)
unlink(rmd_temp)
unlink(rds_path)

cat(sprintf("   [OK] Reporte: %s\n", PATH_HTML_OUT))

# 📑 11. CHECKLIST DE OUTPUTS ------------------------------------------------
cat("\n-- 11. Checklist -------------------------------------------------------\n")
outputs <- c(PATH_HTML_OUT, PATH_NOTAS_OUT)
for (f in outputs) {
  ok <- file.exists(f)
  cat(sprintf("   %s %s\n", ifelse(ok, "✔", "✘"), basename(f)))
}
figs <- list.files(DIR_FIG_SIPA, pattern = "\\.pdf$", full.names = TRUE)
cat(sprintf("   ✔ %d figuras PDF en %s\n", length(figs), basename(DIR_FIG_SIPA)))

# 📑 12. CONTRATO ------------------------------------------------------------
cat("\n-- 12. Contrato --------------------------------------------------------\n")

# Magnitudes seleccionadas (primer, medio, último trimestre)
sel_rows <- comp %>% filter(row_number() %in% c(1, ceiling(n()/2), n()))

contrato_sipa <- list(
  script               = "10e_validacion_sipa.R",
  fecha                = Sys.time(),
  n_trimestres         = nrow(comp),
  periodo_ini          = comp$periodo_q[1],
  periodo_fin          = comp$periodo_q[nrow(comp)],
  cor_total_nivel      = round(cor_total_nivel, 3),
  cor_asal_nivel       = round(cor_asal_nivel, 3),
  cor_indep_nivel      = round(cor_indep_nivel, 3),
  cor_total_delta      = round(cor_total_delta, 3),
  cor_asal_delta       = round(cor_asal_delta, 3),
  cor_indep_delta      = round(cor_indep_delta, 3),
  ratio_eph_sipa_total = round(ratio_total, 2),
  magnitudes           = data.frame(
    quarter    = sel_rows$periodo_q,
    eph_asal   = round(sel_rows$eph_formales_asal_miles),
    sipa_asal  = round(sel_rows$sipa_asalariados),
    eph_indep  = round(sel_rows$eph_formales_indep_miles),
    sipa_indep = round(sel_rows$sipa_independientes),
    stringsAsFactors = FALSE
  )
)

saveRDS(contrato_sipa, file.path(DIR_CONTRATOS, "10e_contrato_sipa_validation.rds"))
cat(sprintf("   [OK] Contrato: 10e_contrato_sipa_validation.rds\n"))

# Timer
rm(comp, comp_long_total, comp_long_asal, comp_long_indep, delta_df, comp_mag)
gc()
cat(sprintf("\nTiempo: %.1f minutos\n", (proc.time() - t_inicio)["elapsed"] / 60))
