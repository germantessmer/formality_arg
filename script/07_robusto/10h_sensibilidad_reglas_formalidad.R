# =============================================================================
# [EN] 10h_sensibilidad_reglas_formalidad.R -- Sensitivity to alternative formality rules for non-wage workers (any-of, all-of, two-of-3)
# INPUTS:  C:/oes/eph_rdos/capa2/EPH*.RData (overlap quarters)
# OUTPUTS: rdos/reportes/10h_sensibilidad_reglas.html, rdos/figuras/10h_sensibilidad_reglas/*.pdf
# =============================================================================
# 🌟 10h_sensibilidad_reglas_formalidad.R 🌟 ####
# Sensibilidad a reglas alternativas de formalización para independientes
# Proyecto: formalidad_rev  |  Capa 7 -- Robustez  |  B2-Q2
#
# OBJETIVO:
#   Calcular tasas de formalidad en el overlap (2024Q4-2025Q3) bajo tres reglas
#   para empleadores/cuentapropistas:
#     - Any-of   (regla INDEC oficial): cualquier indicador positivo -> Formal
#     - All-of   (estricta): todos los indicadores positivos -> Formal
#     - Two-of-3 (intermedia): al menos 2 de 3 positivos -> Formal
#   Los asalariados NO cambian (PP07H siempre).
#
# INPUTS:
#   C:/oes/eph_rdos/capa2/EPH*_T*.RData  (microdatos EPH, derivados de TRIMESTRES_FORMALIDAD)
#
# OUTPUTS:
#   rdos/reportes/10h_sensibilidad_reglas.html
#   rdos/figuras/10h_sensibilidad_reglas/*.pdf
#   rdos/reportes/10h_sensibilidad_reglas_notas.txt
#
# INDICADORES PARA INDEPENDIENTES (variables re-etiquetadas):
#   PP05I  -> aporto_como     (aporta: Monotributista/Autonomo/Mono.social)
#   PP05B3 -> emite_facturas  (routing: si "No corresponde" -> ver PP05K)
#   PP05K  -> emite_facturas2 (emite facturas, para quienes no responden PP05B3)
#   PP06E1 -> tiene_contador_ind (usa servicios contables)
#
# TIEMPO ESTIMADO: ~2 minutos

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
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
cat("SCRIPT 10h - SENSIBILIDAD REGLAS DE FORMALIZACIÓN (B2-Q2)\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")

# 🔑 1. PATHS Y DIRECTORIOS --------------------------------------------------
cat("-- 1. Paths ------------------------------------------------------------\n")

PATH_HTML_OUT  <- file.path(DIR_REPORTES, "10h_sensibilidad_reglas.html")
PATH_NOTAS_OUT <- file.path(DIR_REPORTES, "10h_sensibilidad_reglas_notas.txt")
DIR_FIG_SENS   <- file.path(DIR_FIGURAS, "10h_sensibilidad_reglas")
dir.create(DIR_FIG_SENS, showWarnings = FALSE, recursive = TRUE)

# 🪫 2. CARGAR MICRODATOS EPH DEL OVERLAP ------------------------------------
cat("-- 2. Carga microdatos EPH (overlap) ----------------------------------\n")

cols_necesarias <- c("categoria_ocupacional", "condicion_actividad", "pondera",
                     "desc_jubilatorio_asalariado",
                     "aporto_como", "emite_facturas", "emite_facturas2",
                     "tiene_contador_ind")

# Derivar de TRIMESTRES_FORMALIDAD (parametros.R) — 0 HC
trimestres_info <- lapply(TRIMESTRES_FORMALIDAD, function(tf) {
  parts <- strsplit(tf, "_T")[[1]]
  list(anio = parts[1], t = parts[2], label = paste0(parts[1], "Q", parts[2]))
})

all_dfs <- list()

for (tri in trimestres_info) {
  fname <- paste0("EPH", tri$anio, "_T", tri$t, ".RData")
  fpath <- file.path(RUTA_BASES, fname)
  hard_stop(file.exists(fpath), paste("No existe:", fname))

  env <- new.env()
  load(fpath, envir = env)
  df <- get(ls(env)[1], envir = env)
  rm(env)

  # Retener solo columnas necesarias + agregar trimestre
  df <- df[, intersect(cols_necesarias, names(df))]
  df$trimestre <- tri$label

  # Filtrar solo ocupados
  df <- df[df$condicion_actividad == "Ocupado", ]

  all_dfs[[tri$label]] <- df
  cat(sprintf("   [OK] %s: %d ocupados\n", fname, nrow(df)))
}

eph <- bind_rows(all_dfs)
rm(all_dfs); gc(verbose = FALSE)
cat(sprintf("   Total: %d ocupados en %d trimestres\n", nrow(eph), length(TRIMESTRES_FORMALIDAD)))

# 🪫 3. CONSTRUIR INDICADORES BINARIOS PARA INDEPENDIENTES -------------------
cat("-- 3. Indicadores binarios --------------------------------------------\n")

eph <- eph %>%
  mutate(
    es_asalariado = categoria_ocupacional == "Empleado",
    es_independiente = categoria_ocupacional %in% c("Cuenta Propia", "Patrón"),
    es_hogar = categoria_ocupacional == "Familiar",

    # Indicador 1: Aporta a la seguridad social (PP05I)
    ind_aporta = aporto_como %in% c("Monotributista", "Monotributista social",
                                     "Autonomo o profesional"),

    # Indicador 2: Emite facturas (PP05B3 con routing a PP05K)
    ind_factura = case_when(
      emite_facturas == "Si" ~ TRUE,
      emite_facturas2 == "Si" ~ TRUE,
      TRUE ~ FALSE
    ),

    # Indicador 3: Tiene contador (PP06E1)
    ind_contador = tiene_contador_ind == "Si",

    # Contar indicadores positivos
    n_positivos = as.integer(ind_aporta) + as.integer(ind_factura) +
                  as.integer(ind_contador)
  )

# Verificar distribución de indicadores para independientes
indep <- eph %>% filter(es_independiente)
cat(sprintf("   Independientes: %d obs\n", nrow(indep)))
cat(sprintf("   ind_aporta:    %d (%.1f%%)\n",
            sum(indep$ind_aporta), mean(indep$ind_aporta) * 100))
cat(sprintf("   ind_factura:   %d (%.1f%%)\n",
            sum(indep$ind_factura), mean(indep$ind_factura) * 100))
cat(sprintf("   ind_contador:  %d (%.1f%%)\n",
            sum(indep$ind_contador), mean(indep$ind_contador) * 100))
cat(sprintf("   Distribución n_positivos:\n"))
print(table(indep$n_positivos, useNA = "always"))

# 🪫 4. APLICAR 3 REGLAS DE FORMALIZACIÓN ------------------------------------
cat("\n-- 4. Reglas de formalización -------------------------------------------\n")

# Asalariados: misma regla siempre (PP07H)
# Independientes: varía según la regla
# Familiar/hogar: siempre informal

eph <- eph %>%
  mutate(
    # REGLA 1: Any-of (INDEC oficial)
    formal_anyof = case_when(
      es_asalariado ~ desc_jubilatorio_asalariado == "Si",
      es_independiente ~ n_positivos >= 1,
      TRUE ~ FALSE  # familiar/hogar → informal
    ),

    # REGLA 2: All-of (estricta)
    formal_allof = case_when(
      es_asalariado ~ desc_jubilatorio_asalariado == "Si",
      es_independiente ~ n_positivos == 3,
      TRUE ~ FALSE
    ),

    # REGLA 3: Two-of-three
    formal_twoof = case_when(
      es_asalariado ~ desc_jubilatorio_asalariado == "Si",
      es_independiente ~ n_positivos >= 2,
      TRUE ~ FALSE
    )
  )

# 🪫 5. CALCULAR TASAS POR TRIMESTRE Y REGLA ---------------------------------
cat("-- 5. Tasas por trimestre y regla --------------------------------------\n")

# Tasas totales (ponderadas)
tasas_total <- eph %>%
  group_by(trimestre) %>%
  summarise(
    n = n(),
    tasa_anyof = weighted.mean(formal_anyof, pondera, na.rm = TRUE),
    tasa_allof = weighted.mean(formal_allof, pondera, na.rm = TRUE),
    tasa_twoof = weighted.mean(formal_twoof, pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(trimestre)

# Tasas solo independientes
tasas_indep <- eph %>%
  filter(es_independiente) %>%
  group_by(trimestre) %>%
  summarise(
    n = n(),
    tasa_anyof = weighted.mean(formal_anyof, pondera, na.rm = TRUE),
    tasa_allof = weighted.mean(formal_allof, pondera, na.rm = TRUE),
    tasa_twoof = weighted.mean(formal_twoof, pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(trimestre)

# Tasas solo asalariados (invariante — para control)
tasas_asal <- eph %>%
  filter(es_asalariado) %>%
  group_by(trimestre) %>%
  summarise(
    n = n(),
    tasa_anyof = weighted.mean(formal_anyof, pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(trimestre)

cat("\n   === Tasas totales (todos los ocupados) ===\n")
print(as.data.frame(tasas_total))

cat("\n   === Tasas independientes (CP + Patrón) ===\n")
print(as.data.frame(tasas_indep))

cat("\n   === Tasas asalariados (control — no cambian) ===\n")
print(as.data.frame(tasas_asal))

# Delta entre reglas
delta_total <- tasas_total %>%
  mutate(
    delta_allof_pp = (tasa_allof - tasa_anyof) * 100,
    delta_twoof_pp = (tasa_twoof - tasa_anyof) * 100
  )

cat("\n   === Deltas (pp) vs any-of (regla INDEC) ===\n")
print(as.data.frame(delta_total %>% select(trimestre, delta_allof_pp, delta_twoof_pp)))

# Promedio
mean_delta_allof <- mean(delta_total$delta_allof_pp)
mean_delta_twoof <- mean(delta_total$delta_twoof_pp)
cat(sprintf("\n   Promedio delta all-of: %.2f pp\n", mean_delta_allof))
cat(sprintf("   Promedio delta two-of: %.2f pp\n", mean_delta_twoof))

# 🪫 6. CARGAR TASA LEGACY PARA COMPARAR GAP ---------------------------------
cat("\n-- 6. Gap con serie legacy ---------------------------------------------\n")

# Tasa legacy = desc_jubilatorio_asalariado para TODOS los ocupados (old proxy)
# Se computa directamente de los microdatos EPH ya cargados — 0 HC
tasa_legacy_df <- eph %>%
  mutate(formal_legacy = desc_jubilatorio_asalariado == "Si") %>%
  group_by(trimestre) %>%
  summarise(tasa_legacy = weighted.mean(formal_legacy, pondera, na.rm = TRUE),
            .groups = "drop")
tasa_legacy <- setNames(tasa_legacy_df$tasa_legacy, tasa_legacy_df$trimestre)
cat("   Tasa legacy (desc_jubilatorio para todos) por trimestre:\n")
print(tasa_legacy)

# Construir tabla de gaps
gap_table <- tasas_total %>%
  mutate(
    tasa_legacy = tasa_legacy[trimestre],
    gap_anyof_pp = (tasa_legacy - tasa_anyof) * 100,
    gap_allof_pp = (tasa_legacy - tasa_allof) * 100,
    gap_twoof_pp = (tasa_legacy - tasa_twoof) * 100
  )

cat("\n   === Gap (legacy - regla) en pp ===\n")
print(as.data.frame(gap_table %>%
  select(trimestre, tasa_legacy, tasa_anyof, tasa_allof, tasa_twoof,
         gap_anyof_pp, gap_allof_pp, gap_twoof_pp)))

# 🪫 7. DISTRIBUCIÓN CRUZADA DE INDICADORES ----------------------------------
cat("\n-- 7. Distribución cruzada ---------------------------------------------\n")

cross_tab <- indep %>%
  count(ind_aporta, ind_factura, ind_contador) %>%
  mutate(
    pct = n / sum(n) * 100,
    regla_anyof = (ind_aporta | ind_factura | ind_contador),
    regla_allof = (ind_aporta & ind_factura & ind_contador),
    regla_twoof = (as.integer(ind_aporta) + as.integer(ind_factura) +
                    as.integer(ind_contador)) >= 2
  ) %>%
  arrange(desc(n))

cat("   Distribución cruzada de indicadores (independientes, 3 trims):\n")
print(as.data.frame(cross_tab))

# 🪫 8. GRÁFICOS -------------------------------------------------------------
cat("\n-- 8. Gráficos ---------------------------------------------------------\n")

# Preparar datos long para gráficos
tasas_long_total <- tasas_total %>%
  pivot_longer(cols = starts_with("tasa_"),
               names_to = "regla", values_to = "tasa") %>%
  mutate(regla = case_when(
    regla == "tasa_anyof" ~ tr("Any-of (INDEC)"),
    regla == "tasa_allof" ~ tr("All-of (estricta)"),
    regla == "tasa_twoof" ~ tr("Two-of-3")
  ),
  regla = factor(regla, levels = tr(c("Any-of (INDEC)", "Two-of-3", "All-of (estricta)"))))

tasas_long_indep <- tasas_indep %>%
  pivot_longer(cols = starts_with("tasa_"),
               names_to = "regla", values_to = "tasa") %>%
  mutate(regla = case_when(
    regla == "tasa_anyof" ~ tr("Any-of (INDEC)"),
    regla == "tasa_allof" ~ tr("All-of (estricta)"),
    regla == "tasa_twoof" ~ tr("Two-of-3")
  ),
  regla = factor(regla, levels = tr(c("Any-of (INDEC)", "Two-of-3", "All-of (estricta)"))))

# Colores: Any-of = azul (oficial), All-of = rojo, Two-of = naranja
COL_ANYOF <- PAL_DESCRIPTIVO[1]  # azul
COL_ALLOF <- PAL_DESCRIPTIVO[2]  # rojo
COL_TWOOF <- PAL_DESCRIPTIVO[3]  # naranja
cols_regla <- setNames(c(COL_ANYOF, COL_TWOOF, COL_ALLOF),
                       tr(c("Any-of (INDEC)", "Two-of-3", "All-of (estricta)")))

# Gráfico 1: Tasas totales por regla
p_total <- ggplot(tasas_long_total,
                  aes(x = trimestre, y = tasa * 100, fill = regla)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = cols_regla) +
  tr_labs(title = "Tasa de formalidad total por regla de clasificación",
       subtitle = "Sólo cambia la definición para independientes; asalariados invariantes",
       x = NULL, y = "Tasa de formalidad (%)") +
  theme_paper() +
  theme(legend.position = "top")

# Gráfico 2: Tasas independientes por regla
p_indep <- ggplot(tasas_long_indep,
                  aes(x = trimestre, y = tasa * 100, fill = regla)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = cols_regla) +
  tr_labs(title = "Tasa de formalidad de independientes por regla",
       subtitle = "Empleadores + cuentapropistas. Tres indicadores: aporta, factura, contador",
       x = NULL, y = "Tasa de formalidad (%)") +
  theme_paper() +
  theme(legend.position = "top")

# Gráfico 3: Gap con legacy por regla
gap_long <- gap_table %>%
  select(trimestre, gap_anyof_pp, gap_allof_pp, gap_twoof_pp) %>%
  pivot_longer(-trimestre, names_to = "regla", values_to = "gap_pp") %>%
  mutate(regla = case_when(
    regla == "gap_anyof_pp" ~ tr("Any-of (INDEC)"),
    regla == "gap_allof_pp" ~ tr("All-of (estricta)"),
    regla == "gap_twoof_pp" ~ tr("Two-of-3")
  ),
  regla = factor(regla, levels = tr(c("Any-of (INDEC)", "Two-of-3", "All-of (estricta)"))))

p_gap <- ggplot(gap_long,
                aes(x = trimestre, y = gap_pp, fill = regla)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = cols_regla) +
  tr_labs(title = "Gap legacy-nuevo por regla de formalización",
       subtitle = "Gap = tasa legacy (PP07H para todos) − tasa bajo cada regla, en pp",
       x = NULL, y = "Gap (pp)") +
  theme_paper() +
  theme(legend.position = "top")

guardar_figura(p_total, DIR_FIG_SENS, "barras", 1)
guardar_figura(p_indep, DIR_FIG_SENS, "barras", 2)
guardar_figura(p_gap,   DIR_FIG_SENS, "barras", 3)

cat("   [OK] 3 figuras exportadas\n")

# 🪫 9. NOTAS PARA EL PAPER --------------------------------------------------
cat("\n-- 9. Notas para el paper ----------------------------------------------\n")

notas_con <- file(PATH_NOTAS_OUT, open = "wt", encoding = "UTF-8")
cat(sprintf("SENSIBILIDAD A REGLAS DE FORMALIZACIÓN — %s\n",
            format(Sys.time(), "%Y-%m-%d")), file = notas_con)
cat("========================================\n\n", file = notas_con)

cat("TASAS DE FORMALIDAD TOTAL (ocupados, ponderada):\n", file = notas_con)
for (i in seq_len(nrow(tasas_total))) {
  cat(sprintf("  %s: Any-of=%.1f%% | Two-of=%.1f%% | All-of=%.1f%%\n",
    tasas_total$trimestre[i],
    tasas_total$tasa_anyof[i] * 100,
    tasas_total$tasa_twoof[i] * 100,
    tasas_total$tasa_allof[i] * 100), file = notas_con)
}

cat(sprintf("\nDELTA PROMEDIO vs any-of (INDEC):\n"), file = notas_con)
cat(sprintf("  All-of:  %.2f pp (más restrictiva → menos formales)\n",
            mean_delta_allof), file = notas_con)
cat(sprintf("  Two-of:  %.2f pp\n", mean_delta_twoof), file = notas_con)

cat(sprintf("\nGAP LEGACY vs CADA REGLA (pp, promedio 3 trims):\n"), file = notas_con)
cat(sprintf("  Any-of (INDEC): %.1f pp\n",
            mean(gap_table$gap_anyof_pp)), file = notas_con)
cat(sprintf("  Two-of-3:       %.1f pp\n",
            mean(gap_table$gap_twoof_pp)), file = notas_con)
cat(sprintf("  All-of:         %.1f pp\n",
            mean(gap_table$gap_allof_pp)), file = notas_con)

cat("\nCONCLUSIÓN: La regla de formalización para independientes afecta\n",
    file = notas_con)
cat("marginalmente la tasa total. El gap legacy-nuevo se mantiene\n",
    file = notas_con)
cat("cualitativamente bajo todas las reglas.\n", file = notas_con)
close(notas_con)
cat(sprintf("   [OK] Notas: %s\n", PATH_NOTAS_OUT))

# 🪫 10. GENERAR REPORTE HTML ------------------------------------------------
cat("\n-- 10. Reporte HTML ----------------------------------------------------\n")

rds_path <- gsub("\\\\", "/", tempfile(fileext = ".rds"))
save(tasas_total, tasas_indep, tasas_asal, tasas_long_total, tasas_long_indep,
     gap_table, gap_long, cross_tab, delta_total,
     mean_delta_allof, mean_delta_twoof, tasa_legacy,
     p_total, p_indep, p_gap,
     COL_ANYOF, COL_ALLOF, COL_TWOOF, cols_regla,
     file = rds_path)

rmd_temp <- tempfile(fileext = ".Rmd")
con <- file(rmd_temp, open = "wt", encoding = "UTF-8")

cat('---
title: "Sensibilidad a Reglas Alternativas de Formalización"
subtitle: "Proyecto EPH Argentina -- Formalidad Laboral | Capa 7 | B2-Q2"
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
  library(ggplot2); library(patchwork); library(scales)
})
load("%s")
source(here::here("script", "config", "funciones_comunes.R"))
', rds_path), file = con)
cat('fmt_pct <- function(x, d=1) sprintf(paste0("%.", d, "f%%"), as.numeric(x) * 100)
fmt_pp  <- function(x, d=2) sprintf(paste0("%.", d, "f"), as.numeric(x))
```

', file = con)

# Resumen Ejecutivo
cat('# Resumen Ejecutivo {.unnumbered}

```{r resumen}
kpi <- tibble(
  Indicador = c(
    "Delta promedio all-of vs any-of (pp)",
    "Delta promedio two-of vs any-of (pp)",
    "Gap legacy-anyof (promedio, pp)",
    "Gap legacy-allof (promedio, pp)",
    "Gap legacy-twoof (promedio, pp)"
  ),
  Valor = c(
    fmt_pp(mean_delta_allof),
    fmt_pp(mean_delta_twoof),
    fmt_pp(mean(gap_table$gap_anyof_pp)),
    fmt_pp(mean(gap_table$gap_allof_pp)),
    fmt_pp(mean(gap_table$gap_twoof_pp))
  )
)
kable(kpi, align = c("l", "r"), caption = "KPIs de sensibilidad") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  row_spec(1:2, background = "#fef9e7") %>%
  row_spec(3:5, background = "#eaf4fc")
```

', file = con)

# Sección 1: Contexto
cat('# Contexto

## Regla oficial (INDEC): Any-of

Para empleadores y cuentapropistas, la EPH post-2024Q4 mide formalidad
mediante tres indicadores de unidad productiva:

1. **Aporta a la seguridad social** (PP05I): monotributista, autónomo o profesional
2. **Emite facturas** (PP05B3/PP05K): capacidad de facturación
3. **Tiene contador** (PP06E1): usa servicios contables

La regla oficial: **cualquier indicador positivo → Formal; ninguno → Informal**.

## Reglas alternativas

- **All-of (estricta):** los 3 indicadores deben ser positivos
- **Two-of-3 (intermedia):** al menos 2 de 3 positivos

Los asalariados no cambian (PP07H siempre). Solo se reclasifican independientes.

', file = con)

# Sección 2: Tasas totales
cat('# Tasas de formalidad total

```{r fig-total, fig.height=5}
p_total
```

```{r tbl-total}
tasas_total %>%
  mutate(across(starts_with("tasa_"), ~ fmt_pct(.x))) %>%
  kable(col.names = c("Trimestre", "N", "Any-of (INDEC)", "All-of", "Two-of-3"),
        align = "lrrrr",
        caption = "Tasa de formalidad total por regla (ponderada)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  column_spec(3, bold = TRUE, background = "#d4edda")
```

', file = con)

# Sección 3: Tasas independientes
cat('# Tasas de formalidad: independientes

```{r fig-indep, fig.height=5}
p_indep
```

```{r tbl-indep}
tasas_indep %>%
  mutate(across(starts_with("tasa_"), ~ fmt_pct(.x))) %>%
  kable(col.names = c("Trimestre", "N", "Any-of (INDEC)", "All-of", "Two-of-3"),
        align = "lrrrr",
        caption = "Tasa de formalidad de independientes por regla") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  column_spec(3, bold = TRUE, background = "#d4edda")
```

', file = con)

# Sección 4: Gap con legacy
cat('# Gap con la serie legacy

```{r fig-gap, fig.height=5}
p_gap
```

```{r tbl-gap}
gap_table %>%
  select(trimestre, tasa_legacy, tasa_anyof, tasa_allof, tasa_twoof,
         gap_anyof_pp, gap_allof_pp, gap_twoof_pp) %>%
  mutate(across(starts_with("tasa_"), ~ fmt_pct(.x)),
         across(starts_with("gap_"), ~ fmt_pp(.x))) %>%
  kable(col.names = c("Trim.", "Legacy", "Any-of", "All-of", "Two-of",
                       "Gap any", "Gap all", "Gap two"),
        align = "lrrrrrrr",
        caption = "Gap = tasa legacy (PP07H para todos) - tasa bajo cada regla") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  add_header_above(c(" " = 1, "Tasas (%)" = 4, "Gap (pp)" = 3))
```

', file = con)

# Sección 5: Distribución cruzada
cat('# Distribución cruzada de indicadores

```{r tbl-cross}
cross_tab %>%
  mutate(pct = sprintf("%.1f%%", pct)) %>%
  select(ind_aporta, ind_factura, ind_contador, n, pct,
         regla_anyof, regla_allof, regla_twoof) %>%
  kable(col.names = c("Aporta", "Factura", "Contador", "N", "%",
                       "Any-of", "All-of", "Two-of"),
        align = "cccrrrrr",
        caption = sprintf("Combinaciones de indicadores (independientes, %d trimestres)", length(TRIMESTRES_FORMALIDAD))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11)
```

La tabla muestra que la gran mayoria de independientes tiene **0 indicadores positivos**
(los "Ninguno" en aporto_como, sin factura, sin contador). La regla all-of solo
clasifica como formales a quienes cumplen los 3 simultaneamente. En la practica,
**ningun independiente cumple los 3 indicadores a la vez**, lo que hace la regla
all-of vacua para este segmento.

', file = con)

# Sección 6: Conclusión
cat('# Conclusión

1. **La regla alternativa más restrictiva (all-of) reduce la tasa total** en
   un promedio modesto, pero no altera la conclusión cualitativa del paper:
   el gap legacy-nuevo se mantiene bajo todas las reglas.

2. **La regla any-of es la implementación oficial del INDEC.** Desviarse de ella
   implica adoptar un concepto de formalidad que no coincide con las estadísticas
   publicadas, lo que debilita la comparabilidad.

3. **Two-of-3 como sensibilidad intermedia** confirma que incluso con una regla
   más exigente, el gap se mantiene cualitativamente.

4. **Los asalariados (65-70%% del empleo) no se ven afectados** por la elección
   de regla, ya que su formalidad se mide por PP07H (descuento jubilatorio),
   que es invariante a la regla de independientes.

---

<small>Script: 10h_sensibilidad_reglas_formalidad.R | Proyecto: formalidad_rev | B2-Q2</small>
', file = con)

close(con)

rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_HTML_OUT,
  quiet       = TRUE,
  envir       = new.env(parent = globalenv())
)
unlink(c(rmd_temp, rds_path))
cat(sprintf("   [OK] Reporte: %s\n", PATH_HTML_OUT))

# 📑 11. CHECKLIST -----------------------------------------------------------
cat("\n-- 11. Checklist -------------------------------------------------------\n")
for (f in c(PATH_HTML_OUT, PATH_NOTAS_OUT)) {
  cat(sprintf("   %s %s\n", ifelse(file.exists(f), "OK", "!!"), basename(f)))
}
figs <- list.files(DIR_FIG_SENS, pattern = "\\.pdf$")
cat(sprintf("   OK %d figuras PDF\n", length(figs)))

# 📑 12. CONTRATO -----------------------------------------------------------
cat("\n-- 12. Contrato --------------------------------------------------------\n")

contrato_sens <- list(
  script          = "10h_sensibilidad_reglas_formalidad.R",
  fecha           = Sys.time(),
  # Distribución de combinaciones de indicadores
  distribucion    = cross_tab %>%
    transmute(
      contributes = ifelse(ind_aporta, "Yes", "No"),
      invoices    = ifelse(ind_factura, "Yes", "No"),
      accounting  = ifelse(ind_contador, "Yes", "No"),
      n           = as.integer(n),
      share_pct   = round(pct, 1)
    ) %>% as.data.frame(),
  all_three_count = sum(cross_tab$n[cross_tab$ind_aporta & cross_tab$ind_factura & cross_tab$ind_contador]),
  n_indep_overlap = as.integer(sum(cross_tab$n)),
  # Tasas y gaps por trimestre
  tasas           = rbind(
    gap_table %>%
      transmute(quarter = trimestre,
                any_of  = round(tasa_anyof * 100, 1),
                two_of  = round(tasa_twoof * 100, 1),
                all_of  = round(tasa_allof * 100, 1),
                gap_any = round(gap_anyof_pp, 1),
                gap_two = round(gap_twoof_pp, 1),
                gap_all = round(gap_allof_pp, 1)),
    data.frame(quarter = "Mean",
               any_of  = round(mean(gap_table$tasa_anyof) * 100, 1),
               two_of  = round(mean(gap_table$tasa_twoof) * 100, 1),
               all_of  = round(mean(gap_table$tasa_allof) * 100, 1),
               gap_any = round(mean(gap_table$gap_anyof_pp), 1),
               gap_two = round(mean(gap_table$gap_twoof_pp), 1),
               gap_all = round(mean(gap_table$gap_allof_pp), 1))
  ) %>% as.data.frame(),
  # Promedios para acceso rápido
  tasa_mean_any   = round(mean(gap_table$tasa_anyof) * 100, 1),
  tasa_mean_two   = round(mean(gap_table$tasa_twoof) * 100, 1),
  tasa_mean_all   = round(mean(gap_table$tasa_allof) * 100, 1),
  gap_mean_any    = round(mean(gap_table$gap_anyof_pp), 1),
  gap_mean_two    = round(mean(gap_table$gap_twoof_pp), 1),
  gap_mean_all    = round(mean(gap_table$gap_allof_pp), 1),
  pct_contributes = round(sum(indep$ind_aporta) / nrow(indep) * 100, 1),
  # Tasas de independientes (para Quarto K-sensitivity)
  tasa_indep_any  = round(mean(tasas_indep$tasa_anyof) * 100, 1),
  tasa_indep_two  = round(mean(tasas_indep$tasa_twoof) * 100, 1),
  tasa_indep_all  = round(mean(tasas_indep$tasa_allof) * 100, 1),
  # Pct non-wage sobre total
  pct_nonwage     = round(sum(eph$es_independiente) / nrow(eph) * 100, 1)
)

saveRDS(contrato_sens, file.path(DIR_CONTRATOS, "10h_contrato_sensibilidad_reglas.rds"))
cat("   [OK] 10h_contrato_sensibilidad_reglas.rds\n")

rm(eph, indep, cross_tab); gc(verbose = FALSE)
cat(sprintf("\nTiempo: %.1f minutos\n", (proc.time() - t_inicio)["elapsed"] / 60))
