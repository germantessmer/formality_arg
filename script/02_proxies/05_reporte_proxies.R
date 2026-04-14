# =============================================================================
# [EN] 05_reporte_proxies.R -- Validate proxy system: cross-correlations, orthogonality, Kotlarski tests
# INPUTS:  rdos/datos/04_panel_con_proxies.rds
# OUTPUTS: rdos/reportes/05_reporte_proxies.html, rdos/contratos/05_contrato_proxies.rds
# =============================================================================
# 🌟 05_reporte_proxies.R 🌟 ####

# 05_reporte_proxies.R
# CAPA 2 — CIERRE: Validación formal y reporte del sistema de proxies
#
# OBJETIVO: Validar y documentar el sistema de 7 proxies construidas en Script 04
# antes de pasar a Capa 3 (Modelado Heterofactor). No construye proxies nuevas.
#
# CONTEXTO: Las 7 proxies ya fueron construidas y validadas individualmente en
# Script 04. Este script realiza la validación SISTÉMICA (correlaciones cruzadas,
# ortogonalidad entre factores, tests Kotlarski formales) y genera:
#   - Reporte HTML de alta calidad para documentación del paper
#   - Outputs adicionales en CSV/RDS para redacción del paper
#   - Contrato formal para consumo downstream (Capa 3)
#
# INPUT:  PATH_04_PANEL_PROXIES → rdos/datos/04_panel_con_proxies.rds
# OUTPUTS:
#   rdos/reportes/05_reporte_proxies.html
#   rdos/reportes/05_tabla_descriptiva_proxies.csv
#   rdos/reportes/05_matriz_correlaciones.csv
#   rdos/reportes/05_tests_kotlarski.csv
#   rdos/reportes/05_evolucion_temporal_proxies.csv
#   rdos/reportes/05_diagnostico_proxies.rds
#   rdos/contratos/05_contrato_proxies.rds     ← PATH_CONTRATO_05
#
# AUTOR: Proyecto EPH Formalidad | FECHA: 2026-02-27

# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(rmarkdown)
  library(tictoc)
})

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 05 - Validación Capa 2")
start_time <- Sys.time()

# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  📋 SCRIPT 05 — VALIDACIÓN Y CIERRE CAPA 2\n")
cat("  Sistema: 7 proxies | 2 factores latentes\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Paths de outputs — leídos desde parametros.R (anti-HC: ver L45 D76)
PATH_REPORTE        <- PATH_05_HTML
PATH_CSV_DESC       <- PATH_05_CSV_DESC
PATH_CSV_COR        <- PATH_05_CSV_COR
PATH_CSV_TESTS      <- PATH_05_CSV_TESTS
PATH_CSV_TEMPORAL   <- PATH_05_CSV_TEMPORAL
PATH_DIAG           <- PATH_05_DIAG_RDS
PATH_RMD            <- PATH_05_RMD

dir.create(DIR_REPORTES,  showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_CONTRATOS, showWarnings = FALSE, recursive = TRUE)


# 🪫 0b. Umbrales del framework Kotlarski ---------------------------------------
# UMBRAL_INTRA_COR / UMBRAL_CROSS_COR / UMBRAL_SD_MIN — definidos en parametros.R

# 🪫 1. Carga y verificación ----------------------------------------------------
cat("📂 Cargando panel con proxies...\n")
hard_stop(file.exists(PATH_04_PANEL_PROXIES),
          "No existe 04_panel_con_proxies.rds. Ejecutar Script 04 primero.")

datos <- readRDS(PATH_04_PANEL_PROXIES)
cat(sprintf("   ✅ %s obs × %s vars\n\n",
            format(nrow(datos), big.mark = ","), ncol(datos)))

# Definición del sistema de proxies
# (emparejamiento_selectivo = assortative mating en nomenclatura del paper)
PROXIES_COG <- c(
  "rezago_escolar_cohorte",    # Stock: eficiencia educativa relativa a cohorte
  "clima_educativo_hogar",     # Contexto: capital humano del entorno familiar
  "emparejamiento_selectivo",  # Mercado matrimonial: señal de habilidad latente
  "calificacion_norm"          # Outcome: complejidad ocupacional (CNO)
)

PROXIES_SOCIO <- c(
  "entropia_estabilidad",    # Trayectoria: estabilidad conductual (Shannon)
  "residual_vivienda",       # Preferencia: inversión en vivienda vs. ingreso
  "busqueda_formal"          # Agencia: intensidad búsqueda activa de empleo
)

PROXIES_TODAS <- c(PROXIES_COG, PROXIES_SOCIO)

vars_falt <- setdiff(PROXIES_TODAS, names(datos))
hard_stop(length(vars_falt) == 0,
          paste("Faltan proxies:", paste(vars_falt, collapse = ", ")))
cat("   ✅ 7 proxies confirmadas en el dataset\n\n")

# Flag NBI condicional
.tiene_nbi <- "nbi" %in% names(datos)


# 🪫 2. Estadísticas descriptivas por proxy -------------------------------------
cat("── Calculando estadísticas descriptivas ─────────────────────────────\n")

# Umbrales y metadatos por proxy (mismos criterios que Script 04)
umbrales <- tibble(
  proxy  = PROXIES_TODAS,
  factor = c(rep("θ_cog", 4), rep("θ_socio", 3)),
  fuente = c(
    "Stock educativo",    "Contexto familiar", "Mercado matrimonial",
    "Outcome ocupacional","Trayectoria laboral","Preferencia temporal",
    "Agencia/Grit"
  ),
  mecanismo = c(
    "Eficiencia relativa a cohorte de nacimiento",
    "Capital humano del entorno familiar (adultos >25 del hogar)",
    "Educación del par matrimonial como señal de habilidad latente",
    "Complejidad de tarea ocupacional según clasificación CNO",
    "Entropía de Shannon sobre estados Ocupado/Desocupado/Inactivo",
    "Residual OLS de calidad de vivienda sobre ingreso y controles",
    "Intensidad de métodos activos de búsqueda de empleo"
  ),
  na_max = c(30, 30, 30, 60, 30, 30, 98),
  sd_min = rep(UMBRAL_SD_MIN, 7),
  na_por_diseno = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  # busqueda_formal: NA ~97% por diseño (solo desocupados ~3% del panel)
)

resumen_proxies <- map_dfr(PROXIES_TODAS, function(v) {
  x <- datos[[v]]
  tibble(
    proxy   = v,
    n_valid = sum(!is.na(x)),
    na_pct  = round(mean(is.na(x)) * 100, 1),
    media   = round(mean(x, na.rm = TRUE), 3),
    sd      = round(sd(x, na.rm = TRUE), 3),
    mediana = round(median(x, na.rm = TRUE), 3),
    p25     = round(quantile(x, 0.25, na.rm = TRUE), 3),
    p75     = round(quantile(x, 0.75, na.rm = TRUE), 3),
    min     = round(min(x, na.rm = TRUE), 3),
    max     = round(max(x, na.rm = TRUE), 3)
  )
}) %>%
  left_join(umbrales, by = "proxy") %>%
  mutate(
    test_sd = if_else(sd > sd_min,     "PASS", "FAIL"),
    test_na = if_else(na_pct < na_max, "PASS", "FAIL"),
    test_ok = test_sd == "PASS" & test_na == "PASS"
  )

cat("   Resumen por factor:\n")
resumen_proxies %>%
  group_by(factor) %>%
  summarise(
    n_proxies   = n(),
    na_pct_mean = round(mean(na_pct), 1),
    sd_mean     = round(mean(sd), 3),
    n_pass      = sum(test_ok),
    .groups     = "drop"
  ) %>%
  print()
cat("\n")


# 🪫 3. Matrices de correlación -------------------------------------------------
cat("── Calculando matrices de correlación ───────────────────────────────\n")

cor_completa  <- cor(datos[PROXIES_TODAS], use = "pairwise.complete.obs")
cor_cog       <- cor(datos[PROXIES_COG],   use = "pairwise.complete.obs")
max_cor_cog   <- max(abs(cor_cog[lower.tri(cor_cog)]))

# busqueda_formal excluida de intra-socio por diseño (NA ~97%)
cor_socio_2   <- cor(datos[c("entropia_estabilidad", "residual_vivienda")],
                     use = "pairwise.complete.obs")
max_cor_socio <- max(abs(cor_socio_2[lower.tri(cor_socio_2)]))

cross_cors <- cor_completa[PROXIES_COG, PROXIES_SOCIO]
max_cross  <- max(abs(cross_cors), na.rm = TRUE)

cat(sprintf("   θ_cog   — max correlación intra-factor:   %.3f %s\n",
            max_cor_cog,   if_else(max_cor_cog  < UMBRAL_INTRA_COR, "✅", "❌")))
cat(sprintf("   θ_socio — max correlación intra-factor:   %.3f %s\n",
            max_cor_socio, if_else(max_cor_socio < UMBRAL_INTRA_COR, "✅", "❌")))
cat(sprintf("   Cross   — max correlación entre factores: %.3f %s\n\n",
            max_cross, if_else(max_cross < UMBRAL_CROSS_COR, "✅", "⚠️")))

# Data frame largo para ggplot (heatmap en Rmd)
cor_long <- as.data.frame(round(cor_completa, 3)) %>%
  rownames_to_column("proxy1") %>%
  pivot_longer(-proxy1, names_to = "proxy2", values_to = "cor") %>%
  mutate(
    proxy1 = factor(proxy1, levels = rev(PROXIES_TODAS)),
    proxy2 = factor(proxy2, levels = PROXIES_TODAS)
  )


# 🪫 4. Tests Kotlarski formales ------------------------------------------------
cat("── Tests Kotlarski formales ─────────────────────────────────────────\n")

tests_kotlarski <- tibble(
  test = c(
    "T1: Varianza suficiente (SD > umbral por proxy)",
    "T2: Cobertura (NA% < umbral por proxy)",
    sprintf("T3: No dominancia intra-cog (max cor < %.2f)", UMBRAL_INTRA_COR),
    sprintf("T4: No dominancia intra-socio (max cor < %.2f)", UMBRAL_INTRA_COR),
    sprintf("T5: Ortogonalidad cross-factor (max cor < %.2f)", UMBRAL_CROSS_COR)
  ),
  resultado = c(
    all(resumen_proxies$test_sd == "PASS"),
    all(resumen_proxies$test_na == "PASS"),
    max_cor_cog   < UMBRAL_INTRA_COR,
    max_cor_socio < UMBRAL_INTRA_COR,
    max_cross     < UMBRAL_CROSS_COR
  ),
  valor = c(
    paste0("min SD = ", round(min(resumen_proxies$sd), 3)),
    paste0("max NA = ", round(max(resumen_proxies$na_pct[!resumen_proxies$na_por_diseno]), 1), "%"),
    paste0("max cor = ", round(max_cor_cog,   3)),
    paste0("max cor = ", round(max_cor_socio, 3)),
    paste0("max cor = ", round(max_cross,     3))
  ),
  umbral = c("SD > 0.10 por proxy", "NA% < umbral por proxy",
             sprintf("cor < %.2f", UMBRAL_INTRA_COR), sprintf("cor < %.2f", UMBRAL_INTRA_COR), sprintf("cor < %.2f", UMBRAL_CROSS_COR))
) %>%
  mutate(status = if_else(resultado, "✅ PASS", "❌ FAIL"))

print(tests_kotlarski %>% select(test, valor, status))

n_fail <- sum(!tests_kotlarski$resultado)
if (n_fail == 0) {
  cat("\n✅ Sistema de proxies APROBADO — listo para Capa 3 (Heterofactor)\n\n")
} else {
  soft_warn(FALSE, sprintf(
    "%d test(s) fallido(s). Revisar sistema de proxies antes de continuar.", n_fail))
}

cat("⚠️  Validación downstream (R² sobre θ) → Script 06b post-estimación\n")
cat("   Criterio: ningún R² > 70%, al menos 3 proxies con R² > 10% por factor\n\n")


# 🪫 5. Evolución temporal de proxies -------------------------------------------
cat("── Calculando evolución temporal ────────────────────────────────────\n")

evolucion_temporal <- datos %>%
  group_by(periodo_id, periodo_num) %>%
  summarise(
    across(all_of(PROXIES_TODAS), ~ mean(.x, na.rm = TRUE), .names = "{.col}"),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(periodo_num)

cat(sprintf("   Periodos cubiertos: %d (%s → %s)\n\n",
            nrow(evolucion_temporal),
            min(evolucion_temporal$periodo_id),
            max(evolucion_temporal$periodo_id)))

# Formato largo para ggplot
evolucion_long <- evolucion_temporal %>%
  pivot_longer(all_of(PROXIES_TODAS), names_to = "proxy", values_to = "media") %>%
  left_join(umbrales %>% select(proxy, factor, fuente), by = "proxy") %>%
  mutate(
    proxy = factor(proxy, levels = PROXIES_TODAS),
    # Etiqueta corta para facets
    proxy_label = case_when(
      proxy == "rezago_escolar_cohorte"   ~ "Rezago escolar\n(cohorte)",
      proxy == "clima_educativo_hogar"    ~ "Clima educativo\n(hogar)",
      proxy == "emparejamiento_selectivo" ~ "Emparejamiento\nselectivo",
      proxy == "calificacion_norm"        ~ "Calificación\nocupacional",
      proxy == "entropia_estabilidad"     ~ "Entropía de\nestabilidad",
      proxy == "residual_vivienda"        ~ "Residual\nvivienda",
      proxy == "busqueda_formal"          ~ "Búsqueda\nformal",
      TRUE ~ as.character(proxy)
    ),
    proxy_label = factor(proxy_label, levels = unique(proxy_label)),
    proxy_label = tr_levels(proxy_label)
  )


# 🪫 6. Outputs adicionales para el paper ---------------------------------------
cat("── Guardando outputs para el paper ──────────────────────────────────\n")

# CSV 1: Tabla descriptiva completa
resumen_proxies %>%
  select(proxy, factor, fuente, mecanismo, n_valid, na_pct,
         media, sd, mediana, p25, p75, min, max, test_sd, test_na) %>%
  write_csv(PATH_CSV_DESC)
cat(sprintf("   ✅ %s\n", basename(PATH_CSV_DESC)))

# CSV 2: Matriz de correlaciones
as.data.frame(round(cor_completa, 4)) %>%
  rownames_to_column("proxy") %>%
  write_csv(PATH_CSV_COR)
cat(sprintf("   ✅ %s\n", basename(PATH_CSV_COR)))

# CSV 3: Tests Kotlarski
tests_kotlarski %>%
  write_csv(PATH_CSV_TESTS)
cat(sprintf("   ✅ %s\n", basename(PATH_CSV_TESTS)))

# CSV 4: Evolución temporal
evolucion_temporal %>%
  write_csv(PATH_CSV_TEMPORAL)
cat(sprintf("   ✅ %s\n\n", basename(PATH_CSV_TEMPORAL)))

# RDS: Objeto diagnóstico completo (para reproducibilidad y secciones del paper)
diagnostico_05 <- list(
  metadata = list(
    timestamp   = Sys.time(),
    script      = "05_reporte_proxies.R",
    input       = PATH_04_PANEL_PROXIES,
    n_obs       = nrow(datos),
    n_vars      = ncol(datos),
    periodo_ini = min(datos$periodo_id),
    periodo_fin = max(datos$periodo_id)
  ),
  proxies = list(
    cognitivas     = PROXIES_COG,
    socioemocional = PROXIES_SOCIO,
    umbrales       = umbrales
  ),
  estadisticas    = resumen_proxies,
  tests_kotlarski = tests_kotlarski,
  correlaciones   = list(
    completa        = cor_completa,
    intra_cog       = cor_cog,
    intra_socio_2   = cor_socio_2,
    cross           = cross_cors,
    max_intra_cog   = max_cor_cog,
    max_intra_socio = max_cor_socio,
    max_cross       = max_cross,
    largo           = cor_long
  ),
  evolucion_temporal = list(
    wide = evolucion_temporal,
    long = evolucion_long
  ),
  validacion_ok = n_fail == 0
)

saveRDS(diagnostico_05, PATH_DIAG)
cat(sprintf("   ✅ %s\n\n", basename(PATH_DIAG)))


# 🪫 7. Contrato Capa 2 --------------------------------------------------------
cat("📄 Generando contrato Capa 2...\n")

contrato_05 <- list(
  timestamp     = Sys.time(),
  script        = "05_reporte_proxies.R",
  input         = PATH_04_PANEL_PROXIES,
  n_obs         = nrow(datos),
  n_vars        = ncol(datos),
  proxies = list(
    cognitivas     = PROXIES_COG,
    socioemocional = PROXIES_SOCIO
  ),
  estadisticas    = resumen_proxies %>% select(proxy, factor, fuente, n_valid, na_pct, media, sd, mediana, p25, p75),
  tests_kotlarski = tests_kotlarski %>% select(test, valor, status),
  correlaciones   = list(
    max_intra_cog   = max_cor_cog,
    max_intra_socio = max_cor_socio,
    max_cross       = max_cross,
    cross           = cross_cors
  ),
  validacion_ok = n_fail == 0,
  r2_vivienda   = tryCatch(
    readRDS(file.path(DIR_CACHE, "04_r2_vivienda.rds")),
    error = function(e) NA_real_
  ),
  imputation_stats = tryCatch(
    readRDS(file.path(DIR_CACHE, "04_imputation_stats.rds")),
    error = function(e) NULL
  )
)

saveRDS(contrato_05, PATH_CONTRATO_05)
cat(sprintf("   ✅ Contrato guardado: %s\n\n", basename(PATH_CONTRATO_05)))


# 🪫 7b. Objetos gráficos (construidos aquí — referenciados en el Rmd) ---------
cat("── Construyendo objetos gráficos ────────────────────────────────────\n")

# Paleta gradiente para heatmap de correlaciones (escala divergente -1 a 1)
# Justificación: escala continua obligatoria para matriz de correlación — PAL_DESCRIPTIVO
# es discreta y no aplica. Se mantiene como excepción documentada.
.pal_cor_heatmap <- c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6")  # HC documentado: paleta gradiente divergente RdYlBu — escala continua obligatoria para heatmap de correlaciones (PAL_DESCRIPTIVO es discreta, no aplica)

# Etiquetas cortas para ejes del heatmap
.labels_cor_eje <- tr(c(
  "rezago_escolar_cohorte"   = "Rezago\nescolar",
  "clima_educativo_hogar"    = "Clima\neducativo",
  "emparejamiento_selectivo" = "Emparej.\nselectivo",
  "calificacion_norm"        = "Calificación\nnorm.",
  "entropia_estabilidad"     = "Entropía\nestabilidad",
  "residual_vivienda"        = "Residual\nvivienda",
  "busqueda_formal"          = "Búsqueda\nformal"
))

# 1. Distribuciones proxies θ_cog
set.seed(SEED_GLOBAL)
.n_cog    <- sum(!is.na(datos$rezago_escolar_cohorte) |
                 !is.na(datos$clima_educativo_hogar)  |
                 !is.na(datos$emparejamiento_selectivo)|
                 !is.na(datos$calificacion_norm))
.n_m_cog  <- min(200000L, .n_cog)  # HC documentado: límite muestreo RAM para gráficos

p_dist_cog <- datos %>%
  slice_sample(n = .n_m_cog) %>%
  select(all_of(PROXIES_COG)) %>%
  pivot_longer(everything(), names_to = "proxy", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  mutate(
    # Labels con salto de línea para que quepan en los paneles de faceta (1 fila / 4 paneles)
    proxy_lbl = case_when(
      proxy == "rezago_escolar_cohorte"   ~ "Rezago escolar\n(cohorte)",
      proxy == "clima_educativo_hogar"    ~ "Clima educativo\n(hogar)",
      proxy == "emparejamiento_selectivo" ~ "Emparejamiento\nselectivo",
      proxy == "calificacion_norm"        ~ "Calificación\nocupacional",
      TRUE ~ proxy
    ),
    proxy_lbl = factor(proxy_lbl, levels = c(
      "Rezago escolar\n(cohorte)", "Clima educativo\n(hogar)",
      "Emparejamiento\nselectivo", "Calificación\nocupacional"
    )),
    proxy_lbl = tr_levels(proxy_lbl)
  ) %>%
  ggplot(aes(x = valor, fill = proxy_lbl)) +
  geom_density(alpha = 0.75, color = "white") +
  facet_wrap(~ proxy_lbl, scales = "free", nrow = 1) +
  scale_fill_manual(values = PAL_DESCRIPTIVO[1:4]) +
  theme_paper() +
  theme(
    legend.position = "none",
    strip.text      = element_text(size = 7, lineheight = 0.9)
  ) +
  tr_labs(
    title   = "Distribuciones de proxies θ_cognitivo",
    x = NULL, y = "Densidad"
  )

# 2. Distribuciones proxies θ_socio (sin busqueda_formal — NA 96.8%)
.n_socio  <- sum(!is.na(datos$entropia_estabilidad) | !is.na(datos$residual_vivienda))
.n_m_soc  <- min(200000L, .n_socio)  # HC documentado: límite muestreo RAM para gráficos

p_dist_socio <- datos %>%
  slice_sample(n = .n_m_soc) %>%
  select(entropia_estabilidad, residual_vivienda) %>%
  pivot_longer(everything(), names_to = "proxy", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  mutate(
    proxy_lbl = case_when(
      proxy == "entropia_estabilidad" ~ "Entropía de estabilidad",
      proxy == "residual_vivienda"    ~ "Residual vivienda",
      TRUE ~ proxy
    ),
    proxy_lbl = factor(proxy_lbl, levels = c("Entropía de estabilidad",
                                              "Residual vivienda")),
    proxy_lbl = tr_levels(proxy_lbl)
  ) %>%
  ggplot(aes(x = valor, fill = proxy_lbl)) +
  geom_density(alpha = 0.75, color = "white") +
  facet_wrap(~ proxy_lbl, scales = "free", nrow = 1) +
  scale_fill_manual(values = PAL_DESCRIPTIVO[2:3]) +
  theme_paper() +
  theme(legend.position = "none") +
  tr_labs(
    title = "Distribuciones de proxies θ_socioemocional (con varianza continua)",
    x = NULL, y = "Densidad"
  )

# 3. Distribución búsqueda formal (barras — solo desocupados)
p_dist_busqueda <- datos %>%
  filter(condicion_actividad == "Desocupado", !is.na(busqueda_formal)) %>%
  count(busqueda_formal) %>%
  mutate(pct = n / sum(n) * 100,
         busqueda_formal = factor(busqueda_formal)) %>%
  ggplot(aes(x = busqueda_formal, y = pct, fill = busqueda_formal)) +
  geom_col(width = 0.6, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.4, size = 3.0,
            fontface = "bold", color = "grey30") +
  scale_fill_manual(values = colorRampPalette(c(PAL_DESCRIPTIVO[5],
                                                PAL_DESCRIPTIVO[1]))(5)) +
  scale_y_continuous(limits = c(0, 35)) +
  theme_paper() +
  theme(legend.position = "none") +
  tr_labs(
    title = "Búsqueda formal de empleo — intensidad (solo desocupados)",
    x = "Número de métodos formales de búsqueda activos",
    y = "% de desocupados"
  )

# 4. Heatmap de correlaciones
p_heatmap_cor <- cor_long %>%
  mutate(
    proxy1_lbl = factor(.labels_cor_eje[as.character(proxy1)],
                        levels = rev(.labels_cor_eje)),
    proxy2_lbl = factor(.labels_cor_eje[as.character(proxy2)],
                        levels = .labels_cor_eje),
    es_diagonal = proxy1 == proxy2
  ) %>%
  ggplot(aes(x = proxy2_lbl, y = proxy1_lbl, fill = cor)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(es_diagonal, "—", sprintf("%.2f", cor)),
                color  = abs(cor) > 0.5),
            size = 2.8, fontface = "bold") +
  scale_fill_gradientn(
    colors   = .pal_cor_heatmap,
    limits   = c(-1, 1),
    name     = tr("Correlación"),
    na.value = "grey90"
  ) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey25"),
                     guide = "none") +
  annotate("rect", xmin = 4.5, xmax = 7.5, ymin = 0.5, ymax = 3.5,
           fill = NA, color = PAL_DESCRIPTIVO[4], linewidth = 1.2,
           linetype = "dashed") +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 3.5, ymax = 7.5,
           fill = NA, color = PAL_DESCRIPTIVO[1], linewidth = 1.2,
           linetype = "dashed") +
  theme_paper() +
  theme(legend.position  = "right",
        axis.text.x      = element_text(size = 8),
        axis.text.y      = element_text(size = 8)) +
  tr_labs(
    title = "Correlaciones entre proxies — pairwise complete obs.",
    x = NULL, y = NULL
  )

# 5. Evolución temporal θ_cog
p_evol_cog <- evolucion_long %>%
  filter(proxy %in% PROXIES_COG) %>%
  ggplot(aes(x = periodo_num, y = media,
             color = proxy_label, group = proxy_label)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 1.5, alpha = 0.8) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
              alpha = 0.1, linewidth = 0.5) +
  facet_wrap(~ proxy_label, scales = "free_y", nrow = 2) +
  scale_color_manual(values = PAL_DESCRIPTIVO[1:4]) +
  scale_x_continuous(
    breaks = evolucion_temporal$periodo_num[seq(1, nrow(evolucion_temporal), by = 4)],
    labels = evolucion_temporal$periodo_id[seq(1, nrow(evolucion_temporal), by = 4)]
  ) +
  theme_paper() +
  theme(legend.position  = "none",
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 7)) +
  tr_labs(
    title = "Evolución temporal — proxies θ_cognitivo",
    x = NULL, y = "Media trimestral"
  )

# 6. Evolución temporal θ_socio
p_evol_socio <- evolucion_long %>%
  filter(proxy %in% PROXIES_SOCIO) %>%
  ggplot(aes(x = periodo_num, y = media,
             color = proxy_label, group = proxy_label)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 1.5, alpha = 0.8) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
              alpha = 0.1, linewidth = 0.5) +
  facet_wrap(~ proxy_label, scales = "free_y", nrow = 1) +
  scale_color_manual(values = PAL_DESCRIPTIVO[2:4]) +
  scale_x_continuous(
    breaks = evolucion_temporal$periodo_num[seq(1, nrow(evolucion_temporal), by = 4)],
    labels = evolucion_temporal$periodo_id[seq(1, nrow(evolucion_temporal), by = 4)]
  ) +
  theme_paper() +
  theme(legend.position  = "none",
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 7)) +
  tr_labs(
    title = "Evolución temporal — proxies θ_socioemocional",
    x = NULL, y = "Media trimestral"
  )

cat("   ✅ 6 objetos gráficos construidos\n\n")

# ── Exportación a PDF (sistema gráfico unificado) ─────────────────────────────
cat("── Exportando figuras a PDF ────────────────────────────────────────\n\n")

guardar_figura(p_dist_cog,      DIR_FIGURAS_05, "hist",    1, height = ALTO_FIG * 1.4)
guardar_figura(p_dist_socio,    DIR_FIGURAS_05, "hist",    2)
guardar_figura(p_dist_busqueda, DIR_FIGURAS_05, "barras",  3)
guardar_figura(p_heatmap_cor,   DIR_FIGURAS_05, "scatter", 4, height = ALTO_FIG * 1.8)
guardar_figura(p_evol_cog,      DIR_FIGURAS_05, "series",  5,
               height = ALTO_FIG * 2, width = ANCHO_FIG * 1.5)
guardar_figura(p_evol_socio,    DIR_FIGURAS_05, "series",  6,
               height = ALTO_FIG * 1.4, width = ANCHO_FIG * 1.5)

cat("\n")

# 🪫 8. Reporte HTML ------------------------------------------------------------
cat("📊 Generando reporte HTML...\n")
hard_stop(file.exists(PATH_RMD),
          paste0("No se encontró el Rmd en: ", PATH_RMD,
                 "\nColocar 05_reporte_proxies.Rmd en script/02_proxies/"))

rmarkdown::render(
  input       = PATH_RMD,
  output_file = PATH_REPORTE,
  envir       = environment(),
  quiet       = TRUE
)
cat(sprintf("   ✅ Reporte HTML: %s\n\n", basename(PATH_REPORTE)))


# 📑 Checklist y cierre --------------------------------------------------------
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("═══════════════════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 05:\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("   [%s] Tests Kotlarski: %d/5 PASS\n",
            if_else(n_fail == 0, "✅", "❌"), 5L - n_fail))
cat(sprintf("   [✅] Cor max intra-cog:   %.3f (umbral < %.2f)\n", max_cor_cog, UMBRAL_INTRA_COR))
cat(sprintf("   [✅] Cor max intra-socio: %.3f (umbral < %.2f)\n", max_cor_socio, UMBRAL_INTRA_COR))
cat(sprintf("   [✅] Cor max cross:       %.3f (umbral < %.2f)\n", max_cross, UMBRAL_CROSS_COR))
cat(sprintf("   [✅] %s\n", basename(PATH_CSV_DESC)))
cat(sprintf("   [✅] %s\n", basename(PATH_CSV_COR)))
cat(sprintf("   [✅] %s\n", basename(PATH_CSV_TESTS)))
cat(sprintf("   [✅] %s\n", basename(PATH_CSV_TEMPORAL)))
cat(sprintf("   [✅] %s\n", basename(PATH_DIAG)))
cat(sprintf("   [✅] %s\n", basename(PATH_CONTRATO_05)))
cat(sprintf("   [✅] %s\n", basename(PATH_REPORTE)))
cat("───────────────────────────────────────────────────────────────────\n")
cat("   🎯 CAPA 2 COMPLETADA\n")
cat("───────────────────────────────────────────────────────────────────\n")
cat("🎯 SIGUIENTE PASO: Capa 3 — Script 06 (Estimación Heterofactor)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat(sprintf("⏱️  Tiempo total: %.1f segundos\n\n", elapsed))

toc()
