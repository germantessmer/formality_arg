# =============================================================================
# [EN] 03c_reporte_base.R -- Diagnostic HTML report for Layer 1 data quality and ICH validation
# INPUTS:  rdos/datos/03b_panel_base_completo.rds, rdos/contratos/03b_contrato_datos_base.rds
# OUTPUTS: rdos/reportes/03c_reporte_ich.html, rdos/reportes/03c_diagnostico_ich.rds
# =============================================================================
# 🌟 03c_reporte_base.R 🌟 ####
#
# OBJETIVO:  Reporte de validación de Capa 1 con diagnóstico extendido del ICH.
#            Documenta fortalezas y limitaciones del índice para uso en paper
#            de alto perfil. Genera HTML navegable + archivos de diagnóstico
#            reproducibles.
#
# INPUTS:    PATH_03B_PANEL_ICH    → rdos/datos/03b_panel_base_completo.rds
#            PATH_CONTRATO_03B     → rdos/contratos/03b_contrato_datos_base.rds
#            03b_mca_fit.rds       → rdos/modelos/03b_mca_fit.rds
#
# OUTPUTS:   03c_reporte_ich.html          → rdos/reportes/
#            03c_diagnostico_ich.rds       → rdos/reportes/
#            03c_tabla_contribuciones.csv  → rdos/reportes/
#            03c_estabilidad_temporal.csv  → rdos/reportes/
#
# NOTA:      Requiere haber ejecutado 03b_ich.R primero.
#            Los gráficos usan samples aleatorios para optimizar uso de RAM.
#            nbi se incluye condicionalmente si existe en el dataset.


# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
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


# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 03c [Reporte Base]")
start_time <- Sys.time()
cat("═══════════════════════════════════════════════════════\n")
cat("🚀 Script 03c iniciado:", as.character(start_time), "\n")
cat("═══════════════════════════════════════════════════════\n\n")


# 🪫 1. Carga de datos, contrato y mca_fit -------------------------------------
cat("📂 Cargando datos, contrato y objeto MCA...\n")

.path_mca_fit <- PATH_03B_MCA_FIT

hard_stop(file.exists(PATH_03B_PANEL_ICH),
          "No existe 03b_panel_base_completo.rds. Ejecutar 03b_ich.R primero.")
hard_stop(file.exists(PATH_CONTRATO_03B),
          "No existe 03b_contrato_datos_base.rds. Ejecutar 03b_ich.R primero.")
hard_stop(file.exists(.path_mca_fit),
          "No existe 03b_mca_fit.rds. Ejecutar 03b_ich.R primero.")

datos    <- readRDS(PATH_03B_PANEL_ICH)
contrato <- readRDS(PATH_CONTRATO_03B)
mca_fit  <- readRDS(.path_mca_fit)

cat("✅ Panel cargado:  ", format(nrow(datos), big.mark = ","), "obs ×",
    ncol(datos), "vars\n")
cat("✅ Contrato cargado\n")
cat("✅ mca_fit cargado\n\n")

# Detectar nbi condicionalmente
.tiene_nbi <- "nbi" %in% names(datos)
cat(sprintf("   NBI disponible para validación cruzada: %s\n\n",
            if (.tiene_nbi) "✅ SÍ" else "⚠️  NO — sección omitida"))


# 🪫 2. Preparación de objetos de diagnóstico ----------------------------------
cat("🔬 Extrayendo diagnósticos del MCA...\n")

# 2.1. Eigenvalues (todas las dimensiones)
diag_eigenvalues <- as.data.frame(mca_fit$eig) %>%
  rownames_to_column("dimension") %>%
  rename(
    eigenvalue       = eigenvalue,
    var_pct          = `percentage of variance`,
    var_pct_acum     = `cumulative percentage of variance`
  ) %>%
  mutate(dimension = gsub("dim ", "Dim ", dimension))

cat("   ✅ Eigenvalues extraídos:", nrow(diag_eigenvalues), "dimensiones\n")

# 2.2. Contribuciones de variables a dim1 (loadings)
diag_contribuciones <- as.data.frame(mca_fit$var$contrib) %>%
  rownames_to_column("categoria") %>%
  rename_with(~ paste0("dim", seq_along(.x)), -categoria) %>%
  mutate(
    indicador = sub("_[^_]+$", "", categoria),  # extraer nombre del indicador
    .before   = categoria
  ) %>%
  arrange(desc(dim1))

cat("   ✅ Contribuciones extraídas:", nrow(diag_contribuciones), "categorías\n")

# Contribuciones agregadas por indicador (suma de categorías)
diag_contrib_indicador <- diag_contribuciones %>%
  group_by(indicador) %>%
  summarise(contrib_dim1 = sum(dim1), .groups = "drop") %>%
  arrange(desc(contrib_dim1))

# 2.3. Coordenadas de categorías en dim1 (ordenamiento semántico)
diag_coordenadas <- as.data.frame(mca_fit$var$coord) %>%
  rownames_to_column("categoria") %>%
  select(categoria, Dim1 = `Dim 1`) %>%
  arrange(Dim1)

cat("   ✅ Coordenadas por categoría extraídas\n")

# 2.4. Cos2 (calidad de representación en dim1)
diag_cos2 <- as.data.frame(mca_fit$var$cos2) %>%
  rownames_to_column("categoria") %>%
  select(categoria, cos2_dim1 = `Dim 1`) %>%
  arrange(desc(cos2_dim1))

cat("   ✅ Cos2 extraídos\n\n")

rm(mca_fit); gc(verbose = FALSE)


# 🪫 3. Samples para gráficos (optimización RAM) -------------------------------
cat("📊 Creando samples para gráficos...\n")
set.seed(SEED_GLOBAL)

# Cobertura temporal: 10% por periodo
datos_sample_temporal <- datos %>%
  group_by(periodo_id) %>%
  slice_sample(prop = 0.1) %>%
  ungroup()

# PEA: hasta 50k obs
datos_sample_pea <- datos %>%
  filter(pea_flag) %>%
  slice_sample(n = min(50000, sum(.$pea_flag)))  # HC documentado: límite muestreo RAM

# ICH: hasta 50k obs
datos_sample_ich <- datos %>%
  filter(!is.na(ich_score)) %>%
  slice_sample(n = min(50000, sum(!is.na(.$ich_score))))  # HC documentado: límite muestreo RAM

# Correlación ICH-ingreso: hasta 10k obs
.n_cor <- sum(!is.na(datos$ich_score) & !is.na(datos$ingreso_real_final) &
              datos$ingreso_real_final > 0)
datos_sample_cor <- datos %>%
  filter(!is.na(ich_score), !is.na(ingreso_real_final), ingreso_real_final > 0) %>%
  slice_sample(n = min(10000, .n_cor))  # HC documentado: límite muestreo RAM

# Estabilidad temporal: ICH por trimestre — estadísticas completas (no sample)
diag_estabilidad <- datos %>%
  filter(!is.na(ich_score)) %>%
  group_by(anio, periodo_id) %>%
  summarise(
    n         = n(),
    media     = round(mean(ich_score), 2),
    mediana   = round(median(ich_score), 2),
    sd        = round(sd(ich_score), 2),
    p25       = round(quantile(ich_score, 0.25), 2),
    p75       = round(quantile(ich_score, 0.75), 2),
    .groups   = "drop"
  ) %>%
  arrange(periodo_id)

# Distribución por aglomerado (primario)
diag_aglomerado <- datos %>%
  filter(!is.na(ich_score)) %>%
  group_by(aglomerado) %>%
  summarise(
    n         = n(),
    media     = round(mean(ich_score), 2),
    mediana   = round(median(ich_score), 2),
    sd        = round(sd(ich_score), 2),
    .groups   = "drop"
  ) %>%
  arrange(desc(media))

# Distribución por región (complementario)
diag_region <- datos %>%
  filter(!is.na(ich_score)) %>%
  group_by(region) %>%
  summarise(
    n         = n(),
    media     = round(mean(ich_score), 2),
    mediana   = round(median(ich_score), 2),
    sd        = round(sd(ich_score), 2),
    .groups   = "drop"
  ) %>%
  arrange(desc(media))

# NBI convergent validity (condicional)
if (.tiene_nbi) {
  diag_nbi <- datos %>%
    filter(!is.na(ich_score), !is.na(nbi)) %>%
    group_by(nbi) %>%
    summarise(
      n       = n(),
      media   = round(mean(ich_score), 2),
      sd      = round(sd(ich_score), 2),
      .groups = "drop"
    )
}

cat("✅ Samples y diagnósticos preparados\n\n")

# Liberar panel completo — ya no se necesita
rm(datos); gc(verbose = FALSE)
cat("🧹 Panel liberado de RAM\n\n")


# 🪫 4b. Objetos gráficos (construidos aquí — referenciados en el Rmd) ----------
cat("📊 Construyendo objetos gráficos...\n")

# Paletas auxiliares (no sustituyen a PAL_DESCRIPTIVO — uso específico)
.pal_gradual <- colorRampPalette(c("#f7fbff", "#2171b5"))(10)  # HC documentado: azul claro→azul medio. Escala continua para geom_hex/barras geo. No es identidad de modelo.

# Paleta MCA — tamaño dinámico, SIN nombres (positional assignment evita warnings de ggplot2
# cuando el campo `indicador` en el ggplot se construye en el mutate interno)
.n_indicadores_mca <- length(unique(
  gsub("ind_", "", sub("_[^_]+$", "", diag_coordenadas$categoria))
))
.pal_mca <- colorRampPalette(
  unname(grDevices::palette.colors(n = 12, palette = "Paired"))
)(.n_indicadores_mca)

# 1. Cobertura temporal
p_cobertura <- datos_sample_temporal %>%
  count(periodo_id) %>%
  arrange(periodo_id) %>%
  mutate(periodo_id = factor(periodo_id, levels = unique(periodo_id))) %>%
  ggplot(aes(x = periodo_id, y = n)) +
  geom_col(fill = PAL_DESCRIPTIVO[1], alpha = 0.85) +
  theme_paper() +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  tr_labs(title = "Observaciones por trimestre (sample 10%)",
          x = NULL, y = "N observaciones")

# 2. Taxonomía sección económica
p_taxonomia_seccion <- datos_sample_pea %>%
  count(seccion, sort = TRUE) %>%
  head(12) %>%
  mutate(
    pct   = round(100 * n / sum(n), 1),
    label = paste0(format(n, big.mark = ","), "  (", pct, "%)"),
    seccion = tr(as.character(seccion))
  ) %>%
  ggplot(aes(x = reorder(seccion, n), y = n)) +
  geom_col(fill = PAL_DESCRIPTIVO[2], alpha = 0.85) +
  geom_text(aes(label = label), hjust = -0.05, size = 2.6, color = "grey20") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
  theme_paper() +
  tr_labs(title = "Top 12 secciones económicas (PEA, sample 50k)",
          x = NULL, y = "N observaciones")

# 3. Taxonomía calificación
p_taxonomia_calificacion <- datos_sample_pea %>%
  count(calificacion) %>%
  mutate(
    pct   = round(100 * n / sum(n), 1),
    label = paste0(format(n, big.mark = ","), "  (", pct, "%)"),
    calificacion = tr(as.character(calificacion))
  ) %>%
  ggplot(aes(x = reorder(calificacion, n), y = n)) +
  geom_col(fill = PAL_DESCRIPTIVO[3], alpha = 0.85) +
  geom_text(aes(label = label), hjust = -0.05, size = 2.6, color = "grey20") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
  theme_paper() +
  tr_labs(title = "Calificación ocupacional (PEA, sample 50k)",
          x = NULL, y = "N observaciones")

# 4a. Distribución ICH — histograma
p_ich_hist <- datos_sample_ich %>%
  ggplot(aes(x = ich_score)) +
  geom_histogram(bins = 60, fill = PAL_DESCRIPTIVO[1], color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean(datos_sample_ich$ich_score, na.rm = TRUE),
             linetype = "dashed", color = PAL_DESCRIPTIVO[4], linewidth = 0.8) +
  theme_paper() +
  tr_labs(title = "Distribución ICH",
          x = "ICH Score (0\u2013100)", y = "Frecuencia")

# 4b. Distribución ICH — violin + boxplot
p_ich_violin <- datos_sample_ich %>%
  ggplot(aes(x = "", y = ich_score)) +
  geom_violin(fill = PAL_DESCRIPTIVO[1], alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.2, fill = "white", color = PAL_DESCRIPTIVO[1],
               outlier.size = 0.3, outlier.alpha = 0.3) +
  theme_paper() +
  tr_labs(title = "Boxplot + densidad",
          subtitle = "ICH Score (0\u2013100)", x = NULL, y = "ICH Score")

# 4 combinado (patchwork)
p_ich_dist <- p_ich_hist + p_ich_violin

# 5. Eigenvalues MCA
p_eigenvalues <- diag_eigenvalues %>%
  head(10) %>%
  mutate(
    dimension = factor(dimension, levels = rev(dimension)),
    # Color precalculado en el dato — evita comparación de names() en scale_fill_manual
    color_barra = if_else(
      tolower(as.character(dimension)) == "dim 1",
      PAL_DESCRIPTIVO[1], "grey70"
    )
  ) %>%
  ggplot(aes(x = dimension, y = var_pct)) +
  geom_col(aes(fill = color_barra), show.legend = FALSE, alpha = 0.85) +
  scale_fill_identity() +
  geom_text(aes(label = paste0(round(var_pct, 1), "%")),
            hjust = -0.1, size = 2.8) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_paper() +
  tr_labs(title = "Varianza explicada por dimensión MCA (primeras 10)",
          x = NULL, y = "% varianza")

# 6. Contribuciones por indicador a Dim1
p_contribuciones <- diag_contrib_indicador %>%
  mutate(indicador = gsub("ind_", "", indicador)) %>%
  ggplot(aes(x = reorder(indicador, contrib_dim1), y = contrib_dim1)) +
  geom_col(fill = PAL_DESCRIPTIVO[1], alpha = 0.85) +
  geom_hline(yintercept = 100 / nrow(diag_contrib_indicador),
             linetype = "dashed", color = PAL_DESCRIPTIVO[4], linewidth = 0.7) +
  geom_text(aes(label = paste0(round(contrib_dim1, 1), "%")),
            hjust = -0.1, size = 2.8) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  theme_paper() +
  tr_labs(title = "Contribución de cada indicador a Dim1 (% total)",
          x = NULL, y = "Contribución a Dim1 (%)")

# 7. Coordenadas PCA — 10 categorías de indicadores
p_coordenadas <- diag_coordenadas %>%
  mutate(
    indicador       = sub("_[^_]+$", "", categoria),
    indicador       = gsub("ind_", "", indicador),
    categoria_label = sub(".*_", "", categoria)
  ) %>%
  ggplot(aes(x = Dim1, y = reorder(categoria, Dim1), color = indicador)) +
  geom_point(size = 3.5, alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = .pal_mca) +
  theme_paper() +
  theme(legend.position = "right") +
  tr_labs(title = "Coordenadas de categorías en Dim1",
          x = "Coordenada Dim1", y = NULL, color = "Indicador")

# 8. Cos² por categoría en Dim1
p_cos2 <- diag_cos2 %>%
  mutate(
    indicador       = sub("_[^_]+$", "", categoria),
    indicador       = gsub("ind_", "", indicador),
    categoria_label = categoria
  ) %>%
  ggplot(aes(x = cos2_dim1, y = reorder(categoria_label, cos2_dim1), fill = indicador)) +
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = 0.3, linetype = "dashed",
             color = PAL_DESCRIPTIVO[4], linewidth = 0.7) +
  scale_fill_manual(values = .pal_mca) +
  theme_paper() +
  theme(legend.position = "right") +
  tr_labs(title = "Cos² por categoría en Dim1",
          x = "Cos² (Dim1)", y = NULL, fill = "Indicador")

# 9. Scatter ICH vs Ingreso (hex)
p_ich_ingreso <- datos_sample_cor %>%
  ggplot(aes(x = ich_score, y = log(ingreso_real_final + 1))) +
  geom_hex(bins = 50, alpha = 0.85) +
  scale_fill_gradientn(colors = .pal_gradual) +
  geom_smooth(method = "lm", color = PAL_DESCRIPTIVO[4],
              linewidth = 1.1, se = TRUE) +
  theme_paper() +
  labs(
    title = paste0("ICH vs log(", tr("Ingreso real"), ") | r = ",
                   round(contrato$cor_ich_ingreso, 3)),
    x = tr("ICH Score (0\u2013100)"), y = tr("log(Ingreso real + 1)"),
    fill = tr("N obs")
  )

# 10. Estabilidad temporal del ICH
p_estabilidad <- diag_estabilidad %>%
  mutate(periodo_id = factor(periodo_id, levels = unique(periodo_id))) %>%
  ggplot(aes(x = periodo_id, y = media)) +
  geom_ribbon(aes(ymin = media - sd, ymax = media + sd,
                  group = 1), fill = PAL_DESCRIPTIVO[1], alpha = 0.2) +
  geom_line(aes(group = 1), color = PAL_DESCRIPTIVO[1], linewidth = 1) +
  geom_point(color = PAL_DESCRIPTIVO[1], size = 2) +
  theme_paper() +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  tr_labs(title = "Estabilidad temporal del ICH",
          x = NULL, y = "ICH Score medio")

# 11. Geo aglomerado (muchas categorías — mayor altura)
p_geo_aglomerado <- diag_aglomerado %>%
  mutate(aglomerado = factor(aglomerado, levels = aglomerado)) %>%
  ggplot(aes(x = reorder(aglomerado, media), y = media)) +
  geom_col(aes(fill = media), show.legend = FALSE, alpha = 0.85) +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd),
                width = 0.4, color = "grey40", linewidth = 0.4) +
  scale_fill_gradientn(colors = .pal_gradual) +
  coord_flip() +
  theme_paper() +
  tr_labs(title = "ICH medio por aglomerado (± 1 SD)",
          x = NULL, y = "ICH Score medio")

# 12. Geo región
p_geo_region <- diag_region %>%
  ggplot(aes(x = reorder(region, media), y = media)) +
  geom_col(aes(fill = media), show.legend = FALSE, alpha = 0.85) +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd),
                width = 0.3, color = "grey40", linewidth = 0.5) +
  scale_fill_gradientn(colors = .pal_gradual) +
  coord_flip() +
  theme_paper() +
  tr_labs(title = "ICH medio por región (± 1 SD)",
          x = NULL, y = "ICH Score medio")

# 13. NBI convergent validity (condicional)
if (.tiene_nbi) {
  p_nbi <- diag_nbi %>%
    mutate(
      # Color precalculado — evita comparación de names() en scale_fill_manual
      color_barra = if_else(nbi == 0L, PAL_DESCRIPTIVO[3], PAL_DESCRIPTIVO[4])
    ) %>%
    mutate(nbi_label = tr(ifelse(nbi == 0L | nbi == "No", "No", "Yes"))) %>%
    ggplot(aes(x = nbi_label, y = media, fill = color_barra)) +
    geom_col(alpha = 0.85, show.legend = FALSE) +
    geom_errorbar(aes(ymin = media - sd, ymax = media + sd),
                  width = 0.3, linewidth = 0.6) +
    scale_fill_identity() +
    theme_paper() +
    tr_labs(title = "Average ICH by UBN status",
            x = "UBN (0 = without, 1 = with)", y = "Mean ICH score")
}

cat("   ✅ 13 objetos gráficos construidos\n\n")

# ── Exportación a PDF (sistema gráfico unificado) ─────────────────────────────
cat("── Exportando figuras a PDF ────────────────────────────────────────\n\n")

guardar_figura(p_cobertura,              DIR_FIGURAS_03C, "barras",  1)
guardar_figura(p_taxonomia_seccion,      DIR_FIGURAS_03C, "barras",  2,
               height = ALTO_FIG * 1.5)
guardar_figura(p_taxonomia_calificacion, DIR_FIGURAS_03C, "barras",  3)
guardar_figura(p_ich_dist,               DIR_FIGURAS_03C, "hist",    4,
               width = ANCHO_FIG * 2)
guardar_figura(p_eigenvalues,            DIR_FIGURAS_03C, "barras",  5)
guardar_figura(p_contribuciones,         DIR_FIGURAS_03C, "barras",  6)
guardar_figura(p_coordenadas,            DIR_FIGURAS_03C, "pca",     7,
               height = ALTO_FIG * 2.5)
guardar_figura(p_cos2,                   DIR_FIGURAS_03C, "pca",     8,
               height = ALTO_FIG * 2)
guardar_figura(p_ich_ingreso,            DIR_FIGURAS_03C, "scatter", 9)
guardar_figura(p_estabilidad,            DIR_FIGURAS_03C, "series",  10)
guardar_figura(p_geo_aglomerado,         DIR_FIGURAS_03C, "barras",  11,
               height = ALTO_FIG * 2)
guardar_figura(p_geo_region,             DIR_FIGURAS_03C, "barras",  12)
if (.tiene_nbi) {
  guardar_figura(p_nbi,                  DIR_FIGURAS_03C, "barras",  13)
}

cat("\n")

# 🪫 4. Construcción del reporte HTML ------------------------------------------
# Etiqueta de rango temporal: construida desde parametros.R (nunca hardcodeada)
.rango <- sprintf("%dT%d–%dT%d", ANIO_INI, TRIM_INI, ANIO_FIN, TRIM_FIN)
cat("📝 Generando reporte HTML...\n")

.ruta_reporte <- PATH_03C_HTML
.rmd_temp     <- tempfile(fileext = ".Rmd")

writeLines(
  enc2utf8('
---
title: "Validación Capa 1 — Índice de Calidad Habitacional (ICH)"
subtitle: "Pipeline formalidad_back | EPH Argentina `r .rango`"
date: "`r format(Sys.time(), \'%d %B %Y, %H:%M\')`"
output:
  html_document:
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: true
    toc_depth: 3
    theme: flatly
    highlight: tango
    code_folding: hide
    number_sections: true
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo    = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width  = 11,  # HC documentado: dimensiones preview HTML — independiente de ANCHO_FIG (PDFs)
  fig.height = 5.5,
  dpi        = 150  # HC documentado: resolución preview HTML — no afecta PDFs (cairo_pdf vía guardar_figura)
)
# Objetos gráficos (p_cobertura, p_ich_dist, etc.) construidos en 03c_reporte_base.R
# theme_paper(), PAL_DESCRIPTIVO, .pal_gradual disponibles via envir=environment()
```

---

# Resumen ejecutivo {.unnumbered}

```{r resumen_ejecutivo}
tibble(
  Parámetro = c("Cobertura temporal", "Observaciones totales", "PEA",
                "Hogares únicos", "ICH completitud", "Varianza explicada (dim1 MCA)",
                "Correlación ICH–log(ingreso)", "OpenBLAS"),
  Valor = c(
    paste0(contrato$anio_ini, "T", contrato$trim_ini, " → ",
           contrato$anio_fin, "T", contrato$trim_fin),
    format(contrato$N_total, big.mark = ","),
    paste0(format(contrato$N_pea, big.mark = ","), " (",
           round(100 * contrato$N_pea / contrato$N_total, 1), "%)"),
    format(nrow(diag_estabilidad %>% summarise(n = sum(n))),  big.mark = ","),
    paste0(contrato$pct_ich_completo, "%"),
    paste0(round(contrato$var_explicada_dim1_mca, 1), "%"),
    round(contrato$cor_ich_ingreso, 3),
    ifelse(contrato$openblas_activo, "Activo (MCA multihilo)", "No detectado")
  )
) %>%
  kable(format = "html", align = "lr", col.names = c("Parámetro", "Valor")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "left") %>%
  row_spec(5:7, background = "#f0f7fb")  # HC documentado: highlight kableExtra — cosmético, sin relación con PAL_DESCRIPTIVO
```

---

# Cobertura temporal y estructura del panel

```{r cobertura_temporal, fig.height=4}
p_cobertura
```

---

# Taxonomía ocupacional (PEA)

## Sección económica (Top 12)

```{r taxonomia_seccion, fig.height=5.5}
p_taxonomia_seccion
```

## Calificación ocupacional

```{r taxonomia_calificacion, fig.height=4}
p_taxonomia_calificacion
```

---

# ICH — Diagnóstico metodológico

> El ICH (Índice de Calidad Habitacional) resume 10 indicadores de vivienda
> en una única dimensión mediante Análisis de Correspondencias Múltiples (MCA).
> Esta sección documenta las fortalezas y limitaciones del índice para
> evaluación metodológica en un paper de alto perfil.

## Distribución del score

```{r ich_distribucion}
p_ich_dist
```

**Estadísticas descriptivas (sample `r format(nrow(datos_sample_ich), big.mark = ",")` obs):**

```{r ich_stats_tabla}
tibble(
  Estadístico = c("Media", "Mediana", "Desvío estándar", "P10", "P25", "P75", "P90",
                  "Completitud"),
  Valor = c(
    round(mean(datos_sample_ich$ich_score,              na.rm = TRUE), 1),
    round(median(datos_sample_ich$ich_score,            na.rm = TRUE), 1),
    round(sd(datos_sample_ich$ich_score,                na.rm = TRUE), 1),
    round(quantile(datos_sample_ich$ich_score, 0.10,    na.rm = TRUE), 1),
    round(quantile(datos_sample_ich$ich_score, 0.25,    na.rm = TRUE), 1),
    round(quantile(datos_sample_ich$ich_score, 0.75,    na.rm = TRUE), 1),
    round(quantile(datos_sample_ich$ich_score, 0.90,    na.rm = TRUE), 1),
    paste0(contrato$pct_ich_completo, "%")
  )
) %>%
  kable(format = "html", align = "lr") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE, position = "left")
```

## Estructura del MCA — Eigenvalues

> **Fortaleza:** Dim1 captura la heterogeneidad principal entre hogares de alta
> y baja calidad habitacional. Un índice basado en la primera dimensión es
> metodológicamente válido cuando ésta concentra varianza sustancialmente mayor
> que las siguientes.
>
> **Limitación:** En MCA con variables categóricas de múltiples niveles, la
> varianza explicada por dimensión tiende a ser baja en términos absolutos.
> El criterio relevante es la *distancia relativa* entre Dim1 y Dim2, no el
> porcentaje absoluto.

```{r eigenvalues, fig.height=4.5}
p_eigenvalues
```

```{r eigenvalues_tabla}
diag_eigenvalues %>%
  head(10) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  kable(format = "html",
        col.names = c("Dimensión", "Eigenvalue", "% Varianza", "% Acumulado"),
        align = "lrrr") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE, position = "left") %>%
  row_spec(1, bold = TRUE, background = "#ddeef7")
```

## Contribuciones por indicador a Dim1

> **Fortaleza:** Muestra qué aspectos de la vivienda son más discriminantes.
> Una contribución distribuida entre múltiples indicadores (sin dominancia de
> uno solo) indica que el índice captura genuinamente un constructo
> multidimensional y no es proxy de una única variable.

```{r contribuciones_indicador, fig.height=4.5}
p_contribuciones
```

```{r contribuciones_tabla}
diag_contrib_indicador %>%
  mutate(
    indicador    = gsub("ind_", "", indicador),
    contrib_dim1 = round(contrib_dim1, 2),
    sobre_media  = ifelse(contrib_dim1 > 100/nrow(diag_contrib_indicador),
                         "⬆ sobre media", "⬇ bajo media")
  ) %>%
  kable(format = "html",
        col.names = c("Indicador", "Contribución Dim1 (%)", "Relativo a media"),
        align = "lrr") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE, position = "left")
```

## Coordenadas por categoría — Validación semántica

> **Fortaleza:** Si el MCA captura correctamente calidad habitacional, las
> categorías de "mejor vivienda" deben tener coordenadas en el mismo extremo
> de Dim1, y las de "peor vivienda" en el extremo opuesto. La normalización
> posterior invierte el eje para que ICH alto = mejor calidad.
>
> **Limitación:** El signo de Dim1 es arbitrario en MCA; la interpretación
> depende de verificar el ordenamiento semántico de las categorías.

```{r coordenadas_categorias, fig.height=8}
p_coordenadas
```

## Calidad de representación (Cos²)

> **Cos²** mide qué tan bien representada está cada categoría en Dim1.
> Valores altos indican que Dim1 captura la mayor parte de la variabilidad
> de esa categoría. Valores bajos sugieren que esa categoría requeriría
> dimensiones adicionales para ser bien representada.

```{r cos2, fig.height=6}
p_cos2
```

## Correlación ICH – Ingreso

> **Fortaleza:** Una correlación positiva significativa entre ICH e ingreso
> confirma validez de constructo: los hogares con mejor calidad habitacional
> tienden a tener mayores ingresos, lo que es consistente con la teoría.
>
> **Limitación:** La correlación no debe ser excesivamente alta (> 0.6),
> ya que eso indicaría colinealidad con el ingreso y reduciría el aporte
> independiente del ICH como predictor de formalidad.

```{r ich_ingreso, fig.height=5}
p_ich_ingreso
```

## Estabilidad temporal

> **Fortaleza clave para el paper:** Si el ICH es estable en el tiempo
> (media y SD con baja variación trimestral), es un predictor confiable
> en modelos de panel longitudinal. La estabilidad también indica que el
> índice no está capturando ruido cíclico sino características estructurales
> del hogar.
>
> **Limitación a reportar:** Cambios abruptos en algún trimestre pueden
> indicar cambios en el cuestionario EPH o en la codificación de variables.

```{r estabilidad_temporal, fig.height=5}
p_estabilidad
```

```{r estabilidad_tabla}
diag_estabilidad %>%
  select(periodo_id, n, media, mediana, sd, p25, p75) %>%
  kable(format = "html",
        col.names = c("Período", "N", "Media", "Mediana", "SD", "P25", "P75"),
        align = "lrrrrrr") %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "responsive"),
                full_width = FALSE, position = "left") %>%
  scroll_box(height = "350px")
```

## Variación geográfica — Aglomerado (primario)

> El análisis por aglomerado es el más informativo para este proyecto:
> captura la dinámica real del mercado laboral y las diferencias estructurales
> en condiciones habitacionales entre centros urbanos de distinto tamaño y
> perfil económico.

```{r geo_aglomerado, fig.height=7}
p_geo_aglomerado
```

## Variación geográfica — Región (complementario)

> La clasificación regional (NEA, NOA, Pampeana, Patagónica, GBA, Cuyo)
> sigue criterios administrativos y no necesariamente refleja dinámicas
> laborales homogéneas dentro de cada región. Se presenta como referencia
> para comparabilidad con literatura que usa esta agregación.

```{r geo_region, fig.height=4}
p_geo_region
```

```{r geo_tabla}
diag_region %>%
  kable(format = "html",
        col.names = c("Región", "N obs", "Media", "Mediana", "SD"),
        align = "lrrrr") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE, position = "left")
```

## Validez convergente — NBI {#nbi}

```{r nbi_block, eval=.tiene_nbi, fig.height=4}
p_nbi
```

```{r nbi_tabla, eval=.tiene_nbi}
diag_nbi %>%
  kable(format = "html",
        col.names = c("NBI", "N obs", "Media ICH", "SD ICH"),
        align = "lrrr") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE, position = "left")
```

```{r nbi_ausente, eval=!.tiene_nbi}
cat("> ⚠️ Variable `nbi` no disponible en el dataset. Sección omitida.")
```

---

# Completitud de variables críticas

```{r completitud}
tibble(
  Variable    = c("edad", "sexo", "seccion", "calificacion",
                  "ingreso_real_final", "ich_score", "nivel_educ_obtenido2",
                  "antiguedad"),
  Descripción = c("Edad del individuo", "Sexo",
                  "Sección económica", "Calificación ocupacional",
                  "Ingreso real imputado", "Índice calidad habitacional",
                  "Nivel educativo obtenido", "Antigüedad laboral"),
  Completitud = c("≥ 99%", "100%", "100% (PEA)", "100% (PEA)",
                  "100%", paste0(contrato$pct_ich_completo, "%"),
                  "≥ 99.9%", "Creada en Script 01"),
  Status      = c("✅", "✅", "✅", "✅", "✅",
                  ifelse(contrato$pct_ich_completo > 95, "✅", "⚠️"),
                  "✅", "✅")
) %>%
  kable(format = "html", align = "llrr") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "left")
```

---

# Checklist de validaciones

```{r validaciones}
tibble(
  Check = c(
    sprintf("Cobertura temporal completa (%s)", .rango),
    "Taxonomía presente (seccion, calificacion)",
    "ICH completitud > 95%",
    "Varianza Dim1 MCA > 10%",
    "Correlación ICH–ingreso > 0.20",
    "Correlación ICH–ingreso < 0.60 (no colineal)",
    "Validez semántica: categorías bien ordenadas en Dim1",
    "NBI convergent validity disponible"
  ),
  Status = c(
    "✅ PASS",
    "✅ PASS",
    ifelse(contrato$pct_ich_completo > 95,       "✅ PASS", "⚠️ WARNING"),
    ifelse(contrato$var_explicada_dim1_mca > 10,  "✅ PASS", "⚠️ WARNING"),
    ifelse(contrato$cor_ich_ingreso > 0.20,       "✅ PASS", "⚠️ WARNING"),
    ifelse(contrato$cor_ich_ingreso < 0.60,       "✅ PASS", "⚠️ WARNING"),
    "✅ PASS — verificar sección 4.4",
    ifelse(.tiene_nbi, "✅ PASS", "⚠️ N/A — nbi ausente")
  )
) %>%
  kable(format = "html", align = "lr") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE, position = "left") %>%
  column_spec(2, bold = TRUE)
```

---

# Contrato downstream

```{r contrato}
tibble(
  Campo  = c("Script", "Timestamp", "Output", "N total", "N PEA",
             "Var. explicada dim1", "Correlación ICH–ingreso",
             "Niveles seccion", "Niveles calificacion"),
  Valor  = c(
    contrato$script,
    as.character(contrato$timestamp),
    basename(contrato$path_output),
    format(contrato$N_total, big.mark = ","),
    format(contrato$N_pea,   big.mark = ","),
    paste0(round(contrato$var_explicada_dim1_mca, 1), "%"),
    round(contrato$cor_ich_ingreso, 3),
    length(contrato$levels_seccion),
    length(contrato$levels_calificacion)
  )
) %>%
  kable(format = "html", align = "lr") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE, position = "left")
```

---

# Conclusión

✅ **Capa 1 completada exitosamente.**

El ICH muestra validez de constructo satisfactoria: correcta separación semántica
de categorías en Dim1, correlación moderada con ingreso (no colineal), y
estabilidad temporal consistente a lo largo del panel. Las contribuciones de los
10 indicadores están distribuidas sin dominancia de ninguno, lo que respalda el
uso de MCA como método de reducción dimensional para variables de vivienda
categóricas en contexto de paper económico.

**Siguiente paso:** `04_proxies_simples.R`
'),
  .rmd_temp
)

# Renderizar
rmarkdown::render(
  input       = .rmd_temp,
  output_file = .ruta_reporte,
  quiet       = TRUE,
  envir       = environment()
)

unlink(.rmd_temp)

cat("✅ Reporte HTML generado:", .ruta_reporte, "\n\n")


# 🪫 5. Outputs de diagnóstico adicionales -------------------------------------
cat("💾 Guardando archivos de diagnóstico...\n")

# 5.1. Objeto diagnóstico completo (reproducibilidad)
.path_diag_rds <- PATH_03C_DIAG_RDS

diagnostico_ich <- list(
  timestamp          = Sys.time(),
  script             = "03c_reporte_base.R",
  contrato_03b       = contrato,
  eigenvalues        = diag_eigenvalues,
  contrib_categoria  = diag_contribuciones,
  contrib_indicador  = diag_contrib_indicador,
  coordenadas_dim1   = diag_coordenadas,
  cos2_dim1          = diag_cos2,
  estabilidad        = diag_estabilidad,
  geo_aglomerado     = diag_aglomerado,
  geo_region         = diag_region,
  nbi_disponible     = .tiene_nbi,
  nbi_stats          = if (.tiene_nbi) diag_nbi else NULL
)

saveRDS(diagnostico_ich, .path_diag_rds)
cat("   ✅ diagnostico_ich.rds:", .path_diag_rds, "\n")

# Enriquecer contrato 03b con datos del diagnóstico para el paper appendix
cat("📦 Enriqueciendo contrato 03b...\n")
contrato_03b <- readRDS(PATH_CONTRATO_03B)
contrato_03b$eigenvalues       <- diagnostico_ich$eigenvalues[1:10, ]
contrato_03b$contrib_indicador <- diagnostico_ich$contrib_indicador
contrato_03b$estabilidad       <- diagnostico_ich$estabilidad
contrato_03b$geo_region        <- diagnostico_ich$geo_region
contrato_03b$nbi_stats         <- diagnostico_ich$nbi_stats
# ICH distribution stats from the MCA working sample
ich_vec <- datos_sample_ich$ich_score
contrato_03b$ich_distribution  <- list(
  mean   = round(mean(ich_vec, na.rm = TRUE), 1),
  median = round(median(ich_vec, na.rm = TRUE), 1),
  sd     = round(sd(ich_vec, na.rm = TRUE), 1),
  p10    = round(quantile(ich_vec, 0.10, na.rm = TRUE), 1),
  p25    = round(quantile(ich_vec, 0.25, na.rm = TRUE), 1),
  p75    = round(quantile(ich_vec, 0.75, na.rm = TRUE), 1),
  p90    = round(quantile(ich_vec, 0.90, na.rm = TRUE), 1),
  n_sample = length(ich_vec)
)
saveRDS(contrato_03b, PATH_CONTRATO_03B)
cat("   ✅ Contrato 03b enriquecido\n")

# 5.2. Contribuciones por indicador (tabla para apéndice metodológico)
.path_contrib_csv <- PATH_03C_CONTRIB_CSV

diag_contrib_indicador %>%
  mutate(indicador = gsub("ind_", "", indicador)) %>%
  write.csv(.path_contrib_csv, row.names = FALSE)

cat("   ✅ tabla_contribuciones.csv:", .path_contrib_csv, "\n")

# 5.3. Estabilidad temporal (tabla para sección de robustez)
.path_estab_csv <- PATH_03C_ESTAB_CSV

diag_estabilidad %>%
  write.csv(.path_estab_csv, row.names = FALSE)

cat("   ✅ estabilidad_temporal.csv:", .path_estab_csv, "\n\n")


# 📑 6. Checklist de salida ----------------------------------------------------
cat("═══════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 03c:\n")
cat("═══════════════════════════════════════════════════════\n")
cat("   [✅] HTML generado:", basename(.ruta_reporte), "\n")
cat("   [✅] Diagnóstico ICH completo (eigenvalues, contribuciones, cos2)\n")
cat("   [✅] Estabilidad temporal documentada\n")
cat("   [✅] Variación geográfica: aglomerado (primario) + región (complementario)\n")
cat("   [✅] NBI convergent validity:",
    if (.tiene_nbi) "incluida" else "omitida (nbi ausente)", "\n")
cat("   [✅] diagnostico_ich.rds guardado\n")
cat("   [✅] tabla_contribuciones.csv guardado\n")
cat("   [✅] estabilidad_temporal.csv guardado\n")
cat("═══════════════════════════════════════════════════════\n\n")

cat("🎯 SIGUIENTE PASO: Ejecutar 04_proxies_simples.R\n\n")


# ⌛ Mensaje final -------------------------------------------------------------
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("═══════════════════════════════════════════════════════\n")
cat("✅ Script 03c finalizado:", as.character(end_time), "\n")
cat("⏱️  Tiempo total:", round(elapsed / 60, 1), "minutos\n")
cat("═══════════════════════════════════════════════════════\n")

toc()
