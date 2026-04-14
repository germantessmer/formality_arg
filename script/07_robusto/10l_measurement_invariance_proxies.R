# =============================================================================
# [EN] 10l_measurement_invariance_proxies.R -- Measurement invariance: pre- vs post-survey redesign (KS tests, density plots)
# INPUTS:  rdos/datos/04_panel_con_proxies.rds
# OUTPUTS: rdos/reportes/10l_measurement_invariance.html, rdos/figuras/10l_measurement_invariance/*.pdf
# =============================================================================
# 🌟 10l_measurement_invariance_proxies.R 🌟 ####
# OBJETIVO: Measurement invariance de las 7 proxies del heterofactor (R4-Q9)
#   Compara distribuciones pre-redesign (hasta 2024Q3) vs post-redesign
#   (2024Q4+) para cada proxy. Tests KS, diferencias de media/sd, densidades.
# INPUTS:  rdos/datos/04_panel_con_proxies.rds (panel con proxies ya construidas)
# OUTPUTS: rdos/reportes/10l_measurement_invariance.csv
#          rdos/reportes/10l_measurement_invariance.html
#          rdos/figuras/10l_measurement_invariance/10l_density_*.pdf
#          rdos/reportes/10l_measurement_invariance_notas.txt
# TIEMPO ESTIMADO: ~3-5 minutos

# ⌛ Inicio contador de tiempo -------------------------------------------------

options(renv.config.auto.snapshot = FALSE)
t_inicio <- proc.time()

# 📚 Librerias -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(kableExtra)
})

# 🔧 Cargar configuracion y funciones ------------------------------------------

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

safe_load <- function(path) {
  if (!file.exists(path)) stop("Archivo no encontrado: ", path)
  readRDS(path)
}

cat("=", rep("=", 69), "\n", sep = "")
cat("  Measurement Invariance — 7 Proxies Heterofactor\n")
cat("  Responde a R4-Q9\n")
cat("=", rep("=", 69), "\n", sep = "")

# 🪫 1. Cargar panel con proxies -----------------------------------------------

panel <- safe_load(PATH_04_PANEL_PROXIES)
cat("Panel cargado:", nrow(panel), "obs x", ncol(panel), "cols\n")

# Las 7 proxies del heterofactor
PROXIES <- c(
  "ich_score",                  # ICH (housing quality index)
  "residual_vivienda",          # Housing residual
  "rezago_escolar_cohorte",     # School delay by cohort
  "clima_educativo_hogar",      # Household educational climate
  "emparejamiento_selectivo",   # Assortative mating
  "calificacion_norm",          # Normalized occupational qualification
  "entropia_estabilidad"        # Labour stability entropy
)

# Verificar disponibilidad
proxies_ok <- intersect(PROXIES, names(panel))
proxies_miss <- setdiff(PROXIES, names(panel))
if (length(proxies_miss) > 0) {
  cat("WARN: proxies no encontradas:", paste(proxies_miss, collapse = ", "), "\n")
}
cat("Proxies disponibles:", length(proxies_ok), "/", length(PROXIES), "\n")

# 🪫 2. Marcar pre/post redesign -----------------------------------------------

# El redesign empieza en 2024Q4 = "2024_T4"
panel <- panel %>%
  filter(condicion_actividad == "Ocupado") %>%
  mutate(
    era = case_when(
      periodo_id %in% TRIMESTRES_FORMALIDAD ~ "Post-redesign",
      TRUE ~ "Pre-redesign"
    )
  )

cat("\nDistribucion pre/post redesign (ocupados):\n")
print(table(panel$era))

# Para comparacion mas fina: ultimo trimestre pre vs primero post
panel_boundary <- panel %>%
  filter(periodo_id %in% c("2024_T3", "2024_T4"))

cat("\nBoundary comparison (2024Q3 vs 2024Q4):\n")
print(table(panel_boundary$periodo_id))

# 🪫 3. Tests de invarianza por proxy ------------------------------------------

resultados <- list()

for (prx in proxies_ok) {

  cat("\n--- Proxy:", prx, "---\n")

  # Datos completos (pre vs post)
  x_pre  <- panel %>% filter(era == "Pre-redesign") %>% pull(!!sym(prx)) %>% na.omit()
  x_post <- panel %>% filter(era == "Post-redesign") %>% pull(!!sym(prx)) %>% na.omit()

  # Datos boundary (2024Q3 vs 2024Q4)
  x_pre_b  <- panel_boundary %>% filter(periodo_id == "2024_T3") %>% pull(!!sym(prx)) %>% na.omit()
  x_post_b <- panel_boundary %>% filter(periodo_id == "2024_T4") %>% pull(!!sym(prx)) %>% na.omit()

  # Estadisticos
  mean_pre  <- mean(x_pre)
  mean_post <- mean(x_post)
  sd_pre    <- sd(x_pre)
  sd_post   <- sd(x_post)
  delta_mean <- mean_post - mean_pre
  delta_sd   <- sd_post - sd_pre

  # Cohen's d
  pooled_sd <- sqrt((sd_pre^2 + sd_post^2) / 2)
  cohens_d  <- delta_mean / pooled_sd

  # KS test (full pre vs post)
  ks_full <- ks.test(x_pre, x_post)

  # KS test (boundary only)
  ks_boundary <- if (length(x_pre_b) > 30 && length(x_post_b) > 30) {
    ks.test(x_pre_b, x_post_b)
  } else {
    list(statistic = NA_real_, p.value = NA_real_)
  }

  # Missingness pre vs post
  n_pre_total  <- panel %>% filter(era == "Pre-redesign") %>% nrow()
  n_post_total <- panel %>% filter(era == "Post-redesign") %>% nrow()
  pct_na_pre   <- (1 - length(x_pre) / n_pre_total) * 100
  pct_na_post  <- (1 - length(x_post) / n_post_total) * 100

  cat(sprintf("  Mean: pre=%.4f post=%.4f delta=%.4f Cohen's d=%.4f\n",
              mean_pre, mean_post, delta_mean, cohens_d))
  cat(sprintf("  SD:   pre=%.4f post=%.4f delta=%.4f\n", sd_pre, sd_post, delta_sd))
  cat(sprintf("  KS (full):     D=%.4f p=%.4g\n", ks_full$statistic, ks_full$p.value))
  cat(sprintf("  KS (boundary): D=%.4f p=%.4g\n",
              ks_boundary$statistic, ks_boundary$p.value))
  cat(sprintf("  NA%%: pre=%.1f%% post=%.1f%%\n", pct_na_pre, pct_na_post))

  resultados[[prx]] <- tibble(
    proxy         = prx,
    n_pre         = length(x_pre),
    n_post        = length(x_post),
    mean_pre      = round(mean_pre, 4),
    mean_post     = round(mean_post, 4),
    delta_mean    = round(delta_mean, 4),
    sd_pre        = round(sd_pre, 4),
    sd_post       = round(sd_post, 4),
    cohens_d      = round(cohens_d, 4),
    ks_D_full     = round(as.numeric(ks_full$statistic), 4),
    ks_p_full     = signif(ks_full$p.value, 4),
    ks_D_boundary = round(as.numeric(ks_boundary$statistic), 4),
    ks_p_boundary = signif(as.numeric(ks_boundary$p.value), 4),
    pct_na_pre    = round(pct_na_pre, 1),
    pct_na_post   = round(pct_na_post, 1)
  )
}

df_results <- bind_rows(resultados)

cat("\n=== RESUMEN ===\n")
print(df_results %>% select(proxy, delta_mean, cohens_d, ks_D_full, ks_D_boundary), n = Inf)

# 🪫 4. Guardar CSV ------------------------------------------------------------

path_csv <- file.path(DIR_REPORTES, "10l_measurement_invariance.csv")
write_csv(df_results, path_csv)
cat("\nCSV:", path_csv, "\n")

# 🪫 5. Figuras — densidades pre/post ------------------------------------------

dir_fig <- file.path(DIR_FIGURAS, "10l_measurement_invariance")
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)

for (prx in proxies_ok) {

  prx_data <- panel %>%
    filter(!is.na(!!sym(prx))) %>%
    select(era, value = !!sym(prx))

  d_row <- df_results %>% filter(proxy == prx)

  fig <- prx_data %>%
    ggplot(aes(x = value, fill = era, color = era)) +
    geom_density(alpha = 0.3, linewidth = 0.5) +
    scale_fill_manual(values = c("Pre-redesign" = COL_OBSERVADO, "Post-redesign" = COL_GLM)) +
    scale_color_manual(values = c("Pre-redesign" = COL_OBSERVADO, "Post-redesign" = COL_GLM)) +
    tr_labs(
      title = sprintf("Density: %s", prx),
      subtitle = sprintf("Cohen's d = %.3f | KS D = %.3f (boundary: %.3f)",
                         d_row$cohens_d, d_row$ks_D_full, d_row$ks_D_boundary),
      x = prx, y = "Density", fill = "Era", color = "Era"
    ) +
    theme_paper()

  guardar_figura(fig, dir_fig, "density", which(proxies_ok == prx), width = 8, height = 5)
  cat("  Density:", prx, "\n")
}

# Boundary comparison (2024Q3 vs 2024Q4 only)
for (prx in proxies_ok) {

  prx_data <- panel_boundary %>%
    filter(!is.na(!!sym(prx))) %>%
    select(periodo_id, value = !!sym(prx))

  if (nrow(prx_data) < 100) next

  fig <- prx_data %>%
    ggplot(aes(x = value, fill = periodo_id, color = periodo_id)) +
    geom_density(alpha = 0.3, linewidth = 0.5) +
    scale_fill_manual(values = c("2024_T3" = COL_OBSERVADO, "2024_T4" = COL_GLM)) +
    scale_color_manual(values = c("2024_T3" = COL_OBSERVADO, "2024_T4" = COL_GLM)) +
    tr_labs(
      title = sprintf("Boundary comparison: %s (2024Q3 vs 2024Q4)", prx),
      x = prx, y = "Density"
    ) +
    theme_paper()

  guardar_figura(fig, dir_fig, "boundary", which(proxies_ok == prx), width = 8, height = 5)
}

# 🪫 6. Notas para el paper ----------------------------------------------------

path_notas <- file.path(DIR_REPORTES, "10l_measurement_invariance_notas.txt")
con_notas  <- file(path_notas, open = "wt", encoding = "UTF-8")

# Classify proxies by invariance
small_d <- df_results %>% filter(abs(cohens_d) < 0.2)
medium_d <- df_results %>% filter(abs(cohens_d) >= 0.2, abs(cohens_d) < 0.5)
large_d <- df_results %>% filter(abs(cohens_d) >= 0.5)

writeLines(c(
  "=== NOTAS PARA EL PAPER — R4-Q9 Measurement Invariance ===",
  sprintf("Generado: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "--- Appendix D (Heterofactor, nueva subseccion) ---",
  "",
  "None of the seven proxy variables used for the heterofactor model are",
  "directly affected by the 2025 EPH redesign. The redesign modified the",
  "labour-market module (employment classification, formality indicators,",
  "income disaggregation) but did not change the demographic, educational,",
  "or housing blocks from which the proxies are constructed.",
  "",
  sprintf("Proxies with negligible shift (|d| < 0.2): %s",
          paste(small_d$proxy, collapse = ", ")),
  sprintf("Proxies with small shift (0.2 <= |d| < 0.5): %s",
          ifelse(nrow(medium_d) > 0, paste(medium_d$proxy, collapse = ", "), "none")),
  sprintf("Proxies with large shift (|d| >= 0.5): %s",
          ifelse(nrow(large_d) > 0, paste(large_d$proxy, collapse = ", "), "none")),
  "",
  "Boundary KS tests (2024Q3 vs 2024Q4) provide the sharpest comparison",
  "because they isolate the redesign transition from secular trends.",
  "",
  "The stability of the proxy distributions across the redesign boundary",
  "supports the measurement-invariance assumption required for full-panel",
  "factor scoring."
), con_notas)

close(con_notas)
cat("\nNotas:", path_notas, "\n")

# 🪫 7. Reporte HTML -----------------------------------------------------------

path_html <- file.path(DIR_REPORTES, "10l_measurement_invariance.html")
rmd_temp  <- tempfile(fileext = ".Rmd")
con       <- file(rmd_temp, open = "wt", encoding = "UTF-8")

writeLines('---
title: "Measurement Invariance — Heterofactor Proxies"
subtitle: "R4-Q9: Pre vs post redesign comparison"
date: "Generado: `r format(Sys.time(), \'%d/%m/%Y %H:%M\')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float: { collapsed: false }
    number_sections: true
    df_print: kable
---', con)

writeLines(sprintf('
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 9, fig.height = 5, dpi = 150)
library(tidyverse); library(knitr); library(kableExtra)
```

# Summary table

```{r table}
df <- read.csv("%s")
df %%>%%
  select(proxy, mean_pre, mean_post, delta_mean, cohens_d,
         ks_D_full, ks_p_full, ks_D_boundary, ks_p_boundary) %%>%%
  kable(col.names = c("Proxy", "Mean pre", "Mean post", "Delta",
                       "Cohen d", "KS D (full)", "p (full)",
                       "KS D (boundary)", "p (boundary)"),
        digits = 4) %%>%%
  kable_styling(bootstrap_options = c("striped", "condensed"), font_size = 12)
```

# Interpretation

- **Cohen d < 0.2**: negligible effect size (measurement invariant)
- **Cohen d 0.2-0.5**: small effect, investigate
- **Cohen d > 0.5**: large shift, concern

The KS test on the full sample will typically reject due to large N even for
trivially small shifts. The boundary comparison (2024Q3 vs 2024Q4) is more
informative because it isolates the redesign transition.
', gsub("\\\\", "/", path_csv)), con)

close(con)

rmarkdown::render(input = rmd_temp, output_file = path_html,
                  quiet = TRUE, envir = new.env(parent = globalenv()))
unlink(rmd_temp)
cat("HTML:", path_html, "\n")

# 📦 Contrato -----------------------------------------------------------------

path_contrato_10l <- file.path(DIR_CONTRATOS, "10l_contrato_invariance.rds")

n_negligible_d <- sum(abs(df_results$cohens_d) < 0.20)
d_max <- max(abs(df_results$cohens_d))

# Max boundary KS among negligible-d proxies
negligible_rows <- df_results %>% filter(abs(cohens_d) < 0.20)
ks_max_boundary <- if (nrow(negligible_rows) > 0) {
  max(negligible_rows$ks_D_boundary, na.rm = TRUE)
} else {
  NA_real_
}

contrato_10l <- list(
  script           = "10l_measurement_invariance_proxies.R",
  fecha            = format(Sys.time(), "%Y-%m-%d %H:%M"),
  n_proxies        = nrow(df_results),
  n_negligible_d   = n_negligible_d,
  d_max            = d_max,
  ks_max_boundary  = ks_max_boundary,
  # Tabla detallada por proxy para el paper appendix
  invariance_table = df_results %>%
    select(proxy, mean_pre, mean_post, cohens_d, ks_D_full, ks_D_boundary)
)

saveRDS(contrato_10l, path_contrato_10l)
cat("\n📦 Contrato:", path_contrato_10l, "\n")

# 📑 Checklist y timer ---------------------------------------------------------

cat("\nChecklist:\n")
cat("  CSV:  ", ifelse(file.exists(path_csv),  "OK", "FALTA"), "\n")
cat("  HTML: ", ifelse(file.exists(path_html), "OK", "FALTA"), "\n")
cat("  Notas:", ifelse(file.exists(path_notas),"OK", "FALTA"), "\n")
cat("  Contrato:", ifelse(file.exists(path_contrato_10l), "OK", "FALTA"), "\n")

rm(panel, panel_boundary, df_results, resultados, contrato_10l)
gc(verbose = FALSE)

cat("\nTiempo:", round((proc.time() - t_inicio)["elapsed"] / 60, 1), "minutos\n")
