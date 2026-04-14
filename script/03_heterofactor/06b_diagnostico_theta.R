# =============================================================================
# [EN] 06b_diagnostico_theta.R -- Diagnose factor scores: predictive power on formality (Cohen d, R-squared, decile gradients)
# INPUTS:  rdos/datos/06_theta_predichos.rds, rdos/modelos/06_modelo_heterofactor.rds
# OUTPUTS: rdos/contratos/06b_contrato_diagnostico_theta.rds
# =============================================================================
# 🌟 06b_diagnostico_theta.R 🌟 ####
# OBJETIVO:
#    Evaluar si θ_A y θ_B tienen poder predictivo real sobre formalidad laboral
#    antes de invertir tiempo en scripts downstream (06c, 06d, LASSO, backcasting).
#    Lee el contrato de 06a al inicio para reconstruir el contexto de la corrida.
#
# INPUTS:
#    - PATH_06_THETA          → rdos/datos/06_theta_predichos.rds    (Script 06a)
#    - PATH_06_MODELO_HETERO  → rdos/modelos/06_modelo_heterofactor.rds (Script 06a)
#    - PATH_CONTRATO_06       → rdos/contratos/06_contrato_heterofactor.rds (Script 06a)
#
# OUTPUTS:
#    - PATH_CONTRATO_06B      → rdos/contratos/06b_contrato_diagnostico_theta.rds
#
# TESTS:
#    1. Distribución de θ por condición de formalidad (d Cohen + t-test Welch)
#    2. R² de regresión formalidad ~ θ (LPM con controles demográficos)
#    3. θ por decil de ingreso — validación externa sobre PEA
#    4. Correlación θ con variables observables teóricamente relacionadas
#
# TIEMPO ESTIMADO: ~2 minutos


# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(tictoc)
  library(tidyverse)
})


# 🔧 Cargar configuración y funciones ------------------------------------------

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))


# ⌛ Inicio contador de tiempo -------------------------------------------------

tic("Script 06b")
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  🌟 06b_diagnostico_theta.R — DIAGNÓSTICO θ vs FORMALIDAD\n")
cat(sprintf("  Iniciado: %s\n", format(start_time, "%Y-%m-%d %H:%M:%S")))
cat("═══════════════════════════════════════════════════════════════════\n\n")


# 🪫 1. Verificación de inputs -------------------------------------------------

cat("── 1. Verificación de inputs ───────────────────────────────────────\n")

hard_stop(file.exists(PATH_CONTRATO_06),
          paste0("No existe contrato 06a en: ", PATH_CONTRATO_06))
hard_stop(file.exists(PATH_06_MODELO_HETERO),
          paste0("No existe modelo heterofactor en: ", PATH_06_MODELO_HETERO))
hard_stop(file.exists(PATH_06_THETA),
          paste0("No existe panel con theta en: ", PATH_06_THETA))

cat("   [✅] Los 3 inputs existen en disco\n\n")


# 🪫 2. Carga de datos y reconstrucción de contexto de 06a ---------------------
#
# El contrato de 06a es la única fuente para reconstruir el contexto de la
# corrida (parámetros, convergencia, LogLik). Se lee defensivamente porque
# la estructura interna puede variar entre versiones del script 06a.

cat("── 2. Contexto de 06a ──────────────────────────────────────────────\n\n")

# ── Contrato de 06a ───────────────────────────────────────────────────────────
cat("📋 Leyendo contrato_06...\n")
contrato_06 <- readRDS(PATH_CONTRATO_06)

cat("   Campos disponibles: ",
    paste(names(contrato_06), collapse = ", "), "\n\n")

cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│  CONTEXTO 06a — heterofactor_estimacion                        │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n")

# Acceso defensivo: la ubicación exacta de campos varía según versión de 06a
if (!is.null(contrato_06$nombre_final)) {
  cat(sprintf("   Modelo seleccionado:  %s\n", contrato_06$nombre_final))
} else {
  cat("   Modelo seleccionado:  [ver modelo_heterofactor.rds]\n")
}
if (!is.null(contrato_06$loglik %||% contrato_06$logLik)) {
  cat(sprintf("   LogLik modelo final:  %.2f\n",
              contrato_06$loglik %||% contrato_06$logLik))
}
if (!is.null(contrato_06$convergencia)) {
  cat(sprintf("   Convergencia:         %s\n",
              if (contrato_06$convergencia == 0) "0 ✅" else
              paste0(contrato_06$convergencia, " ⚠️")))
}
if (!is.null(contrato_06$n_core)) {
  cat(sprintf("   N base_core (MLE):    %s\n",
              format(contrato_06$n_core, big.mark = ",")))
}
if (!is.null(contrato_06$cobertura_theta)) {
  cat(sprintf("   Cobertura θ (panel):  %.1f%%\n",
              contrato_06$cobertura_theta * 100))
}
if (!is.null(contrato_06$cor_theta_AB)) {
  cat(sprintf("   Cor(θ_A, θ_B):        %.4f\n", contrato_06$cor_theta_AB))
}
cat("\n")

# ── Modelo heterofactor ───────────────────────────────────────────────────────
cat("📂 Cargando modelo heterofactor...\n")
modelo_hetero <- readRDS(PATH_06_MODELO_HETERO)

nombre_final <- modelo_hetero$nombre_final
if (is.null(nombre_final)) {
  soft_warn("nombre_final no encontrado en modelo_heterofactor — usando 'desconocido'")
  nombre_final <- "desconocido"
}
cat(sprintf("   [✅] Modelo final: %s\n", nombre_final))

modelo_sel <- modelo_hetero$modelo_final
if (!is.null(modelo_sel$params)) {
  cat(sprintf("   Parámetros disponibles: %s\n",
              paste(names(modelo_sel$params), collapse = ", ")))
}
cat("\n")

# ── Panel con theta ───────────────────────────────────────────────────────────
cat("📂 Cargando panel con θ...\n")
panel <- readRDS(PATH_06_THETA)
cat(sprintf("   [✅] %s obs × %s vars\n\n",
            format(nrow(panel), big.mark = ","), ncol(panel)))


# 🪫 3. Preparación de datos ---------------------------------------------------
#
# Cobertura de θ se reporta sobre el panel total Y sobre la PEA por separado.
# El filtro base_core (n_cog≥3 AND n_socio≥2) excluye por diseño a inactivos
# sin historial laboral y menores → la cobertura relevante para el pipeline
# (LASSO y backcasting sobre PEA) es la cobertura sobre la PEA.

cat("── 3. Preparando datos ─────────────────────────────────────────────\n")

dat <- panel %>%
  filter(!is.na(theta_A), !is.na(theta_B)) %>%
  mutate(
    # Codificación binaria de formalidad — solo disponible en TRIMESTRES_FORMALIDAD
    formal = case_when(
      formalidad_empleo == "Formal oficial"   ~ 1L,
      formalidad_empleo == "Informal oficial" ~ 0L,
      TRUE                                    ~ NA_integer_
    ),
    ocupado     = condicion_actividad == "Ocupado",
    ingreso_num = as.numeric(ingreso_real_final)
  )

# Cobertura sobre panel total y PEA
n_panel          <- nrow(panel)
n_pea            <- sum(panel$pea_flag == TRUE, na.rm = TRUE)
n_con_theta      <- nrow(dat)
n_pea_con_theta  <- sum(!is.na(panel$theta_A) & panel$pea_flag == TRUE, na.rm = TRUE)
n_con_formalidad <- sum(!is.na(dat$formal))

cat(sprintf("   Panel total:               %s obs\n",
            format(n_panel, big.mark = ",")))
cat(sprintf("   PEA:                       %s obs (%.1f%% del panel)\n",
            format(n_pea, big.mark = ","),
            n_pea / n_panel * 100))
cat(sprintf("   θ disponible (total):      %s obs (%.1f%% del panel)\n",
            format(n_con_theta, big.mark = ","),
            n_con_theta / n_panel * 100))
cat(sprintf("   θ disponible (PEA):        %s obs (%.1f%% de la PEA)\n",
            format(n_pea_con_theta, big.mark = ","),
            n_pea_con_theta / n_pea * 100))
cat(sprintf("   Con formalidad observada:  %s obs\n\n",
            format(n_con_formalidad, big.mark = ",")))

# Advertencia si no hay obs con formalidad — no debería ocurrir salvo error en input
if (n_con_formalidad == 0) {
  soft_warn("Sin obs con formalidad_empleo observada. Tests 1 y 2 serán omitidos.")
}


# 🪫 4. Test 1: Distribución de θ por condición de formalidad ------------------
#
# Pregunta: ¿los trabajadores formales e informales tienen distribuciones de
# θ estadísticamente distintas? Se reporta d Cohen como medida de tamaño del
# efecto para comparabilidad entre θ_A y θ_B y con literatura relacionada.
#
# Muestra: ocupados con formalidad observada (últimos 4 trimestres del panel)
# Umbral: |d| > 0.3 efecto medio-grande | |d| > 0.1 pequeño | resto mínimo

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  TEST 1: θ por condición de formalidad (ocupados)\n")
cat("═══════════════════════════════════════════════════════════════════\n")

dat_ocup <- dat %>% filter(ocupado, !is.na(formal))

# Inicializar para resumen ejecutivo y contrato
d_A <- NA_real_
d_B <- NA_real_

if (nrow(dat_ocup) == 0) {
  soft_warn("Sin ocupados con formalidad observada — TEST 1 omitido.")
} else {

  for (theta_var in c("theta_A", "theta_B")) {

    cat(sprintf("\n── %s ──────────────────────────────────────────────────────\n",
                theta_var))

    stats_por_formal <- dat_ocup %>%
      group_by(formal) %>%
      summarise(
        n     = n(),
        media = mean(.data[[theta_var]], na.rm = TRUE),
        sd    = sd(.data[[theta_var]],   na.rm = TRUE),
        p25   = quantile(.data[[theta_var]], 0.25, na.rm = TRUE),
        p75   = quantile(.data[[theta_var]], 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(label = if_else(formal == 1L, "Formal  ", "Informal"))

    for (i in seq_len(nrow(stats_por_formal))) {
      cat(sprintf("   %-10s  n=%s  media=%+.4f  sd=%.4f  [%.4f, %.4f]\n",
                  stats_por_formal$label[i],
                  format(stats_por_formal$n[i], big.mark = ","),
                  stats_por_formal$media[i],
                  stats_por_formal$sd[i],
                  stats_por_formal$p25[i],
                  stats_por_formal$p75[i]))
    }

    # t-test de Welch (no asume igualdad de varianzas entre grupos)
    vec_formal   <- dat_ocup[[theta_var]][dat_ocup$formal == 1L]
    vec_informal <- dat_ocup[[theta_var]][dat_ocup$formal == 0L]

    tt <- t.test(vec_formal, vec_informal, var.equal = FALSE)

    d_cohen <- diff(tt$estimate) / sqrt(mean(c(
      var(vec_formal,   na.rm = TRUE),
      var(vec_informal, na.rm = TRUE)
    )))

    sig <- if (tt$p.value < 0.001) "***" else
           if (tt$p.value < 0.01)  "**"  else
           if (tt$p.value < 0.05)  "*"   else "ns"

    cat(sprintf("   Δ(formal-informal) = %+.4f | t=%.1f | p=%s %s | d=%.3f\n",
                diff(tt$estimate), tt$statistic,
                format(tt$p.value, digits = 3), sig, d_cohen))
    cat(sprintf("   %s\n",
                if (abs(d_cohen) > 0.3) "✅ Efecto medio-grande" else
                if (abs(d_cohen) > 0.1) "⚠️  Efecto pequeño"     else
                "❌ Efecto mínimo"))

    if (theta_var == "theta_A") d_A <- d_cohen
    if (theta_var == "theta_B") d_B <- d_cohen
  }
}


# 🪫 5. Test 2: R² de formalidad ~ θ (LPM) ------------------------------------
#
# Pregunta: ¿cuánta varianza adicional de formalidad explican θ_A y θ_B sobre
# y por encima de los controles demográficos estándar?
#
# Especificación base: formal ~ sexo + edad + edad² + región
# Se estiman 4 modelos: base (m0), +θ_A, +θ_B, +θ_A+θ_B
# Umbral ΔR²: > 0.02 sustancial | > 0.005 marginal | resto no discrimina

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  TEST 2: R² de formalidad ~ θ (LPM)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

r2_base <- NA_real_; r2_A  <- NA_real_
r2_B    <- NA_real_; r2_AB <- NA_real_
coef_tA <- NA_real_; coef_tB <- NA_real_

if (nrow(dat_ocup) == 0) {
  soft_warn("Sin ocupados con formalidad observada — TEST 2 omitido.")
} else {

  dat_lpm <- dat_ocup %>%
    mutate(
      sexo_num   = if_else(as.character(sexo) == "Mujeres", 1L, 0L),
      edad_num   = as.numeric(edad),
      region_chr = as.character(region)
    ) %>%
    filter(!is.na(sexo_num), !is.na(edad_num))

  # Modelo nulo: solo controles demográficos
  m0 <- lm(formal ~ sexo_num + edad_num + I(edad_num^2) + region_chr,
            data = dat_lpm)
  r2_base <- summary(m0)$r.squared

  # Modelos incrementales con θ
  mA  <- lm(formal ~ theta_A + sexo_num + edad_num + I(edad_num^2) + region_chr,
             data = dat_lpm)
  mB  <- lm(formal ~ theta_B + sexo_num + edad_num + I(edad_num^2) + region_chr,
             data = dat_lpm)
  mAB <- lm(formal ~ theta_A + theta_B + sexo_num + edad_num + I(edad_num^2) + region_chr,
             data = dat_lpm)

  r2_A  <- summary(mA)$r.squared
  r2_B  <- summary(mB)$r.squared
  r2_AB <- summary(mAB)$r.squared

  cat(sprintf("   R² base (sin θ):        %.4f\n",              r2_base))
  cat(sprintf("   R² + θ_A:               %.4f  (Δ = +%.4f)\n", r2_A,  r2_A  - r2_base))
  cat(sprintf("   R² + θ_B:               %.4f  (Δ = +%.4f)\n", r2_B,  r2_B  - r2_base))
  cat(sprintf("   R² + θ_A + θ_B:         %.4f  (Δ = +%.4f)\n", r2_AB, r2_AB - r2_base))

  # Coeficientes del modelo conjunto (θ_A + θ_B)
  coef_mAB <- coef(summary(mAB))
  for (v in c("theta_A", "theta_B")) {
    if (v %in% rownames(coef_mAB)) {
      b   <- coef_mAB[v, "Estimate"]
      se  <- coef_mAB[v, "Std. Error"]
      pv  <- coef_mAB[v, "Pr(>|t|)"]
      sig <- if (pv < 0.001) "***" else
             if (pv < 0.01)  "**"  else
             if (pv < 0.05)  "*"   else "ns"
      cat(sprintf("   coef(%s) = %+.4f  (SE=%.4f, p%s)\n", v, b, se, sig))
      if (v == "theta_A") coef_tA <- b
      if (v == "theta_B") coef_tB <- b
    }
  }

  delta_r2 <- r2_AB - r2_base
  cat(sprintf("\n   %s\n",
              if (delta_r2 > 0.02)  "✅ θ aporta información sustancial" else
              if (delta_r2 > 0.005) "⚠️  θ aporta información marginal"  else
              "❌ θ no discrimina formalidad"))
}


# 🪫 6. Test 3: θ por decil de ingreso (validación externa) --------------------
#
# Pregunta: ¿el factor cognitivo (θ_A) presenta el gradiente esperado con el
# ingreso? Se usa la PEA completa (no solo ocupados) porque el backcasting se
# aplica sobre toda la PEA — la relación θ-ingreso debe ser informativa más
# allá de los ocupados actuales.
#
# Umbrales: |r| > 0.95 demasiado alto (endogeneidad) | > 0.70 alto | resto ok
# Nota: umbrales más exigentes que LEGACY_06b para detectar problemas
# anticipadamente, dado que 06c usa esta información para comparar modelos.

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  TEST 3: θ por decil de ingreso (validación externa — PEA)\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  (θ_A cognitivo debería crecer con el ingreso, pero no demasiado)\n\n")

cor_A_ing <- NA_real_
cor_B_ing <- NA_real_

# PEA completa con ingreso positivo: incluye desocupados con historial de ingreso
dat_ing <- dat %>%
  filter(!is.na(ingreso_num), ingreso_num > 0) %>%
  mutate(decil = ntile(ingreso_num, 10))

if (nrow(dat_ing) < 100) {
  soft_warn("Menos de 100 obs con ingreso > 0 — TEST 3 omitido.")
} else {

  decil_stats <- dat_ing %>%
    group_by(decil) %>%
    summarise(
      theta_A_media = mean(theta_A, na.rm = TRUE),
      theta_B_media = mean(theta_B, na.rm = TRUE),
      n             = n(),
      .groups       = "drop"
    )

  cat(sprintf("   %5s  %10s  %10s\n", "Decil", "θ_A_media", "θ_B_media"))
  for (i in seq_len(nrow(decil_stats))) {
    cat(sprintf("     %2d    %+.4f     %+.4f\n",
                decil_stats$decil[i],
                decil_stats$theta_A_media[i],
                decil_stats$theta_B_media[i]))
  }

  cor_A_ing <- cor(decil_stats$decil, decil_stats$theta_A_media)
  cor_B_ing <- cor(decil_stats$decil, decil_stats$theta_B_media)

  # Umbrales extendidos: detectar endogeneidad potencial relevante para 06c/06d
  flag_A <- if (abs(cor_A_ing) > 0.95) "❌ DEMASIADO ALTO (posible endogeneidad)" else
            if (abs(cor_A_ing) > 0.70) "⚠️  Alto"                                  else
            "✅ Razonable"
  flag_B <- if (abs(cor_B_ing) > 0.95) "❌ DEMASIADO ALTO" else
            if (abs(cor_B_ing) > 0.70) "⚠️  Alto"           else
            "✅ Razonable"

  cat(sprintf("\n   Cor(decil, θ_A) = %+.4f  %s\n", cor_A_ing, flag_A))
  cat(sprintf("   Cor(decil, θ_B) = %+.4f  %s\n",   cor_B_ing, flag_B))
}


# 🪫 7. Test 4: Correlaciones θ con variables observables ----------------------
#
# Pregunta: ¿cada factor correlaciona con los observables teóricamente
# asociados en la dirección esperada? Valida que la identificación del modelo
# produjo factores con la interpretación económica correcta.
#
# Referencias teóricas con signo esperado:
#   θ_A (cognitivo):      ICH score (+), clima educativo hogar (+)
#   θ_B (socioemocional): entropía estabilidad (+), residual vivienda (+)
# Umbral magnitud: |r| > 0.2 ok | > 0.05 marginal | resto no discrimina

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  TEST 4: Correlaciones θ con variables observables\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  (referencias teóricas esperadas)\n\n")

vars_obs <- list(
  list(var = "ich_score",             label = "ICH score",             theta = "A", signo = "+"),
  list(var = "clima_educativo_hogar", label = "Clima educ. hogar",     theta = "A", signo = "+"),
  list(var = "entropia_estabilidad",  label = "Entropía estabilidad",  theta = "B", signo = "+"),
  list(var = "residual_vivienda",     label = "Residual vivienda",     theta = "B", signo = "+")
)

cors_observables <- list()

for (v in vars_obs) {

  if (!v$var %in% names(dat)) {
    cat(sprintf("   %-32s — variable no encontrada en el panel\n", v$label))
    next
  }

  x         <- as.numeric(dat[[v$var]])
  theta_vec <- dat[[paste0("theta_", v$theta)]]
  idx       <- !is.na(x) & !is.na(theta_vec)

  if (sum(idx) < 100) {
    cat(sprintf("   %-32s — menos de 100 obs válidas, omitido\n", v$label))
    next
  }

  r           <- cor(x[idx], theta_vec[idx])
  esperado_ok <- (v$signo == "+" && r > 0) || (v$signo == "-" && r < 0)
  icon_mag    <- if (abs(r) > 0.2) "✅" else if (abs(r) > 0.05) "⚠️ " else "❌"
  icon_signo  <- if (esperado_ok) "✅signo" else "❌signo"

  cat(sprintf("   %-32s ~ θ_%s: r=%+.4f %s %s\n",
              v$label, v$theta, r, icon_mag, icon_signo))

  cors_observables[[v$var]] <- r
}


# 🪫 8. Resumen ejecutivo ------------------------------------------------------
#
# Veredicto basado en las 3 métricas clave del pipeline downstream:
#   ΔR² > 0.02 AND |d_A| > 0.2 AND |cor_A_ing| > 0.5 → ÚTIL
#   ΔR² > 0.005 OR |d_A| > 0.1                        → MARGINAL
#   resto                                               → NO DISCRIMINA

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  📋 RESUMEN EJECUTIVO\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

delta_r2 <- if (!is.na(r2_AB) && !is.na(r2_base)) r2_AB - r2_base else NA_real_

if (!is.na(delta_r2) && !is.na(d_A) && !is.na(cor_A_ing)) {

  veredicto <- if (delta_r2 > 0.02 && abs(d_A) > 0.2 && abs(cor_A_ing) > 0.5) {
    "✅ θ ÚTIL — proceder con scripts downstream"
  } else if (delta_r2 > 0.005 || abs(d_A) > 0.1) {
    "⚠️  θ MARGINAL — evaluar antes de continuar"
  } else {
    "❌ θ NO DISCRIMINA — replantear proxies antes de continuar"
  }

  cat(sprintf("   %s\n\n", veredicto))
  cat(sprintf("   Δ R² (formalidad ~ θ_AB):    +%.4f\n", delta_r2))
  cat(sprintf("   d Cohen (θ_A, formalidad):    %.3f\n",  abs(d_A)))
  cat(sprintf("   Cor(decil_ing, θ_A):           %+.4f\n", cor_A_ing))

} else {
  veredicto <- "⚠️  INCOMPLETO — algún test fue omitido (ver warnings)"
  cat(sprintf("   %s\n", veredicto))
  if (!is.na(delta_r2))  cat(sprintf("   Δ R² (formalidad ~ θ_AB):    +%.4f\n", delta_r2))
  if (!is.na(d_A))       cat(sprintf("   d Cohen (θ_A, formalidad):    %.3f\n",  abs(d_A)))
  if (!is.na(cor_A_ing)) cat(sprintf("   Cor(decil_ing, θ_A):           %+.4f\n", cor_A_ing))
}

cat(sprintf("\n   Modelo de referencia: %s\n", nombre_final))
cat("═══════════════════════════════════════════════════════════════════\n\n")


# 🪫 9. Guardado — Contrato 06b ------------------------------------------------
#
# El contrato registra todas las métricas de los 4 tests para consumo de
# scripts downstream (06c, 06d) y para el reporte HTML final de la sección.
# Se registran dos métricas de cobertura:
#   pct_cobertura_total: referencia poblacional (incluye inactivos/menores)
#   pct_cobertura_pea:   relevante para el pipeline (backcasting sobre PEA)

cat("── 💾 Guardando contrato 06b ────────────────────────────────────────\n")

contrato_06b <- list(
  # Meta
  timestamp            = Sys.time(),
  nombre_modelo        = nombre_final,
  # Cobertura
  n_panel              = n_panel,
  n_pea                = n_pea,
  n_con_theta          = n_con_theta,
  n_pea_con_theta      = n_pea_con_theta,
  pct_cobertura_total  = n_con_theta / n_panel,   # incluye inactivos/menores
  pct_cobertura_pea    = n_pea_con_theta / n_pea, # relevante para pipeline
  n_con_formalidad     = n_con_formalidad,
  # Test 1 — d Cohen por formalidad
  d_cohen_A            = d_A,
  d_cohen_B            = d_B,
  # Test 2 — R² LPM
  r2_base              = r2_base,
  r2_tA                = r2_A,
  r2_tB                = r2_B,
  r2_tAB               = r2_AB,
  delta_r2             = delta_r2,
  coef_tA              = coef_tA,
  coef_tB              = coef_tB,
  # Test 3 — Cor decil de ingreso
  cor_decil_tA         = cor_A_ing,
  cor_decil_tB         = cor_B_ing,
  # Test 4 — Correlaciones con observables
  cors_observables     = cors_observables,
  # Resumen
  veredicto            = veredicto,
  # Referencia a 06a (defensivo: campos pueden variar entre versiones)
  ref_06_loglik        = contrato_06$loglik       %||% contrato_06$logLik    %||% NA_real_,
  ref_06_convergencia  = contrato_06$convergencia %||% NA,
  ref_06_n_core        = contrato_06$n_core       %||% NA_real_,
  ref_06_modelo_B      = contrato_06$modelo_B_corre %||% NA
)

saveRDS(contrato_06b, PATH_CONTRATO_06B, compress = FALSE)
cat(sprintf("   [✅] Guardado: %s\n\n", PATH_CONTRATO_06B))


# 📑 Checklist -----------------------------------------------------------------

cat("═══════════════════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 06b:\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("   [✅] Contrato 06a leído e impreso\n")
cat(sprintf("   [✅] Modelo heterofactor cargado — modelo final: %s\n", nombre_final))
cat(sprintf("   [✅] Panel con θ: %s obs\n", format(n_panel, big.mark = ",")))
cat(sprintf("   [✅] Cobertura total:  %.1f%% del panel (incluye inactivos/menores)\n",
            n_con_theta / n_panel * 100))
cat(sprintf("   [✅] Cobertura PEA:    %.1f%% de la PEA  (relevante para pipeline)\n",
            n_pea_con_theta / n_pea * 100))
cat("   [✅] Test 1 — d Cohen por formalidad\n")
cat("   [✅] Test 2 — R² LPM\n")
cat("   [✅] Test 3 — Cor decil de ingreso (PEA completa)\n")
cat("   [✅] Test 4 — Correlaciones con observables\n")
cat(sprintf("   [✅] Resumen ejecutivo — %s\n", veredicto))
cat(sprintf("   [✅] Contrato 06b en disco: %s\n", file.exists(PATH_CONTRATO_06B)))
cat("───────────────────────────────────────────────────────────────────\n")
cat("   SIGUIENTE PASO: 06c_diagnostico_comparativo.R\n")
cat("   Correr los 4 tests sobre θ del Modelo A y comparar con Modelo B.\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")


# ⌛ Tiempo de ejecución -------------------------------------------------------

end_time <- Sys.time()
cat(sprintf("⏱️  Tiempo de ejecución: %.1f minutos\n",
            as.numeric(difftime(end_time, start_time, units = "mins"))))
toc()
