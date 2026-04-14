# =============================================================================
# [EN] 06c_diagnostico_comparativo.R -- Compare Model A (parsimonious) vs Model B (full): sign validation and model selection
# INPUTS:  rdos/datos/06_theta_predichos.rds, rdos/modelos/06_modelo_heterofactor.rds
# OUTPUTS: rdos/contratos/06c_contrato_comparativo.rds
# =============================================================================
# 🌟 06c_diagnostico_comparativo.R 🌟 ####
# OBJETIVO:
#    Determinar si el Modelo A (parsimonioso, 77 parámetros) produce θ_A con
#    comportamiento teóricamente correcto (gradiente positivo respecto a ingreso
#    y formalidad), para fundamentar la decisión de selección de modelo A vs B
#    con base empírica. El Modelo B fue seleccionado por LR test pero θ_A resultó
#    con signo invertido en todos los tests de 06b — este script evalúa si el
#    problema es del Modelo B o del sistema de proxies.
#
# INPUTS:
#    - PATH_06_THETA             → rdos/datos/06_theta_predichos.rds
#      Panel 1,795,386 × 77: panel completo + theta_A + theta_B (del Modelo B)
#    - PATH_06_MODELO_HETERO     → rdos/modelos/06_modelo_heterofactor.rds
#      list(modelo_A, modelo_B, modelo_final, nombre_final)
#      modelo_A contiene sus propios scores/theta_data
#    - PATH_CONTRATO_06          → rdos/contratos/06_contrato_heterofactor.rds
#    - PATH_CONTRATO_06B         → rdos/contratos/06b_contrato_diagnostico_theta.rds
#      Cargado para contexto pero ya NO se usa para métricas del Modelo B
#
# OUTPUTS:
#    - PATH_CONTRATO_06C         → rdos/contratos/06c_contrato_comparativo.rds
#
# CONTEXTO CRÍTICO:
#    06b reveló que θ_A del Modelo B tiene signo INVERTIDO en todos los tests:
#    coef LPM = -0.258***, Cor(decil,θ_A) = -0.951, ICH~θ_A: r=-0.088.
#    El problema persiste con MAXIT=1200 (modelo convergido) → es estructural.
#    θ_B se comporta correctamente. LR test: B significativamente mejor
#    (stat=66,834, df=224, p~0). 06c determina si usar A (interpretabilidad)
#    o B (estadística) o ir a 06d.
#
# NO modificar 06a ni 06b. Modelo B se computa directamente desde modelo_hetero$modelo_B.
#
# TIEMPO ESTIMADO: ~3 minutos


# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(tictoc)
})


# 🔧 Cargar configuración y funciones ------------------------------------------

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))


# ⌛ Inicio contador de tiempo -------------------------------------------------

tic("Script 06c [Diagnóstico Comparativo A vs B]")
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  🔬 SCRIPT 06c — DIAGNÓSTICO COMPARATIVO θ: MODELO A vs MODELO B\n")
cat("  ¿Produce el Modelo A un θ_A con gradiente teóricamente correcto?\n")
cat(sprintf("  Inicio: %s\n", as.character(start_time)))
cat("═══════════════════════════════════════════════════════════════════\n\n")


# 🪫 1. Carga y validación de inputs -------------------------------------------
# [Verificar existencia de los 4 archivos antes de leer. contrato_06b es la
#  fuente de verdad para todas las métricas del Modelo B — no recalcular.]

cat("── 1. Carga y validación de inputs ────────────────────────────────\n\n")

hard_stop(file.exists(PATH_06_THETA),
          paste0("No existe 06_theta_predichos.rds. Ejecutar 06a primero.\n",
                 "   Ruta esperada: ", PATH_06_THETA))
hard_stop(file.exists(PATH_06_MODELO_HETERO),
          paste0("No existe 06_modelo_heterofactor.rds. Ejecutar 06a primero.\n",
                 "   Ruta esperada: ", PATH_06_MODELO_HETERO))
hard_stop(file.exists(PATH_CONTRATO_06B),
          paste0("No existe 06b_contrato_diagnostico_theta.rds. Ejecutar 06b primero.\n",
                 "   Ruta esperada: ", PATH_CONTRATO_06B))
hard_stop(file.exists(PATH_CONTRATO_06),
          paste0("No existe 06_contrato_heterofactor.rds. Ejecutar 06a primero.\n",
                 "   Ruta esperada: ", PATH_CONTRATO_06))

cat("📂 Cargando panel con theta (Modelo B como modelo_final)...\n")
panel_theta <- readRDS(PATH_06_THETA)
cat(sprintf("   ✅ %s obs × %s vars\n",
            format(nrow(panel_theta), big.mark = ","), ncol(panel_theta)))

cat("📂 Cargando modelo heterofactor unificado...\n")
modelo_hetero <- readRDS(PATH_06_MODELO_HETERO)
cat(sprintf("   ✅ Componentes: %s\n", paste(names(modelo_hetero), collapse = ", ")))
cat(sprintf("   ✅ Modelo final seleccionado en 06a: %s\n", modelo_hetero$nombre_final))

cat("📂 Cargando contrato_06b (referencia Modelo B — no recalcular)...\n")
contrato_06b <- readRDS(PATH_CONTRATO_06B)
cat(sprintf("   ✅ d_cohen_A=%.4f | cor_decil_tA=%.4f | delta_r2=%.4f\n",
            contrato_06b$d_cohen_A,
            contrato_06b$cor_decil_tA,
            contrato_06b$delta_r2))

cat("📂 Cargando contrato_06 (contexto de 06a)...\n")
contrato_06 <- readRDS(PATH_CONTRATO_06)
cat("   ✅ Cargado\n\n")


# 🪫 2. Extracción de θ del Modelo A y construcción del panel comparativo ------
# [El Modelo A tiene sus propios scores — distintos de theta_A/theta_B del panel,
#  que corresponden al modelo_final (B). Verificar estructura defensivamente:
#  el objeto puede guardar scores en $theta_data, $scores o $theta_scores.]

cat("── 2. Extracción de θ del Modelo A ────────────────────────────────\n\n")

mA_obj <- modelo_hetero$modelo_A
hard_stop(!is.null(mA_obj),
          "modelo_hetero$modelo_A es NULL — verificar estructura del output de 06a.")

cat("🔍 Verificando estructura de modelo_hetero$modelo_A...\n")
cat(sprintf("   Nombres top-level: %s\n", paste(names(mA_obj), collapse = ", ")))

# Buscar scores defensivamente en orden de prioridad
theta_data_mA <- NULL

if (!is.null(mA_obj$theta_data)) {
  theta_data_mA <- mA_obj$theta_data
  cat("   ✅ Scores encontrados en $theta_data\n")
} else if (!is.null(mA_obj$scores)) {
  theta_data_mA <- mA_obj$scores
  cat("   ✅ Scores encontrados en $scores\n")
} else if (!is.null(mA_obj$theta_scores)) {
  theta_data_mA <- mA_obj$theta_scores
  cat("   ✅ Scores encontrados en $theta_scores\n")
} else {
  cat("   ⚠️  Scores no encontrados en ubicaciones estándar.\n")
  cat("   Estructura completa de modelo_hetero$modelo_A:\n")
  str(mA_obj, max.level = 2)
  hard_stop(FALSE,
            "Ajustar la búsqueda de scores en la Sección 2 antes de continuar.")
}

cat(sprintf("   Dimensiones theta_data_mA: %s obs × %s cols\n",
            format(nrow(theta_data_mA), big.mark = ","), ncol(theta_data_mA)))
cat(sprintf("   Columnas: %s\n\n", paste(names(theta_data_mA), collapse = ", ")))

# Identificar columna de ID (defensivo — puede ser id_individuo_hist o id_individuo)
# Identificar columnas theta en los scores del Modelo A
col_tA_raw <- intersect(c("theta_A", "thetaA", "theta_a"), names(theta_data_mA))
col_tB_raw <- intersect(c("theta_B", "thetaB", "theta_b"), names(theta_data_mA))
hard_stop(length(col_tA_raw) > 0,
          paste0("No se encontró theta_A en scores del Modelo A.\n",
                 "   Columnas: ", paste(names(theta_data_mA), collapse = ", ")))
hard_stop(length(col_tB_raw) > 0,
          paste0("No se encontró theta_B en scores del Modelo A.\n",
                 "   Columnas: ", paste(names(theta_data_mA), collapse = ", ")))
col_tA_raw <- col_tA_raw[1]
col_tB_raw <- col_tB_raw[1]

# Join por id_individuo_hist + periodo_id: clave compuesta única persona-período
# (06a guarda theta_data con estas dos columnas, no con id_individuo)
scores_mA <- theta_data_mA %>%
  select(
    id_individuo_hist,
    periodo_id,
    theta_A_mA = all_of(col_tA_raw),
    theta_B_mA = all_of(col_tB_raw)
  )

cat(sprintf("   Scores renombrados: theta_A_mA, theta_B_mA (%s filas)\n",
            format(nrow(scores_mA), big.mark = ",")))

# Join 1-a-1 por clave compuesta — explota con error si hay duplicados
panel_comp <- panel_theta %>%
  left_join(scores_mA, by = c("id_individuo_hist", "periodo_id"),
            relationship = "one-to-one")

n_con_mA <- sum(!is.na(panel_comp$theta_A_mA))
pct_mA   <- n_con_mA / nrow(panel_comp) * 100
n_con_mB <- sum(!is.na(panel_comp$theta_A))

cat(sprintf("\n   Con θ Modelo A: %s (%.1f%% del panel)\n",
            format(n_con_mA, big.mark = ","), pct_mA))
cat(sprintf("   Con θ Modelo B: %s (%.1f%% del panel)\n",
            format(n_con_mB, big.mark = ","), n_con_mB / nrow(panel_comp) * 100))

hard_stop(pct_mA > 50,
          sprintf("Cobertura de θ_mA sospechosamente baja: %.1f%%. Revisar join.", pct_mA))
cat("   ✅ Cobertura razonable — join exitoso\n\n")


# ── Extracción de θ del Modelo B ────────────────────────────────────────────
# [Modelo B tiene sus propios scores en modelo_hetero$modelo_B$theta_data.
#  Los extraemos directamente para computar los 4 tests sin depender de 06b.]

cat("── 2bis. Extracción de θ del Modelo B ─────────────────────────────\n\n")

mB_obj <- modelo_hetero$modelo_B
hard_stop(!is.null(mB_obj),
          "modelo_hetero$modelo_B es NULL — verificar estructura del output de 06a.")

theta_data_mB <- NULL
if (!is.null(mB_obj$theta_data)) {
  theta_data_mB <- mB_obj$theta_data
  cat("   ✅ Scores Modelo B encontrados en $theta_data\n")
} else if (!is.null(mB_obj$scores)) {
  theta_data_mB <- mB_obj$scores
  cat("   ✅ Scores Modelo B encontrados en $scores\n")
} else if (!is.null(mB_obj$theta_scores)) {
  theta_data_mB <- mB_obj$theta_scores
  cat("   ✅ Scores Modelo B encontrados en $theta_scores\n")
} else {
  cat("   ⚠️  Scores no encontrados en ubicaciones estándar.\n")
  str(mB_obj, max.level = 2)
  hard_stop(FALSE,
            "Ajustar la búsqueda de scores del Modelo B antes de continuar.")
}

cat(sprintf("   Dimensiones theta_data_mB: %s obs × %s cols\n",
            format(nrow(theta_data_mB), big.mark = ","), ncol(theta_data_mB)))
cat(sprintf("   Columnas: %s\n\n", paste(names(theta_data_mB), collapse = ", ")))

col_tA_raw_mB <- intersect(c("theta_A", "thetaA", "theta_a"), names(theta_data_mB))
col_tB_raw_mB <- intersect(c("theta_B", "thetaB", "theta_b"), names(theta_data_mB))
hard_stop(length(col_tA_raw_mB) > 0, "No se encontró theta_A en scores del Modelo B")
hard_stop(length(col_tB_raw_mB) > 0, "No se encontró theta_B en scores del Modelo B")

scores_mB <- theta_data_mB %>%
  select(id_individuo_hist, periodo_id,
         theta_A_mB = all_of(col_tA_raw_mB[1]),
         theta_B_mB = all_of(col_tB_raw_mB[1]))

panel_comp <- panel_comp %>%
  left_join(scores_mB, by = c("id_individuo_hist", "periodo_id"),
            relationship = "one-to-one")

n_con_mB_direct <- sum(!is.na(panel_comp$theta_A_mB))
cat(sprintf("   Con θ Modelo B (directo): %s (%.1f%% del panel)\n",
            format(n_con_mB_direct, big.mark = ","),
            n_con_mB_direct / nrow(panel_comp) * 100))
cat("   ✅ Join Modelo B exitoso\n\n")


# 🪫 3. Los 4 tests sobre el Modelo A ------------------------------------------
# [Misma lógica que 06b, aplicada sobre theta_A_mA y theta_B_mA.
#  Convención de signo: d Cohen = (formal_mean - informal_mean) / SD_pooled.
#  Positivo = formales tienen más θ = DIRECCIÓN CORRECTA para ambos factores.]

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  📊 TESTS SOBRE MODELO A (parsimonioso — 77 parámetros)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Subconjuntos base reutilizados en múltiples tests
dat_base <- panel_comp %>%
  filter(!is.na(theta_A_mA), !is.na(theta_B_mA)) %>%
  mutate(
    formal_bin = case_when(
      formalidad_empleo == "Formal oficial"   ~ 1L,
      formalidad_empleo == "Informal oficial" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

dat_ocup_mA <- dat_base %>%
  filter(condicion_actividad == "Ocupado", !is.na(formal_bin))

cat(sprintf("   θ Modelo A disponible: %s obs\n",
            format(nrow(dat_base), big.mark = ",")))
cat(sprintf("   Ocupados con formalidad observada: %s obs\n\n",
            format(nrow(dat_ocup_mA), big.mark = ",")))


# 🪫 3a. Test 1 — d Cohen por condición de formalidad -------------------------
# [θ_A cognitivo debe ser MAYOR en formales: d = (formal - informal) > 0 = correcto]

cat("── TEST 1: θ por condición de formalidad (ocupados) ────────────────\n")

resultados_t1 <- list()

for (theta_var in c("theta_A_mA", "theta_B_mA")) {
  lbl   <- if (theta_var == "theta_A_mA") "θ_A_mA" else "θ_B_mA"
  v_inf <- dat_ocup_mA[[theta_var]][dat_ocup_mA$formal_bin == 0L]
  v_frm <- dat_ocup_mA[[theta_var]][dat_ocup_mA$formal_bin == 1L]

  m_inf     <- mean(v_inf, na.rm = TRUE)
  m_frm     <- mean(v_frm, na.rm = TRUE)
  sd_pooled <- sqrt((var(v_frm, na.rm = TRUE) + var(v_inf, na.rm = TRUE)) / 2)
  d_cohen   <- (m_frm - m_inf) / sd_pooled    # + = formales > informales = correcto
  tt        <- t.test(v_frm, v_inf, var.equal = FALSE)
  sig       <- if (tt$p.value < 0.001) "***" else
               if (tt$p.value < 0.01)  "**"  else
               if (tt$p.value < 0.05)  "*"   else "ns"

  signo_ok  <- d_cohen > 0
  mag_icon  <- if (abs(d_cohen) >= 0.30) "✅ Efecto medio-grande" else
               if (abs(d_cohen) >= 0.10) "⚠️  Efecto pequeño"    else "❌ Efecto mínimo"
  sign_icon <- if (signo_ok) "✅ signo correcto" else "❌ SIGNO INVERTIDO"

  cat(sprintf("\n   %s:\n", lbl))
  cat(sprintf("     Informal  n=%s  media=%+.4f  sd=%.4f\n",
              format(length(v_inf), big.mark = ","),
              m_inf, sqrt(var(v_inf, na.rm = TRUE))))
  cat(sprintf("     Formal    n=%s  media=%+.4f  sd=%.4f\n",
              format(length(v_frm), big.mark = ","),
              m_frm, sqrt(var(v_frm, na.rm = TRUE))))
  cat(sprintf("     Δ(formal−informal)=%+.4f | t=%.1f%s | d=%+.3f\n",
              m_frm - m_inf, tt$statistic, sig, d_cohen))
  cat(sprintf("     %s  |  %s\n", mag_icon, sign_icon))

  resultados_t1[[theta_var]] <- list(
    media_inf = m_inf,
    media_frm = m_frm,
    d_cohen   = d_cohen,
    signo_ok  = signo_ok
  )
}

cat("\n")


# 🪫 3b. Test 2 — R² LPM (formalidad ~ θ) ------------------------------------
# [Misma especificación que 06b. Verificar que R² base coincida con contrato_06b.]

cat("── TEST 2: R² de formalidad ~ θ (LPM) ──────────────────────────────\n")

dat_lpm <- dat_ocup_mA %>%
  mutate(
    sexo_num   = if_else(as.character(sexo) == "Mujeres", 1, 0),
    edad_num   = as.numeric(edad),
    region_chr = as.character(region)
  ) %>%
  filter(!is.na(sexo_num), !is.na(edad_num), !is.na(region_chr))

cat(sprintf("\n   Muestra LPM: %s obs\n", format(nrow(dat_lpm), big.mark = ",")))

m0  <- lm(formal_bin ~ sexo_num + edad_num + I(edad_num^2) + region_chr, data = dat_lpm)
mA  <- lm(formal_bin ~ theta_A_mA + sexo_num + edad_num + I(edad_num^2) + region_chr,
          data = dat_lpm)
mB  <- lm(formal_bin ~ theta_B_mA + sexo_num + edad_num + I(edad_num^2) + region_chr,
          data = dat_lpm)
mAB <- lm(formal_bin ~ theta_A_mA + theta_B_mA + sexo_num + edad_num + I(edad_num^2) + region_chr,
          data = dat_lpm)

r2_base <- summary(m0)$r.squared
r2_a    <- summary(mA)$r.squared
r2_b    <- summary(mB)$r.squared
r2_ab   <- summary(mAB)$r.squared
dr2     <- r2_ab - r2_base

# Verificar consistencia con 06b (misma especificación → R² base debe coincidir)
r2_base_06b <- contrato_06b$r2_base %||% NA_real_
if (!is.na(r2_base_06b)) {
  dif_r2base  <- abs(r2_base - r2_base_06b)
  icon_base   <- if (dif_r2base < 0.005) "✅ consistente con 06b" else
                 sprintf("⚠️  difiere de 06b en %.4f (revisar muestra)", dif_r2base)
  cat(sprintf("\n   R² base (sin θ):             %.4f  %s\n", r2_base, icon_base))
} else {
  cat(sprintf("\n   R² base (sin θ):             %.4f\n", r2_base))
}

cat(sprintf("   R² + θ_A_mA:                 %.4f  (Δ=+%.4f)\n", r2_a,  r2_a  - r2_base))
cat(sprintf("   R² + θ_B_mA:                 %.4f  (Δ=+%.4f)\n", r2_b,  r2_b  - r2_base))
cat(sprintf("   R² + θ_A_mA + θ_B_mA:       %.4f  (Δ=+%.4f)\n", r2_ab, dr2))

coef_mAB   <- coef(summary(mAB))
coef_tA_mA <- NA_real_
coef_tB_mA <- NA_real_

for (v in c("theta_A_mA", "theta_B_mA")) {
  if (v %in% rownames(coef_mAB)) {
    b   <- coef_mAB[v, "Estimate"]
    se  <- coef_mAB[v, "Std. Error"]
    pv  <- coef_mAB[v, "Pr(>|t|)"]
    sig <- if (pv < 0.001) "***" else if (pv < 0.01) "**" else if (pv < 0.05) "*" else "ns"
    sn  <- if (b > 0) "✅ positivo" else "❌ NEGATIVO"
    cat(sprintf("   coef(%s) = %+.4f  (SE=%.4f, p%s)  %s\n",
                v, b, se, sig, sn))
    if (v == "theta_A_mA") coef_tA_mA <- b
    if (v == "theta_B_mA") coef_tB_mA <- b
  }
}

dr2_icon <- if (dr2 > 0.02)  "✅ θ aporta información sustancial" else
            if (dr2 > 0.005) "⚠️  θ aporta información marginal"  else
            "❌ θ no discrimina formalidad"
cat(sprintf("\n   %s\n\n", dr2_icon))


# 🪫 3c. Test 3 — Cor(decil_ing, θ): validación externa -----------------------
# [PEA completa con ingreso_real_final > 0. θ_A cognitivo debe crecer con
#  el ingreso: Cor > 0 (dirección correcta) y < 0.80 (sin endogeneidad excesiva).]

cat("── TEST 3: θ por decil de ingreso (PEA completa) ───────────────────\n")
cat("   [θ_A cognitivo debe crecer con ingreso: Cor > 0 y < 0.80]\n\n")

dat_ing <- panel_comp %>%
  filter(!is.na(ingreso_real_final), ingreso_real_final > 0,
         !is.na(theta_A_mA), !is.na(theta_B_mA)) %>%
  mutate(decil = ntile(ingreso_real_final, 10))

hard_stop(nrow(dat_ing) > 1000,
          sprintf("Muestra para Test 3 insuficiente: %d obs. Revisar filtro.", nrow(dat_ing)))

cat(sprintf("   Muestra: %s obs (PEA con ingreso_real_final > 0)\n\n",
            format(nrow(dat_ing), big.mark = ",")))

decil_stats <- dat_ing %>%
  group_by(decil) %>%
  summarise(
    tA_mA_media = mean(theta_A_mA, na.rm = TRUE),
    tB_mA_media = mean(theta_B_mA, na.rm = TRUE),
    n           = n(),
    .groups     = "drop"
  )

cat(sprintf("   %5s  %12s  %12s  %10s\n", "Decil", "θ_A_mA_med", "θ_B_mA_med", "n"))
cat(sprintf("   %s\n", strrep("─", 48)))
for (i in seq_len(nrow(decil_stats))) {
  cat(sprintf("   %5d  %+12.4f  %+12.4f  %10s\n",
              decil_stats$decil[i],
              decil_stats$tA_mA_media[i],
              decil_stats$tB_mA_media[i],
              format(decil_stats$n[i], big.mark = ",")))
}

cor_decil_tA_mA <- cor(decil_stats$decil, decil_stats$tA_mA_media)
cor_decil_tB_mA <- cor(decil_stats$decil, decil_stats$tB_mA_media)

flag_tA <- if      (cor_decil_tA_mA < 0)    "❌ SIGNO INVERTIDO (decrece con ingreso)" else
           if      (cor_decil_tA_mA > 0.95)  "❌ DEMASIADO ALTO (posible endogeneidad)" else
           if      (cor_decil_tA_mA > 0.70)  "⚠️  Alto — aceptable pero vigilar"        else
           "✅ Positivo y razonable"

flag_tB <- if (abs(cor_decil_tB_mA) > 0.95) "⚠️  Alto"   else "✅ Razonable"

cat(sprintf("\n   Cor(decil, θ_A_mA) = %+.4f  %s\n", cor_decil_tA_mA, flag_tA))
cat(sprintf("   Cor(decil, θ_B_mA) = %+.4f  %s\n\n",  cor_decil_tB_mA, flag_tB))


# 🪫 3d. Test 4 — Correlaciones θ con variables observables --------------------
# [Mismas 4 variables que 06b. Signos esperados: θ_A ~ ICH (+), clima (+);
#  θ_B ~ entropía (+), residual_vivienda (+).
#  Nota: la proxy cognitiva emparejamiento_selectivo (antes assortative_mating
#  en el LEGACY) no integra este test — los 4 observables son externos al modelo.]

cat("── TEST 4: Correlaciones θ con variables observables ───────────────\n")
cat("   [Signos esperados según la teoría]\n\n")

vars_obs <- list(
  list(var = "ich_score",             label = "ICH score",             theta = "A", signo = "+"),
  list(var = "clima_educativo_hogar", label = "Clima educativo hogar", theta = "A", signo = "+"),
  list(var = "entropia_estabilidad",  label = "Entropía estabilidad",  theta = "B", signo = "+"),
  list(var = "residual_vivienda",     label = "Residual vivienda",     theta = "B", signo = "+")
)

cors_obs_mA <- list()

for (v in vars_obs) {
  col_theta <- if (v$theta == "A") "theta_A_mA" else "theta_B_mA"

  if (!v$var %in% names(panel_comp)) {
    cat(sprintf("   %-30s ~ θ_%s_mA: ⚠️  variable no encontrada en panel\n",
                v$label, v$theta))
    next
  }

  x         <- as.numeric(panel_comp[[v$var]])
  theta_vec <- panel_comp[[col_theta]]
  idx       <- !is.na(x) & !is.na(theta_vec)

  if (sum(idx) < 100) {
    cat(sprintf("   %-30s ~ θ_%s_mA: ⚠️  muestra insuficiente (%d obs)\n",
                v$label, v$theta, sum(idx)))
    next
  }

  r          <- cor(x[idx], theta_vec[idx])
  signo_ok   <- (v$signo == "+" && r > 0) || (v$signo == "-" && r < 0)
  mag_icon   <- if (abs(r) > 0.20) "✅ mag." else if (abs(r) > 0.05) "⚠️  mag." else "❌ mag."
  signo_icon <- if (signo_ok) "✅ signo" else "❌ SIGNO"

  cat(sprintf("   %-30s ~ θ_%s_mA: r=%+.4f  %s  %s\n",
              v$label, v$theta, r, mag_icon, signo_icon))

  cors_obs_mA[[v$var]] <- r
}

cat("\n")


# 🪫 3bis. Los 4 tests sobre el Modelo B (computados directamente) -----------
# [Misma lógica que sección 3 pero usando theta_A_mB y theta_B_mB extraídos
#  directamente de modelo_hetero$modelo_B. NO depende de contrato_06b.]

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  📊 TESTS SOBRE MODELO B (completo — computados directamente)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Subconjuntos base para Modelo B
dat_base_mB <- panel_comp %>%
  filter(!is.na(theta_A_mB), !is.na(theta_B_mB)) %>%
  mutate(
    formal_bin = case_when(
      formalidad_empleo == "Formal oficial"   ~ 1L,
      formalidad_empleo == "Informal oficial" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

dat_ocup_mB <- dat_base_mB %>%
  filter(condicion_actividad == "Ocupado", !is.na(formal_bin))

cat(sprintf("   θ Modelo B disponible: %s obs\n",
            format(nrow(dat_base_mB), big.mark = ",")))
cat(sprintf("   Ocupados con formalidad observada: %s obs\n\n",
            format(nrow(dat_ocup_mB), big.mark = ",")))


# 🪫 3bis-a. Test 1 — d Cohen por condición de formalidad (Modelo B) ---------

cat("── TEST 1 (Modelo B): θ por condición de formalidad (ocupados) ────\n")

resultados_t1_mB <- list()

for (theta_var in c("theta_A_mB", "theta_B_mB")) {
  lbl   <- if (theta_var == "theta_A_mB") "θ_A_mB" else "θ_B_mB"
  v_inf <- dat_ocup_mB[[theta_var]][dat_ocup_mB$formal_bin == 0L]
  v_frm <- dat_ocup_mB[[theta_var]][dat_ocup_mB$formal_bin == 1L]

  m_inf     <- mean(v_inf, na.rm = TRUE)
  m_frm     <- mean(v_frm, na.rm = TRUE)
  sd_inf    <- sd(v_inf, na.rm = TRUE)
  sd_frm    <- sd(v_frm, na.rm = TRUE)
  sd_pooled <- sqrt((var(v_frm, na.rm = TRUE) + var(v_inf, na.rm = TRUE)) / 2)

  # Detectar scores degenerados (esencialmente constantes)
  degenerado <- is.na(sd_pooled) || sd_pooled < 1e-10 ||
                is.na(sd_inf) || sd_inf < 1e-10 ||
                is.na(sd_frm) || sd_frm < 1e-10

  if (degenerado) {
    cat(sprintf("\n   %s:\n", lbl))
    cat(sprintf("     Informal  n=%s  media=%+.4f  sd=%.4e\n",
                format(length(v_inf), big.mark = ","), m_inf, sd_inf %||% 0))
    cat(sprintf("     Formal    n=%s  media=%+.4f  sd=%.4e\n",
                format(length(v_frm), big.mark = ","), m_frm, sd_frm %||% 0))
    cat("     ⚠️  Scores esencialmente constantes — t.test no aplicable\n")
    cat("     (cargas degeneradas del Modelo B producen varianza ~0)\n")

    resultados_t1_mB[[theta_var]] <- list(
      media_inf  = m_inf,
      media_frm  = m_frm,
      d_cohen    = NA_real_,
      signo_ok   = FALSE,
      degenerado = TRUE
    )
    next
  }

  d_cohen   <- (m_frm - m_inf) / sd_pooled
  tt        <- t.test(v_frm, v_inf, var.equal = FALSE)
  sig       <- if (tt$p.value < 0.001) "***" else
               if (tt$p.value < 0.01)  "**"  else
               if (tt$p.value < 0.05)  "*"   else "ns"

  signo_ok  <- d_cohen > 0
  mag_icon  <- if (abs(d_cohen) >= 0.30) "✅ Efecto medio-grande" else
               if (abs(d_cohen) >= 0.10) "⚠️  Efecto pequeño"    else "❌ Efecto mínimo"
  sign_icon <- if (signo_ok) "✅ signo correcto" else "❌ SIGNO INVERTIDO"

  cat(sprintf("\n   %s:\n", lbl))
  cat(sprintf("     Informal  n=%s  media=%+.4f  sd=%.4f\n",
              format(length(v_inf), big.mark = ","),
              m_inf, sd_inf))
  cat(sprintf("     Formal    n=%s  media=%+.4f  sd=%.4f\n",
              format(length(v_frm), big.mark = ","),
              m_frm, sd_frm))
  cat(sprintf("     Δ(formal−informal)=%+.4f | t=%.1f%s | d=%+.3f\n",
              m_frm - m_inf, tt$statistic, sig, d_cohen))
  cat(sprintf("     %s  |  %s\n", mag_icon, sign_icon))

  resultados_t1_mB[[theta_var]] <- list(
    media_inf = m_inf,
    media_frm = m_frm,
    d_cohen   = d_cohen,
    signo_ok  = signo_ok
  )
}

cat("\n")


# 🪫 3bis-b. Test 2 — R² LPM (formalidad ~ θ) (Modelo B) --------------------

cat("── TEST 2 (Modelo B): R² de formalidad ~ θ (LPM) ──────────────────\n")

dat_lpm_mB <- dat_ocup_mB %>%
  mutate(
    sexo_num   = if_else(as.character(sexo) == "Mujeres", 1, 0),
    edad_num   = as.numeric(edad),
    region_chr = as.character(region)
  ) %>%
  filter(!is.na(sexo_num), !is.na(edad_num), !is.na(region_chr))

cat(sprintf("\n   Muestra LPM (Modelo B): %s obs\n", format(nrow(dat_lpm_mB), big.mark = ",")))

m0_mB  <- lm(formal_bin ~ sexo_num + edad_num + I(edad_num^2) + region_chr, data = dat_lpm_mB)
mA_mB  <- lm(formal_bin ~ theta_A_mB + sexo_num + edad_num + I(edad_num^2) + region_chr,
              data = dat_lpm_mB)
mB_mB  <- lm(formal_bin ~ theta_B_mB + sexo_num + edad_num + I(edad_num^2) + region_chr,
              data = dat_lpm_mB)
mAB_mB <- lm(formal_bin ~ theta_A_mB + theta_B_mB + sexo_num + edad_num + I(edad_num^2) + region_chr,
              data = dat_lpm_mB)

r2_base_mB <- summary(m0_mB)$r.squared
r2_a_mB    <- summary(mA_mB)$r.squared
r2_b_mB    <- summary(mB_mB)$r.squared
r2_ab_mB   <- summary(mAB_mB)$r.squared
dr2_mB     <- r2_ab_mB - r2_base_mB

cat(sprintf("\n   R² base (sin θ):             %.4f\n", r2_base_mB))
cat(sprintf("   R² + θ_A_mB:                 %.4f  (Δ=+%.4f)\n", r2_a_mB,  r2_a_mB  - r2_base_mB))
cat(sprintf("   R² + θ_B_mB:                 %.4f  (Δ=+%.4f)\n", r2_b_mB,  r2_b_mB  - r2_base_mB))
cat(sprintf("   R² + θ_A_mB + θ_B_mB:       %.4f  (Δ=+%.4f)\n", r2_ab_mB, dr2_mB))

coef_mAB_mB <- coef(summary(mAB_mB))
coef_tA_mB  <- NA_real_
coef_tB_mB  <- NA_real_

for (v in c("theta_A_mB", "theta_B_mB")) {
  if (v %in% rownames(coef_mAB_mB)) {
    b   <- coef_mAB_mB[v, "Estimate"]
    se  <- coef_mAB_mB[v, "Std. Error"]
    pv  <- coef_mAB_mB[v, "Pr(>|t|)"]
    sig <- if (pv < 0.001) "***" else if (pv < 0.01) "**" else if (pv < 0.05) "*" else "ns"
    sn  <- if (b > 0) "✅ positivo" else "❌ NEGATIVO"
    cat(sprintf("   coef(%s) = %+.4f  (SE=%.4f, p%s)  %s\n",
                v, b, se, sig, sn))
    if (v == "theta_A_mB") coef_tA_mB <- b
    if (v == "theta_B_mB") coef_tB_mB <- b
  }
}

dr2_icon_mB <- if (dr2_mB > 0.02)  "✅ θ aporta información sustancial" else
               if (dr2_mB > 0.005) "⚠️  θ aporta información marginal"  else
               "❌ θ no discrimina formalidad"
cat(sprintf("\n   %s\n\n", dr2_icon_mB))


# 🪫 3bis-c. Test 3 — Cor(decil_ing, θ): validación externa (Modelo B) ------

cat("── TEST 3 (Modelo B): θ por decil de ingreso (PEA completa) ───────\n")
cat("   [θ_A cognitivo debe crecer con ingreso: Cor > 0 y < 0.80]\n\n")

dat_ing_mB <- panel_comp %>%
  filter(!is.na(ingreso_real_final), ingreso_real_final > 0,
         !is.na(theta_A_mB), !is.na(theta_B_mB)) %>%
  mutate(decil = ntile(ingreso_real_final, 10))

hard_stop(nrow(dat_ing_mB) > 1000,
          sprintf("Muestra para Test 3 mB insuficiente: %d obs.", nrow(dat_ing_mB)))

cat(sprintf("   Muestra: %s obs (PEA con ingreso_real_final > 0)\n\n",
            format(nrow(dat_ing_mB), big.mark = ",")))

decil_stats_mB <- dat_ing_mB %>%
  group_by(decil) %>%
  summarise(
    tA_mB_media = mean(theta_A_mB, na.rm = TRUE),
    tB_mB_media = mean(theta_B_mB, na.rm = TRUE),
    n           = n(),
    .groups     = "drop"
  )

cat(sprintf("   %5s  %12s  %12s  %10s\n", "Decil", "θ_A_mB_med", "θ_B_mB_med", "n"))
cat(sprintf("   %s\n", strrep("─", 48)))
for (i in seq_len(nrow(decil_stats_mB))) {
  cat(sprintf("   %5d  %+12.4f  %+12.4f  %10s\n",
              decil_stats_mB$decil[i],
              decil_stats_mB$tA_mB_media[i],
              decil_stats_mB$tB_mB_media[i],
              format(decil_stats_mB$n[i], big.mark = ",")))
}

cor_decil_tA_mB <- tryCatch(cor(decil_stats_mB$decil, decil_stats_mB$tA_mB_media),
                            warning = function(w) NA_real_)
cor_decil_tB_mB <- tryCatch(cor(decil_stats_mB$decil, decil_stats_mB$tB_mB_media),
                            warning = function(w) NA_real_)

flag_tA_mB <- if (is.na(cor_decil_tA_mB))       "⚠️  NA — scores constantes (degenerado)" else
              if      (cor_decil_tA_mB < 0)      "❌ SIGNO INVERTIDO (decrece con ingreso)" else
              if      (cor_decil_tA_mB > 0.95)   "❌ DEMASIADO ALTO (posible endogeneidad)" else
              if      (cor_decil_tA_mB > 0.70)   "⚠️  Alto — aceptable pero vigilar"        else
              "✅ Positivo y razonable"

flag_tB_mB <- if (is.na(cor_decil_tB_mB))       "⚠️  NA — scores constantes" else
              if (abs(cor_decil_tB_mB) > 0.95)   "⚠️  Alto"   else "✅ Razonable"

if (is.na(cor_decil_tA_mB)) {
  cat(sprintf("\n   Cor(decil, θ_A_mB) =     NA  %s\n", flag_tA_mB))
} else {
  cat(sprintf("\n   Cor(decil, θ_A_mB) = %+.4f  %s\n", cor_decil_tA_mB, flag_tA_mB))
}
cat(sprintf("   Cor(decil, θ_B_mB) = %+.4f  %s\n\n",
            cor_decil_tB_mB %||% NA_real_, flag_tB_mB))


# 🪫 3bis-d. Test 4 — Correlaciones θ con variables observables (Modelo B) ---

cat("── TEST 4 (Modelo B): Correlaciones θ con variables observables ───\n")
cat("   [Signos esperados según la teoría]\n\n")

cors_obs_mB <- list()

for (v in vars_obs) {
  col_theta <- if (v$theta == "A") "theta_A_mB" else "theta_B_mB"

  if (!v$var %in% names(panel_comp)) {
    cat(sprintf("   %-30s ~ θ_%s_mB: ⚠️  variable no encontrada en panel\n",
                v$label, v$theta))
    next
  }

  x         <- as.numeric(panel_comp[[v$var]])
  theta_vec <- panel_comp[[col_theta]]
  idx       <- !is.na(x) & !is.na(theta_vec)

  if (sum(idx) < 100) {
    cat(sprintf("   %-30s ~ θ_%s_mB: ⚠️  muestra insuficiente (%d obs)\n",
                v$label, v$theta, sum(idx)))
    next
  }

  r          <- tryCatch(cor(x[idx], theta_vec[idx]), warning = function(w) NA_real_)

  if (is.na(r)) {
    cat(sprintf("   %-30s ~ θ_%s_mB: r=    NA  ⚠️  scores constantes (degenerado)\n",
                v$label, v$theta))
    cors_obs_mB[[v$var]] <- NA_real_
    next
  }

  signo_ok   <- (v$signo == "+" && r > 0) || (v$signo == "-" && r < 0)
  mag_icon   <- if (abs(r) > 0.20) "✅ mag." else if (abs(r) > 0.05) "⚠️  mag." else "❌ mag."
  signo_icon <- if (signo_ok) "✅ signo" else "❌ SIGNO"

  cat(sprintf("   %-30s ~ θ_%s_mB: r=%+.4f  %s  %s\n",
              v$label, v$theta, r, mag_icon, signo_icon))

  cors_obs_mB[[v$var]] <- r
}

cat("\n")


# 🪫 4. Tabla comparativa Modelo A vs Modelo B ---------------------------------
# [Métricas del Modelo B computadas directamente en sección 3bis — ya no depende
#  de contrato_06b. Esto elimina el bug de 06b diagnosticando modelo_final variable.]

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  📊 TABLA COMPARATIVA: MODELO A vs MODELO B\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Definir métricas con criterios de evaluación — Modelo B de sección 3bis
metricas_comp <- list(
  list(nm = "d Cohen θ_A (formal−informal)", umbral = "> 0 y |d|≥0.30",
       val_a = resultados_t1$theta_A_mA$d_cohen,
       val_b = resultados_t1_mB$theta_A_mB$d_cohen,
       ok    = function(v) !is.na(v) && v > 0 && abs(v) >= 0.30),
  list(nm = "d Cohen θ_B (formal−informal)", umbral = "> 0 y |d|≥0.30",
       val_a = resultados_t1$theta_B_mA$d_cohen,
       val_b = resultados_t1_mB$theta_B_mB$d_cohen,
       ok    = function(v) !is.na(v) && v > 0 && abs(v) >= 0.30),
  list(nm = "coef θ_A en LPM (signo)", umbral = "positivo",
       val_a = coef_tA_mA,
       val_b = coef_tA_mB,
       ok    = function(v) !is.na(v) && v > 0),
  list(nm = "ΔR² (formalidad ~ θ_AB)", umbral = "> 0.02",
       val_a = dr2,
       val_b = dr2_mB,
       ok    = function(v) !is.na(v) && v > 0.02),
  list(nm = "Cor(decil_ing, θ_A)", umbral = "pos y < 0.80",
       val_a = cor_decil_tA_mA,
       val_b = cor_decil_tA_mB,
       ok    = function(v) !is.na(v) && v > 0 && v < 0.80),
  list(nm = "Cor(decil_ing, θ_B)", umbral = "< 0.80",
       val_a = cor_decil_tB_mA,
       val_b = cor_decil_tB_mB,
       ok    = function(v) !is.na(v) && abs(v) < 0.80),
  list(nm = "ICH score ~ θ_A (signo)", umbral = "r > 0",
       val_a = cors_obs_mA$ich_score     %||% NA_real_,
       val_b = cors_obs_mB$ich_score     %||% NA_real_,
       ok    = function(v) !is.na(v) && v > 0),
  list(nm = "Clima educ hogar ~ θ_A (signo)", umbral = "r > 0",
       val_a = cors_obs_mA$clima_educativo_hogar  %||% NA_real_,
       val_b = cors_obs_mB$clima_educativo_hogar  %||% NA_real_,
       ok    = function(v) !is.na(v) && v > 0)
)

# Imprimir tabla
cat(sprintf("   %-35s  %+9s  %+9s  %-14s  %s\n",
            "Métrica", "Modelo A", "Modelo B", "Umbral", "A ✓?  B ✓?"))
cat(sprintf("   %s\n", strrep("─", 84)))

n_ok_A <- 0L
n_ok_B <- 0L

for (m in metricas_comp) {
  fmt_v  <- function(v) if (is.na(v)) "     NA" else sprintf("%+.4f", v)
  ok_a   <- tryCatch(m$ok(m$val_a), error = function(e) FALSE)
  ok_b   <- tryCatch(m$ok(m$val_b), error = function(e) FALSE)
  ia     <- if (ok_a) "✅" else "❌"
  ib     <- if (ok_b) "✅" else "❌"
  if (ok_a) n_ok_A <- n_ok_A + 1L
  if (ok_b) n_ok_B <- n_ok_B + 1L
  cat(sprintf("   %-35s  %s  %s  %-14s  %s     %s\n",
              m$nm, fmt_v(m$val_a), fmt_v(m$val_b), m$umbral, ia, ib))
}

cat(sprintf("   %s\n", strrep("─", 84)))
cat(sprintf("   %-35s  %9s  %9s\n",
            "TOTAL criterios satisfechos:",
            sprintf("%d / %d", n_ok_A, length(metricas_comp)),
            sprintf("%d / %d", n_ok_B, length(metricas_comp))))
cat("\n")


# 🪫 5. Resumen ejecutivo y recomendación de modelo ----------------------------
# [La recomendación es automática pero indicativa — la decisión final es del usuario.
#  Lógica basada en las dos condiciones más determinantes de θ_A:
#  (1) signo del coef LPM  (2) Cor(decil, θ_A) positivo y razonable.]

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  📋 RESUMEN EJECUTIVO — SELECCIÓN DE MODELO\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

signo_tA_mA_correcto <- !is.na(coef_tA_mA) && coef_tA_mA > 0
signo_tA_B_correcto  <- !is.na(coef_tA_mB) && coef_tA_mB > 0
cor_tA_mA_ok         <- !is.na(cor_decil_tA_mA) && cor_decil_tA_mA > 0 && cor_decil_tA_mA < 0.80
cor_tA_B_ok          <- !is.na(cor_decil_tA_mB) &&
                         cor_decil_tA_mB > 0 && cor_decil_tA_mB < 0.80

cat("   Diagnóstico θ_A (indicadores determinantes):\n")
cat(sprintf("   Modelo A: coef LPM %s | Cor(decil) %s\n",
            if (signo_tA_mA_correcto) "✅ positivo" else "❌ negativo",
            if (cor_tA_mA_ok)  "✅ correcto y razonable" else
            if (!is.na(cor_decil_tA_mA) && cor_decil_tA_mA < 0) "❌ negativo" else "⚠️  > 0.80"))
cat(sprintf("   Modelo B: coef LPM %s | Cor(decil) %s  (computado directamente)\n\n",
            if (signo_tA_B_correcto) "✅ positivo" else "❌ negativo",
            if (cor_tA_B_ok)  "✅ correcto y razonable" else "❌ negativo y |r|>0.80"))

# Lógica de recomendación (3 opciones)
if (signo_tA_mA_correcto && cor_tA_mA_ok) {
  recomendacion <- "A) ✅ USAR MODELO A — interpretación económica correcta, LR test sacrificado"
  justificacion <- paste0(
    "El Modelo A (77 parámetros) produce θ_A con gradiente correcto respecto\n",
    "   a ingreso y formalidad. El LR test favorece al Modelo B estadísticamente,\n",
    "   pero θ_A del Modelo B tiene signo invertido en todos los tests de 06b.\n",
    "   Criterio de interpretabilidad económica prevalece sobre el estadístico puro:\n",
    "   un factor latente con signo correcto es más útil para el backcasting."
  )
} else if (!signo_tA_mA_correcto && !signo_tA_B_correcto) {
  recomendacion <- "C) ⚠️  AMBOS MODELOS PROBLEMÁTICOS — evaluar opciones en 06d"
  justificacion <- paste0(
    "El Modelo A también produce θ_A con signo incorrecto — el problema no\n",
    "   es del Modelo B sino del sistema de proxies cognitivas. La inversión\n",
    "   de signo es estructural: las proxies no capturan el gradiente formal/\n",
    "   informal en la dirección esperada en ninguno de los dos modelos.\n",
    "   06d debe evaluar alternativas: ortogonalización post-hoc, re-especificación\n",
    "   de proxies, o uso directo de θ_B como factor único."
  )
} else {
  recomendacion <- "B) ✅ USAR MODELO B — LR test + investigar inversión de signo θ_A en 06d"
  justificacion <- paste0(
    "El Modelo A mejora parcialmente el comportamiento de θ_A respecto al\n",
    "   Modelo B, pero no lo resuelve completamente. El Modelo B sigue siendo\n",
    sprintf("   el mejor estadísticamente (LR test: stat=%.0f, df=%d, p≈0).\n",
            contrato_06$lr_test$LR_statistic %||% NA_real_,
            contrato_06$lr_test$df            %||% NA_integer_),
    "   Proceder con Modelo B y documentar la limitación de θ_A. 06d puede\n",
    "   evaluar si la inversión impacta materialmente las predicciones del backcasting."
  )
}

cat(sprintf("   RECOMENDACIÓN:\n   %s\n\n", recomendacion))
cat(sprintf("   Justificación:\n   %s\n\n", justificacion))
cat(sprintf("   Criterios satisfechos — Modelo A: %d/8  |  Modelo B: %d/8\n\n",
            n_ok_A, n_ok_B))
cat("   ⚠️  La decisión final corresponde al usuario — este script presenta\n")
cat("       la evidencia empírica para fundamentarla.\n\n")


# 🪫 6. Construcción y guardado del contrato -----------------------------------
# [Contrato 06c: métricas del Modelo A + Modelo B (ambos computados directamente)
#  + tabla comparativa + recomendación. Ya no depende de contrato_06b para B.]

cat("── 6. Guardando contrato 06c ───────────────────────────────────────\n")

contrato_06c <- list(

  # Métricas Modelo A — los 4 tests
  d_cohen_A_mA        = resultados_t1$theta_A_mA$d_cohen,  # con signo: + = formales > informales
  d_cohen_B_mA        = resultados_t1$theta_B_mA$d_cohen,
  signo_tA_mA_ok      = signo_tA_mA_correcto,
  r2_base_mA          = r2_base,
  r2_a_mA             = r2_a,
  r2_b_mA             = r2_b,
  r2_ab_mA            = r2_ab,
  delta_r2_mA         = dr2,
  coef_tA_mA          = coef_tA_mA,
  coef_tB_mA          = coef_tB_mA,
  cor_decil_tA_mA     = cor_decil_tA_mA,
  cor_decil_tB_mA     = cor_decil_tB_mA,
  cors_observables_mA = cors_obs_mA,

  # Métricas Modelo B — los 4 tests (computados directamente, no de 06b)
  d_cohen_A_mB        = resultados_t1_mB$theta_A_mB$d_cohen,
  d_cohen_B_mB        = resultados_t1_mB$theta_B_mB$d_cohen,
  coef_tA_mB          = coef_tA_mB,
  coef_tB_mB          = coef_tB_mB,
  r2_base_mB          = r2_base_mB,
  r2_ab_mB            = r2_ab_mB,
  delta_r2_mB         = dr2_mB,
  cor_decil_tA_mB     = cor_decil_tA_mB,
  cor_decil_tB_mB     = cor_decil_tB_mB,
  cors_observables_mB = cors_obs_mB,

  # Tabla comparativa
  tabla_comparativa   = metricas_comp,
  n_criterios_ok_A    = n_ok_A,
  n_criterios_ok_B    = n_ok_B,
  n_criterios_total   = length(metricas_comp),

  # Recomendación
  recomendacion       = recomendacion,
  justificacion       = justificacion,

  # Referencias y metadata
  ref_contrato_06b    = PATH_CONTRATO_06B,
  modelo_final_06a    = modelo_hetero$nombre_final,
  n_panel             = nrow(panel_theta),
  n_con_theta_mA      = n_con_mA,
  pct_cobertura_mA    = round(pct_mA, 2),
  n_muestra_lpm       = nrow(dat_lpm),
  n_muestra_test3     = nrow(dat_ing),
  timestamp           = Sys.time(),
  script              = "06c_diagnostico_comparativo.R"
)

saveRDS(contrato_06c, PATH_CONTRATO_06C)
cat(sprintf("   ✅ Contrato guardado: %s\n\n", PATH_CONTRATO_06C))


# 🪫 7. Limpieza de objetos intermedios ----------------------------------------

rm(list = c("panel_theta", "modelo_hetero", "contrato_06", "mA_obj", "mB_obj",
            "theta_data_mA", "scores_mA", "theta_data_mB", "scores_mB",
            "panel_comp",
            "dat_base", "dat_ocup_mA", "dat_lpm", "dat_ing", "decil_stats",
            "dat_base_mB", "dat_ocup_mB", "dat_lpm_mB", "dat_ing_mB", "decil_stats_mB",
            "coef_mAB", "m0", "mA", "mB", "mAB",
            "coef_mAB_mB", "m0_mB", "mA_mB", "mB_mB", "mAB_mB"))
gc(verbose = FALSE)


# 📑 Checklist -----------------------------------------------------------------

end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("═══════════════════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 06c:\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("   [✅] θ del Modelo A extraído desde modelo_hetero$modelo_A\n")
cat("   [✅] θ del Modelo B extraído desde modelo_hetero$modelo_B\n")
cat("   [✅] Join al panel por id_individuo_hist\n")
cat("   [✅] Tests 1-4 Modelo A (d Cohen, LPM, decil, observables)\n")
cat("   [✅] Tests 1-4 Modelo B (d Cohen, LPM, decil, observables) — computados directamente\n")
cat("   [✅] Tabla comparativa Modelo A vs Modelo B (8 métricas)\n")
cat("   [✅] Resumen ejecutivo con recomendación A/B/C\n")
cat(sprintf("   [✅] Contrato guardado: %s\n", basename(PATH_CONTRATO_06C)))
cat("───────────────────────────────────────────────────────────────────\n")
cat("   SIGUIENTE PASO:\n")
cat("     Recomendación A → documentar decisión → 07a_lasso_LPM.R\n")
cat("     Recomendación B → documentar decisión → 07a_lasso_LPM.R\n")
cat("     Recomendación C → 06d_comparacion_opciones_theta.R\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat(sprintf("⏱️  Tiempo total: %.1f minutos\n\n", elapsed))

toc()
