# =============================================================================
# [EN] 10a_robustness_heterofactor.R -- Robustness: re-estimate heterofactor excluding overlap quarters, compare factor stability
# INPUTS:  rdos/datos/04_panel_con_proxies.rds, rdos/datos/06_theta_predichos.rds
# OUTPUTS: rdos/reportes/00_robustness_heterofactor.html, rdos/contratos/10a_*.rds
# =============================================================================
# 🌟 10a_robustness_heterofactor.R 🌟 ####
# OBJETIVO:  A1+ — Robustness check del heterofactor bifactorial.
#            Re-estima excluyendo 2024Q4-2025Q3 (los 4 trimestres con
#            formalidad observada = training del LASSO) y compara θ_A, θ_B
#            y cargas factoriales con el modelo original.
#
# LÓGICA:   El reviewer pregunta si el heterofactor pudo "ver" los períodos
#            de training. Mostramos que la estimación sobre el panel pre-2024Q4
#            produce factores virtualmente idénticos (cor > 0.99 esperada).
#
# INPUTS:
#   PATH_04_PANEL_PROXIES  → rdos/datos/04_panel_con_proxies.rds
#   PATH_06_THETA          → rdos/datos/06_theta_predichos.rds (original)
#   PATH_06_MODELO_HETERO  → rdos/modelos/06_modelo_heterofactor.rds (original)
#
# OUTPUTS:
#   rdos/modelos/00_robustness_modelo_hetero_restringido.rds
#   rdos/datos/00_robustness_theta_restringido.rds
#   rdos/reportes/00_robustness_heterofactor.csv
#   rdos/reportes/00_robustness_heterofactor.html
#   rdos/reportes/00_robustness_heterofactor_log.txt
#   rdos/contratos/10a_contrato_robustness_hetero.rds
#   rdos/figuras/10a_robustness_heterofactor/*.pdf
#
# PARÁMETROS: idénticos a 06a_ (N_MAX_MLE=20k, K_NODES=9, MAXIT_A=150,
#             MAXIT_B=1200, SEED=123, GPU chunk=500k)
#
# TIEMPO ESTIMADO: ~6h (dominado por MLE + scoring GPU del Modelo B)

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(fastGHQuad)
  library(tictoc)
  library(compiler)
  library(Rcpp)
  library(reticulate)
  library(ggplot2)
  library(patchwork)
  library(knitr)
  library(kableExtra)
  library(rmarkdown)
})

HAS_MATRIXSTATS <- requireNamespace("matrixStats", quietly = TRUE)


# 🔧 Cargar configuración y funciones ------------------------------------------

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

log_path <- file.path(DIR_REPORTES, "00_robustness_heterofactor_log.txt")
sink(log_path, split = TRUE)

# ⌛ Inicio contador de tiempo -------------------------------------------------

tic("Robustness COMPLETO")
t_inicio <- proc.time()
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════\n")
cat("🔬 00_robustness_heterofactor.R — Robustness A1+\n")
cat("═══════════════════════════════════════════════════════\n")
cat("🚀 INICIO:", as.character(start_time), "\n\n")

# Paths de outputs propios (sin HC: construidos sobre DIR_* de parametros.R)
PATH_ROBUSTNESS_MODELO <- file.path(DIR_MODELOS,  "00_robustness_modelo_hetero_restringido.rds")
PATH_ROBUSTNESS_THETA  <- file.path(DIR_DATOS,    "00_robustness_theta_restringido.rds")
PATH_ROBUSTNESS_CSV    <- file.path(DIR_REPORTES, "00_robustness_heterofactor.csv")
PATH_ROBUSTNESS_HTML   <- file.path(DIR_REPORTES, "00_robustness_heterofactor.html")

# ── Período de exclusión (derivado de TRIMESTRES_FORMALIDAD — sin HC) ──────────
# TRIMESTRES_FORMALIDAD = c("2024_T4", "2025_T1", "2025_T2", "2025_T3")
parse_periodo_id <- function(s) {
  parts <- strsplit(s, "_T")[[1]]
  as.integer(parts[1]) * 10L + as.integer(parts[2])
}
PERIODO_EXCLUIR_DESDE <- parse_periodo_id(TRIMESTRES_FORMALIDAD[1])
# = 20244: excluye 2024Q4, 2025Q1, 2025Q2, 2025Q3 inclusive

cat("🔧 Periodo de exclusión: periodo_id >=", PERIODO_EXCLUIR_DESDE, "\n")
cat("   Trimestres excluidos:", paste(TRIMESTRES_FORMALIDAD, collapse = ", "), "\n\n")


# 🪫 1. Compilación Rcpp + OpenMP --------------------------------------------

cat("─────────────────────────────────────────────────────\n")
cat("🔧 1. Compilación Rcpp + OpenMP\n")
cat("─────────────────────────────────────────────────────\n")

# El .cpp fue generado por 06a_heterofactor_estimacion.R — reusar directamente
CPP_PATH <- here::here("script", "03_heterofactor", "heterofactor_fiml.cpp")
hard_stop(file.exists(CPP_PATH),
          "heterofactor_fiml.cpp no existe. Ejecutar 06a_ primero.")

if (!exists("compute_log_m_grid_cpp") || !exists("compute_ll_full_cpp")) {
  cat("🔨 Compilando C++ con OpenMP...\n")
  Sys.setenv(PKG_CXXFLAGS = "-fopenmp -O3")
  Sys.setenv(PKG_LIBS     = "-fopenmp")
  tryCatch(
    sourceCpp(CPP_PATH, verbose = FALSE),
    error = function(e) {
      cat("❌ Error compilación:\n", as.character(e), "\n")
      stop("No se pudo compilar C++")
    }
  )
}

omp_info <- test_openmp()
if (omp_info$openmp_enabled) {
  N_THREADS <- omp_info$n_threads
  cat("✅ OpenMP ACTIVO —", N_THREADS, "threads\n")
} else {
  N_THREADS <- 1L
  cat("⚠️  OpenMP no disponible — MLE más lento (1 thread)\n")
}


# 🪫 2. GPU Setup -- PyTorch via reticulate ----------------------------------

cat("\n─────────────────────────────────────────────────────\n")
cat("🖥️  2. GPU Setup (PyTorch via reticulate)\n")
cat("─────────────────────────────────────────────────────\n")

use_condaenv("r-reticulate", required = TRUE)
torch <- import("torch")

GPU_DISPONIBLE <- torch$cuda$is_available()
GPU_DEVICE     <- if (GPU_DISPONIBLE) "cuda" else "cpu"
GPU_CHUNK_SIZE <- 500000L  # 500k obs/chunk → peak VRAM ~1.13 GB (float32)

if (GPU_DISPONIBLE) {
  cat("✅ GPU:", py_to_r(torch$cuda$get_device_name(0L)), "\n")
  cat("   Chunk size:", format(GPU_CHUNK_SIZE, big.mark = ","), "obs\n")
} else {
  cat("⚠️  GPU no disponible — scoring en CPU (más lento)\n")
}

# Función scoring GPU (idéntica a 06a_)
py_run_string("
import torch
import numpy as np
import math

def scoring_gpu(M_np, m_hat_np, theta_A_nodes_np, theta_B_nodes_np,
                alpha_A_np, alpha_B_np, sigma_eps_np,
                chunk_size=500000, device='cuda'):
    N, n_M = M_np.shape
    K      = len(theta_A_nodes_np)
    tA = np.repeat(theta_A_nodes_np, K)
    tB = np.tile(theta_B_nodes_np, K)
    K2 = len(tA)
    alpha_A    = torch.tensor(alpha_A_np,    dtype=torch.float32, device=device)
    alpha_B    = torch.tensor(alpha_B_np,    dtype=torch.float32, device=device)
    sigma_eps  = torch.tensor(sigma_eps_np,  dtype=torch.float32, device=device)
    theta_A_2d = torch.tensor(tA,           dtype=torch.float32, device=device)
    theta_B_2d = torch.tensor(tB,           dtype=torch.float32, device=device)
    log_sqrt2pi = math.log(math.sqrt(2 * math.pi))
    log_m_grid_out = torch.zeros(N, K2, dtype=torch.float32)
    for start in range(0, N, chunk_size):
        end = min(start + chunk_size, N)
        M_c    = torch.tensor(M_np[start:end],     dtype=torch.float32, device=device)
        mhat_c = torch.tensor(m_hat_np[start:end], dtype=torch.float32, device=device)
        mask = ~torch.isnan(M_c)
        M_c  = torch.nan_to_num(M_c, nan=0.0)
        mean_grid = (mhat_c[:, None, :]
                    + theta_A_2d[None, :, None] * alpha_A[None, None, :]
                    + theta_B_2d[None, :, None] * alpha_B[None, None, :])
        z     = (M_c[:, None, :] - mean_grid) / sigma_eps[None, None, :]
        log_d = -0.5 * z**2 - torch.log(sigma_eps[None, None, :]) - log_sqrt2pi
        log_d = log_d * mask[:, None, :].float()
        log_m_chunk = log_d.sum(dim=2)
        log_m_grid_out[start:end] = log_m_chunk.cpu()
        del M_c, mhat_c, mean_grid, z, log_d, log_m_chunk, mask
        if device == 'cuda':
            torch.cuda.empty_cache()
    return log_m_grid_out.numpy()
")
cat("✅ scoring_gpu definido\n")


# 🪫 3. Parámetros del modelo (idénticos a 06a_) -----------------------------

cat("\n─────────────────────────────────────────────────────\n")
cat("⚙️  3. Parámetros (idénticos a 06a_)\n")
cat("─────────────────────────────────────────────────────\n")

set.seed(SEED_GLOBAL)

N_MAX_MLE    <- 20000L
K_NODES      <- 9L
MAXIT_A      <- 150L
MAXIT_B      <- 1200L
TOL          <- 1e-6
N_MIN_CORE   <- 5000L

PROXIES_TODAS <- c(
  "rezago_escolar_cohorte",
  "clima_educativo_hogar",
  "emparejamiento_selectivo",
  "calificacion_norm",
  "entropia_estabilidad",
  "residual_vivienda",
  "busqueda_formal"
)

FACTOR_STRUCTURE <- list(
  theta_A = c("rezago_escolar_cohorte", "clima_educativo_hogar",
              "emparejamiento_selectivo", "calificacion_norm"),
  theta_B = c("entropia_estabilidad", "residual_vivienda", "busqueda_formal")
)

enableJIT(3)

cat("   N_MAX_MLE =", format(N_MAX_MLE, big.mark = ","), "\n")
cat("   K_NODES   =", K_NODES, "(", K_NODES^2, "nodos 2D)\n")
cat("   MAXIT_A   =", MAXIT_A, "| MAXIT_B =", MAXIT_B, "\n")
cat("   N_THREADS =", N_THREADS, "\n")
cat("   GPU chunk =", format(GPU_CHUNK_SIZE, big.mark = ","), "obs\n")


# 🪫 4. Funciones auxiliares (idénticas a 06a_) ------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

row_max <- function(mat) {
  if (HAS_MATRIXSTATS) {
    tryCatch(matrixStats::rowMaxs(mat), error = function(e) apply(mat, 1, max))
  } else {
    apply(mat, 1, max)
  }
}

row_sums <- function(mat) {
  if (HAS_MATRIXSTATS) {
    tryCatch(matrixStats::rowSums2(mat), error = function(e) rowSums(mat))
  } else {
    rowSums(mat)
  }
}

crear_dummies_geo <- function(geo_chr, geo_levels, drop_base = TRUE) {
  f <- factor(geo_chr, levels = geo_levels)
  X <- model.matrix(~ f - 1, na.action = na.pass)
  colnames(X) <- paste0("geo_", seq_len(ncol(X)))
  if (drop_base && ncol(X) >= 2) X <- X[, -1, drop = FALSE]
  X
}


# 🪫 5. Carga de datos -------------------------------------------------------

cat("\n─────────────────────────────────────────────────────\n")
cat("📂 5. Carga de datos\n")
cat("─────────────────────────────────────────────────────\n")

hard_stop(file.exists(PATH_04_PANEL_PROXIES),
          paste0("04_panel_con_proxies.rds no encontrado en: ", DIR_DATOS))
hard_stop(file.exists(PATH_06_THETA),
          "06_theta_predichos.rds no encontrado. Ejecutar 06a_ primero.")
hard_stop(file.exists(PATH_06_MODELO_HETERO),
          "06_modelo_heterofactor.rds no encontrado. Ejecutar 06a_ primero.")

panel <- readRDS(PATH_04_PANEL_PROXIES)
cat("✅ Panel cargado:", format(nrow(panel), big.mark = ","), "obs ×",
    ncol(panel), "vars\n")

proxies_faltantes <- setdiff(PROXIES_TODAS, names(panel))
hard_stop(length(proxies_faltantes) == 0,
          paste0("Proxies faltantes: ", paste(proxies_faltantes, collapse = ", ")))
cat("✅ 7 proxies presentes\n")


# 🪫 6. Ingeniería de variables (completa sobre panel total) -----------------
# CRÍTICO: safe_scale() usa media/sd del panel COMPLETO → misma escala que 06a_

cat("\n─────────────────────────────────────────────────────\n")
cat("🔨 6. Ingeniería de variables (panel completo para escala consistente)\n")
cat("─────────────────────────────────────────────────────\n")

datos_model <- panel %>%
  mutate(
    sexo_num = case_when(
      as.character(sexo) == "Varones" ~ 0,
      as.character(sexo) == "Mujeres" ~ 1,
      TRUE ~ NA_real_
    ),
    edad_scaled    = safe_scale(as.numeric(edad)),
    edad_sq        = edad_scaled^2,
    region_chr     = as.character(region),
    aglomerado_chr = as.character(aglomerado),
    log_ich        = log(pmax(as.numeric(ich_score), 1))
  ) %>%
  mutate(across(all_of(PROXIES_TODAS), ~ safe_scale(as.numeric(.x))))

cat("✅ datos_model:", format(nrow(datos_model), big.mark = ","), "obs\n")

# ── Restricción del período ────────────────────────────────────────────────────
datos_restringido <- datos_model %>%
  filter(periodo_id < PERIODO_EXCLUIR_DESDE)

n_full  <- nrow(datos_model)
n_restr <- nrow(datos_restringido)
cat("   Panel completo:    ", format(n_full,  big.mark = ","), "obs\n")
cat("   Panel restringido: ", format(n_restr, big.mark = ","), "obs (excluyendo",
    paste(TRIMESTRES_FORMALIDAD, collapse = "/"), ")\n")
cat("   Obs excluidas:     ", format(n_full - n_restr, big.mark = ","), "\n\n")

rm(panel); gc()


# 🪫 7. Función de estimación (copia exacta de 06a_) -------------------------

cat("─────────────────────────────────────────────────────\n")
cat("⚙️  7. Definiendo estimar_heterofactor_bifactor()\n")
cat("─────────────────────────────────────────────────────\n")

estimar_heterofactor_bifactor <- function(datos,
                                          proxies,
                                          factor_structure,
                                          covars,
                                          geo_var,
                                          maxit,
                                          modelo_nombre = "Modelo") {

  cat("\n────────────────────────────────────────────────────────\n")
  cat("🔧 ESTIMANDO:", modelo_nombre, "\n")
  cat("────────────────────────────────────────────────────────\n")

  n_cog   <- rowSums(!is.na(datos[, factor_structure$theta_A]))
  n_socio <- rowSums(!is.na(datos[, factor_structure$theta_B]))

  base_core <- datos %>%
    mutate(.n_cog = n_cog, .n_socio = n_socio) %>%
    filter(
      !is.na(sexo_num),
      !is.na(edad_scaled),
      !is.na(!!sym(geo_var)),
      !!sym(geo_var) != "",
      .n_cog   >= 3,
      .n_socio >= 2
    ) %>%
    select(-.n_cog, -.n_socio)

  if ("log_ich" %in% covars) {
    base_core <- base_core %>% filter(!is.na(log_ich))
  }
  if ("aglomerado_chr" %in% covars) {
    base_core <- base_core %>% filter(!is.na(aglomerado_chr), aglomerado_chr != "")
  }

  cat("   base_core:", format(nrow(base_core), big.mark = ","), "obs\n")
  cat("   (filtro: n_cog≥3 AND n_socio≥2)\n")

  for (p in proxies) {
    pna <- round(100 * mean(is.na(base_core[[p]])), 1)
    if (pna > 0) cat(sprintf("   NA en base_core — %-28s: %s%% (FIML)\n", p, pna))
  }

  hard_stop(nrow(base_core) >= N_MIN_CORE,
            paste0("base_core muy chica (N=", nrow(base_core), " < ", N_MIN_CORE, ")"))

  base_mle <- base_core

  if (nrow(base_mle) > N_MAX_MLE) {
    cat("   Muestreo estratificado por", geo_var, "...\n")

    plan_muestreo <- base_mle %>%
      count(!!sym(geo_var), name = "n_disp") %>%
      mutate(
        n_target = pmax(1L, as.integer(round(N_MAX_MLE * (n_disp / sum(n_disp))))),
        n_sample = pmin(n_target, n_disp)
      )

    base_mle <- base_mle %>%
      left_join(plan_muestreo %>% select(!!sym(geo_var), n_sample), by = geo_var) %>%
      group_split(!!sym(geo_var), .keep = TRUE) %>%
      purrr::map_dfr(function(df_g) {
        n_s <- max(1L, min(as.integer(df_g$n_sample[1]), nrow(df_g)))
        df_g %>% slice_sample(n = n_s)
      }) %>%
      select(-n_sample)

    cat("   Post-muestreo:", format(nrow(base_mle), big.mark = ","), "\n")
  } else {
    cat("   Sin muestreo (N ≤ N_MAX_MLE)\n")
  }

  geo_levels <- sort(unique(base_core[[geo_var]]))
  hard_stop(length(geo_levels) >= 2,
            paste0(geo_var, " insuficiente (levels < 2)"))

  armar_X <- function(df, geo_lvls) {
    X_geo  <- crear_dummies_geo(df[[geo_var]], geo_lvls, drop_base = TRUE)
    X_list <- list(df %>% select(sexo_num, edad_scaled, edad_sq),
                   as.data.frame(X_geo))
    if ("log_ich" %in% covars)
      X_list <- c(X_list, list(df %>% select(log_ich)))
    if ("aglomerado_chr" %in% covars) {
      aglo_lvls <- sort(unique(base_core$aglomerado_chr))
      X_aglo    <- crear_dummies_geo(df$aglomerado_chr, aglo_lvls, drop_base = TRUE)
      colnames(X_aglo) <- paste0("aglo_", seq_len(ncol(X_aglo)))
      X_list <- c(X_list, list(as.data.frame(X_aglo)))
    }
    as.matrix(bind_cols(X_list))
  }

  X <- armar_X(base_mle, geo_levels)
  M <- as.matrix(base_mle[, proxies])

  N   <- nrow(X)
  n_M <- ncol(M)
  n_X <- ncol(X)

  cat("   Dimensiones MLE: N =", format(N, big.mark = ","),
      "| n_M =", n_M, "| n_X =", n_X, "\n")

  gh         <- gaussHermiteData(K_NODES)
  nodes_1d   <- gh$x * sqrt(2)
  weights_1d <- gh$w / sqrt(pi)

  grid_2d    <- expand.grid(theta_A = nodes_1d, theta_B = nodes_1d)
  K_nodes_2d <- nrow(grid_2d)

  weights_2d <- expand.grid(w_A = weights_1d, w_B = weights_1d) %>%
                  mutate(w = w_A * w_B) %>%
                  pull(w)

  log_prior_2d <- dnorm(grid_2d$theta_A, 0, 1, log = TRUE) +
                  dnorm(grid_2d$theta_B, 0, 1, log = TRUE)

  theta_A_grid  <- matrix(rep(grid_2d$theta_A, each = N), nrow = N, ncol = K_nodes_2d)
  theta_B_grid  <- matrix(rep(grid_2d$theta_B, each = N), nrow = N, ncol = K_nodes_2d)
  w_mat         <- matrix(rep(weights_2d,       each = N), nrow = N, ncol = K_nodes_2d)
  log_prior_mat <- matrix(rep(log_prior_2d,     each = N), nrow = N, ncol = K_nodes_2d)

  cat("   K_nodes_2D =", K_nodes_2d, "\n")

  LOG_PRIOR_2D   <- log_prior_2d
  LOG_WEIGHTS_2D <- log(weights_2d)

  carga_en_A <- proxies %in% factor_structure$theta_A
  carga_en_B <- proxies %in% factor_structure$theta_B

  eval_count <- 0
  last_print <- 0

  ll_bifactor <- function(par) {
    eval_count <<- eval_count + 1
    if (eval_count - last_print >= 10) {
      cat(".")
      last_print <<- eval_count
    }

    if (any(!is.finite(par))) return(-1e12)

    idx <- 1
    alpha_m      <- par[idx:(idx + n_M - 1)]; idx <- idx + n_M
    alpha_A_free <- par[idx:(idx + sum(carga_en_A) - 1)]; idx <- idx + sum(carga_en_A)
    alpha_B_free <- par[idx:(idx + sum(carga_en_B) - 1)]; idx <- idx + sum(carga_en_B)

    alpha_A_full <- numeric(n_M)
    alpha_A_full[carga_en_A] <- alpha_A_free
    alpha_B_full <- numeric(n_M)
    alpha_B_full[carga_en_B] <- alpha_B_free

    beta_m_vec <- par[idx:(idx + n_X * n_M - 1)]; idx <- idx + n_X * n_M
    beta_m     <- matrix(beta_m_vec, nrow = n_X, ncol = n_M)
    sigma_eps  <- exp(par[idx:(idx + n_M - 1)])

    if (any(sigma_eps < 1e-6) || any(sigma_eps > 1e6)) return(-1e12)

    tryCatch(
      compute_ll_full_cpp(
        M            = M,
        X            = X,
        alpha_m      = alpha_m,
        beta_m       = beta_m,
        theta_A_grid = theta_A_grid,
        theta_B_grid = theta_B_grid,
        alpha_A      = alpha_A_full,
        alpha_B      = alpha_B_full,
        sigma_eps    = sigma_eps,
        log_prior    = LOG_PRIOR_2D,
        log_weights  = LOG_WEIGHTS_2D,
        n_threads    = N_THREADS
      ),
      error = function(e) -1e12
    )
  }

  ll_bifactor <- cmpfun(ll_bifactor)

  cat("   Verificando compute_ll_full_cpp...\n")
  .par_test <- c(colMeans(M, na.rm = TRUE),
                 rep(0.3, sum(carga_en_A)),
                 rep(0.3, sum(carga_en_B)),
                 rep(0, n_X * n_M),
                 log(pmax(apply(M, 2, sd, na.rm = TRUE), 0.1)))
  .par_test[!is.finite(.par_test)] <- 0

  .idx <- 1
  .am  <- .par_test[.idx:(.idx + n_M - 1)];                    .idx <- .idx + n_M
  .aAf <- .par_test[.idx:(.idx + sum(carga_en_A) - 1)];        .idx <- .idx + sum(carga_en_A)
  .aBf <- .par_test[.idx:(.idx + sum(carga_en_B) - 1)];        .idx <- .idx + sum(carga_en_B)
  .bv  <- .par_test[.idx:(.idx + n_X * n_M - 1)];              .idx <- .idx + n_X * n_M
  .bm  <- matrix(.bv, nrow = n_X, ncol = n_M)
  .sig <- exp(.par_test[.idx:(.idx + n_M - 1)])
  .aA  <- numeric(n_M); .aA[carga_en_A] <- .aAf
  .aB  <- numeric(n_M); .aB[carga_en_B] <- .aBf

  .ll_orig <- tryCatch({
    .mhat <- matrix(rep(.am, each = N), N, n_M) + X %*% .bm
    lmg   <- compute_log_m_grid_cpp(M, .mhat, theta_A_grid, theta_B_grid,
                                    .aA, .aB, .sig, N_THREADS)
    lint  <- lmg + log_prior_mat
    mx    <- row_max(lint)
    sum(mx + log(pmax(row_sums(exp(lint - mx) * w_mat), 1e-300)))
  }, error = function(e) NA_real_)

  .ll_new <- tryCatch(ll_bifactor(.par_test), error = function(e) NA_real_)
  .diff   <- abs(.ll_orig - .ll_new)
  cat(sprintf("   |ll_orig - ll_new| = %.2e  %s\n",
              .diff, if (is.finite(.diff) && .diff < 1e-6) "\u2705 OK" else "\u274c REVISAR"))
  stopifnot("compute_ll_full_cpp no coincide con funcion original" =
            is.finite(.diff) && .diff < 1e-6)
  rm(.par_test, .ll_orig, .ll_new, .diff, .idx, .am, .aAf, .aBf,
     .bv, .bm, .sig, .aA, .aB, .mhat, lmg, lint, mx)

  start_alpha_m <- colMeans(M, na.rm = TRUE)

  beta_m_ols <- tryCatch({
    B <- matrix(0, nrow = n_X, ncol = n_M)
    for (j in seq_len(n_M)) {
      fit   <- lm(M[, j] ~ X - 1, na.action = na.omit)
      coefs <- coef(fit)
      coefs[is.na(coefs)]      <- 0
      coefs[abs(coefs) > 10]   <- 0
      B[seq_along(coefs), j]   <- coefs
    }
    B
  }, error = function(e) matrix(0, nrow = n_X, ncol = n_M))

  sigma_eps_start <- pmax(apply(M, 2, sd, na.rm = TRUE), 0.1)
  par_start <- c(
    start_alpha_m,
    rep(0.3, sum(carga_en_A)),
    rep(0.3, sum(carga_en_B)),
    as.numeric(beta_m_ols),
    log(sigma_eps_start)
  )
  par_start[!is.finite(par_start)] <- 0

  cat("   BFGS (MAXIT =", maxit, ")...\n   ")
  t_opt_start <- Sys.time()

  res <- optim(
    par     = par_start,
    fn      = function(p) -ll_bifactor(p),
    method  = "BFGS",
    control = list(maxit = maxit, reltol = TOL)
  )

  t_opt_min <- as.numeric(difftime(Sys.time(), t_opt_start, units = "mins"))
  cat("\n")

  conv_icon <- ifelse(res$convergence == 0, "✅", "⚠️")
  cat("  ", conv_icon, "Convergencia:", res$convergence,
      "| LogLik:", round(-res$value, 2),
      "| Tiempo:", round(t_opt_min, 1), "min\n")

  soft_warn(res$convergence == 0,
            paste0("BFGS no convergió (code=", res$convergence, "). ",
                   "Considerar MAXIT mayor."))

  par_hat <- res$par
  idx <- 1
  alpha_m_hat      <- par_hat[idx:(idx + n_M - 1)]; idx <- idx + n_M
  alpha_A_free_hat <- par_hat[idx:(idx + sum(carga_en_A) - 1)]; idx <- idx + sum(carga_en_A)
  alpha_B_free_hat <- par_hat[idx:(idx + sum(carga_en_B) - 1)]; idx <- idx + sum(carga_en_B)
  beta_m_vec_hat   <- par_hat[idx:(idx + n_X * n_M - 1)]; idx <- idx + n_X * n_M
  beta_m_hat       <- matrix(beta_m_vec_hat, nrow = n_X, ncol = n_M)
  sigma_eps_hat    <- exp(par_hat[idx:(idx + n_M - 1)])

  alpha_A_hat <- numeric(n_M); alpha_A_hat[carga_en_A] <- alpha_A_free_hat
  alpha_B_hat <- numeric(n_M); alpha_B_hat[carga_en_B] <- alpha_B_free_hat

  cat("   Cargas θ_A:", paste(round(alpha_A_free_hat, 3), collapse = ", "), "\n")
  cat("   Cargas θ_B:", paste(round(alpha_B_free_hat, 3), collapse = ", "), "\n")

  cat("   Scoring GPU (base_core:", format(nrow(base_core), big.mark = ","), "obs)...\n")
  t_score_start <- Sys.time()

  X_sc     <- armar_X(base_core, geo_levels)
  M_sc     <- as.matrix(base_core[, proxies])

  Nn       <- nrow(X_sc)
  m_hat_sc <- matrix(rep(alpha_m_hat, each = Nn), nrow = Nn, ncol = n_M) +
              X_sc %*% beta_m_hat

  log_m_grid_sc_np <- py$scoring_gpu(
    M_np              = M_sc,
    m_hat_np          = m_hat_sc,
    theta_A_nodes_np  = grid_2d$theta_A[seq_len(K_NODES)],
    theta_B_nodes_np  = grid_2d$theta_B[seq(1, K_nodes_2d, by = K_NODES)],
    alpha_A_np        = alpha_A_hat,
    alpha_B_np        = alpha_B_hat,
    sigma_eps_np      = sigma_eps_hat,
    chunk_size        = GPU_CHUNK_SIZE,
    device            = GPU_DEVICE
  )

  log_m_grid_sc <- py_to_r(log_m_grid_sc_np)

  log_prior_sc  <- matrix(rep(log_prior_2d, each = Nn), nrow = Nn, ncol = K_nodes_2d)
  w_mat_sc      <- matrix(rep(weights_2d,   each = Nn), nrow = Nn, ncol = K_nodes_2d)

  theta_A_grid_sc <- matrix(rep(grid_2d$theta_A, each = Nn), nrow = Nn, ncol = K_nodes_2d)
  theta_B_grid_sc <- matrix(rep(grid_2d$theta_B, each = Nn), nrow = Nn, ncol = K_nodes_2d)

  log_post_kernel <- log_m_grid_sc + log_prior_sc
  maxrow_sc       <- row_max(log_post_kernel)
  post_unnorm     <- exp(log_post_kernel - maxrow_sc) * w_mat_sc
  den_sc          <- pmax(row_sums(post_unnorm), 1e-300)

  theta_A_eb <- row_sums(post_unnorm * theta_A_grid_sc) / den_sc
  theta_B_eb <- row_sums(post_unnorm * theta_B_grid_sc) / den_sc

  t_score_min <- as.numeric(difftime(Sys.time(), t_score_start, units = "mins"))
  cat("   ✅ Scoring completo:", format(Nn, big.mark = ","), "obs |",
      round(t_score_min, 1), "min\n")

  list(
    nombre      = modelo_nombre,
    covariables = covars,
    meta = list(
      timestamp        = format(Sys.time(), "%Y%m%d_%H%M%S"),
      N_core           = nrow(base_core),
      N_mle            = nrow(base_mle),
      N_scoring        = Nn,
      proxies          = proxies,
      factor_structure = factor_structure,
      geo_var          = geo_var,
      geo_levels       = geo_levels,
      K_nodes_1D       = K_NODES,
      K_nodes_2D       = K_nodes_2d,
      maxit            = maxit,
      config           = paste0("Rcpp+OpenMP (MLE FIML) + PyTorch GPU ", GPU_DEVICE, " (scoring)")
    ),
    optim        = res,
    convergencia = res$convergence,
    loglik       = -res$value,
    params = list(
      alpha_m   = alpha_m_hat,
      alpha_A   = alpha_A_hat,
      alpha_B   = alpha_B_hat,
      beta_m    = beta_m_hat,
      sigma_eps = sigma_eps_hat
    ),
    theta_data = base_core %>%
      select(id_individuo_hist, periodo_id) %>%
      mutate(theta_A = theta_A_eb, theta_B = theta_B_eb)
  )
}

cat("✅ estimar_heterofactor_bifactor() definida\n")


# 🪫 8. Estimación Modelo A (restringido) ------------------------------------

cat("\n═══════════════════════════════════════════════════════\n")
cat("🎯 8. MODELO A RESTRINGIDO: PARSIMONIOSO (MAXIT =", MAXIT_A, ")\n")
cat("═══════════════════════════════════════════════════════\n")

tic("Modelo A restringido")
modelo_A_restr <- estimar_heterofactor_bifactor(
  datos            = datos_restringido,
  proxies          = PROXIES_TODAS,
  factor_structure = FACTOR_STRUCTURE,
  covars           = c("sexo_num", "edad_scaled", "edad_sq"),
  geo_var          = "region_chr",
  maxit            = MAXIT_A,
  modelo_nombre    = "Modelo A Restringido (Parsimonioso)"
)
t_A_r <- toc(quiet = TRUE)
cat("⏱️  Modelo A restringido:", round((t_A_r$toc - t_A_r$tic) / 60, 1), "min\n\n")


# 🪫 9. Estimación Modelo B (restringido) ------------------------------------

cat("═══════════════════════════════════════════════════════\n")
cat("🎯 9. MODELO B RESTRINGIDO: COMPLETO (MAXIT =", MAXIT_B, ", ~2-3 hs)\n")
cat("═══════════════════════════════════════════════════════\n")

tic("Modelo B restringido")
modelo_B_restr <- estimar_heterofactor_bifactor(
  datos            = datos_restringido,
  proxies          = PROXIES_TODAS,
  factor_structure = FACTOR_STRUCTURE,
  covars           = c("sexo_num", "edad_scaled", "edad_sq", "log_ich", "aglomerado_chr"),
  geo_var          = "region_chr",
  maxit            = MAXIT_B,
  modelo_nombre    = "Modelo B Restringido (Completo)"
)
t_B_r <- toc(quiet = TRUE)
cat("⏱️  Modelo B restringido:", round((t_B_r$toc - t_B_r$tic) / 60, 1), "min\n\n")


# 🪫 10. Likelihood Ratio Test (restringido) ---------------------------------

cat("═══════════════════════════════════════════════════════\n")
cat("📊 10. LR Test — selección Modelo A vs B (restringido)\n")
cat("═══════════════════════════════════════════════════════\n")

loglik_A_r   <- modelo_A_restr$loglik
loglik_B_r   <- modelo_B_restr$loglik
n_params_A_r <- length(modelo_A_restr$optim$par)
n_params_B_r <- length(modelo_B_restr$optim$par)
df_diff_r    <- n_params_B_r - n_params_A_r
LR_stat_r    <- 2 * (loglik_B_r - loglik_A_r)
p_value_r    <- pchisq(LR_stat_r, df = df_diff_r, lower.tail = FALSE)

cat("   LogLik A:", round(loglik_A_r, 2), "(", n_params_A_r, "params)\n")
cat("   LogLik B:", round(loglik_B_r, 2), "(", n_params_B_r, "params)\n")
cat("   LR stat: ", round(LR_stat_r, 2), "| df:", df_diff_r,
    "| p-value:", format(p_value_r, scientific = TRUE, digits = 3), "\n\n")

if (p_value_r < 0.05) {
  cat("✅ DECISIÓN: Modelo B restringido → usar Modelo B\n\n")
  modelo_final_restr        <- modelo_B_restr
  modelo_final_restr_nombre <- "B"
} else {
  cat("✅ DECISIÓN: Modelo A restringido (parsimonia)\n\n")
  modelo_final_restr        <- modelo_A_restr
  modelo_final_restr_nombre <- "A"
}


# 🪫 Guardar modelo restringido -----------------------------------------------

saveRDS(
  list(
    modelo_A      = modelo_A_restr,
    modelo_B      = modelo_B_restr,
    modelo_final  = modelo_final_restr,
    nombre_final  = modelo_final_restr_nombre,
    periodo_excluido = TRIMESTRES_FORMALIDAD,
    lr_test = list(LR = LR_stat_r, df = df_diff_r, p_value = p_value_r)
  ),
  PATH_ROBUSTNESS_MODELO
)
cat("✅ Modelo restringido guardado:", basename(PATH_ROBUSTNESS_MODELO), "\n")

theta_restringido <- modelo_final_restr$theta_data
saveRDS(theta_restringido, PATH_ROBUSTNESS_THETA)
cat("✅ Thetas restringidos guardados:", basename(PATH_ROBUSTNESS_THETA), "\n\n")


# 🪫 11. Comparación con modelo original -------------------------------------

cat("═══════════════════════════════════════════════════════\n")
cat("🔍 11. Comparación con modelo original\n")
cat("═══════════════════════════════════════════════════════\n")

# Cargar thetas y modelo original
panel_original    <- readRDS(PATH_06_THETA)
modelo_original   <- readRDS(PATH_06_MODELO_HETERO)
modelo_orig_final <- modelo_original$modelo_final

theta_original <- panel_original %>%
  filter(!is.na(theta_A), !is.na(theta_B)) %>%
  select(id_individuo_hist, periodo_id, theta_A, theta_B)

cat("   θ originales disponibles:   ", format(nrow(theta_original),    big.mark = ","), "\n")
cat("   θ restringidos disponibles: ", format(nrow(theta_restringido), big.mark = ","), "\n")

# Join: solo obs presentes en ambos (inner join)
comparacion <- inner_join(
  theta_restringido %>% rename(theta_A_restr = theta_A, theta_B_restr = theta_B),
  theta_original    %>% rename(theta_A_orig  = theta_A, theta_B_orig  = theta_B),
  by = c("id_individuo_hist", "periodo_id")
) %>%
  mutate(
    anio = as.integer(substr(as.character(periodo_id), 1, 4)),
    trim = as.integer(substr(as.character(periodo_id), 5, 5)),
    periodo_label = paste0(anio, "_T", trim)
  )

cat("   Obs en comparación:         ", format(nrow(comparacion), big.mark = ","), "\n\n")

# ── Correlaciones ────────────────────────────────────────────────────────────

cor_A_pearson  <- cor(comparacion$theta_A_orig, comparacion$theta_A_restr,
                      use = "complete.obs", method = "pearson")
cor_A_spearman <- cor(comparacion$theta_A_orig, comparacion$theta_A_restr,
                      use = "complete.obs", method = "spearman")
rmse_A <- sqrt(mean((comparacion$theta_A_orig - comparacion$theta_A_restr)^2, na.rm = TRUE))

cor_B_pearson  <- cor(comparacion$theta_B_orig, comparacion$theta_B_restr,
                      use = "complete.obs", method = "pearson")
cor_B_spearman <- cor(comparacion$theta_B_orig, comparacion$theta_B_restr,
                      use = "complete.obs", method = "spearman")
rmse_B <- sqrt(mean((comparacion$theta_B_orig - comparacion$theta_B_restr)^2, na.rm = TRUE))

cat("   Correlaciones θ_A (orig vs restringido):\n")
cat("     Pearson  =", round(cor_A_pearson,  4), "\n")
cat("     Spearman =", round(cor_A_spearman, 4), "\n")
cat("     RMSE     =", round(rmse_A,         4), "\n\n")

cat("   Correlaciones θ_B (orig vs restringido):\n")
cat("     Pearson  =", round(cor_B_pearson,  4), "\n")
cat("     Spearman =", round(cor_B_spearman, 4), "\n")
cat("     RMSE     =", round(rmse_B,         4), "\n\n")

# ── Tabla de correlaciones para CSV ─────────────────────────────────────────

tbl_cor <- data.frame(
  Factor   = c("theta_A (Cognitivo)", "theta_B (Socioemocional)"),
  Pearson  = round(c(cor_A_pearson,  cor_B_pearson),  4),
  Spearman = round(c(cor_A_spearman, cor_B_spearman), 4),
  RMSE     = round(c(rmse_A,         rmse_B),          4),
  N_obs    = nrow(comparacion)
)

# ── Comparación de cargas factoriales ────────────────────────────────────────

make_loadings_df <- function(modelo, label) {
  params    <- modelo$params
  alpha_A   <- params$alpha_A
  alpha_B   <- params$alpha_B
  data.frame(
    Proxy    = PROXIES_TODAS,
    Factor   = ifelse(PROXIES_TODAS %in% FACTOR_STRUCTURE$theta_A, "theta_A", "theta_B"),
    Carga    = ifelse(PROXIES_TODAS %in% FACTOR_STRUCTURE$theta_A, alpha_A, alpha_B),
    Modelo   = label,
    stringsAsFactors = FALSE
  )
}

df_loadings <- bind_rows(
  make_loadings_df(modelo_orig_final,   "Original"),
  make_loadings_df(modelo_final_restr,  "Restringido")
) %>%
  pivot_wider(names_from = Modelo, values_from = Carga) %>%
  mutate(Diferencia = round(Restringido - Original, 4),
         Original   = round(Original, 4),
         Restringido = round(Restringido, 4))

cat("   Comparación de cargas factoriales:\n")
print(df_loadings)
cat("\n")

# ── Serie temporal: media θ por período ──────────────────────────────────────

ts_data <- comparacion %>%
  group_by(periodo_label) %>%
  summarise(
    theta_A_orig  = mean(theta_A_orig,  na.rm = TRUE),
    theta_A_restr = mean(theta_A_restr, na.rm = TRUE),
    theta_B_orig  = mean(theta_B_orig,  na.rm = TRUE),
    theta_B_restr = mean(theta_B_restr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(periodo_label)

# ── Exportar CSV ─────────────────────────────────────────────────────────────

write.csv(tbl_cor,    PATH_ROBUSTNESS_CSV, row.names = FALSE)
cat("✅ CSV guardado:", basename(PATH_ROBUSTNESS_CSV), "\n")


# 🪫 12. Figuras para el reporte ---------------------------------------------

cat("\n─────────────────────────────────────────────────────\n")
cat("📊 12. Generando figuras\n")
cat("─────────────────────────────────────────────────────\n")

# Función auxiliar: R² en scatter
r2_label <- function(x, y) {
  r2 <- cor(x, y, use = "complete.obs")^2
  sprintf("r = %.4f | R² = %.4f", sqrt(r2), r2)
}

# ── Fig 1: Scatter θ_A ────────────────────────────────────────────────────────
fig_A <- ggplot(comparacion %>% slice_sample(n = min(50000, nrow(comparacion))),
                aes(x = theta_A_orig, y = theta_A_restr)) +
  geom_point(alpha = 0.15, size = 0.6, color = COL_GLM) +
  geom_abline(slope = 1, intercept = 0, color = COL_OBSERVADO, linewidth = 0.8, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = COL_SLS, linewidth = 0.7) +
  tr_labs(
    title    = expression(paste("Robustness: ", theta[A], " (Cognitivo)")),
    subtitle = paste0(r2_label(comparacion$theta_A_orig, comparacion$theta_A_restr),
                      "  |  N = ", format(nrow(comparacion), big.mark = ",")),
    x = expression(paste(theta[A], " — Modelo Original (2016Q4-2025Q3)")),
    y = expression(paste(theta[A], " — Modelo Restringido (2016Q4-2024Q3)"))
  ) +
  theme_paper()

# ── Fig 2: Scatter θ_B ────────────────────────────────────────────────────────
fig_B <- ggplot(comparacion %>% slice_sample(n = min(50000, nrow(comparacion))),
                aes(x = theta_B_orig, y = theta_B_restr)) +
  geom_point(alpha = 0.15, size = 0.6, color = COL_LPM) +
  geom_abline(slope = 1, intercept = 0, color = COL_OBSERVADO, linewidth = 0.8, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = COL_SLS, linewidth = 0.7) +
  tr_labs(
    title    = expression(paste("Robustness: ", theta[B], " (Socioemocional)")),
    subtitle = paste0(r2_label(comparacion$theta_B_orig, comparacion$theta_B_restr),
                      "  |  N = ", format(nrow(comparacion), big.mark = ",")),
    x = expression(paste(theta[B], " — Modelo Original (2016Q4-2025Q3)")),
    y = expression(paste(theta[B], " — Modelo Restringido (2016Q4-2024Q3)"))
  ) +
  theme_paper()

# ── Fig 3: Serie temporal — media θ_A y θ_B por período ──────────────────────
ts_long <- ts_data %>%
  pivot_longer(cols = c(theta_A_orig, theta_A_restr, theta_B_orig, theta_B_restr),
               names_to = "serie", values_to = "valor") %>%
  mutate(
    Factor = ifelse(grepl("theta_A", serie), "theta_A (Cognitivo)", "theta_B (Socioemocional)"),
    Modelo = ifelse(grepl("_orig", serie), "Original", "Restringido")
  )

fig_ts <- ggplot(ts_long, aes(x = periodo_label, y = valor,
                               color = Modelo, group = Modelo)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ Factor, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Original" = COL_GLM, "Restringido" = COL_SLS)) +
  tr_labs(
    title    = "Media trimestral: factores latentes — original vs restringido",
    subtitle = paste0("Original: 2016Q4-2025Q3 | Restringido: 2016Q4-2024Q3",
                      " (sin ", paste(TRIMESTRES_FORMALIDAD, collapse = "/"), ")"),
    x = "Trimestre", y = "Media θ (Empirical Bayes)", color = "Muestra de estimación"
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# ── Fig 4: Cargas factoriales (original vs restringido) ───────────────────────
df_load_long <- df_loadings %>%
  pivot_longer(cols = c(Original, Restringido),
               names_to = "Modelo", values_to = "Carga")

fig_load <- ggplot(df_load_long,
                   aes(x = reorder(Proxy, Carga), y = Carga, fill = Modelo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  facet_wrap(~ Factor, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("Original" = COL_GLM, "Restringido" = COL_SLS)) +
  coord_flip() +
  tr_labs(
    title    = "Cargas factoriales: original vs restringido",
    subtitle = "Diferencias pequeñas = robustez de la estructura factorial",
    x = NULL, y = "Carga estimada (α)",
    fill = "Muestra de estimación"
  ) +
  theme_paper()

cat("✅ 4 figuras generadas\n")


# 🪫 13. Reporte HTML --------------------------------------------------------

cat("\n─────────────────────────────────────────────────────\n")
cat("📝 13. Generando reporte HTML\n")
cat("─────────────────────────────────────────────────────\n")

# Exportar figuras a PDF (cairo_pdf via guardar_figura)
guardar_figura(fig_A,    DIR_FIGURAS_10A, "scatter",  1, width = ANCHO_FIG, height = ANCHO_FIG * 0.75)
guardar_figura(fig_B,    DIR_FIGURAS_10A, "scatter",  2, width = ANCHO_FIG, height = ANCHO_FIG * 0.75)
guardar_figura(fig_ts,   DIR_FIGURAS_10A, "series",   3, width = ANCHO_FIG * 1.2, height = ALTO_FIG * 1.5)
guardar_figura(fig_load, DIR_FIGURAS_10A, "barras",   4, width = ANCHO_FIG * 1.1, height = ANCHO_FIG * 0.75)

# Guardar también PNGs en tempdir para incrustar en HTML (base64)
fig_dir <- tempdir()

fig_paths <- list(
  scatter_A = file.path(fig_dir, "rob_scatter_A.png"),
  scatter_B = file.path(fig_dir, "rob_scatter_B.png"),
  tseries   = file.path(fig_dir, "rob_tseries.png"),
  loadings  = file.path(fig_dir, "rob_loadings.png")
)

# HC documentado: PNGs temporales para HTML report — no para publicación
ggsave(fig_paths$scatter_A, fig_A,   width = 8, height = 6, dpi = 150, bg = "white")
ggsave(fig_paths$scatter_B, fig_B,   width = 8, height = 6, dpi = 150, bg = "white")
ggsave(fig_paths$tseries,   fig_ts,  width = 10, height = 7, dpi = 150, bg = "white")
ggsave(fig_paths$loadings,  fig_load, width = 9, height = 6, dpi = 150, bg = "white")

# Tabla HTML — correlaciones
tbl_cor_html <- kbl(tbl_cor, format = "html", align = "lrrrr",
                    col.names = c("Factor", "Pearson r", "Spearman ρ", "RMSE", "N obs"),
                    caption = "Tabla 1. Correlaciones: θ Original vs θ Restringido") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "left") %>%
  row_spec(0, bold = TRUE)

# Tabla HTML — cargas factoriales
tbl_load_html <- kbl(df_loadings, format = "html", align = "llrrr",
                     col.names = c("Proxy", "Factor", "Original", "Restringido", "Diferencia"),
                     caption = "Tabla 2. Cargas factoriales: Original vs Restringido") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "left") %>%
  row_spec(0, bold = TRUE)

# Tabla HTML — comparación de modelos
tbl_meta <- data.frame(
  Métrica    = c("Muestra estimación", "N base_core", "N MLE", "LogLik",
                 "Convergencia", "Modelo seleccionado"),
  Original   = c(
    paste0("2016Q4–2025Q3"),
    format(modelo_orig_final$meta$N_core, big.mark = ","),
    format(modelo_orig_final$meta$N_mle, big.mark = ","),
    round(modelo_orig_final$loglik, 2),
    ifelse(modelo_orig_final$convergencia == 0, "Sí", "No"),
    modelo_original$nombre_final
  ),
  Restringido = c(
    paste0("2016Q4–2024Q3 (sin ", paste(TRIMESTRES_FORMALIDAD, collapse = "/"), ")"),
    format(modelo_final_restr$meta$N_core, big.mark = ","),
    format(modelo_final_restr$meta$N_mle, big.mark = ","),
    round(modelo_final_restr$loglik, 2),
    ifelse(modelo_final_restr$convergencia == 0, "Sí", "No"),
    modelo_final_restr_nombre
  )
)

tbl_meta_html <- kbl(tbl_meta, format = "html", align = "lll",
                     caption = "Tabla 3. Comparación de modelos") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "left") %>%
  row_spec(0, bold = TRUE)

# Construir HTML via Rmd (estilo proyecto: flatly + toc_float)

# Guardar objetos para el Rmd en un .rds temporal
rds_temp <- tempfile(fileext = ".rds")
saveRDS(list(
  tbl_cor       = tbl_cor,
  tbl_meta      = tbl_meta,
  df_loadings   = df_loadings,
  cor_A_pearson  = cor_A_pearson,
  cor_A_spearman = cor_A_spearman,
  cor_B_pearson  = cor_B_pearson,
  cor_B_spearman = cor_B_spearman,
  rmse_A         = rmse_A,
  rmse_B         = rmse_B,
  fig_paths      = fig_paths,
  TRIMESTRES_FORMALIDAD = TRIMESTRES_FORMALIDAD,
  modelo_orig_final     = modelo_orig_final,
  modelo_final_restr    = modelo_final_restr,
  modelo_original       = modelo_original,
  modelo_final_restr_nombre = modelo_final_restr_nombre
), rds_temp)

rds_path_fwd <- gsub("\\\\", "/", rds_temp)
fig_scatter_A_fwd <- gsub("\\\\", "/", fig_paths$scatter_A)
fig_scatter_B_fwd <- gsub("\\\\", "/", fig_paths$scatter_B)
fig_tseries_fwd   <- gsub("\\\\", "/", fig_paths$tseries)
fig_loadings_fwd  <- gsub("\\\\", "/", fig_paths$loadings)

rmd_temp <- tempfile(fileext = ".Rmd")
con <- file(rmd_temp, open = "wt", encoding = "UTF-8")

# YAML
cat('---
title: "Robustness A1+: Heterofactor Bifactorial"
subtitle: "Proyecto EPH Argentina -- Formalidad Laboral 2016T4-2025T3 | Capa 7 | Robustez"
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

# SETUP chunk
cat(sprintf('```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,
                      fig.width=11, fig.height=5.5, dpi=150)
suppressPackageStartupMessages({
  library(tidyverse); library(knitr); library(kableExtra)
  library(ggplot2); library(patchwork); library(scales)
})
dat <- readRDS("%s")
list2env(dat, envir = environment())
source(here::here("script", "config", "funciones_comunes.R"))
```

', rds_path_fwd), file = con)

# Sección: Objetivo y parámetros
cat(sprintf('# Contexto {-}

**Objetivo:** Re-estimar el heterofactor excluyendo los 4 trimestres con
formalidad observada (%s).
Si los factores son estables, el modelo no está sobreajustado al período de training del LASSO.

- **Período original:** 2016Q4--2025Q3
- **Período restringido:** 2016Q4--2024Q3
- **Parámetros:** N_MAX_MLE = 20,000 | K_NODES = 9 | MAXIT_B = 1,200 | SEED = 123

', paste(TRIMESTRES_FORMALIDAD, collapse = ", ")), file = con)

# Sección 1: Comparación de modelos
cat('# Comparación de modelos

```{r tbl-meta}
kbl(tbl_meta, format = "html", align = "lll",
    caption = "Tabla 3. Comparación de modelos") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE)
```

', file = con)

# Sección 2: Correlaciones
cat('# Correlaciones: factores originales vs restringidos

```{r cor-highlight, results="asis"}
cat(sprintf("<p style=\\"font-size:1.15em; font-weight:bold; color:#1a6634;\\">
θ<sub>A</sub> (Cognitivo): Pearson r = %.4f | Spearman ρ = %.4f | RMSE = %.4f</p>",
  cor_A_pearson, cor_A_spearman, rmse_A))
cat(sprintf("<p style=\\"font-size:1.15em; font-weight:bold; color:#1a6634;\\">
θ<sub>B</sub> (Socioemocional): Pearson r = %.4f | Spearman ρ = %.4f | RMSE = %.4f</p>",
  cor_B_pearson, cor_B_spearman, rmse_B))
```

```{r tbl-cor}
kbl(tbl_cor, format = "html", align = "lrrrr",
    col.names = c("Factor", "Pearson r", "Spearman ρ", "RMSE", "N obs"),
    caption = "Tabla 1. Correlaciones: θ Original vs θ Restringido") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1, background = "#eaf4fc") %>%
  row_spec(2, background = "#fef9e7")
```

', file = con)

# Sección 3: Scatter θ_A
cat(sprintf('# Scatter: θ_A (Cognitivo)

Cada punto es una observación (id × trimestre). Línea roja = 45°; azul = regresión OLS.

```{r fig-scatter-a, fig.cap="Scatter θ_A: original vs restringido"}
knitr::include_graphics("%s")
```

', fig_scatter_A_fwd), file = con)

# Sección 4: Scatter θ_B
cat(sprintf('# Scatter: θ_B (Socioemocional)

```{r fig-scatter-b, fig.cap="Scatter θ_B: original vs restringido"}
knitr::include_graphics("%s")
```

', fig_scatter_B_fwd), file = con)

# Sección 5: Serie temporal
cat(sprintf('# Serie temporal: media trimestral de θ

Media ponderada por obs en cada trimestre. Superposición visual de ambas series.

```{r fig-tseries, fig.cap="Serie temporal de θ: original vs restringido"}
knitr::include_graphics("%s")
```

', fig_tseries_fwd), file = con)

# Sección 6: Cargas factoriales
cat(sprintf('# Cargas factoriales

```{r tbl-loadings}
kbl(df_loadings, format = "html", align = "llrrr",
    col.names = c("Proxy", "Factor", "Original", "Restringido", "Diferencia"),
    caption = "Tabla 2. Cargas factoriales: Original vs Restringido") %%>%%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center", font_size = 11) %%>%%
  row_spec(0, bold = TRUE)
```

```{r fig-loadings, fig.cap="Cargas factoriales: original vs restringido"}
knitr::include_graphics("%s")
```

', fig_loadings_fwd), file = con)

# Sección 7: Conclusión
cat('# Conclusión

La correlación entre factores original y restringido indica si el heterofactor
es estable ante la exclusión del período de training.

```{r conclusion, results="asis"}
cat(sprintf("- **Correlación Pearson θ_A:** %.4f\\n", cor_A_pearson))
cat(sprintf("- **Correlación Pearson θ_B:** %.4f\\n", cor_B_pearson))
```

> Un r > 0.99 confirma que la estimación del heterofactor no está contaminada
> por la disponibilidad de formalidad observada en 2024Q4--2025Q3.

---

<small>Script: 10a_robustness_heterofactor.R | Proyecto: formalidad_rev | Referato R1 — Observación A1+</small>
', file = con)

close(con)

rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_ROBUSTNESS_HTML,
  quiet       = TRUE,
  envir       = new.env(parent = globalenv())
)

unlink(c(rmd_temp, rds_temp))
cat("Reporte HTML guardado:", basename(PATH_ROBUSTNESS_HTML), "\n")


# 📜 Contrato de validación ---------------------------------------------------

cat("\n─────────────────────────────────────────────────────\n")
cat("📜 Generando contrato de validación\n")
cat("─────────────────────────────────────────────────────\n")

# Diferencias máximas en cargas factoriales
max_diff_carga <- max(abs(df_loadings$Diferencia), na.rm = TRUE)

PATH_CONTRATO_10A <- file.path(DIR_CONTRATOS, "10a_contrato_robustness_hetero.rds")

contrato_10a <- list(
  script       = "10a_robustness_heterofactor.R",
  fecha        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),

  # Correlaciones theta_A (original vs restringido)
  cor_A_pearson  = cor_A_pearson,
  cor_A_spearman = cor_A_spearman,
  rmse_A         = rmse_A,


  # Correlaciones theta_B (original vs restringido)
  cor_B_pearson  = cor_B_pearson,
  cor_B_spearman = cor_B_spearman,
  rmse_B         = rmse_B,

  # Cargas factoriales
  df_loadings       = df_loadings,
  max_diff_carga    = max_diff_carga,

  # Likelihood ratio test (restringido)
  lr_stat       = LR_stat_r,
  lr_df         = df_diff_r,
  lr_pvalue     = p_value_r,
  modelo_final  = modelo_final_restr_nombre,

  # Metadata
  n_comparacion    = nrow(comparacion),
  n_full           = n_full,
  n_restr          = n_restr,
  loglik_A_restr   = loglik_A_r,
  loglik_B_restr   = loglik_B_r,
  periodos_excluidos = TRIMESTRES_FORMALIDAD,

  # Paths de outputs
  outputs = list(
    modelo = PATH_ROBUSTNESS_MODELO,
    theta  = PATH_ROBUSTNESS_THETA,
    csv    = PATH_ROBUSTNESS_CSV,
    html   = PATH_ROBUSTNESS_HTML,
    figuras = DIR_FIGURAS_10A
  )
)

saveRDS(contrato_10a, PATH_CONTRATO_10A)
cat("✅ Contrato guardado:", basename(PATH_CONTRATO_10A), "\n")


# 📑 CHECKLIST ---------------------------------------------------------------

end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("\n═══════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST ROBUSTNESS A1+\n")
cat("═══════════════════════════════════════════════════════\n")
cat("   [✅] C++ compilado y verificado\n")
cat("   [✅] PyTorch scoring (", GPU_DEVICE, ")\n")
cat("   [✅] Modelo A restringido (MAXIT=", MAXIT_A, ") | conv:", modelo_A_restr$convergencia, "\n")
cat("   [✅] Modelo B restringido (MAXIT=", MAXIT_B, ") | conv:", modelo_B_restr$convergencia, "\n")
cat("   [✅] Modelo final restringido: Modelo", modelo_final_restr_nombre, "\n")
cat("   [✅] Comparación θ_A: Pearson r =", round(cor_A_pearson,  4), "\n")
cat("   [✅] Comparación θ_B: Pearson r =", round(cor_B_pearson,  4), "\n")
fig_pdfs <- list.files(DIR_FIGURAS_10A, pattern = "\\.pdf$", full.names = TRUE)
for (f in fig_pdfs) cat("   [✅] figura PDF:", basename(f), "\n")
for (f in fig_paths) cat("   [", ifelse(file.exists(f), "✅", "❌"), "] figura HTML:", basename(f), "\n")
cat("   [", ifelse(file.exists(PATH_ROBUSTNESS_MODELO), "✅", "❌"), "]",
    basename(PATH_ROBUSTNESS_MODELO), "\n")
cat("   [", ifelse(file.exists(PATH_ROBUSTNESS_THETA),  "✅", "❌"), "]",
    basename(PATH_ROBUSTNESS_THETA), "\n")
cat("   [", ifelse(file.exists(PATH_ROBUSTNESS_CSV),    "✅", "❌"), "]",
    basename(PATH_ROBUSTNESS_CSV), "\n")
cat("   [", ifelse(file.exists(PATH_ROBUSTNESS_HTML),   "✅", "❌"), "]",
    basename(PATH_ROBUSTNESS_HTML), "\n")
cat("   [", ifelse(file.exists(PATH_CONTRATO_10A),      "✅", "❌"), "]",
    basename(PATH_CONTRATO_10A), "\n")

cat("\n   TIEMPOS:\n")
cat("   • Modelo A restringido:", round((t_A_r$toc - t_A_r$tic) / 60, 1), "min\n")
cat("   • Modelo B restringido:", round((t_B_r$toc - t_B_r$tic) / 60, 1), "min\n")
cat("   • TOTAL:               ", round(elapsed, 1), "min\n")
cat("═══════════════════════════════════════════════════════\n\n")

rm(panel_original, datos_model, datos_restringido); gc()
torch$cuda$empty_cache()

t_total <- proc.time() - t_inicio
cat("Tiempo total:", round(t_total["elapsed"] / 60, 1), "minutos\n")
toc()

sink()
