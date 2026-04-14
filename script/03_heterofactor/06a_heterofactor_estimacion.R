# =============================================================================
# [EN] 06a_heterofactor_estimacion.R -- Estimate two-factor latent model (FIML) with C++/OpenMP likelihood and GPU scoring
# INPUTS:  rdos/datos/04_panel_con_proxies.rds, rdos/contratos/05_contrato_proxies.rds
# OUTPUTS: rdos/modelos/06_modelo_heterofactor.rds, rdos/datos/06_theta_predichos.rds
# =============================================================================
# 🌟 06_heterofactor_estimacion.R 🌟 ####
# OBJETIVO:  Estimar el modelo heterofactor bi-factorial de Sarzosa & Urzúa
#            (2016) adaptado. Produce factores latentes θ_A (cognitivo) y
#            θ_B (socioemocional) para todo el panel EPH 2016T4–2025T3.
#
# ARQUITECTURA:
#   ✅ MLE:     compute_ll_full_cpp — loop único C++/OpenMP (3.7x vs original)
#   ✅ FIML:    Maneja NAs por diseño (calificacion_norm 54.9%, busqueda_formal 96.8%)
#   ✅ base_core: filtro por cobertura mínima (≥3 cog, ≥2 socio)
#   ✅ Scoring: PyTorch GPU (chunked 500k obs/chunk); fallback a CPU automático
#
# INPUT:
#   PATH_04_PANEL_PROXIES → rdos/datos/04_panel_con_proxies.rds (1,795,386 × 75)
#   PATH_CONTRATO_05      → rdos/contratos/05_contrato_proxies.rds
#
# OUTPUTS:
#   PATH_06_MODELO_HETERO → rdos/modelos/06_modelo_heterofactor.rds
#                           (lista: modelo_A, modelo_B, modelo_final, nombre_final)
#   PATH_06_THETA         → rdos/datos/06_theta_predichos.rds
#                           (panel completo + theta_A + theta_B)
#   PATH_CONTRATO_06      → rdos/contratos/06_contrato_heterofactor.rds
#
# PROXIES θ_cog  (4): rezago_escolar_cohorte, clima_educativo_hogar,
#                     emparejamiento_selectivo, calificacion_norm
# PROXIES θ_socio(3): entropia_estabilidad, residual_vivienda, busqueda_formal
#
# PARÁMETROS:
#   N_MAX_MLE = 20,000 | K_NODES = 9 (81 nodos 2D)
#   MAXIT_A = 150 | MAXIT_B = 400 | GPU_CHUNK = 500,000
#
# REFERENCIA: LEGACY_06_heterofactor_estimacion.R


# 📚 Librerías ####

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(fastGHQuad)
  library(tictoc)
  library(compiler)
  library(Rcpp)
  library(reticulate)
})

HAS_MATRIXSTATS <- requireNamespace("matrixStats", quietly = TRUE)


# 🔧 Configuración + Timer ####

source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

log_path <- file.path(DIR_REPORTES, "06a_log.txt")
sink(log_path, split = TRUE)   # split=TRUE: muestra en consola Y guarda en archivo

tic("Script 06 COMPLETO")
start_time <- Sys.time()

cat("═══════════════════════════════════════════════════════\n")
cat("🌟 06_heterofactor_estimacion.R — Heterofactor Bi-factorial\n")
cat("═══════════════════════════════════════════════════════\n")
cat("🚀 INICIO:", as.character(start_time), "\n\n")


# 🪫 1. Compilación Rcpp + OpenMP (FIML C++) ####

cat("─────────────────────────────────────────────────────\n")
cat("🔧 1. Compilación Rcpp + OpenMP (FIML C++)\n")
cat("─────────────────────────────────────────────────────\n")

CPP_PATH <- here::here("script", "03_heterofactor", "heterofactor_fiml.cpp")

cpp_code <- r"(
// heterofactor_fiml.cpp
// Heterofactor bi-factorial con FIML: ignora NAs por observacion/proxy
// Compilar con: sourceCpp("heterofactor_fiml.cpp")

#include <Rcpp.h>
#include <cmath>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// Auxiliar: log-densidad normal
inline double dnorm_log(double x, double mean, double sd) {
  const double log_sqrt_2pi = 0.918938533204673;
  double z = (x - mean) / sd;
  return -log_sqrt_2pi - log(sd) - 0.5 * z * z;
}

//' Calcular log_m_grid con FIML y OpenMP
//'
//' Para cada (i, k), acumula dnorm solo sobre proxies j donde M(i,j) no es NA.
//' R transmite NA_real_ como NaN en matrices double — std::isnan() lo detecta.
//'
//' @param M            Matrix [N x n_M] proxies observadas (NaN donde NA)
//' @param m_hat        Matrix [N x n_M] medias base (X * beta_m + alpha_m)
//' @param theta_A_grid Matrix [N x K]   valores theta_A por nodo
//' @param theta_B_grid Matrix [N x K]   valores theta_B por nodo
//' @param alpha_A      NumericVector [n_M] cargas en theta_A
//' @param alpha_B      NumericVector [n_M] cargas en theta_B
//' @param sigma_eps    NumericVector [n_M] SDs residuales
//' @param n_threads    Int threads OpenMP (0 = auto)
//' @return Matrix [N x K] log-densidades acumuladas (FIML)
//'
// [[Rcpp::export]]
NumericMatrix compute_log_m_grid_cpp(
    NumericMatrix M,
    NumericMatrix m_hat,
    NumericMatrix theta_A_grid,
    NumericMatrix theta_B_grid,
    NumericVector alpha_A,
    NumericVector alpha_B,
    NumericVector sigma_eps,
    int n_threads = 0
) {
  int N    = M.nrow();
  int n_M  = M.ncol();
  int K    = theta_A_grid.ncol();

  if (m_hat.nrow() != N || m_hat.ncol() != n_M)
    stop("Dimensiones de m_hat incorrectas");
  if (theta_A_grid.nrow() != N || theta_B_grid.nrow() != N)
    stop("Dimensiones de theta grids incorrectas");
  if (theta_B_grid.ncol() != K)
    stop("theta_B_grid debe tener K columnas");
  if (alpha_A.size() != n_M || alpha_B.size() != n_M || sigma_eps.size() != n_M)
    stop("Vectores de parametros deben tener longitud n_M");

#ifdef _OPENMP
  if (n_threads > 0) omp_set_num_threads(n_threads);
#endif

  NumericMatrix log_m_grid(N, K);

#ifdef _OPENMP
  #pragma omp parallel for schedule(static)
#endif
  for (int i = 0; i < N; i++) {
    for (int k = 0; k < K; k++) {
      double log_sum = 0.0;
      for (int j = 0; j < n_M; j++) {
        // FIML: ignorar proxy si es NA (transmitido como NaN desde R)
        if (std::isnan(M(i, j))) continue;

        double mean_ijk = m_hat(i, j)
                        + alpha_A[j] * theta_A_grid(i, k)
                        + alpha_B[j] * theta_B_grid(i, k);
        log_sum += dnorm_log(M(i, j), mean_ijk, sigma_eps[j]);
      }
      log_m_grid(i, k) = log_sum;
    }
  }
  return log_m_grid;
}

// Alias para scoring (misma función — interfaz idéntica)
// [[Rcpp::export]]
NumericMatrix compute_log_m_grid_scoring_cpp(
    NumericMatrix M,
    NumericMatrix m_hat,
    NumericMatrix theta_A_grid,
    NumericMatrix theta_B_grid,
    NumericVector alpha_A,
    NumericVector alpha_B,
    NumericVector sigma_eps,
    int n_threads = 0
) {
  return compute_log_m_grid_cpp(
    M, m_hat, theta_A_grid, theta_B_grid,
    alpha_A, alpha_B, sigma_eps, n_threads
  );
}

// [[Rcpp::export]]
List test_openmp() {
  int n_threads = 1;
  bool openmp_enabled = false;
#ifdef _OPENMP
  openmp_enabled = true;
  #pragma omp parallel
  {
    #pragma omp single
    n_threads = omp_get_num_threads();
  }
#endif
  return List::create(
    Named("openmp_enabled") = openmp_enabled,
    Named("n_threads")      = n_threads
  );
}

// =============================================================================
// compute_ll_full_cpp
// Log-likelihood completo del modelo bifactorial FIML en un unico loop C++.
//
// CAMBIOS VS compute_log_m_grid_cpp:
//   - Computa m_hat[i,:] localmente en cada thread (stack, sin heap)
//   - Integra log-sum-exp sobre k dentro del mismo loop de i
//   - schedule(dynamic, 64) para manejar la carga desbalanceada por NAs
//   - Retorna double (ll escalar): elimina matrices intermedias log_m_grid,
//     log_integrand (~25 MB por evaluacion -> reduccion de GC pressure)
//
// EQUIVALENCIA VERIFICADA:
//   |ll_full - ll_original| < 1e-9 (test en produccion: ~1e-11)
//
// PRECONDICIONES:
//   - log_prior[k] = dnorm(theta_A[k],0,1,log) + dnorm(theta_B[k],0,1,log)
//   - log_weights[k] = log(weights_2d[k])
//   - beta_m: [n_X x n_M] col-major (como matrices R)
// =============================================================================

// [[Rcpp::export]]
double compute_ll_full_cpp(
    NumericMatrix M,              // [N x n_M]   proxies (NaN donde NA)
    NumericMatrix X,              // [N x n_X]   covariables
    NumericVector alpha_m,        // [n_M]       interceptos proxies
    NumericMatrix beta_m,         // [n_X x n_M] coeficientes (col-major)
    NumericMatrix theta_A_grid,   // [N x K]     valores theta_A por nodo
    NumericMatrix theta_B_grid,   // [N x K]     valores theta_B por nodo
    NumericVector alpha_A,        // [n_M]       cargas en theta_A
    NumericVector alpha_B,        // [n_M]       cargas en theta_B
    NumericVector sigma_eps,      // [n_M]       SDs residuales
    NumericVector log_prior,      // [K]         log-prior por nodo (pre-computado)
    NumericVector log_weights,    // [K]         log(weights_2d) (pre-computado)
    int n_threads = 0
) {
  int N   = M.nrow();
  int n_M = M.ncol();
  int K   = theta_A_grid.ncol();
  int n_X = X.ncol();

  if (M.nrow() != X.nrow())
    stop("M y X deben tener el mismo numero de filas");
  if (theta_A_grid.nrow() != N || theta_B_grid.nrow() != N)
    stop("theta grids deben tener N filas");
  if (theta_A_grid.ncol() != K || theta_B_grid.ncol() != K)
    stop("theta grids deben tener K columnas");
  if (alpha_A.size() != n_M || alpha_B.size() != n_M || sigma_eps.size() != n_M)
    stop("alpha_A, alpha_B, sigma_eps deben tener longitud n_M");
  if (alpha_m.size() != n_M)
    stop("alpha_m debe tener longitud n_M");
  if (beta_m.nrow() != n_X || beta_m.ncol() != n_M)
    stop("beta_m debe ser [n_X x n_M]");
  if (log_prior.size() != K || log_weights.size() != K)
    stop("log_prior y log_weights deben tener longitud K");

#ifdef _OPENMP
  if (n_threads > 0) omp_set_num_threads(n_threads);
#endif

  double ll = 0.0;

#ifdef _OPENMP
  #pragma omp parallel for schedule(dynamic, 64) reduction(+:ll)
#endif
  for (int i = 0; i < N; i++) {

    // ── m_hat[i,:] — stack local, sin heap ──────────────────────────────────
    // n_M=7, K=81: tamanios pequenos y fijos en produccion
    double mhi[7];   // n_M fijo = 7
    for (int j = 0; j < n_M; j++) {
      mhi[j] = alpha_m[j];
      for (int x = 0; x < n_X; x++) {
        // NumericMatrix es col-major: X(i,x) = X[i + x*N]
        mhi[j] += X(i, x) * beta_m(x, j);
      }
    }

    // ── Log-densidad acumulada por nodo k ────────────────────────────────────
    double lse[81];  // K fijo = 81 (9x9)
    double max_lse = -1e300;

    for (int k = 0; k < K; k++) {
      // log_prior[k] + log(w_k) + sum_j dnorm_log(M[i,j], ...)
      double s = log_prior[k] + log_weights[k];

      for (int j = 0; j < n_M; j++) {
        // FIML: ignorar proxy si es NA (R transmite NA_real_ como NaN)
        double m_ij = M(i, j);
        if (std::isnan(m_ij)) continue;

        double mean_ijk = mhi[j]
          + alpha_A[j] * theta_A_grid(i, k)
          + alpha_B[j] * theta_B_grid(i, k);

        // dnorm_log inline (evita overhead de funcion)
        double z = (m_ij - mean_ijk) / sigma_eps[j];
        s += -0.918938533204673 - std::log(sigma_eps[j]) - 0.5 * z * z;
      }

      lse[k] = s;
      if (s > max_lse) max_lse = s;
    }

    // ── log-sum-exp sobre k ──────────────────────────────────────────────────
    double sum_exp = 0.0;
    for (int k = 0; k < K; k++) sum_exp += std::exp(lse[k] - max_lse);
    if (sum_exp < 1e-300) sum_exp = 1e-300;

    ll += max_lse + std::log(sum_exp);
  }

  return ll;
}

)"

writeLines(cpp_code, CPP_PATH)

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
  N_THREADS <- omp_info$n_threads  # Usar el conteo real detectado (e.g. 16)
  cat("✅ OpenMP ACTIVO —", N_THREADS, "threads\n")
} else {
  N_THREADS <- 1L
  cat("⚠️  OpenMP no disponible — MLE más lento (1 thread)\n")
}


# 🪫 2. GPU Setup — PyTorch via reticulate ####

cat("\n─────────────────────────────────────────────────────\n")
cat("🖥️  2. GPU Setup (PyTorch via reticulate)\n")
cat("─────────────────────────────────────────────────────\n")

use_condaenv("r-reticulate", required = TRUE)
torch <- import("torch")

GPU_DISPONIBLE <- torch$cuda$is_available()
GPU_DEVICE     <- if (GPU_DISPONIBLE) "cuda" else "cpu"
GPU_CHUNK_SIZE <- 500000L  # 500k obs/chunk → peak VRAM ~1.13 GB (float32)

if (GPU_DISPONIBLE) {
  vram_total <- as.numeric(torch$cuda$get_device_properties(0L)$total_memory) / 1024^3
  cat("✅ GPU ACTIVA:", py_to_r(torch$cuda$get_device_name(0L)), "\n")
  cat("   VRAM total:", round(vram_total, 2), "GB\n")
  cat("   Chunk size:", format(GPU_CHUNK_SIZE, big.mark = ","), "obs →",
      "peak VRAM ~", round(GPU_CHUNK_SIZE * 81 * 7 * 4 / 1024^3, 2), "GB\n")
} else {
  cat("⚠️  GPU no disponible — scoring en CPU (más lento)\n")
}

# Función de scoring GPU en Python (FIML vectorizado)
py_run_string("
import torch
import numpy as np

def scoring_gpu(M_np, m_hat_np, theta_A_nodes_np, theta_B_nodes_np,
                alpha_A_np, alpha_B_np, sigma_eps_np,
                chunk_size=500000, device='cuda'):
    '''
    FIML scoring bi-factorial vectorizado con PyTorch.

    Parametros
    ----------
    M_np           : ndarray [N, n_M]  — proxies observadas (np.nan donde NA)
    m_hat_np       : ndarray [N, n_M]  — medias base (alpha_m + X @ beta_m)
    theta_A_nodes  : ndarray [K]       — nodos GH 1D para theta_A
    theta_B_nodes  : ndarray [K]       — nodos GH 1D para theta_B
    alpha_A_np     : ndarray [n_M]     — cargas theta_A
    alpha_B_np     : ndarray [n_M]     — cargas theta_B
    sigma_eps_np   : ndarray [n_M]     — SDs residuales
    chunk_size     : int
    device         : str               — 'cuda' o 'cpu'

    Retorna
    -------
    log_m_grid : ndarray [N, K*K]  — log-densidades acumuladas (FIML)
                 columnas = producto cartesiano theta_A x theta_B
    '''
    import math

    N, n_M = M_np.shape
    K      = len(theta_A_nodes_np)

    # Producto cartesiano theta_A x theta_B -> K^2 nodos 2D
    tA = np.repeat(theta_A_nodes_np, K)   # [K^2]
    tB = np.tile(theta_B_nodes_np, K)     # [K^2]
    K2 = len(tA)

    # Tensores de parametros -> device (pequenos, sin chunking)
    alpha_A    = torch.tensor(alpha_A_np,    dtype=torch.float32, device=device)  # [n_M]
    alpha_B    = torch.tensor(alpha_B_np,    dtype=torch.float32, device=device)  # [n_M]
    sigma_eps  = torch.tensor(sigma_eps_np,  dtype=torch.float32, device=device)  # [n_M]
    theta_A_2d = torch.tensor(tA,           dtype=torch.float32, device=device)  # [K2]
    theta_B_2d = torch.tensor(tB,           dtype=torch.float32, device=device)  # [K2]
    log_sqrt2pi = math.log(math.sqrt(2 * math.pi))

    log_m_grid_out = torch.zeros(N, K2, dtype=torch.float32)  # resultado en CPU

    for start in range(0, N, chunk_size):
        end = min(start + chunk_size, N)

        # Chunk -> GPU (float32)
        M_c    = torch.tensor(M_np[start:end],     dtype=torch.float32, device=device)  # [chunk, n_M]
        mhat_c = torch.tensor(m_hat_np[start:end], dtype=torch.float32, device=device)  # [chunk, n_M]

        # Mask FIML: True donde M es observado (no NaN)
        mask = ~torch.isnan(M_c)  # [chunk, n_M]
        M_c  = torch.nan_to_num(M_c, nan=0.0)  # NaN -> 0 (no afecta porque mask lo excluye)

        # Media condicional: [chunk, K2, n_M]
        mean_grid = (mhat_c[:, None, :]
                    + theta_A_2d[None, :, None] * alpha_A[None, None, :]
                    + theta_B_2d[None, :, None] * alpha_B[None, None, :])

        # Log-densidad normal por celda: [chunk, K2, n_M]
        z     = (M_c[:, None, :] - mean_grid) / sigma_eps[None, None, :]
        log_d = -0.5 * z**2 - torch.log(sigma_eps[None, None, :]) - log_sqrt2pi

        # FIML: anular contribucion donde M era NA
        log_d = log_d * mask[:, None, :].float()

        # Acumular sobre proxies -> [chunk, K2]
        log_m_chunk = log_d.sum(dim=2)

        log_m_grid_out[start:end] = log_m_chunk.cpu()

        del M_c, mhat_c, mean_grid, z, log_d, log_m_chunk, mask
        if device == 'cuda':
            torch.cuda.empty_cache()

    return log_m_grid_out.numpy()
")

cat("✅ Función scoring_gpu definida en Python\n")


# 🪫 3. Parámetros del modelo ####

cat("\n─────────────────────────────────────────────────────\n")
cat("⚙️  3. Parámetros\n")
cat("─────────────────────────────────────────────────────\n")

set.seed(SEED_GLOBAL)

N_MAX_MLE    <- 20000L
K_NODES      <- 9L
MAXIT_A      <- 150L    # Modelo parsimonioso
MAXIT_B      <- 1200L    # Modelo completo (250 no convergió, MAXIT escalado)
TOL          <- 1e-6
# N_THREADS: asignado en Sección 1 por test_openmp() — NO redefinir aquí
N_MIN_CORE   <- 5000L

# Estructura factorial — 7 proxies
# ⚠️ emparejamiento_selectivo ≡ assortative_mating del LEGACY (decisión Diálogo 4)
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


# 🪫 4. Funciones auxiliares locales ####

`%||%` <- function(a, b) if (!is.null(a)) a else b

row_max <- function(mat) {
  # tryCatch: matrixStats puede fallar en regiones numéricas extremas durante BFGS
  # (especialmente Modelo B con n_X grande). Base R maneja esos casos silenciosamente.
  if (HAS_MATRIXSTATS) {
    tryCatch(matrixStats::rowMaxs(mat), error = function(e) apply(mat, 1, max))
  } else {
    apply(mat, 1, max)
  }
}

row_sums <- function(mat) {
  # tryCatch: ídem row_max
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


# 🪫 5. Carga de datos ####

cat("\n─────────────────────────────────────────────────────\n")
cat("📂 5. Carga de datos\n")
cat("─────────────────────────────────────────────────────\n")

hard_stop(file.exists(PATH_04_PANEL_PROXIES),
          paste0("No existe 04_panel_con_proxies.rds. Verificar DIR_DATOS: ", DIR_DATOS))
hard_stop(file.exists(PATH_CONTRATO_05),
          "No existe 05_contrato_proxies.rds. Ejecutar Script 05 primero.")

panel       <- readRDS(PATH_04_PANEL_PROXIES)
contrato_05 <- readRDS(PATH_CONTRATO_05)

cat("✅ Panel cargado:", format(nrow(panel), big.mark = ","), "obs ×",
    ncol(panel), "vars\n")
cat("   Memoria:", round(object.size(panel) / 1024^2, 1), "MB\n")

# Verificar proxies
proxies_faltantes <- setdiff(PROXIES_TODAS, names(panel))
hard_stop(length(proxies_faltantes) == 0,
          paste0("Proxies faltantes: ", paste(proxies_faltantes, collapse = ", ")))

cat("✅ 7 proxies presentes en el panel\n")

# Resumen de NAs por proxy (referencia)
cat("\n   NA% por proxy (por diseño):\n")
for (p in PROXIES_TODAS) {
  pct    <- round(100 * mean(is.na(panel[[p]])), 1)
  factor <- if (p %in% FACTOR_STRUCTURE$theta_A) "θ_A" else "θ_B"
  cat(sprintf("   %-30s [%s]: %s%%\n", p, factor, pct))
}


# 🪫 6. Ingeniería de variables ####

cat("\n─────────────────────────────────────────────────────\n")
cat("🔨 6. Ingeniería de variables\n")
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
  # Escalar proxies a z-score antes del modelo.
  # Necesario: SDs van de 0.245 (entropia) a 12.779 (residual_vivienda) — rango ~52x.
  # Sin escalar, el gradiente de residual_vivienda domina BFGS y colapsa las cargas.
  # sigma_eps sigue capturando varianza residual, ahora en escala estandarizada.
  mutate(across(all_of(PROXIES_TODAS), ~ safe_scale(as.numeric(.x))))

cat("✅ Covariables preparadas\n")
cat("   sexo_num:    Varones=0, Mujeres=1 (NA si otro)\n")
cat("   edad_scaled: z-score global\n")
cat("   log_ich:     log(max(ich_score, 1))\n")
cat("   7 proxies:   z-score (safe_scale)\n")


# 🪫 7. Función de estimación ####

cat("\n─────────────────────────────────────────────────────\n")
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

  # ── 7.1 base_core: filtro por cobertura mínima por factor ──────────────────
  # Criterio: ≥3 de 4 cognitivas observadas Y ≥2 de 3 socioemocionales observadas.
  # Garantiza identificación por factor sin exigir complete.cases en 7 proxies
  # (lo que daría ~3% del panel). NAs remanentes → FIML en C++.

  n_cog   <- rowSums(!is.na(datos[, factor_structure$theta_A]))
  n_socio <- rowSums(!is.na(datos[, factor_structure$theta_B]))

  base_core <- datos %>%
    mutate(.n_cog = n_cog, .n_socio = n_socio) %>%
    filter(
      !is.na(sexo_num),
      !is.na(edad_scaled),
      !is.na(!!sym(geo_var)),
      !!sym(geo_var) != "",
      .n_cog   >= 3,    # ≥3 de 4 cognitivas observadas
      .n_socio >= 2     # ≥2 de 3 socioemocionales observadas
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

  # NAs remanentes en base_core (por proxy — informativo)
  for (p in proxies) {
    pna <- round(100 * mean(is.na(base_core[[p]])), 1)
    if (pna > 0) cat(sprintf("   NA en base_core — %-28s: %s%% (FIML)\n", p, pna))
  }

  hard_stop(nrow(base_core) >= N_MIN_CORE,
            paste0("base_core muy chica (N=", nrow(base_core), " < ", N_MIN_CORE, ")"))

  # ── 7.2 Muestreo estratificado para MLE ────────────────────────────────────

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
        # Pre-calcular n fuera del pipe (Regla: nunca n() dentro de slice_sample)
        n_s <- max(1L, min(as.integer(df_g$n_sample[1]), nrow(df_g)))
        df_g %>% slice_sample(n = n_s)
      }) %>%
      select(-n_sample)

    cat("   Post-muestreo:", format(nrow(base_mle), big.mark = ","), "\n")
  } else {
    cat("   Sin muestreo (N ≤ N_MAX_MLE)\n")
  }

  # ── 7.3 Matrices de diseño ─────────────────────────────────────────────────

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
  M <- as.matrix(base_mle[, proxies])  # NAs preservados → C++ FIML los maneja

  N   <- nrow(X)
  n_M <- ncol(M)
  n_X <- ncol(X)

  cat("   Dimensiones MLE: N =", format(N, big.mark = ","),
      "| n_M =", n_M, "| n_X =", n_X, "\n")

  # ── 7.4 Cuadratura de Gauss-Hermite 2D ────────────────────────────────────

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

  # Pre-computar constantes para compute_ll_full_cpp (una sola vez, fuera del closure)
  # LOG_PRIOR_2D  ≡ log_prior_2d (ya calculado)
  # LOG_WEIGHTS_2D = log(weights_2d): evita recalcular log en cada evaluación de ll
  LOG_PRIOR_2D   <- log_prior_2d
  LOG_WEIGHTS_2D <- log(weights_2d)

  # ── 7.5 Log-likelihood con Rcpp (FIML activo vía C++) ─────────────────────

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

    # compute_ll_full_cpp: calcula m_hat, log-densidad e integración en un único
    # loop C++ paralelo. Elimina matrices intermedias [N×K] (~25 MB/eval).
    # Speedup medido: ~3.7x (Modelo A) y ~2.9x (Modelo B) vs implementación original.
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

  # ── 7.5b Verificación numérica compute_ll_full_cpp (UNA sola vez por modelo) ──
  # Confirma equivalencia con la función original antes de lanzar BFGS.
  # Umbral: |diff| < 1e-6. Resultado esperado según tests en producción: ~1e-11.

  cat("   Verificando compute_ll_full_cpp vs función original...\n")
  .par_test <- c(colMeans(M, na.rm = TRUE),            # alpha_m  [n_M]
                 rep(0.3, sum(carga_en_A)),             # alpha_A_free
                 rep(0.3, sum(carga_en_B)),             # alpha_B_free
                 rep(0, n_X * n_M),                    # beta_m_vec
                 log(pmax(apply(M, 2, sd, na.rm=TRUE), 0.1)))  # log_sigma [n_M]
  .par_test[!is.finite(.par_test)] <- 0

  # Parsear .par_test con los mismos índices que ll_bifactor
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
  }, error = function(e) {
    cat("   ERROR en ll_orig:", conditionMessage(e), "\n")
    NA_real_
  })

  .ll_new <- tryCatch(
    ll_bifactor(.par_test),
    error = function(e) {
      cat("   ERROR en ll_new:", conditionMessage(e), "\n")
      NA_real_
    }
  )

  .diff <- abs(.ll_orig - .ll_new)
  cat(sprintf("   |ll_orig - ll_new| = %.2e  %s\n",
              .diff, if (is.finite(.diff) && .diff < 1e-6) "\u2705 OK" else "\u274c REVISAR"))
  stopifnot("compute_ll_full_cpp no coincide con funcion original" =
            is.finite(.diff) && .diff < 1e-6)
  rm(.par_test, .ll_orig, .ll_new, .diff, .idx, .am, .aAf, .aBf,
     .bv, .bm, .sig, .aA, .aB, .mhat, lmg, lint, mx)

  # ── 7.6 Valores iniciales ──────────────────────────────────────────────────

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

  # ── 7.7 Optimización BFGS ─────────────────────────────────────────────────

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

  # ── 7.8 Parsear parámetros estimados ──────────────────────────────────────

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
  cat("   sigma_eps: ", paste(round(sigma_eps_hat, 3),    collapse = ", "), "\n")

  # ── 7.9 Scoring Universal — PyTorch GPU ───────────────────────────────────
  # Corre sobre base_core completo (no solo base_mle).
  # Un único forward pass vectorizado en GPU, chunked para respetar VRAM.

  cat("   Scoring GPU (base_core:", format(nrow(base_core), big.mark = ","), "obs)...\n")
  t_score_start <- Sys.time()

  X_sc     <- armar_X(base_core, geo_levels)
  M_sc     <- as.matrix(base_core[, proxies])  # NAs preservados → FIML en Python

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

  # Convertir resultado numpy → R matrix
  log_m_grid_sc <- py_to_r(log_m_grid_sc_np)  # [Nn, K_nodes_2d]

  # Posterior Empirical Bayes
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

  # ── 7.10 Retornar objeto modelo ───────────────────────────────────────────

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


# 🪫 8. Estimación — Modelos A y B ####

cat("\n═══════════════════════════════════════════════════════\n")
cat("🎯 8. MODELO A: PARSIMONIOSO (MAXIT =", MAXIT_A, ")\n")
cat("═══════════════════════════════════════════════════════\n")

tic("Modelo A")
modelo_A <- estimar_heterofactor_bifactor(
  datos            = datos_model,
  proxies          = PROXIES_TODAS,
  factor_structure = FACTOR_STRUCTURE,
  covars           = c("sexo_num", "edad_scaled", "edad_sq"),
  geo_var          = "region_chr",
  maxit            = MAXIT_A,
  modelo_nombre    = "Modelo A (Parsimonioso)"
)
t_A <- toc(quiet = TRUE)
cat("⏱️  Modelo A:", round((t_A$toc - t_A$tic) / 60, 1), "min\n\n")


# ── MODELO B: activar tras validar Modelo A ───────────────────────────────────
RUN_MODELO_B <- TRUE

if (RUN_MODELO_B) {
  cat("═══════════════════════════════════════════════════════\n")
  cat("🎯 8. MODELO B: COMPLETO (MAXIT =", MAXIT_B, ", ~2-3 hs)\n")
  cat("═══════════════════════════════════════════════════════\n")

  tic("Modelo B")
  modelo_B <- estimar_heterofactor_bifactor(
    datos            = datos_model,
    proxies          = PROXIES_TODAS,
    factor_structure = FACTOR_STRUCTURE,
    covars           = c("sexo_num", "edad_scaled", "edad_sq", "log_ich", "aglomerado_chr"),
    geo_var          = "region_chr",
    maxit            = MAXIT_B,
    modelo_nombre    = "Modelo B (Completo)"
  )
  t_B <- toc(quiet = TRUE)
  cat("⏱️  Modelo B:", round((t_B$toc - t_B$tic) / 60, 1), "min\n\n")
} else {
  cat("\n⏭️  MODELO B: omitido (RUN_MODELO_B = FALSE)\n")
  cat("   Activar: RUN_MODELO_B <- TRUE cuando Modelo A esté validado\n\n")
  modelo_B <- NULL
  t_B      <- list(toc = 0, tic = 0)
}


# 🪫 9. Likelihood Ratio Test ####

cat("═══════════════════════════════════════════════════════\n")
cat("📊 9. Likelihood Ratio Test\n")
cat("═══════════════════════════════════════════════════════\n")

loglik_A   <- modelo_A$loglik
n_params_A <- length(modelo_A$optim$par)

if (!is.null(modelo_B)) {
  loglik_B   <- modelo_B$loglik
  n_params_B <- length(modelo_B$optim$par)
  df_diff    <- n_params_B - n_params_A
  LR_stat    <- 2 * (loglik_B - loglik_A)
  p_value    <- pchisq(LR_stat, df = df_diff, lower.tail = FALSE)

  cat("   LogLik A:", round(loglik_A, 2), "(", n_params_A, "params)\n")
  cat("   LogLik B:", round(loglik_B, 2), "(", n_params_B, "params)\n")
  cat("   LR stat: ", round(LR_stat, 2), "| df:", df_diff,
      "| p-value:", format(p_value, scientific = TRUE, digits = 3), "\n\n")

  if (p_value < 0.05) {
    cat("✅ DECISIÓN: Modelo B significativamente mejor → usar Modelo B\n\n")
    modelo_final        <- modelo_B
    modelo_final_nombre <- "B"
  } else {
    cat("✅ DECISIÓN: Modelo A suficiente (parsimonia) → usar Modelo A\n\n")
    modelo_final        <- modelo_A
    modelo_final_nombre <- "A"
  }
} else {
  cat("   LogLik A:", round(loglik_A, 2), "(", n_params_A, "params)\n")
  cat("   Modelo B no estimado — Modelo A seleccionado por defecto\n\n")
  LR_stat             <- NA_real_
  df_diff             <- NA_integer_
  p_value             <- NA_real_
  modelo_final        <- modelo_A
  modelo_final_nombre <- "A"
}


# 🪫 10. Validaciones ####

cat("═══════════════════════════════════════════════════════\n")
cat("🔎 10. Validaciones (Modelo", modelo_final_nombre, ")\n")
cat("═══════════════════════════════════════════════════════\n")

theta_final <- modelo_final$theta_data

cat("   θ_A: mean =", round(mean(theta_final$theta_A, na.rm = TRUE), 3),
    "| sd =", round(sd(theta_final$theta_A, na.rm = TRUE), 3), "\n")
cat("   θ_B: mean =", round(mean(theta_final$theta_B, na.rm = TRUE), 3),
    "| sd =", round(sd(theta_final$theta_B, na.rm = TRUE), 3), "\n")

cor_AB <- cor(theta_final$theta_A, theta_final$theta_B, use = "complete.obs")
cat("   Cor(θ_A, θ_B) =", round(cor_AB, 4),
    ifelse(abs(cor_AB) < 0.15, "✅ PASS (ortogonal)", "⚠️  revisar"), "\n")

# R² individual de proxies sobre θ (advertencia temprana — validación completa en 06b)
cat("\n   R² individual de proxies (referencia rápida):\n")
for (p in PROXIES_TODAS) {
  factor_name <- if (p %in% FACTOR_STRUCTURE$theta_A) "θ_A" else "θ_B"

  dat_tmp <- modelo_final$theta_data %>%
    left_join(datos_model %>% select(id_individuo_hist, periodo_id, !!sym(p)),
              by = c("id_individuo_hist", "periodo_id")) %>%
    filter(!is.na(!!sym(p)), !is.na(theta_A), !is.na(theta_B))

  if (nrow(dat_tmp) < 100) {
    cat(sprintf("   %-30s [%s]: n insuf.\n", p, factor_name))
    next
  }

  theta_p <- if (p %in% FACTOR_STRUCTURE$theta_A) dat_tmp$theta_A else dat_tmp$theta_B
  r2 <- tryCatch({
    fit <- lm(theta_p ~ dat_tmp[[p]])
    summary(fit)$r.squared
  }, error = function(e) NA_real_)

  icon <- if (!is.na(r2) && r2 > 0.70) "⚠️ DOMINANTE" else "✅"
  cat(sprintf("   %-30s [%s]: R²=%.3f %s\n", p, factor_name,
              ifelse(is.na(r2), 0, r2), icon))
}


# 🪫 11. Merge con panel completo ####

cat("\n═══════════════════════════════════════════════════════\n")
cat("🔗 11. Merge con panel completo\n")
cat("═══════════════════════════════════════════════════════\n")

panel_con_theta <- panel %>%
  left_join(
    theta_final %>% select(id_individuo_hist, periodo_id, theta_A, theta_B),
    by = c("id_individuo_hist", "periodo_id")
  )

n_theta   <- sum(!is.na(panel_con_theta$theta_A))
pct_theta <- round(100 * n_theta / nrow(panel_con_theta), 1)

cat("   Panel original:", format(nrow(panel), big.mark = ","), "obs\n")
cat("   θ disponible:  ", format(n_theta,     big.mark = ","),
    "(", pct_theta, "%)\n")

soft_warn(pct_theta >= 90,
          paste0("Cobertura θ baja: ", pct_theta, "% < 90%. Revisar filtro base_core."))


# 💾 Guardar outputs ####

cat("\n═══════════════════════════════════════════════════════\n")
cat("💾 Guardando outputs\n")
cat("═══════════════════════════════════════════════════════\n")

# Output 1: Panel completo + thetas
saveRDS(panel_con_theta, PATH_06_THETA, compress = FALSE)
cat("✅", basename(PATH_06_THETA),
    "—", format(nrow(panel_con_theta), big.mark = ","), "obs ×",
    ncol(panel_con_theta), "vars\n")

# Output 2: Modelos (lista unificada: A + B + final + nombre)
modelo_hetero_obj <- list(
  modelo_A     = modelo_A,
  modelo_B     = modelo_B,
  modelo_final = modelo_final,
  nombre_final = modelo_final_nombre
)
saveRDS(modelo_hetero_obj, PATH_06_MODELO_HETERO)
cat("✅", basename(PATH_06_MODELO_HETERO),
    "— modelo_final =", modelo_final_nombre, "\n")

# Output 3: Contrato
contrato_06 <- list(
  timestamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
  version   = "06_heterofactor_v2.0 — FIML + GPU scoring",
  seed      = SEED_GLOBAL,

  inputs = list(
    panel_proxies = basename(PATH_04_PANEL_PROXIES),
    contrato_05   = basename(PATH_CONTRATO_05),
    n_obs_panel   = nrow(panel),
    n_vars_panel  = ncol(panel)
  ),

  config = list(
    N_MAX_MLE           = N_MAX_MLE,
    K_NODES             = K_NODES,
    MAXIT_A             = MAXIT_A,
    MAXIT_B             = MAXIT_B,
    TOL                 = TOL,
    N_THREADS           = N_THREADS,
    GPU_CHUNK           = GPU_CHUNK_SIZE,
    GPU_DEVICE          = GPU_DEVICE,
    fiml                = TRUE,
    base_core_criterio  = "n_cog >= 3 AND n_socio >= 2"
  ),

  proxies          = PROXIES_TODAS,
  factor_structure = FACTOR_STRUCTURE,

  modelos = list(
    A = list(
      nombre       = modelo_A$nombre,
      N_core       = modelo_A$meta$N_core,
      N_mle        = modelo_A$meta$N_mle,
      N_scoring    = modelo_A$meta$N_scoring,
      convergencia = modelo_A$convergencia,
      loglik       = modelo_A$loglik,
      n_params     = n_params_A,
      tiempo_min   = round((t_A$toc - t_A$tic) / 60, 1)
    ),
    B = if (!is.null(modelo_B)) list(
      nombre       = modelo_B$nombre,
      N_core       = modelo_B$meta$N_core,
      N_mle        = modelo_B$meta$N_mle,
      N_scoring    = modelo_B$meta$N_scoring,
      convergencia = modelo_B$convergencia,
      loglik       = modelo_B$loglik,
      n_params     = length(modelo_B$optim$par),
      tiempo_min   = round((t_B$toc - t_B$tic) / 60, 1)
    ) else list(nombre = "no estimado")
  ),

  lr_test = list(
    LR_statistic        = LR_stat,
    df                  = df_diff,
    p_value             = p_value,
    modelo_seleccionado = modelo_final_nombre
  ),

  validaciones = list(
    cor_theta_AB         = cor_AB,
    pct_theta_disponible = pct_theta
  ),

  # Factor loadings for the paper appendix
  loadings = data.frame(
    proxy      = PROXIES_TODAS,
    factor     = c(rep("theta_A", length(FACTOR_STRUCTURE$theta_A)),
                   rep("theta_B", length(FACTOR_STRUCTURE$theta_B))),
    alpha_A_mA = round(modelo_A$params$alpha_A, 3),
    alpha_B_mA = round(modelo_A$params$alpha_B, 3),
    sigma_mA   = round(modelo_A$params$sigma_eps, 3),
    alpha_A_mB = if (!is.null(modelo_B)) round(modelo_B$params$alpha_A, 3) else NA_real_,
    alpha_B_mB = if (!is.null(modelo_B)) round(modelo_B$params$alpha_B, 3) else NA_real_,
    sigma_mB   = if (!is.null(modelo_B)) round(modelo_B$params$sigma_eps, 3) else NA_real_,
    stringsAsFactors = FALSE
  ),

  outputs = list(
    panel_con_theta    = basename(PATH_06_THETA),
    modelo_heterofactor = basename(PATH_06_MODELO_HETERO)
  )
)

saveRDS(contrato_06, PATH_CONTRATO_06)
cat("✅", basename(PATH_CONTRATO_06), "\n")


# 📑 CHECKLIST SCRIPT 06 ####

end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("\n═══════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 06\n")
cat("═══════════════════════════════════════════════════════\n")
cat("   [✅] compute_ll_full_cpp compilada y verificada (|diff|<1e-6)\n")
cat("   [✅] PyTorch scoring (", GPU_DEVICE, ")\n")
cat("   [✅] base_core: n_cog≥3 AND n_socio≥2\n")
cat("   [✅] Modelo A estimado (MAXIT=", MAXIT_A, ")\n")
cat("   [", ifelse(RUN_MODELO_B, "✅", "⏭️"), "] Modelo B estimado (MAXIT=", MAXIT_B, ")\n")
cat("   [", ifelse(modelo_A$convergencia == 0, "✅", "⚠️"), "] Convergencia A:", modelo_A$convergencia, "\n")
cat("   [", if (!is.null(modelo_B)) ifelse(modelo_B$convergencia == 0, "✅", "⚠️") else "⏭️", "] Convergencia B:",
    if (!is.null(modelo_B)) modelo_B$convergencia else "no estimado", "\n")
cat("   [", ifelse(abs(cor_AB) < 0.15, "✅", "⚠️"), "] Cor(θ_A,θ_B):", round(cor_AB, 3), "\n")
cat("   [", ifelse(pct_theta >= 90, "✅", "⚠️"), "] Cobertura θ:", pct_theta, "%\n")
cat("   [✅] 3 outputs guardados\n\n")

cat("   TIEMPOS:\n")
cat("   • Modelo A:", round((t_A$toc - t_A$tic) / 60, 1), "min\n")
cat("   • Modelo B:", if (RUN_MODELO_B && !is.null(modelo_B))
                        paste0(round((t_B$toc - t_B$tic) / 60, 1), " min")
                      else "omitido", "\n")
cat("   • TOTAL:   ", round(elapsed, 1), "min\n")
cat("═══════════════════════════════════════════════════════\n\n")

cat("🎯 SIGUIENTE PASO: 06b_diagnostico_theta.R\n")
cat("   Input:  PATH_06_THETA + PATH_06_MODELO_HETERO\n")
cat("   Output: rdos/reportes/06b_diagnostico_theta.html\n")
cat("   Validación crítica: R² por proxy sobre θ\n")
cat("   (≥3 proxies con R²>10%, ninguna >70%)\n\n")

cat("✅ Script 06 finalizado:", as.character(end_time), "\n")
cat("═══════════════════════════════════════════════════════\n")

toc()
sink()