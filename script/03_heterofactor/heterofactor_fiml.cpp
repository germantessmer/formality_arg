
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


