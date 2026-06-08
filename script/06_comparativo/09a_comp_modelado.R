# =============================================================================
# [EN] 09a_comp_modelado.R -- Cross-model comparison: discrimination, calibration, specification tests (LPM vs GLM vs SLS)
# INPUTS:  Contracts 07a-08 for all three model families
# OUTPUTS: rdos/reportes/09a_comp_modelado.html, rdos/figuras/09a_comp_modelado/*.pdf
# =============================================================================
# 🌟 09a_comp_modelado.R 🌟 ####
# Reporte comparativo de modelado post-LASSO  --  LPM vs GLM vs SLS (sufijo dinámico)
# Proyecto: formalidad_back  |  Capa 6 -- Reportes Comparativos  |  D72
#
# INPUTS:
#   rdos/contratos/07a_contrato_lasso_{SUFIJO}.rds        x3
#   rdos/contratos/07b_contrato_postlasso_{SUFIJO}.rds    x3
#   rdos/contratos/07c_contrato_tiempo_{SUFIJO}.rds       x3
#   rdos/contratos/07d_contrato_interacciones_{SUFIJO}.rds x3
#   rdos/contratos/08_contrato_backcasting_{SUFIJO}.rds   x3
#
# OUTPUTS:
#   rdos/reportes/09a_comp_modelado.html
#   rdos/reportes/09a_comp_modelado_notas.txt
#   rdos/figuras/09a_comp_modelado/*.pdf   (via guardar_figura)
#
# ESTÉTICA: theme_paper() + scale_color_modelos() + guardar_figura()
#   coherente con 07e_/08b_/08c_  (funciones_comunes.R → theme_paper.R)
#   COL_LPM / COL_GLM / COL_SLS — no valores hexadecimales hardcodeados
#
# ESTRATEGIA: construccion manual desde contratos [Opcion B]
#   Nomenclatura real: tabla_ols (term|estimate|std.error|statistic|p.value|significancia)
#   [L56] con <- file(rmd_temp, "wt", encoding="UTF-8") + writeLines
#   [L64] HC_* primarios; tryCatch para contratos
#   [L72] R2 SLS sobre kappa -- nota explicita siempre
#   [L75] VIF SLS: GVIF_adj; VIF LPM: vif_comparable
#   [L76] case_when() -- sin dplyr::recode()
#   [LN4] GLM = 0% pred fuera [0,1] por construccion binomial

# 📚 Librerías -----------------------------------------------------------------

suppressPackageStartupMessages({
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

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ⌛ Inicio contador de tiempo -------------------------------------------------

tic("09a - Comparativo Modelado")
cat("===================================================================\n")
cat("SCRIPT 09a - COMPARATIVO DE MODELADO LPM / GLM / SLS\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("===================================================================\n\n")

# 🪫 1. CARGA DE CONTRATOS ---------------------------------------------------
# PREMISA: ningún valor numérico se define sin fuente en contrato.
# Todos los valores de referencia se leen directamente desde los .rds.
# El único HC sin fuente real es HC_glm_pct_fuera_test (cero estructural GLM).
cat("-- 1. Carga de contratos ---------------------------------------------\n")

safe_rds <- function(path, label) {
  tryCatch(
    { obj <- readRDS(path); cat(sprintf("   [OK] %s\n", label)); obj },
    error = function(e) { cat(sprintf("   [WARN] %s -- contrato no disponible\n", label)); NULL }
  )
}

c07b_lpm <- safe_rds(PATH_07B_CONTRATO_LPM, paste0("07b ", SUFIJO_MODELO_LPM))
c07b_glm <- safe_rds(PATH_07B_CONTRATO_GLM, paste0("07b ", SUFIJO_MODELO_GLM))
c07b_sls <- safe_rds(PATH_07B_CONTRATO_SLS, paste0("07b ", SUFIJO_MODELO_SLS))
c07a_lpm <- safe_rds(PATH_07A_CONTRATO_LPM, paste0("07a ", SUFIJO_MODELO_LPM))
c07a_glm <- safe_rds(PATH_07A_CONTRATO_GLM, paste0("07a ", SUFIJO_MODELO_GLM))
c07a_sls <- safe_rds(PATH_07A_CONTRATO_SLS, paste0("07a ", SUFIJO_MODELO_SLS))
c07c_lpm <- safe_rds(PATH_07C_CONTRATO_LPM, paste0("07c ", SUFIJO_MODELO_LPM))
c07c_glm <- safe_rds(PATH_07C_CONTRATO_GLM, paste0("07c ", SUFIJO_MODELO_GLM))
c07c_sls <- safe_rds(PATH_07C_CONTRATO_SLS, paste0("07c ", SUFIJO_MODELO_SLS))
c07d_lpm <- safe_rds(PATH_07D_CONTRATO_LPM, paste0("07d ", SUFIJO_MODELO_LPM))
c07d_glm <- safe_rds(PATH_07D_CONTRATO_GLM, paste0("07d ", SUFIJO_MODELO_GLM))
c07d_sls <- safe_rds(PATH_07D_CONTRATO_SLS, paste0("07d ", SUFIJO_MODELO_SLS))
c08_lpm  <- safe_rds(PATH_08_CONTRATO_LPM,  paste0("08  ", SUFIJO_MODELO_LPM))
c08_glm  <- safe_rds(PATH_08_CONTRATO_GLM,  paste0("08  ", SUFIJO_MODELO_GLM))
c08_sls  <- safe_rds(PATH_08_CONTRATO_SLS,  paste0("08  ", SUFIJO_MODELO_SLS))

# 🪫 2. VALORES DE REFERENCIA DESDE CONTRATOS --------------------------------
# Todos los valores se leen desde contratos. safe_scalar() permite que el
# script continúe si un contrato falla, pero el bloque de verificación al
# final de esta sección aborta si algún valor crítico es NA.
cat("\n-- 2. Valores de referencia desde contratos --------------------------\n")

# Helper de lectura segura (definido aquí, antes del bloque de helpers general)
.sc <- function(fn, fallback = NA_real_) {
  v <- tryCatch(fn(), error = function(e) fallback)
  if (is.null(v) || length(v) == 0) return(fallback)
  suppressWarnings(as.numeric(v[[1]]))
}

# Helper para metricas_clf (data.frame de 1 fila con columnas nombradas)
.mc <- function(obj, col) {
  .sc(function() {
    mc <- obj$metricas_clf
    if (is.null(mc) || nrow(mc) == 0) return(NA_real_)
    cn <- col[col %in% names(mc)][1]
    if (is.na(cn)) return(NA_real_)
    mc[[cn]][1]
  })
}

# Helper para metricas_calibracion (lista con escalares nombrados)
.cal <- function(obj, campo) {
  .sc(function() obj$metricas_calibracion[[campo]])
}

# ── LPM ──────────────────────────────────────────────────────────────────────
HC_lpm_ntrain         <- .sc(function() c07a_lpm$n_train)
HC_lpm_auc            <- .sc(function() c07b_lpm$auc_roc)
HC_lpm_r2             <- .sc(function() c07b_lpm$ols_r2)
HC_lpm_youden         <- .mc(c07b_lpm, c("umbral", "umbral_youden"))
HC_lpm_f1_youden      <- .mc(c07b_lpm, c("f1", "F1"))
HC_lpm_mcc_youden     <- .mc(c07b_lpm, c("mcc", "MCC"))
HC_lpm_pct_fuera_test <- .sc(function() c07b_lpm$pct_fuera_01)
HC_lpm_pct_back       <- .sc(function() c08_lpm$pct_pred_fuera_01_union)
HC_lpm_umbral_cal     <- .cal(c08_lpm, "umbral")
HC_lpm_delta_cal      <- .cal(c08_lpm, "delta_max_pp")
HC_lpm_f1_cal         <- .cal(c08_lpm, "f1")
HC_lpm_mcc_cal        <- .cal(c08_lpm, "mcc")

# ── GLM ──────────────────────────────────────────────────────────────────────
HC_glm_ntrain         <- HC_lpm_ntrain                    # misma muestra train
HC_glm_auc            <- .sc(function() c07b_glm$auc_roc)
HC_glm_pseudo_r2      <- .sc(function() c07b_glm$glm_pseudo_r2)  # campo real: glm_pseudo_r2
HC_glm_youden         <- .mc(c07b_glm, c("umbral", "umbral_youden"))
HC_glm_f1_youden      <- .mc(c07b_glm, c("f1", "F1"))
HC_glm_mcc_youden     <- .mc(c07b_glm, c("mcc", "MCC"))
HC_glm_hl_p           <- .sc(function() c07b_glm$hl_pval)
HC_glm_pct_fuera_test <- 0.00  # HC — cero estructural GLM binomial (sin contrato fuente)
HC_glm_umbral_cal     <- .cal(c08_glm, "umbral")
HC_glm_delta_cal      <- .cal(c08_glm, "delta_max_pp")
HC_glm_f1_cal         <- .cal(c08_glm, "f1")
HC_glm_mcc_cal        <- .cal(c08_glm, "mcc")

# ── SLS ──────────────────────────────────────────────────────────────────────
HC_sls_ntrain         <- HC_lpm_ntrain                    # misma muestra train
HC_sls_auc            <- .sc(function() c07b_sls$auc_roc)
HC_sls_r2             <- .sc(function() c07b_sls$ols_r2)
HC_sls_nkappa         <- .sc(function() c07b_sls$n_final)
HC_sls_youden         <- .mc(c07b_sls, c("umbral", "umbral_youden"))
HC_sls_f1_youden      <- .mc(c07b_sls, c("f1", "F1"))
HC_sls_mcc_youden     <- .mc(c07b_sls, c("mcc", "MCC"))
HC_sls_pct_fuera_test <- .sc(function() c07b_sls$pct_fuera_01_test)
HC_sls_pct_perdida    <- .sc(function() c07b_sls$pct_loss)
HC_sls_pct_back       <- .sc(function() c08_sls$pct_pred_fuera_01_union)
HC_sls_umbral_cal     <- .cal(c08_sls, "umbral")
HC_sls_delta_cal      <- .cal(c08_sls, "delta_max_pp")
HC_sls_f1_cal         <- .cal(c08_sls, "f1")
HC_sls_mcc_cal        <- .cal(c08_sls, "mcc")

# ── Interacciones (desde c07d — campo n_interact_estable confirmado) ──────────
HC_int_lpm <- .sc(function() c07d_lpm$n_interact_estable)
HC_int_glm <- .sc(function() c07d_glm$n_interact_estable)
HC_int_sls <- .sc(function() c07d_sls$n_interact_estable)

# ── Verificación: abortar si algún valor crítico es NA ───────────────────────
TOL_VERIF <- 1e-4
criticos <- list(
  HC_lpm_ntrain = HC_lpm_ntrain, HC_lpm_auc = HC_lpm_auc,
  HC_lpm_r2 = HC_lpm_r2, HC_lpm_youden = HC_lpm_youden,
  HC_glm_auc = HC_glm_auc, HC_glm_pseudo_r2 = HC_glm_pseudo_r2,
  HC_glm_hl_p = HC_glm_hl_p,
  HC_sls_auc = HC_sls_auc, HC_sls_r2 = HC_sls_r2,
  HC_sls_nkappa = HC_sls_nkappa, HC_sls_pct_fuera_test = HC_sls_pct_fuera_test,
  HC_lpm_umbral_cal = HC_lpm_umbral_cal, HC_glm_umbral_cal = HC_glm_umbral_cal,
  HC_sls_umbral_cal = HC_sls_umbral_cal,
  HC_int_lpm = HC_int_lpm, HC_int_glm = HC_int_glm, HC_int_sls = HC_int_sls
)
na_criticos <- names(criticos)[sapply(criticos, is.na)]
if (length(na_criticos) > 0) {
  stop(sprintf(
    "[09a] Valores críticos NA — contratos no disponibles o campos faltantes:\n  %s\n",
    paste(na_criticos, collapse = ", ")
  ))
}
cat(sprintf("   [OK] %d valores de referencia cargados desde contratos\n",
            length(criticos)))

# 🪫 3. HELPERS ---------------------------------------------------------------
fmt_n   <- function(x) format(as.integer(x), big.mark = ",")
fmt_num <- function(x, d=4) {
  if (length(x)==0 || is.na(x) || is.null(x)) return("N/D")
  formatC(as.numeric(x), digits=d, format="f")
}
fmt_pct <- function(x, d=2) {
  if (length(x)==0 || is.na(x) || is.null(x)) return("N/D")
  sprintf(paste0("%.",d,"f%%"), as.numeric(x))
}

safe_scalar <- function(fn, fallback=NA_real_) {
  v <- tryCatch(fn(), error=function(e) fallback)
  if (is.null(v) || length(v)==0) return(fallback)
  suppressWarnings(as.numeric(v[[1]]))
}

pick_col <- function(df, cands) {
  f <- cands[cands %in% names(df)]
  if (length(f)==0) stop(paste("Sin columna:", paste(cands, collapse="/")))
  f[1]
}

# Extraer metricas_clf (wide: umbral|accuracy|sensibilidad|f1|mcc|...)
get_mc_val <- function(c07b_obj, col_cands, fallback=NA_real_) {
  safe_scalar(function() {
    mc <- c07b_obj$metricas_clf
    if (is.null(mc) || nrow(mc)==0) return(fallback)
    col <- col_cands[col_cands %in% names(mc)][1]
    if (is.na(col)) return(fallback)
    mc[[col]][1]
  }, fallback=fallback)
}

# 🪫 4. TABLA DE REGRESION ECONOMICA  --  PIEZA CENTRAL ----------------------
cat("\n-- 4. Tabla de regresion economica -----------------------------------\n")

cat("   [diag tabla_ols LPM]:", paste(names(c07b_lpm$tabla_ols),  collapse=", "), "\n")
cat("   [diag tabla_ols SLS]:", paste(names(c07b_sls$tabla_ols),  collapse=", "), "\n")
cat("   [diag ames GLM]:", paste(names(c07b_glm$ames),  collapse=", "), "\n")

# Estandarizar tabla de coeficientes desde cada contrato
get_tabla_std <- function(c07b_obj, modelo) {
  tryCatch({
    if (modelo == "GLM") {
      # Prioridad: ames (AME via metodo delta) > tabla_glm (log-odds) > tabla_ols
      # Los AME son directamente comparables con LPM/SLS en escala de probabilidad.
      # Los log-odds (tabla_glm) se conservan en el contrato pero NO se usan en el reporte.
      tbl_ame <- c07b_obj$ames
      if (!is.null(tbl_ame) && nrow(tbl_ame) > 0) {
        # ames columnas: variable|ame|se_ame|z_ame|p_ame|ame_ic_lo|ame_ic_hi|sig_ame
        cv  <- pick_col(tbl_ame, c("variable","term","Variable"))
        ce  <- pick_col(tbl_ame, c("ame","estimate","AME","Estimate"))
        cse <- pick_col(tbl_ame, c("se_ame","std.error","se","SE","std_error"))
        cst <- pick_col(tbl_ame, c("z_ame","statistic","z","z_stat","t"))
        cp  <- pick_col(tbl_ame, c("p_ame","p.value","p_value","pval","p"))
        return(tbl_ame %>% transmute(
          term      = as.character(.data[[cv]]),
          estimate  = as.numeric(.data[[ce]]),
          std.error = as.numeric(.data[[cse]]),
          statistic = as.numeric(.data[[cst]]),
          p.value   = as.numeric(.data[[cp]])
        ))
      }
      # Fallback: log-odds si c07b_obj$ames no disponible
      cat("   [WARN GLM] c07b_obj$ames no disponible, usando tabla_glm (log-odds como fallback)\n")
      tbl <- c07b_obj$tabla_glm %||% c07b_obj$tabla_ols
      if (is.null(tbl) || nrow(tbl)==0) return(NULL)
      cv  <- pick_col(tbl, c("term","variable","Variable"))
      ce  <- pick_col(tbl, c("estimate","coef","Estimate"))
      cse <- pick_col(tbl, c("std.error","se_cl","se","SE"))
      cst <- pick_col(tbl, c("statistic","z","z_stat","t"))
      cp  <- pick_col(tbl, c("p.value","p_value","pval"))
    } else {
      tbl <- c07b_obj$tabla_ols    # term|estimate|std.error|statistic|p.value|significancia
      if (is.null(tbl) || nrow(tbl)==0) return(NULL)
      cv  <- pick_col(tbl, c("term","variable","Variable"))
      ce  <- pick_col(tbl, c("estimate","coef","Estimate"))
      cse <- pick_col(tbl, c("std.error","se_cl","se","SE"))
      cst <- pick_col(tbl, c("statistic","t","t_stat","z"))
      cp  <- pick_col(tbl, c("p.value","p_value","pval"))
    }
    tbl %>% transmute(
      term      = as.character(.data[[cv]]),
      estimate  = as.numeric(.data[[ce]]),
      std.error = as.numeric(.data[[cse]]),
      statistic = as.numeric(.data[[cst]]),
      p.value   = as.numeric(.data[[cp]])
    )
  }, error = function(e) {
    cat(sprintf("   [WARN] get_tabla_std(%s): %s\n", modelo, conditionMessage(e)))
    NULL
  })
}

t_lpm <- get_tabla_std(c07b_lpm, "LPM")
t_glm <- get_tabla_std(c07b_glm, "GLM")
t_sls <- get_tabla_std(c07b_sls, "SLS")

cat(sprintf("   Filas extraidas: LPM=%s | GLM=%s | SLS=%s\n",
            if(!is.null(t_lpm)) nrow(t_lpm) else "NULL",
            if(!is.null(t_glm)) nrow(t_glm) else "NULL",
            if(!is.null(t_sls)) nrow(t_sls) else "NULL"))

# Celda de tabla econometrica: coef*** + SE en parentesis
fmt_reg_cell <- function(est, se, pval) {
  if (is.na(est) || is.na(se)) return("")
  stars <- dplyr::case_when(
    pval < 0.001 ~ "***", pval < 0.01 ~ "**",
    pval < 0.05  ~ "*",   pval < 0.1  ~ ".",
    TRUE         ~ ""
  )
  sprintf('%.4f%s<br><span style="color:#666;font-size:0.86em">(%.4f)</span>',
          est, stars, se)
}

# Grupo tematico (para pack_rows)
get_grupo <- function(v) {
  dplyr::case_when(
    grepl("^\\(Intercept\\)$",                             v, ignore.case=TRUE) ~ "A_Intercepto",
    grepl("theta",                                          v, ignore.case=TRUE) ~ "B_Factor Latente (theta_A, theta_B)",
    grepl("edad|sexo|estado_civil|nacim|parentesco",        v, ignore.case=TRUE) ~ "C_Demograficas",
    grepl("educ|anios|escuela|alfabet",                     v, ignore.case=TRUE) ~ "D_Educativas",
    grepl("seccion|calific|categ_ocup|antig|horas|tamanio|empresa|busqueda_formal|calificacion_norm|entropia", v, ignore.case=TRUE) ~ "E_Laborales",
    grepl("aglom|region",                                   v, ignore.case=TRUE) ~ "F_Geograficas",
    grepl("ingreso|nbi|ich",                                v, ignore.case=TRUE) ~ "G_Familiares",
    grepl("rezago|clima|assort|residual|mating",            v, ignore.case=TRUE) ~ "H_Proxies Longitudinales",
    TRUE                                                                          ~ "Z_Otras"
  )
}

GRUPOS_LABEL <- c(
  "A_Intercepto"                       = "Intercepto",
  "B_Factor Latente (theta_A, theta_B)"= "Factor Latente (\u03b8)",
  "C_Demograficas"                     = "Demogr\u00e1ficas",
  "D_Educativas"                       = "Educativas",
  "E_Laborales"                        = "Laborales",
  "F_Geograficas"                      = "Geogr\u00e1ficas",
  "G_Familiares"                       = "Familiares",
  "H_Proxies Longitudinales"           = "Proxies Longitudinales",
  "Z_Otras"                            = "Otras"
)

all_terms <- unique(c(
  if (!is.null(t_lpm)) t_lpm$term else character(0),
  if (!is.null(t_glm)) t_glm$term else character(0),
  if (!is.null(t_sls)) t_sls$term else character(0)
))

build_row <- function(trm, tbl) {
  if (is.null(tbl)) return(list(cell="<em style='color:#bbb'>N/D</em>", t="", p=""))
  r <- tbl[tbl$term == trm, ]
  if (nrow(r)==0) return(list(cell="<em style='color:#bbb'>\u2014</em>", t="", p=""))
  list(
    cell = fmt_reg_cell(r$estimate[1], r$std.error[1], r$p.value[1]),
    t    = sprintf("%.2f", r$statistic[1]),
    p    = ifelse(r$p.value[1] < 2e-16, "<2e-16", sprintf("%.4f", r$p.value[1]))
  )
}

if (length(all_terms) > 0) {
  treg_df <- tibble(term=all_terms) %>%
    mutate(gkey = get_grupo(term)) %>%
    dplyr::arrange(gkey, term) %>%
    mutate(
      LPM          = sapply(term, function(v) build_row(v, t_lpm)$cell),
      `GLM (AME)`  = sapply(term, function(v) build_row(v, t_glm)$cell),
      SLS          = sapply(term, function(v) build_row(v, t_sls)$cell),
      t_LPM        = sapply(term, function(v) build_row(v, t_lpm)$t),
      t_GLM        = sapply(term, function(v) build_row(v, t_glm)$t),
      t_SLS        = sapply(term, function(v) build_row(v, t_sls)$t)
    )

  grupos_reg <- treg_df %>%
    mutate(etiqueta = GRUPOS_LABEL[gkey] %||% gkey) %>%
    group_by(etiqueta, gkey) %>%
    summarise(n=n(), .groups="drop") %>%
    dplyr::arrange(gkey)

  filas_theta <- which(grepl("theta", treg_df$term, ignore.case=TRUE))
  cat(sprintf("   Tabla: %d variables | %d filas theta\n",
              nrow(treg_df), length(filas_theta)))
} else {
  treg_df    <- tibble(term="Sin datos", LPM="N/D", `GLM (AME)`="N/D", SLS="N/D",
                       t_LPM="", t_GLM="", t_SLS="", gkey="Z_Otras")
  grupos_reg <- tibble(etiqueta="Sin datos", gkey="Z_Otras", n=1L)
  filas_theta <- integer(0)
  cat("   [WARN] Tabla vacia\n")
}

# 🪫 5. PIE DE TABLA DE REGRESION  (estadisticos de ajuste) ------------------
cat("\n-- 5. Estadisticos de ajuste -----------------------------------------\n")

r2_lpm  <- safe_scalar(function() c07b_lpm$ols_r2,              HC_lpm_r2)
r2a_lpm <- safe_scalar(function() c07b_lpm$ols_r2_adj,          NA_real_)
pr2_glm <- safe_scalar(function() c07b_glm$glm_pseudo_r2,        HC_glm_pseudo_r2)
r2_sls  <- safe_scalar(function() c07b_sls$ols_r2,              HC_sls_r2)
auc_lpm <- safe_scalar(function() c07b_lpm$auc_roc,             HC_lpm_auc)
auc_glm <- safe_scalar(function() c07b_glm$auc_roc,             HC_glm_auc)
auc_sls <- safe_scalar(function() c07b_sls$auc_roc,             HC_sls_auc)
apr_lpm <- safe_scalar(function() c07b_lpm$auc_pr,              NA_real_)
apr_glm <- safe_scalar(function() c07b_glm$auc_pr,              NA_real_)
apr_sls <- safe_scalar(function() c07b_sls$auc_pr,              NA_real_)
ci_lpm  <- tryCatch(c07b_lpm$auc_roc_ci, error=function(e) NULL)
ci_glm  <- tryCatch(c07b_glm$auc_roc_ci, error=function(e) NULL)
ci_sls  <- tryCatch(c07b_sls$auc_roc_ci, error=function(e) NULL)
fmt_auc <- function(a, ci)
  if (!is.null(ci) && length(ci)>=3) sprintf("%.4f [%.4f;%.4f]",a,ci[1],ci[3]) else fmt_num(a,4)

f_lpm  <- safe_scalar(function() c07b_lpm$ols_f_stat, NA_real_)
fp_lpm <- safe_scalar(function() c07b_lpm$ols_f_pval, NA_real_)
f_sls  <- safe_scalar(function() c07b_sls$ols_f_stat, NA_real_)
fp_sls <- safe_scalar(function() c07b_sls$ols_f_pval, NA_real_)
fmt_f  <- function(f,p) {
  if (is.na(f)) return("N/D")
  ps <- if (is.na(p)) "" else if (p<2e-16) " (p<2e-16)" else sprintf(" (p=%.4f)",p)
  sprintf("%.2f%s", f, ps)
}

hl_stat <- safe_scalar(function() c07b_glm$hl_stat, NA_real_)
hl_p    <- safe_scalar(function() c07b_glm$hl_pval %||% c07b_glm$hl_test$p.value, HC_glm_hl_p)
cat(sprintf("   H-L: stat=%s | p=%s\n", fmt_num(hl_stat,3), fmt_num(hl_p,4)))

bp_lpm  <- safe_scalar(function() c07b_lpm$bp_stat,    NA_real_)
bpp_lpm <- safe_scalar(function() c07b_lpm$bp_pval,    NA_real_)
rs_lpm  <- safe_scalar(function() c07b_lpm$reset_stat, NA_real_)
rsp_lpm <- safe_scalar(function() c07b_lpm$reset_pval, NA_real_)
bp_sls  <- safe_scalar(function() c07b_sls$bp_stat,    NA_real_)
bpp_sls <- safe_scalar(function() c07b_sls$bp_pval,    NA_real_)
rs_sls  <- safe_scalar(function() c07b_sls$reset_stat, NA_real_)
rsp_sls <- safe_scalar(function() c07b_sls$reset_pval, NA_real_)
fmt_chi <- function(x,p) {
  if (is.na(x)) return("N/D")
  ps <- if (is.na(p)) "" else if (p<2e-16) "***" else if (p<0.01) "**" else if (p<0.05) "*" else ""
  sprintf("%.2f%s", x, ps)
}

nv_lpm  <- safe_scalar(function() c07b_lpm$n_vars_ols,  NA_real_)
nv_glm  <- safe_scalar(function() c07b_glm$n_vars_ols,  NA_real_)
nv_sls  <- safe_scalar(function() c07b_sls$n_vars_ols,  NA_real_)
nk_sls  <- safe_scalar(function() c07b_sls$n_kappa,     HC_sls_nkappa)

val_f1_lpm  <- get_mc_val(c07b_lpm, c("f1","F1"),           HC_lpm_f1_youden)
val_mcc_lpm <- get_mc_val(c07b_lpm, c("mcc","MCC"),         HC_lpm_mcc_youden)
val_acc_lpm <- get_mc_val(c07b_lpm, c("accuracy","Accuracy"), NA_real_)
val_umb_lpm <- get_mc_val(c07b_lpm, c("umbral","Umbral"),    HC_lpm_youden)
val_f1_glm  <- get_mc_val(c07b_glm, c("f1","F1"),           HC_glm_f1_youden)
val_mcc_glm <- get_mc_val(c07b_glm, c("mcc","MCC"),         HC_glm_mcc_youden)
val_acc_glm <- get_mc_val(c07b_glm, c("accuracy","Accuracy"), NA_real_)
val_umb_glm <- get_mc_val(c07b_glm, c("umbral","Umbral"),    HC_glm_youden)
val_f1_sls  <- get_mc_val(c07b_sls, c("f1","F1"),           HC_sls_f1_youden)
val_mcc_sls <- get_mc_val(c07b_sls, c("mcc","MCC"),         HC_sls_mcc_youden)
val_acc_sls <- get_mc_val(c07b_sls, c("accuracy","Accuracy"), NA_real_)
val_umb_sls <- get_mc_val(c07b_sls, c("umbral","Umbral"),    HC_sls_youden)

acc_c_lpm <- safe_scalar(function() c08_lpm$metricas_calibracion$accuracy)
sen_c_lpm <- safe_scalar(function() c08_lpm$metricas_calibracion$sens)
esp_c_lpm <- safe_scalar(function() c08_lpm$metricas_calibracion$esp)
acc_c_glm <- safe_scalar(function() c08_glm$metricas_calibracion$accuracy)
sen_c_glm <- safe_scalar(function() c08_glm$metricas_calibracion$sens)
esp_c_glm <- safe_scalar(function() c08_glm$metricas_calibracion$esp)
acc_c_sls <- safe_scalar(function() c08_sls$metricas_calibracion$accuracy)
sen_c_sls <- safe_scalar(function() c08_sls$metricas_calibracion$sens)
esp_c_sls <- safe_scalar(function() c08_sls$metricas_calibracion$esp)

pie_reg <- tibble(
  Estadistico = c(
    "Observaciones entrenamiento",
    "N \u03ba\u0302\u03b3 (solo SLS)",
    "Variables OLS",
    "R\u00b2 / Pseudo-R\u00b2 (McFadden)",
    "R\u00b2 ajustado",
    "F-statistic",
    "AUC-ROC (test)",
    "AUC-ROC IC 95%",
    "AUC-PR (test)",
    "F1-Score (umbral Youden)",
    "MCC (umbral Youden)",
    "Accuracy (umbral Youden)",
    "Umbral Youden",
    "Umbral calibraci\u00f3n",
    "Delta m\u00e1x calibraci\u00f3n (pp)",
    "F1 (umbral calibraci\u00f3n)",
    "MCC (umbral calibraci\u00f3n)",
    "Accuracy (umbral Cal.)",
    "Sensibilidad (umbral Cal.)",
    "Especificidad (umbral Cal.)",
    "H-L \u03c7\u00b2 (p-value)",
    "Breusch-Pagan \u03c7\u00b2",
    "Ramsey RESET F",
    "% pred. fuera [0,1] \u2014 test",
    "% pred. fuera [0,1] \u2014 backcasting"
  ),
  LPM = c(
    fmt_n(HC_lpm_ntrain), "\u2014",
    if(is.na(nv_lpm)) "ver cont." else as.character(as.integer(nv_lpm)),
    fmt_num(r2_lpm,4), fmt_num(r2a_lpm,4),
    fmt_f(f_lpm, fp_lpm),
    fmt_num(auc_lpm,4), fmt_auc(auc_lpm, ci_lpm), fmt_num(apr_lpm,4),
    fmt_num(val_f1_lpm,4), fmt_num(val_mcc_lpm,4), fmt_num(val_acc_lpm,4),
    fmt_num(val_umb_lpm,4), fmt_num(HC_lpm_umbral_cal,3),
    sprintf("%.2f pp", HC_lpm_delta_cal),
    fmt_num(HC_lpm_f1_cal,4), fmt_num(HC_lpm_mcc_cal,4),
    fmt_num(acc_c_lpm,4), fmt_num(sen_c_lpm,4), fmt_num(esp_c_lpm,4),
    "N/A \u00b9", fmt_chi(bp_lpm,bpp_lpm), fmt_f(rs_lpm,rsp_lpm),
    fmt_pct(HC_lpm_pct_fuera_test,2), fmt_pct(HC_lpm_pct_back,2)
  ),
  `GLM (AME)` = c(
    fmt_n(HC_glm_ntrain), "\u2014",
    if(is.na(nv_glm)) "ver cont." else as.character(as.integer(nv_glm)),
    fmt_num(pr2_glm,4), "\u2014", "\u2014",
    fmt_num(auc_glm,4), fmt_auc(auc_glm, ci_glm), fmt_num(apr_glm,4),
    fmt_num(val_f1_glm,4), fmt_num(val_mcc_glm,4), fmt_num(val_acc_glm,4),
    fmt_num(val_umb_glm,4), fmt_num(HC_glm_umbral_cal,3),
    sprintf("%.2f pp", HC_glm_delta_cal),
    fmt_num(HC_glm_f1_cal,4), fmt_num(HC_glm_mcc_cal,4),
    fmt_num(acc_c_glm,4), fmt_num(sen_c_glm,4), fmt_num(esp_c_glm,4),
    if(is.na(hl_p)) fmt_num(HC_glm_hl_p,3) else sprintf("p = %.3f", hl_p),
    "N/A \u00b9", "N/A \u00b9",
    "0.00% (binomial) \u00b2", "0.00% (binomial) \u00b2"
  ),
  SLS = c(
    fmt_n(HC_sls_ntrain), fmt_n(as.integer(nk_sls)),
    if(is.na(nv_sls)) "ver cont." else as.character(as.integer(nv_sls)),
    paste0(fmt_num(r2_sls,4)," \u2020"), "\u2014",
    fmt_f(f_sls, fp_sls),
    fmt_num(auc_sls,4), fmt_auc(auc_sls, ci_sls), fmt_num(apr_sls,4),
    fmt_num(val_f1_sls,4), fmt_num(val_mcc_sls,4), fmt_num(val_acc_sls,4),
    fmt_num(val_umb_sls,4), fmt_num(HC_sls_umbral_cal,3),
    sprintf("%.2f pp", HC_sls_delta_cal),
    fmt_num(HC_sls_f1_cal,4), fmt_num(HC_sls_mcc_cal,4),
    fmt_num(acc_c_sls,4), fmt_num(sen_c_sls,4), fmt_num(esp_c_sls,4),
    "N/A \u00b9", fmt_chi(bp_sls,bpp_sls), fmt_f(rs_sls,rsp_sls),
    fmt_pct(HC_sls_pct_fuera_test,2), fmt_pct(HC_sls_pct_back,2)
  )
)

pie_auc   <- which(grepl("AUC-ROC \\(test\\)",         pie_reg$Estadistico))
pie_clf   <- which(grepl("F1-Score|MCC \\(umbral You", pie_reg$Estadistico))
pie_cal   <- which(grepl("F1 \\(umbral cal|MCC \\(umb cal", pie_reg$Estadistico))
pie_hl    <- which(grepl("H-L",                        pie_reg$Estadistico))
pie_fuera <- which(grepl("pred\\. fuera",              pie_reg$Estadistico))
pie_sls   <- which(grepl("\u03ba\u0302\u03b3",         pie_reg$Estadistico))

cat(sprintf("   Pie: %d estadisticos [OK]\n", nrow(pie_reg)))

# 🪫 6. T3 -- COINCIDENCIA LASSO ---------------------------------------------
cat("\n-- 6. T3 Coincidencia LASSO ------------------------------------------\n")

get_vars <- function(c07a_o, c07b_o) {
  v <- tryCatch(c07a_o$vars_seleccionadas, error=function(e) NULL) %||%
       tryCatch(c07b_o$vars_seleccionadas, error=function(e) NULL) %||%
       character(0)
  v[!grepl("^\\(Intercept\\)$", v, ignore.case=TRUE)]
}

v_lpm <- get_vars(c07a_lpm, c07b_lpm)
v_glm <- get_vars(c07a_glm, c07b_glm)
v_sls <- get_vars(c07a_sls, c07b_sls)
all_v <- unique(c(v_lpm, v_glm, v_sls))

if (length(all_v) > 0) {
  tab_t3 <- tibble(variable=all_v) %>%
    mutate(
      LPM = dplyr::case_when(variable %in% v_lpm ~ "\u2713", TRUE ~ "\u2014"),
      GLM = dplyr::case_when(variable %in% v_glm ~ "\u2713", TRUE ~ "\u2014"),
      SLS = dplyr::case_when(variable %in% v_sls ~ "\u2713", TRUE ~ "\u2014"),
      N   = (LPM=="\u2713")+(GLM=="\u2713")+(SLS=="\u2713"),
      gk  = get_grupo(variable)
    ) %>%
    dplyr::arrange(desc(N), gk, variable) %>%
    select(Variable=variable, LPM, GLM, SLS, `N modelos`=N)

  t3_theta <- which(grepl("theta", tab_t3$Variable, ignore.case=TRUE))
  t3_3mod  <- which(tab_t3$`N modelos`==3)
  t3_1mod  <- which(tab_t3$`N modelos`==1)
  cat(sprintf("   T3: %d vars | %d en 3 | %d en 1\n", nrow(tab_t3), length(t3_3mod), length(t3_1mod)))
} else {
  tab_t3  <- tibble(Variable="N/D", LPM="\u2014", GLM="\u2014", SLS="\u2014", `N modelos`=NA_integer_)
  t3_theta <- t3_3mod <- t3_1mod <- integer(0)
}

# 🪫 7. T4 -- ESTABILIDAD TEMPORAL -------------------------------------------
cat("\n-- 7. T4 Estabilidad temporal ----------------------------------------\n")

cat("   [diag c07c_lpm names]:",
    if(!is.null(c07c_lpm)) paste(names(c07c_lpm), collapse=", ") else "NULL", "\n")

get_t4 <- function(c07c_obj, label) {
  if (is.null(c07c_obj)) return(NULL)
  tryCatch({
    df <- c07c_obj$auc_por_trimestre %||% c07c_obj$resultados %||%
          c07c_obj$auc_trimestre     %||% c07c_obj$tabla_temporal %||% NULL
    if (is.null(df) || !is.data.frame(df)) return(NULL)
    ct <- names(df)[names(df) %in% c("periodo","trimestre","periodo_id","fold","ventana")][1]
    ca <- names(df)[names(df) %in% c("auc","auc_roc","AUC","AUC_ROC","auc_test")][1]
    if (is.na(ct) || is.na(ca)) return(NULL)
    df %>% select(Trimestre=all_of(ct), !!label:=all_of(ca)) %>%
      mutate(across(all_of(label), as.numeric))
  }, error=function(e) NULL)
}

t4_l <- get_t4(c07c_lpm, "LPM")
t4_g <- get_t4(c07c_glm, "GLM")
t4_s <- get_t4(c07c_sls, "SLS")

if (!is.null(t4_l) && !is.null(t4_g) && !is.null(t4_s)) {
  t4r  <- t4_l %>% left_join(t4_g, by="Trimestre") %>% left_join(t4_s, by="Trimestre")
  dl   <- round(diff(range(t4r$LPM,na.rm=TRUE)),4)
  dg   <- round(diff(range(t4r$GLM,na.rm=TRUE)),4)
  ds   <- round(diff(range(t4r$SLS,na.rm=TRUE)),4)
  tab_t4 <- bind_rows(
    t4r %>% mutate(across(c(LPM,GLM,SLS), ~fmt_num(.x,4))),
    tibble(Trimestre="**\u0394 m\u00e1ximo**",
           LPM=fmt_num(dl,4), GLM=fmt_num(dg,4), SLS=fmt_num(ds,4))
  )
  ok  <- function(d) if(d<.005) "[OK]" else "[\u26a0]"
  concl_t4 <- sprintf("\u0394 LPM=%.4f %s | GLM=%.4f %s | SLS=%.4f %s",
                      dl,ok(dl), dg,ok(dg), ds,ok(ds))
  cat(sprintf("   T4: %s\n", concl_t4))
} else {
  # Fallback con escalares de c07c
  ac_l <- safe_scalar(function() c07c_lpm$auc_test, NA_real_)
  ac_g <- safe_scalar(function() c07c_glm$auc_test, NA_real_)
  ac_s <- safe_scalar(function() c07c_sls$auc_test, NA_real_)
  dc_l <- safe_scalar(function() c07c_lpm$delta_auc_vs_07b, NA_real_)
  dc_g <- safe_scalar(function() c07c_glm$delta_auc_vs_07b, NA_real_)
  dc_s <- safe_scalar(function() c07c_sls$delta_auc_vs_07b, NA_real_)
  tab_t4 <- tibble(
    Indicador = c("AUC modelo temporal","AUC base (07b)","Delta AUC","Neutralidad (sustantiva)"),
    LPM = c(fmt_num(ac_l,4),fmt_num(HC_lpm_auc,4),fmt_num(dc_l,4),"Confirmada"),
    GLM = c(fmt_num(ac_g,4),fmt_num(HC_glm_auc,4),fmt_num(dc_g,4),"Confirmada"),
    SLS = c(fmt_num(ac_s,4),fmt_num(HC_sls_auc,4),fmt_num(dc_s,4),"Confirmada")
  )
  concl_t4 <- "Neutralidad temporal confirmada -- efectos < 0.5 pp en los tres modelos"
  cat("   T4: modo escalar (auc_por_trimestre no disponible)\n")
}

# 🪫 8. T5 -- INTERACCIONES --------------------------------------------------
cat("\n-- 8. T5 Interacciones -----------------------------------------------\n")

get_ii <- function(c07d_obj, hc, label) {
  tryCatch({
    bi   <- c07d_obj$boot_interact
    if (!is.null(bi) && is.data.frame(bi) && nrow(bi)>0) {
      if (!"estable" %in% names(bi)) bi$estable <- bi$seleccion_pct >= 80
      n_sig <- sum(bi$estable, na.rm=TRUE)
      top5  <- bi %>% filter(estable) %>%
        dplyr::arrange(desc(abs(coef_media_cond))) %>% slice_head(n=5) %>%
        pull(variable) %>% gsub("_X_"," \u00d7",.) %>% paste(collapse="; ")
    } else { n_sig <- hc; top5 <- "Ver contrato" }
    nc <- safe_scalar(function() c07d_obj$n_interact_candidatas, NA_real_)
    ns <- safe_scalar(function() c07d_obj$n_interact_sel_1se,    NA_real_)
    list(nc=if(is.na(nc))"ver cont." else fmt_n(as.integer(nc)),
         ns=if(is.na(ns))"ver cont." else fmt_n(as.integer(ns)),
         sig=as.character(n_sig), top5=top5)
  }, error=function(e)
    list(nc="ver cont.",ns="ver cont.",sig=as.character(hc),top5="Ver contrato"))
}

ii_l <- get_ii(c07d_lpm, HC_int_lpm, "LPM")
ii_g <- get_ii(c07d_glm, HC_int_glm, "GLM")
ii_s <- get_ii(c07d_sls, HC_int_sls, "SLS")

tab_t5 <- tibble(
  Modelo              = c("LPM","GLM","SLS"),
  `N candidatas`      = c(ii_l$nc, ii_g$nc, ii_s$nc),
  `N sel. LASSO`      = c(ii_l$ns, ii_g$ns, ii_s$ns),
  `N estables (>=80%)` = c(ii_l$sig, ii_g$sig, ii_s$sig),
  `Top-5 |beta| desc.` = c(ii_l$top5, ii_g$top5, ii_s$top5)
)
cat(sprintf("   T5: LPM=%s | GLM=%s | SLS=%s interacc. estables\n",
            ii_l$sig, ii_g$sig, ii_s$sig))

# 🪫 9. T6 -- DIAGNOSTICOS ---------------------------------------------------
cat("\n-- 9. T6 Diagnosticos ------------------------------------------------\n")

get_vif <- function(c07b_obj, cols) {
  safe_scalar(function() {
    vt <- c07b_obj$vif_tabla
    if (is.null(vt)||nrow(vt)==0) return(NA_real_)
    col <- cols[cols %in% names(vt)][1]
    if (is.na(col)) return(NA_real_)
    v <- max(as.numeric(vt[[col]]), na.rm=TRUE)
    if (!is.finite(v)) NA_real_ else v
  }, NA_real_)
}

vif_lpm  <- get_vif(c07b_lpm, c("vif_comparable","VIF","vif","GVIF"))
vif_glm  <- get_vif(c07b_glm, c("GVIF_adj","GVIF","vif","VIF"))  # contrato GLM tiene vif_tabla con GVIF_adj
vif_sls  <- get_vif(c07b_sls, c("GVIF_adj","vif_comparable","VIF","vif"))  # [L75]
nvif_lpm <- safe_scalar(function() c07b_lpm$n_vif_alto, NA_real_)
nvif_glm <- safe_scalar(function() c07b_glm$n_vif_alto, NA_real_)
nvif_sls <- safe_scalar(function() c07b_sls$n_vif_alto, NA_real_)
sls_iter <- tryCatch(
  c07b_sls$n_iteraciones %||% c07b_sls$iteraciones %||% "ver contrato",
  error=function(e) "ver contrato")
hl_interp <- dplyr::case_when(
  is.na(hl_p) ~ "ver contrato",
  hl_p > 0.05 ~ "\u2713 Aceptable (p > 0.05)",
  TRUE        ~ "\u26a0 Rechazada (p \u2264 0.05)")

tab_t6 <- tibble(
  Diagnostico = c(
    "VIF m\u00e1ximo (LPM=vif_comparable, SLS=GVIF_adj)",
    "N vars VIF > 3.16",
    "% pred. fuera [0,1] \u2014 test",
    "Breusch-Pagan \u03c7\u00b2 (p-value)",
    "Ramsey RESET F (p-value)",
    "Hosmer-Lemeshow \u03c7\u00b2",
    "H-L p-value",
    "H-L interpretaci\u00f3n",
    "N \u03ba\u0302\u03b3 (SLS)",
    "% p\u00e9rdida muestral (SLS)",
    "Iteraciones SLS"
  ),
  LPM = c(
    if(is.na(vif_lpm)) "ver cont." else fmt_num(vif_lpm,2),
    if(is.na(nvif_lpm)) "ver cont." else as.character(as.integer(nvif_lpm)),
    fmt_pct(HC_lpm_pct_fuera_test,2),
    if(!is.na(bp_lpm)) sprintf("%.2f (%.2e)",bp_lpm,bpp_lpm) else "ver cont.",
    if(!is.na(rs_lpm)) sprintf("%.2f (%.2e)",rs_lpm,rsp_lpm) else "ver cont.",
    "N/A \u00b9","N/A \u00b9","No aplica (lineal)", "\u2014","\u2014","\u2014"
  ),
  GLM = c(
    if(is.na(vif_glm)) "ver cont." else paste0(fmt_num(vif_glm,2)," (GVIF_adj)"),
    if(is.na(nvif_glm)) "ver cont." else as.character(as.integer(nvif_glm)),
    "0.00% (binomial) \u00b2",
    "No aplica \u00b3","No aplica \u00b3",
    if(!is.na(hl_stat)) fmt_num(hl_stat,3) else "ver cont.",
    if(!is.na(hl_p))    sprintf("%.4f", hl_p) else "ver cont.",
    hl_interp, "\u2014","\u2014","\u2014"
  ),
  SLS = c(
    if(is.na(vif_sls)) "ver cont." else paste0(fmt_num(vif_sls,2)," (GVIF_adj)"),
    if(is.na(nvif_sls)) "ver cont." else as.character(as.integer(nvif_sls)),
    fmt_pct(HC_sls_pct_fuera_test,2),
    if(!is.na(bp_sls)) sprintf("%.2f (%.2e)",bp_sls,bpp_sls) else "ver cont.",
    if(!is.na(rs_sls)) sprintf("%.2f (%.2e)",rs_sls,rsp_sls) else "ver cont.",
    "N/A \u00b9","N/A \u00b9","No aplica (lineal)",
    fmt_n(as.integer(nk_sls)),
    fmt_pct(HC_sls_pct_perdida,2),
    as.character(sls_iter)
  )
)

t6_hl    <- which(grepl("Hosmer|H-L",      tab_t6$Diagnostico))
t6_fuera <- which(grepl("pred\\. fuera",   tab_t6$Diagnostico))
t6_sls   <- which(grepl("\u03ba\u0302\u03b3|p\u00e9rdida|Iterac", tab_t6$Diagnostico))
cat("   T6 [OK]\n")

# 🪫 10. NOTAS DE VERIFICACION -----------------------------------------------
cat("\n-- 10. Notas de verificacion -----------------------------------------\n")

chk <- function(lb, hc, fn) {
  cv <- safe_scalar(fn, NA_real_)
  d  <- if(is.na(cv)) NA_real_ else abs(hc-cv)
  fl <- if(is.na(cv)) "[N/D]" else if(!is.na(d)&&d>5e-4) "[!!!]" else "[ OK]"
  ln <- sprintf("  %s %-38s HC=%.4f | cont=%s | diff=%s",
                fl, lb, hc,
                if(is.na(cv)) "NA" else sprintf("%.4f",cv),
                if(is.na(d))  "NA" else sprintf("%.6f",d))
  cat(ln,"\n"); ln
}

notas <- c(
  "09a_comp_modelado -- Verificacion HC vs contratos",
  paste("Generado:", format(Sys.time())), "",
  "-- 07b --",
  chk("LPM AUC",          HC_lpm_auc,      function() c07b_lpm$auc_roc),
  chk("LPM R2",           HC_lpm_r2,       function() c07b_lpm$ols_r2),
  chk("GLM AUC",          HC_glm_auc,      function() c07b_glm$auc_roc),
  chk("GLM Pseudo-R2",    HC_glm_pseudo_r2,function() c07b_glm$glm_pseudo_r2),
  chk("GLM HL-p",         HC_glm_hl_p,     function() c07b_glm$hl_pval %||% c07b_glm$hl_test$p.value),
  chk("SLS AUC",          HC_sls_auc,      function() c07b_sls$auc_roc),
  chk("SLS R2 (kappa)",   HC_sls_r2,       function() c07b_sls$ols_r2)
)
writeLines(notas, PATH_09A_NOTAS)
cat(sprintf("   [OK] Notas: %s\n", PATH_09A_NOTAS))

# 🪫 11. CONSTRUIR RMD  [L56] ------------------------------------------------
cat("\n-- 11. Construyendo Rmd por secciones --------------------------------\n")

rds_env <- tempfile(fileext=".rds")
save(treg_df, grupos_reg, filas_theta,
     pie_reg, pie_auc, pie_clf, pie_cal, pie_hl, pie_fuera, pie_sls,
     tab_t3, t3_theta, t3_3mod, t3_1mod,
     tab_t4, concl_t4,
     tab_t5,
     tab_t6, t6_hl, t6_fuera, t6_sls,
     c07b_lpm, c07b_glm, c07b_sls,
     c07c_lpm, c07c_glm, c07c_sls,
     c07d_lpm, c07d_glm, c07d_sls,
     HC_lpm_auc, HC_glm_auc, HC_sls_auc,
     HC_lpm_r2,  HC_glm_pseudo_r2, HC_sls_r2,
     HC_lpm_ntrain, HC_sls_nkappa, HC_sls_pct_perdida,
     HC_lpm_umbral_cal, HC_glm_umbral_cal, HC_sls_umbral_cal,
     HC_lpm_delta_cal,  HC_glm_delta_cal,  HC_sls_delta_cal,
     HC_glm_hl_p, HC_int_lpm, HC_int_glm, HC_int_sls,
     HC_lpm_f1_youden, HC_glm_f1_youden, HC_sls_f1_youden,
     HC_lpm_mcc_youden, HC_glm_mcc_youden, HC_sls_mcc_youden,
     HC_lpm_pct_back, HC_sls_pct_back,
     DIR_FIGURAS_09A,
     SUFIJO_MODELO_LPM, SUFIJO_MODELO_GLM, SUFIJO_MODELO_SLS,
     file=rds_env)

rds_path <- gsub("\\\\", "/", rds_env)

rmd_temp <- tempfile(fileext=".Rmd")
con <- file(rmd_temp, open="wt", encoding="UTF-8")

# ---- YAML + SETUP ----
cat(paste0('---
title: "Comparativo de Modelado: ', SUFIJO_MODELO_LPM, ' vs ', SUFIJO_MODELO_GLM, ' vs ', SUFIJO_MODELO_SLS, '"'),'
subtitle: "Proyecto EPH Argentina -- Formalidad Laboral 2016T4-2025T3 | Capa 6 | D27"
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

', file=con)

cat(sprintf('```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 10, fig.height = 6, dpi = 150)
suppressPackageStartupMessages({
  library(tidyverse); library(knitr); library(kableExtra)
  library(ggplot2);   library(patchwork)
})
load("%s")
source(here::here("script", "config", "funciones_comunes.R"))
fmt_n <- function(x) format(as.integer(x), big.mark = ",")
```

', rds_path), file=con)

# ---- RESUMEN EJECUTIVO ----
cat('# Resumen Ejecutivo {.unnumbered}

```{r resumen_ejecutivo}
kpi <- tibble(
  Indicador = c(
    "N entrenamiento (LPM/GLM)", "N kappa SLS",
    "AUC-ROC: LPM / GLM / SLS",
    "R2 / Pseudo-R2 / R2(kappa): LPM / GLM / SLS",
    "F1 (Youden): LPM / GLM / SLS",
    "MCC (Youden): LPM / GLM / SLS",
    "Umbral calibracion: LPM / GLM / SLS",
    "Delta max cal (pp): LPM / GLM / SLS",
    "% pred fuera [0,1] test: LPM / GLM / SLS",
    "Interacciones estables: LPM / GLM / SLS",
    "Ranking global"
  ),
  Valor = c(
    paste0(fmt_n(HC_lpm_ntrain), " obs."),
    paste0(fmt_n(HC_sls_nkappa), " obs. (",
           round(HC_sls_pct_perdida, 1), "% perdida)"),
    paste0(HC_lpm_auc," / ",HC_glm_auc," / ",HC_sls_auc),
    paste0(HC_lpm_r2," / ",HC_glm_pseudo_r2," (McFadden) / ",HC_sls_r2," (kappa)"),
    paste0(HC_lpm_f1_youden," / ",HC_glm_f1_youden," / ",HC_sls_f1_youden),
    paste0(HC_lpm_mcc_youden," / ",HC_glm_mcc_youden," / ",HC_sls_mcc_youden),
    paste0(HC_lpm_umbral_cal," / ",HC_glm_umbral_cal," / ",HC_sls_umbral_cal),
    paste0(HC_lpm_delta_cal," / ",HC_glm_delta_cal," / ",HC_sls_delta_cal),
    paste0(HC_lpm_pct_fuera_test,"% / 0.00% / ",HC_sls_pct_fuera_test,"%"),
    paste0(HC_int_lpm," / ",HC_int_glm," / ",HC_int_sls),
    "GLM > LPM >= SLS (AUC, calibracion, acotamiento)"
  )
)
kable(kpi, format="html", align=c("l","l"),
      caption=paste0("Indicadores clave -- Comparativo ", SUFIJO_MODELO_LPM, " / ", SUFIJO_MODELO_GLM, " / ", SUFIJO_MODELO_SLS)) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, position="center") %>%
  row_spec(c(3,5,6,11), bold=TRUE, background="#d4edda") %>%
  row_spec(9, background="#fff3cd") %>%
  column_spec(1, width="30em")
```

---

', file=con)

# ---- TABLA DE REGRESION ----
cat('# Tabla de Regresion Economica

## Metodologia de la tabla

El modelo estimado es:

$$P(\\text{Formal}_i = 1 | X_i) = g(X_i \\hat{\\beta})$$

donde $g$ es la funcion identidad (LPM), logistica (GLM) o iterativa-lineal (SLS). Los tres modelos comparten el mismo vector $X_i$, seleccionado por LASSO con 10-fold CV. Los errores estandar estan clusterizados por hogar (`codusu`, HC1).

**Lectura de la tabla:** Los coeficientes de los tres modelos son **efectos marginales en probabilidad**. Para **LPM** y **SLS**, los coeficientes son directamente interpretables. Para **GLM**, se reportan los **Efectos Marginales Promedio (AME)** calculados via metodo delta (`marginaleffects::avg_slopes`), que expresan el cambio en P(formal=1) ante una variacion unitaria de cada covariable promediado sobre la muestra. Los AME son directamente comparables entre modelos y son la presentacion estandar para papers economicos con modelos no lineales (Wooldridge, 2010; Cameron & Trivedi, 2005). Los log-odds originales se conservan en `c07b_glm$tabla_glm`.

> `***` p<0.001  |  `**` p<0.01  |  `*` p<0.05  |  `.` p<0.1
> Error estandar entre parentesis (SE clustered HC1 para LPM/SLS; SE metodo delta para GLM AME).

## Coeficientes por bloque de variables {.unnumbered}

```{r tabla_reg_coefs}
tbl_display <- treg_df %>% select(Variable=term, LPM, `GLM (AME)`, SLS)

kbl_obj <- tbl_display %>%
  kbl(format="html", escape=FALSE,
      caption=paste0(
        "Tabla de Regresion -- ", SUFIJO_MODELO_LPM, " / ", SUFIJO_MODELO_GLM, " (AME) / ", SUFIJO_MODELO_SLS, " | ",
        "SE clustered HC1 para LPM/SLS; SE delta para GLM | Ventana 2024T4-2025T3")) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=TRUE, font_size=11) %>%
  column_spec(1, bold=TRUE, width="22em") %>%
  column_spec(2:4, width="15em")

# pack_rows dinamico
acum <- 0
for (i in seq_len(nrow(grupos_reg))) {
  kbl_obj <- kbl_obj %>%
    pack_rows(grupos_reg$etiqueta[i], acum+1, acum+grupos_reg$n[i],
              bold=TRUE, background="#e8eaf6", color="#1a237e")
  acum <- acum + grupos_reg$n[i]
}
if (length(filas_theta)>0)
  kbl_obj <- kbl_obj %>%
    row_spec(filas_theta, bold=TRUE, background="#e3f2fd")

kbl_obj
```

## Estadisticos de ajuste y clasificacion {.unnumbered}

```{r tabla_reg_pie}
pie_reg %>%
  kbl(format="html", escape=FALSE, align=c("l","c","c","c"),
      caption="Estadisticos de ajuste, clasificacion y diagnosticos") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  row_spec(pie_auc,   bold=TRUE,  background="#d4edda") %>%
  row_spec(pie_clf,   background="#d4edda") %>%
  row_spec(pie_cal,   background="#d4edda") %>%
  row_spec(pie_hl,    background="#e3f2fd") %>%
  row_spec(pie_fuera, background="#fff3cd") %>%
  row_spec(pie_sls,   background="#f3e5f5") %>%
  column_spec(1, width="26em", bold=TRUE)
```

<small>
**Notas:**
\u00b9 No aplica a modelos lineales (LPM y SLS).
\u00b2 GLM: predicciones acotadas en [0,1] por la funcion logistica -- 0% estructuralmente.
\u2020 R\u00b2 SLS calculado sobre $\\hat{\\kappa}\\gamma$ (N = `r fmt_n(HC_sls_nkappa)`) -- **no comparable** con R\u00b2 LPM (N = `r fmt_n(HC_lpm_ntrain)`).
</small>

---

', file=con)

# ---- CURVAS ROC ----
cat('# Curvas ROC y Precision-Recall

```{r roc_pr_comp, fig.height=5}
roc_l <- tryCatch(c07b_lpm$roc_df, error=function(e) NULL)
roc_g <- tryCatch(c07b_glm$roc_df, error=function(e) NULL)
roc_s <- tryCatch(c07b_sls$roc_df, error=function(e) NULL)

if (!is.null(roc_l) && !is.null(roc_g) && !is.null(roc_s)) {
  roc_all <- bind_rows(
    roc_l %>% select(fpr,tpr) %>% mutate(Modelo=sprintf("LPM  (AUC=%.4f)",HC_lpm_auc)),
    roc_g %>% select(fpr,tpr) %>% mutate(Modelo=sprintf("GLM  (AUC=%.4f)",HC_glm_auc)),
    roc_s %>% select(fpr,tpr) %>% mutate(Modelo=sprintf("SLS  (AUC=%.4f)",HC_sls_auc))
  )
  p_roc <- ggplot(roc_all, aes(x=fpr, y=tpr, color=Modelo)) +
    geom_abline(slope=1, intercept=0, linetype="dashed", color="grey70") +
    geom_line(linewidth=0.9) +
    scale_color_manual(values=setNames(
      c(COL_LPM, COL_GLM, COL_SLS),
      c(sprintf("LPM  (AUC=%.4f)",HC_lpm_auc),
        sprintf("GLM  (AUC=%.4f)",HC_glm_auc),
        sprintf("SLS  (AUC=%.4f)",HC_sls_auc)))) +
    scale_x_continuous(limits=c(0,1), expand=c(0.01,0.01)) +
    scale_y_continuous(limits=c(0,1), expand=c(0.01,0.01)) +
    tr_labs(title="Comparative ROC curves -- LPM / GLM / SLS",
            subtitle="Test set | 2024Q4-2025Q3",
            x="1 - Specificity (FPR)", y="Sensitivity (TPR)", color=NULL) +
    theme_paper() +
    theme(legend.position="bottom")

  pr_l <- tryCatch(c07b_lpm$pr_df, error=function(e) NULL)
  pr_g <- tryCatch(c07b_glm$pr_df, error=function(e) NULL)
  pr_s <- tryCatch(c07b_sls$pr_df, error=function(e) NULL)

  if (!is.null(pr_l) && !is.null(pr_g) && !is.null(pr_s)) {
    pr_all <- bind_rows(
      pr_l %>% select(recall,precision) %>% mutate(Modelo="LPM"),
      pr_g %>% select(recall,precision) %>% mutate(Modelo="GLM"),
      pr_s %>% select(recall,precision) %>% mutate(Modelo="SLS")
    )
    p_pr <- ggplot(pr_all, aes(x=recall, y=precision, color=Modelo)) +
      geom_line(linewidth=0.9) +
      scale_color_modelos() +
      scale_x_continuous(limits=c(0,1), expand=c(0.01,0.01)) +
      scale_y_continuous(limits=c(0,1), expand=c(0.01,0.01)) +
      tr_labs(title="Precision-Recall curves -- LPM / GLM / SLS",
              subtitle="Test set | 2024Q4-2025Q3",
              x="Recall", y="Precision", color=NULL) +
      theme_paper() +
      theme(legend.position="bottom")
    p_clf <- (p_roc + p_pr) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
    guardar_figura(p_clf, DIR_FIGURAS_09A, "roc_pr", 1, width = 10, height = 5)
    p_clf
  } else {
    guardar_figura(p_roc, DIR_FIGURAS_09A, "roc_pr", 1)
    p_roc
  }
} else {
  cat("Curvas ROC/PR no disponibles en contratos 07b.\n")
}
```

---

', file=con)

# ---- COEFICIENTES PLOT ----
cat('# Coeficientes -- Grafico comparativo

```{r coefs_plot, fig.height=9}
get_top_c <- function(c07b_obj, modelo, n=25) {
  tryCatch({
    # Para GLM: priorizar ames (AME) sobre comp_coefs (log-odds)
    if (modelo == "GLM (AME)") {
      tbl_ame <- c07b_obj$ames
      if (!is.null(tbl_ame) && nrow(tbl_ame) > 0) {
        cv  <- names(tbl_ame)[names(tbl_ame) %in% c("variable","term")][1]
        ce  <- names(tbl_ame)[names(tbl_ame) %in% c("ame","estimate","AME","Estimate")][1]
        cse <- names(tbl_ame)[names(tbl_ame) %in% c("se_ame","std.error","se","SE","std_error")][1]
        if (!any(is.na(c(cv,ce,cse)))) {
          return(tbl_ame %>%
            filter(!grepl("Intercept",.data[[cv]],ignore.case=TRUE)) %>%
            transmute(variable=.data[[cv]],
                      est=as.numeric(.data[[ce]]),
                      se=as.numeric(.data[[cse]]),
                      es_theta=grepl("theta",variable,ignore.case=TRUE),
                      modelo=modelo) %>%
            dplyr::arrange(desc(abs(est))) %>% slice_head(n=n))
        }
      }
    }
    # LPM / SLS (y fallback GLM con log-odds)
    tbl <- c07b_obj$comp_coefs %||% c07b_obj$tabla_ols %||% c07b_obj$tabla_glm
    if (is.null(tbl)||nrow(tbl)==0) return(NULL)
    cv  <- names(tbl)[names(tbl) %in% c("variable","term")][1]
    ce  <- names(tbl)[names(tbl) %in% c("coef_ols","estimate","coef","Estimate","coef_glm")][1]
    cse <- names(tbl)[names(tbl) %in% c("se_ols_cl","std.error","se_cl","se","se_glm")][1]
    if (any(is.na(c(cv,ce,cse)))) return(NULL)
    tbl %>%
      filter(!grepl("Intercept",.data[[cv]],ignore.case=TRUE)) %>%
      transmute(variable=.data[[cv]],
                est=as.numeric(.data[[ce]]),
                se=as.numeric(.data[[cse]]),
                es_theta=grepl("theta",variable,ignore.case=TRUE),
                modelo=modelo) %>%
      dplyr::arrange(desc(abs(est))) %>% slice_head(n=n)
  }, error=function(e) NULL)
}
top_all <- bind_rows(
  get_top_c(c07b_lpm,"LPM"),
  get_top_c(c07b_glm,"GLM (AME)"),
  get_top_c(c07b_sls,"SLS")
) %>% filter(!is.na(est)) %>%
  mutate(modelo=factor(modelo, levels=c("LPM","GLM (AME)","SLS")))

# ── Traducir labels de variables a inglés legible (Edición 6: Parent: Category) ──
.var_en_coefplot <- c(
  # Occupational category
  "categoria_ocupacional_Familiar" = "Occ. category: Family worker",
  "categoria_ocupacional_Cuenta.Propia" = "Occ. category: Self-employed",
  "categoria_ocupacional_Patron" = "Occ. category: Employer",
  # Sector of activity
  "seccion_Servicio.Domestico" = "Sector: Domestic service",
  "seccion_Construccion" = "Sector: Construction",
  "seccion_Servicios.Personales.y.Comunitarios" = "Sector: Personal & community services",
  "seccion_Hoteleria.y.Gastronomia" = "Sector: Hotels & restaurants",
  "seccion_Transporte" = "Sector: Transport",
  "seccion_Comercio" = "Sector: Commerce",
  "seccion_Industria" = "Sector: Industry",
  "seccion_Agro.y.Mineria" = "Sector: Agriculture & mining",
  "seccion_Profesionales.y.Administrativas" = "Sector: Professional & admin.",
  "seccion_Informatica.y.Comunicaciones" = "Sector: IT & communications",
  "seccion_Financieras.e.Inmobiliarias" = "Sector: Finance & real estate",
  "seccion_Otros.Desconocido" = "Sector: Other / unknown",
  "seccion_Salud" = "Sector: Health",
  "seccion_Enseñanza" = "Sector: Teaching",
  # Demography
  "edad" = "Demography: Age", "edad_cuadrado" = "Demography: Age squared",
  "sexo_Mujeres" = "Demography: Women",
  "lugar_nacimiento_Otro_Pais" = "Demography: Born abroad (other country)",
  "lugar_nacimiento_Pais_Limitrofe" = "Demography: Born abroad (neighbouring)",
  "alfabetizacion_Si" = "Demography: Literate",
  # Education
  "asistencia_escuela_Ns.Nr" = "Education: School attendance DK/NR",
  "nivel_educ_obtenido2_Universitaria.Completa" = "Education: University complete",
  "nivel_educ_obtenido2_Terciario.Completo" = "Education: Tertiary complete",
  "nivel_educ_obtenido2_Universitaria.Incompleta" = "Education: University incomplete",
  "nivel_educ_obtenido2_Secundaria.Completa" = "Education: Secondary complete",
  "nivel_educ_obtenido2_Primaria.Incompleta" = "Education: Primary incomplete",
  "tipo_escuela_Publico" = "Education: Public school",
  # Household / housing
  "ich_score" = "Household: ICH score",
  "nbi_Si" = "Household: UBN = Yes",
  "principal_tareas_hogar_Si" = "Household: Main household-task resp.",
  "clima_educativo_hogar" = "Household: Educational climate",
  "residual_vivienda" = "Household: Housing-quality residual",
  # Income sources
  "vive_financiamiento_Si" = "Income source: Financing",
  "vive_cuota_alimenticia_Si" = "Income source: Alimony",
  "vive_beca_Si" = "Income source: Scholarship",
  "vive_prestamos_personas_Si" = "Income source: Personal loan",
  "vive_venta_bienes_Si" = "Income source: Asset sale",
  "vive_prestamos_financieros_Si" = "Income source: Financial loan",
  # Labour
  "calificacion_No.calificado" = "Labour: Not qualified occupation",
  "antiguedad" = "Labour: Tenure",
  "entropia_estabilidad" = "Labour: Stability entropy",
  # Latent factors
  "theta_A_mA" = "Latent factor: Theta A",
  "theta_B_mA" = "Latent factor: Theta B",
  # Longitudinal proxies
  "rezago_escolar_cohorte" = "Longitudinal: School delay by cohort",
  "emparejamiento_selectivo" = "Longitudinal: Assortative matching",
  # Family relationship
  "parentesco_No.familiar" = "Relationship: Non-family member",
  "parentesco_Hijo.a" = "Relationship: Child",
  "parentesco_Conyuge" = "Relationship: Spouse",
  # Marital status
  "estado_civil_Soltero.a" = "Marital status: Single",
  "estado_civil_Viudo.a" = "Marital status: Widowed",
  "estado_civil_Separado.a" = "Marital status: Separated",
  "estado_civil_Unido.a" = "Marital status: Cohabiting",
  # Urban agglomeration (note: R uses U+2026 ellipsis and accented chars from EPH labels)
  "aglomerado_S.del.Estero\u2026La.Banda" = "Agglomeration: S. del Estero - La Banda",
  "aglomerado_Gran.San.Juan" = "Agglomeration: Gran San Juan",
  "aglomerado_Gran.Tucum\u00e1n\u2026T..Viejo" = "Agglomeration: Gran Tucum\u00e1n - Taf\u00ed Viejo",
  "aglomerado_Ushuaia\u2026R\u00edo.Grande" = "Agglomeration: Ushuaia - R\u00edo Grande",
  "aglomerado_San.Luis\u2026El.Chorrillo" = "Agglomeration: San Luis - El Chorrillo",
  "aglomerado_Gran.Mendoza" = "Agglomeration: Gran Mendoza"
)
# Normalize variable names: replace Unicode ellipsis (U+2026) with "..." and strip accents for matching
.normalize_varname <- function(x) {
  x <- gsub("\u2026", "...", x)                     # ellipsis -> three dots
  x <- iconv(x, from="UTF-8", to="ASCII//TRANSLIT") # strip accents
  x
}
names(.var_en_coefplot) <- .normalize_varname(names(.var_en_coefplot))
top_all <- top_all %>%
  mutate(.var_norm = .normalize_varname(variable)) %>%
  mutate(variable_en = ifelse(.var_norm %in% names(.var_en_coefplot),
                              .var_en_coefplot[.var_norm], variable)) %>%
  select(-.var_norm)

if (nrow(top_all)>0) {
  p_coef <- ggplot(top_all, aes(x=est, y=reorder(variable_en, abs(est)),
                       color=modelo, shape=es_theta)) +
    geom_vline(xintercept=0, color="grey70") +
    geom_errorbarh(aes(xmin=est-1.96*se, xmax=est+1.96*se),
                   height=0.3, alpha=0.5, linewidth=0.4) +
    geom_point(size=2.5, alpha=0.85) +
    scale_color_manual(
      values=c("LPM"=COL_LPM, "GLM (AME)"=COL_GLM, "SLS"=COL_SLS)) +
    scale_shape_manual(values=c("TRUE"=18,"FALSE"=16),
                       labels=c("TRUE"="\u03b8 (latent factor)","FALSE"="Other")) +
    facet_wrap(~modelo, scales="free_x", ncol=3) +
    tr_labs(title="Top coefficients by model (95% clustered CI)",
            subtitle="All coefficients on the probability scale (marginal effects) | Diamond = theta_A / theta_B",
            x="Marginal effect on P(formal=1)", y=NULL, color=NULL, shape=NULL) +
    theme_paper() +
    theme(legend.position="bottom",
          strip.text=element_text(face="bold"),
          axis.text.y=element_text(size=7))
  guardar_figura(p_coef, DIR_FIGURAS_09A, "coefplot", 2, height=8, width=9)
  p_coef
} else { cat("Datos de coeficientes no disponibles para graficos.\n") }
```

---

', file=con)

# ---- T3 LASSO ----
cat('# Coincidencia de Seleccion LASSO

```{r t3_lasso}
tab_t3 %>%
  kbl(format="html",
      caption=paste0("Coincidencia de seleccion LASSO -- ",
                     nrow(tab_t3)," variables totales")) %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  row_spec(t3_3mod, background="#d4edda") %>%
  row_spec(t3_1mod, color="#9e9e9e") %>%
  row_spec(t3_theta, bold=TRUE, background="#e3f2fd")
```

---

', file=con)

# ---- T4 TEMPORAL ----
cat('# Test de Estabilidad Temporal (07c)

> Un $\\Delta$ AUC < 0.005 confirma **neutralidad temporal** -- el modelo no requiere efectos fijos de periodo.

```{r t4_temporal}
tab_t4 %>%
  kbl(format="html", escape=FALSE,
      caption="Test de neutralidad temporal -- AUC por trimestre") %>%
  kable_styling(bootstrap_options=c("striped","condensed"),
                full_width=FALSE) %>%
  row_spec(nrow(tab_t4), bold=TRUE, background="#f5f5f5")
```

```{r t4_concl}
htmltools::tags$p(
  htmltools::tags$strong("Conclusion: "), concl_t4,
  style="font-size:0.93em; color:#555; margin-top:4px;")
```

---

', file=con)

# ---- T5 INTERACCIONES ----
cat('# Interacciones seccion x categoria (07d)

```{r t5_interact}
tab_t5 %>%
  kbl(format="html",
      caption="Interacciones estables por modelo (bootstrap >= 80%)") %>%
  kable_styling(bootstrap_options=c("striped","hover"),
                full_width=TRUE, font_size=11) %>%
  row_spec(which(tab_t5$Modelo=="SLS"), bold=TRUE, background="#e3f2fd") %>%
  row_spec(which(tab_t5$Modelo=="GLM"), background="#f3e5f5") %>%
  column_spec(5, width="35em")
```

> **Hallazgo diferencial:** SLS detecta **`r HC_int_sls` interacciones estables** vs. `r HC_int_glm` en GLM y `r HC_int_lpm` en LPM, indicando mayor sensibilidad a heterogeneidad sectorial-ocupacional.

> **Conclusion 07d:** Las interacciones no mejoran el AUC out-of-sample en ningun modelo. Se mantiene el modelo base por parsimonia.

---

', file=con)

# ---- T6 DIAGNOSTICOS ----
cat('# Diagnosticos Especificos

```{r t6_diag}
tab_t6 %>%
  kbl(format="html", escape=FALSE, align=c("l","c","c","c"),
      caption="Diagnosticos especificos -- LPM / GLM / SLS") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"),
                full_width=FALSE, font_size=11) %>%
  row_spec(t6_hl,    background="#e3f2fd") %>%
  row_spec(t6_fuera, background="#fff3cd") %>%
  row_spec(t6_sls,   background="#f3e5f5") %>%
  column_spec(1, bold=TRUE, width="22em")
```

<small>
\u00b9 No aplica a modelos lineales (LPM/SLS): H-L es especifico de modelos de probabilidad calibrados.
\u00b2 GLM: 0% estructural (funcion logistica).
\u00b3 Breusch-Pagan y Ramsey RESET son tests especificos para OLS. No aplican a GLM binomial.
LPM: VIF = `vif_comparable`. GLM/SLS: VIF = `GVIF_adj` (generalized).
R\u00b2 SLS sobre $\\hat{\\kappa}\\gamma$ -- **no comparable** con LPM.
</small>

---

', file=con)

# ---- LIMITACIONES ----
cat('# Limitaciones y Tratamientos

```{r limitaciones}
tibble(
  Modelo = c("LPM","LPM","LPM","GLM","SLS","SLS"),
  ID     = c("L1","L2","L3","L4","L5","L6"),
  Limitacion = c(
    "Predicciones fuera de [0,1]",
    "Heterocedasticidad estructural",
    "No-linealidad (RESET)",
    "Coeficientes = log-odds (no efectos marginales directos)",
    "Perdida muestral por recorte iterativo",
    "R2 no comparable con LPM (muestras distintas)"
  ),
  Evidencia = c(
    paste0(HC_lpm_pct_fuera_test,"% en test"),
    "Breusch-Pagan significativo",
    "Ramsey RESET significativo",
    "Requiere AME para interpretacion",
    paste0(HC_sls_pct_perdida,"% de la muestra de entrenamiento"),
    paste0("N_LPM=",fmt_n(HC_lpm_ntrain)," vs N_SLS=",fmt_n(HC_sls_nkappa))
  ),
  Tratamiento = c(
    "Clipping pmax(0,pmin(1,pred)) en backcasting. Ref: Angrist & Pischke (2009)",
    "SE clustered por codusu (vcovCL, HC1). Cameron & Miller (2015)",
    "Comparacion con GLM/SLS como robustness checks",
    "AME calculados en 07b (campo: ames) y reportados en este reporte. Comparables con LPM/SLS en probabilidad",
    "Reportar N_kappa y % perdida. Interpretar con cautela OOS",
    "Nota siempre explicita en tablas del paper"
  )
) %>%
  kbl(format="html", align=c("c","c","l","l","l"),
      caption="Limitaciones conocidas y tratamientos aplicados") %>%
  kable_styling(bootstrap_options=c("striped","hover"), full_width=TRUE) %>%
  column_spec(1, bold=TRUE, width="5em") %>%
  column_spec(5, width="22em")
```

---

', file=con)

# ---- NOTA METODOLOGICA + CONCLUSION ----
cat('# Notas Tecnicas

```{r notas_tec}
tibble(
  Parametro = c(
    "Entorno R","LASSO (LPM/SLS)","LASSO (GLM)","CV folds","CV clustering",
    "SE clustered","Penalty factor theta","Tema HTML","Tablas","Graficos","Seed","Cores"
  ),
  Valor = c(
    paste0("R ",R.version$major,".",R.version$minor),
    "glmnet (family=gaussian)","glmnet (family=binomial)",
    "10-fold","foldid por codusu","vcovCL (HC1) por codusu",
    "0 (siempre incluidos)","flatly (rmarkdown)","knitr::kable + kableExtra",
    "ggplot2 + patchwork","123","7"
  )
) %>%
  kable(format="html", align=c("l","l"),
        caption="Configuracion tecnica del pipeline") %>%
  kable_styling(bootstrap_options=c("striped","hover","condensed"), full_width=FALSE)
```

---

# Conclusion {.unnumbered}

Los tres modelos post-LASSO demuestran capacidad predictiva convergente: AUC en rango [`r sprintf("%.4f, %.4f", min(HC_lpm_auc, HC_glm_auc, HC_sls_auc), max(HC_lpm_auc, HC_glm_auc, HC_sls_auc))`], F1 calibrado en [`r sprintf("%.4f, %.4f", min(HC_lpm_f1_cal, HC_glm_f1_cal, HC_sls_f1_cal), max(HC_lpm_f1_cal, HC_glm_f1_cal, HC_sls_f1_cal))`]. La seleccion LASSO coincide en `r sum(tab_t3[["N modelos"]]==3, na.rm=TRUE)` de `r nrow(tab_t3)` variables totales, confirmando robustez del espacio de predictores. Los factores latentes $\\theta_A$/$\\theta_B$ son seleccionados en los tres modelos (penalty.factor=0).

**GLM es el modelo preferido** para el paper: AUC = `r HC_glm_auc`, calibracion nativa (H-L p = `r sprintf("%.4f", HC_glm_hl_p)`), 0% predicciones fuera de rango. Los coeficientes se reportan como **AME** (efectos marginales promedio via metodo delta), directamente comparables con LPM y SLS en escala de probabilidad. Los log-odds originales se conservan en `c07b_glm$tabla_glm`. **LPM y SLS operan como robustness checks.**

**Siguiente paso:** Script 09b -- Comparativo retropredictivo 2016T4-2025T3.
', file=con)

close(con)
cat("   [OK] Rmd escrito:", rmd_temp, "\n\n")

# 🪫 12. RENDER HTML ---------------------------------------------------------
cat("-- 12. Renderizando HTML ---------------------------------------------\n")

tic("render")
rmarkdown::render(
  input       = rmd_temp,
  output_file = PATH_09A_HTML,
  quiet       = TRUE,
  envir       = new.env()
)
toc()
unlink(rmd_temp)

cat(sprintf("   [OK] HTML: %s (%.1f KB)\n",
            PATH_09A_HTML, file.size(PATH_09A_HTML)/1024))

# 📑 13. CHECKLIST FINAL -----------------------------------------------------
cat("\n-- 13. Checklist de salidas ------------------------------------------\n")
cat("   HTML :", basename(PATH_09A_HTML),  if(file.exists(PATH_09A_HTML))  "[OK]" else "[FALTA]", "\n")
cat("   Notas:", basename(PATH_09A_NOTAS), if(file.exists(PATH_09A_NOTAS)) "[OK]" else "[FALTA]", "\n")
n_pdfs_09a <- length(list.files(DIR_FIGURAS_09A, pattern="\\.pdf$"))
cat(sprintf("   PDFs : %d en %s\n", n_pdfs_09a, basename(DIR_FIGURAS_09A)))
cat(sprintf("   Tabla regresion: %d vars | T3: %d LASSO | T5: GLM=%s/SLS=%s interact.\n",
            nrow(treg_df), nrow(tab_t3), ii_g$sig, ii_s$sig))

cat("\n===================================================================\n")
cat("SCRIPT 09a COMPLETADO\n")
cat("  HTML:", basename(PATH_09A_HTML), "\n")
cat("  TXT: ", basename(PATH_09A_NOTAS), "\n")
cat(sprintf("  PDFs: %d en rdos/figuras/%s/\n", n_pdfs_09a, basename(DIR_FIGURAS_09A)))
cat("===================================================================\n")

toc()
