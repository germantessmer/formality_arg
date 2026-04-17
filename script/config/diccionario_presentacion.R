# =============================================================================
# [EN] diccionario_presentacion.R -- Translation dictionary (ES to EN) for plot labels, table headers, and factor levels
# INPUTS:  parametros.R (for IDIOMA setting)
# OUTPUTS: Functions: tr(), tr_labs(), tr_levels(), tr_df()
# =============================================================================
# ==============================================================================
# diccionario_presentacion.R
# Diccionario de traduccion ES <-> EN para la capa de presentacion
# ==============================================================================
# OBJETIVO:
#    Proveer traduccion transparente de strings en figuras y tablas.
#    Los datos y scripts permanecen en español. La traduccion ocurre
#    exclusivamente al momento de generar labels de presentacion.
#
# REQUISITO PREVIO:
#    source("parametros.R")   — necesario para IDIOMA
#
# EXPORTA:
#    IDIOMA               — "en" (paper) o "es" (tesis)
#    tr(x)                — traduce string(s) individuales
#    tr_labs(...)          — wrapper para labs() con traduccion automatica
#    tr_levels(x)          — traduce levels de un factor
#    tr_df(df, cols)        — traduce columnas especificas de un data.frame
#
# CONVENCIONES:
#    - ILO/World Bank standard para terminologia laboral
#    - Nombres de regiones y aglomerados: se mantienen en español (proper nouns)
#    - Codigos INDEC (PP07H, PP05I): se mantienen como identificadores tecnicos
# ==============================================================================

suppressPackageStartupMessages(library(ggplot2))

# ── 1. PARAMETRO DE IDIOMA ───────────────────────────────────────────────────
# Controla si tr() traduce (en) o devuelve el original (es)
IDIOMA <- "en"   # "en" = paper (default), "es" = tesis

# ── 2. DICCIONARIO ───────────────────────────────────────────────────────────
# Mapeo ES → EN. Si IDIOMA == "es", tr() devuelve el input sin cambios.
# Organizado por categoria semantica.

.DICT <- list(

  # ── Sectores economicos (SECCION_LEVELS) ──────────────────────────────────
  "Admin Publica y Defensa"          = "Public Admin. & Defence",
  "Agro y Mineria"                   = "Agriculture & Mining",
  "Comercio"                         = "Commerce",
  "Construccion"                     = "Construction",
  "Enseñanza"                        = "Teaching",

  "Financieras e Inmobiliarias"      = "Finance & Real Estate",
  "Hoteleria y Gastronomia"          = "Hotels & Restaurants",
  "Industria"                        = "Industry",
  "Informatica y Comunicaciones"     = "ICT",
  "Organismos Extraterritoriales"    = "Extraterritorial Orgs.",
  "Otros/Desconocido"                = "Other/Unknown",
  "Profesionales y Administrativas"  = "Professional & Admin.",
  "Salud"                            = "Health",
  "Servicio Domestico"               = "Domestic Service",
  "Servicios Personales y Comunitarios" = "Personal & Community Svc.",
  "Transporte"                       = "Transport",
  "No aplica (No PEA)"              = "N/A (Out of Labour Force)",
  "Sin Experiencia Previa"           = "No Prior Experience",

  # ── Calificacion ocupacional (CALIFICACION_LEVELS) ────────────────────────
  "An\u00f3malo"                     = "Anomalous",
  "No calificado"                    = "Unskilled",
  "Ns/Nc"                            = "DK/NR",
  "Operativo"                        = "Operative",
  "Profesional"                      = "Professional",
  "T\u00e9cnico"                     = "Technical",
  "Sin Calificaci\u00f3n"           = "Unclassified",

  # ── Categoria ocupacional ─────────────────────────────────────────────────
  "Asalariado"                       = "Employee",
  "Empleado"                         = "Employee",
  "Patron"                           = "Employer",
  "Patr\u00f3n"                      = "Employer",
  "Cuenta Propia"                    = "Self-employed",
  "Cta. propia"                      = "Self-empl.",
  "Familiar"                         = "Unpaid Family",
  "Otros"                            = "Other",

  # ── Condicion de formalidad ───────────────────────────────────────────────
  "Formal"                           = "Formal",
  "Informal"                         = "Informal",
  "S\u00ed"                          = "Yes",
  "Si"                               = "Yes",
  "No"                               = "No",
  "No corresponde"                   = "Not applicable",

  # ── Labels de ejes comunes ────────────────────────────────────────────────
  "Densidad"                         = "Density",
  "Frecuencia"                       = "Frequency",
  "Trimestre"                        = "Quarter",
  "Tasa de formalidad (%)"           = "Formality rate (%)",
  "Tasa de formalidad"               = "Formality rate",
  "Tasa (%)"                         = "Rate (%)",
  "N observaciones"                  = "N observations",
  "% varianza"                       = "% variance",
  "Indicador"                        = "Indicator",
  "Indicator"                        = "Indicator",
  "Decil de ingreso"                 = "Income decile",
  "Media del factor"                 = "Factor mean",
  "Correlaci\u00f3n"                 = "Correlation",
  "ICH Score medio"                  = "Mean ICH score",
  "ICH Score (0\u2013100)"           = "ICH score (0\u2013100)",
  "log(Ingreso real + 1)"           = "log(Real income + 1)",

  # ── Contribuciones MCA ────────────────────────────────────────────────────
  "Contribuci\u00f3n a Dim1 (%)"     = "Contribution to Dim 1 (%)",

  # ── Labels de proxies ─────────────────────────────────────────────────────
  "Rezago escolar\n(cohorte)"        = "School delay\n(cohort)",
  "Clima educativo\n(hogar)"         = "Educational climate\n(household)",
  "Emparejamiento\nselectivo"        = "Assortative\nmatching",
  "Calificaci\u00f3n\nocupacional"   = "Occupational\nqualification",
  "Entrop\u00eda de\nestabilidad"    = "Labour-stab.\nentropy",
  "Residual\nvivienda"               = "Housing\nresidual",
  "B\u00fasqueda\nformal"            = "Formal search\nintensity",
  # Versiones cortas (usadas en heatmaps)
  "Rezago\nescolar"                  = "School\ndelay",
  "Clima\neducativo"                 = "Educ.\nclimate",
  "Emparej.\nselectivo"              = "Assort.\nmatching",
  "Calificaci\u00f3n\nnorm."         = "Qualif.\n(norm.)",
  "Entrop\u00eda\nestabilidad"       = "Labour-stab.\nentropy",
  # Sin salto de linea
  "Rezago escolar"                   = "School delay",
  "Clima educativo"                  = "Educational climate",
  "Emparejamiento selectivo"         = "Assortative matching",
  "Calificacion ocupacional"         = "Occupational qualification",
  "Entropia de estabilidad"          = "Labour-stability entropy",
  "Residual vivienda"                = "Housing residual",
  "Busqueda formal"                  = "Formal search intensity",
  "Entrop\u00eda de estabilidad"     = "Labour-stability entropy",

  # ── Labels de ejes figuras 05 ────────────────────────────────────────────
  "% de desocupados"                 = "% of unemployed",
  "Media trimestral"                 = "Quarterly mean",

  # ── Heterofactor ──────────────────────────────────────────────────────────
  "Modelo A (seleccionado)"          = "Model A (selected)",
  "Modelo B (LR Test)"               = "Model B (LR test)",
  "Muestra de estimaci\u00f3n"       = "Estimation sample",

  # ── Labels de series backcasting / hibrido ────────────────────────────────
  "Observado"                        = "Observed",
  "Estimado"                         = "Estimated",
  "Predicho"                         = "Predicted",
  "Ocupados"                         = "Employed",
  "Desocupados"                      = "Unemployed",
  "Inactivos"                        = "Inactive",
  "PEA"                              = "EAP",
  "Metodolog\u00eda anterior"        = "Legacy measure",
  "Serie anterior (proxy)"           = "Legacy series (proxy)",
  "Serie bridged"                    = "Bridged series",
  "Serie h\u00edbrida"               = "Hybrid series",
  "Exclusi\u00f3n pandemia"          = "Pandemic exclusion",

  # ── SIPA validation ───────────────────────────────────────────────────────
  "EPH (expandido)"                  = "EPH (expanded)",
  "SIPA (registro)"                  = "SIPA (admin. registry)",
  "Miles de personas"                = "Thousands of persons",
  "Variaci\u00f3n intertrimestral"   = "Quarter-on-quarter change",

  # ── Sensibilidad reglas ───────────────────────────────────────────────────
  "Regla any-of (INDEC)"             = "Any-of rule (INDEC)",
  "Regla two-of-3"                   = "Two-of-3 rule",
  "Regla all-of"                     = "All-of rule",

  # ── Gap decomposition ────────────────────────────────────────────────────
  "Expansi\u00f3n conceptual"        = "Concept expansion",
  "Reclasificaci\u00f3n"             = "Reclassification",
  "Gap total (pp)"                   = "Total gap (pp)",

  # ── NBI ───────────────────────────────────────────────────────────────────
  "Sin NBI"                          = "Without UBN",
  "Con NBI"                          = "With UBN",
  "NBI (0 = sin NBI, 1 = con NBI)"  = "UBN (0 = without, 1 = with)",

  # ── Indicadores MCA del ICH (nombres de categorías) ───────────────────────
  "casa"                               = "House",
  "departamento"                       = "Apartment",
  "otro_tipo_vivienda"                 = "Other dwelling",
  "piso_alta_calidad"                  = "Floor: high quality",
  "piso_mediana_calidad"               = "Floor: medium quality",
  "piso_precario"                      = "Floor: precarious",
  "techo_alta_solidez"                 = "Roof: high solidity",
  "techo_calidad_media"                = "Roof: medium quality",
  "techo_precario"                     = "Roof: precarious",
  "agua_exterior"                      = "Water: external",
  "agua_red_interior"                  = "Water: indoor network",
  "saneamiento_deficitario"            = "Sanitation: deficit",
  "saneamiento_optimo"                 = "Sanitation: optimal",
  "hacinamiento_critico"               = "Overcrowding: critical",
  "hacinamiento_moderado"              = "Overcrowding: moderate",
  "sin_hacinamiento"                   = "No overcrowding",
  # Indicadores MCA agregados (nombres cortos para barras)
  "piso"                               = "Floor",
  "techo"                              = "Roof",
  "agua"                               = "Water",
  "saneamiento"                        = "Sanitation",
  "hacinamiento"                       = "Overcrowding",
  "tipo_vivienda"                      = "Dwelling type",
  "n_ambientes"                        = "Rooms",
  "piso_mediana"                       = "Floor (med.)",
  "techo_calidad"                      = "Roof quality",
  # Categorías MCA adicionales (Cos², coordenadas, contribuciones)
  "combustible_precario"               = "Fuel: precarious",
  "gas_red"                            = "Gas: network",
  "gas_tubo_garrafa"                   = "Gas: bottled",
  "inquilino"                          = "Tenant",
  "ocupante_precario"                  = "Occupant (precarious)",
  "propietario"                        = "Owner",
  "si_garage"                          = "Garage: yes",
  "no_garage"                          = "Garage: no",
  "si_lavadero"                        = "Laundry: yes",
  "no_lavadero"                        = "Laundry: no",
  "0_ambientes"                        = "0 rooms",
  "1_ambientes"                        = "1 room",
  "2_ambientes"                        = "2 rooms",
  "3_ambientes"                        = "3 rooms",
  "4_ambientes"                        = "4 rooms",
  "5_mas_ambientes"                    = "5+ rooms",
  "NA_ambientes"                       = "Rooms: NA",

  # ── Labels de figuras 03c (ICH / MCA) ─────────────────────────────────────
  "Observaciones por trimestre (sample 10%)" = "Observations per quarter (10% sample)",
  "Top 12 secciones econ\u00f3micas (PEA, sample 50k)" = "Top 12 economic sectors (EAP, 50k sample)",
  "Calificaci\u00f3n ocupacional (PEA, sample 50k)" = "Occupational qualification (EAP, 50k sample)",
  "Distribuci\u00f3n ICH"                    = "ICH distribution",
  "Boxplot + densidad"                       = "Boxplot + density",
  "Varianza explicada por dimensi\u00f3n MCA (primeras 10)" = "Variance explained by MCA dimension (first 10)",
  "Contribuci\u00f3n de cada indicador a Dim1 (% total)" = "Contribution of each indicator to Dim 1 (% total)",
  "Coordenadas de categor\u00edas en Dim1"   = "Category coordinates on Dim 1",
  "Coordenada Dim1"                          = "Dim 1 coordinate",
  "Cos\u00b2 por categor\u00eda en Dim1"     = "Cos\u00b2 by category on Dim 1",
  "Cos\u00b2 (Dim1)"                         = "Cos\u00b2 (Dim 1)",
  "Estabilidad temporal del ICH"             = "Temporal stability of the ICH",
  "ICH medio por aglomerado (\u00b1 1 SD)"  = "Mean ICH by agglomerate (\u00b1 1 SD)",
  "ICH medio por regi\u00f3n (\u00b1 1 SD)" = "Mean ICH by region (\u00b1 1 SD)",
  "ICH medio por condici\u00f3n NBI"         = "Mean ICH by UBN status",
  "N obs"                                    = "N obs",
  "ICH Score"                                = "ICH score",
  "Ingreso real"                             = "Real income",

  # ── Labels de figuras 05 (proxies) ────────────────────────────────────────
  "Distribuciones de proxies \u03b8_cognitivo" = "Distributions of cognitive factor proxies",
  "Distribuciones de proxies \u03b8_socioemocional (con varianza continua)" = "Distributions of socioemotional factor proxies",
  "B\u00fasqueda formal de empleo \u2014 intensidad (solo desocupados)" = "Formal job search intensity (unemployed only)",
  "Correlaciones entre proxies \u2014 pairwise complete obs." = "Pairwise correlations among proxies",
  "Evoluci\u00f3n temporal \u2014 proxies \u03b8_cognitivo" = "Temporal evolution of cognitive proxies",
  "Evoluci\u00f3n temporal \u2014 proxies \u03b8_socioemocional" = "Temporal evolution of socioemotional proxies",
  "N\u00famero de m\u00e9todos formales de b\u00fasqueda activos" = "Number of active formal search methods",

  # ── Labels de figuras 06d (heterofactor) ──────────────────────────────────
  "Distribuci\u00f3n de factores latentes \u2014 Modelo A (seleccionado)" = "Distribution of latent factors \u2014 Model A (selected)",
  "Ortogonalidad entre factores latentes \u2014 Modelo A" = "Orthogonality between latent factors \u2014 Model A",
  "Factores latentes por condici\u00f3n de formalidad \u2014 Modelo A" = "Latent factors by formality status \u2014 Model A",
  "Factores latentes por decil de ingreso \u2014 Comparativo A vs B" = "Latent factors by income decile \u2014 Model A vs B",
  "Cargas factoriales sobre \u03b8_A \u2014 Modelo A vs Modelo B" = "Factor loadings on \u03b8_A \u2014 Model A vs Model B",

  # ── Labels adicionales 06d (ejes, leyendas) ────────────────────────────────
  "Valor del factor"                   = "Factor value",
  "Carga (alpha_A)"                    = "Loading (alpha_A)",
  "Modelo A"                           = "Model A",
  "Modelo B"                           = "Model B",
  "\u03b8_A (cognitivo)"               = "\u03b8_A (cognitive)",
  "\u03b8_B (socioemocional)"          = "\u03b8_B (socioemotional)",
  "\u03b8_A Modelo A"                  = "\u03b8_A Model A",
  "\u03b8_B Modelo A"                  = "\u03b8_B Model A",
  "\u03b8_A Modelo B"                  = "\u03b8_A Model B",
  "\u03b8_B Modelo B"                  = "\u03b8_B Model B",

  # ── Labels de figuras 09a/09b/09c (comparativos) ──────────────────────────
  "Modelo"                             = "Model",
  "Fuente"                             = "Source",
  "Personas (millones)"                = "Persons (millions)",
  "Probabilidad predicha"              = "Predicted probability",
  "Probabilidad"                       = "Probability",
  "Observada (EPH)"                    = "Observed (EPH)",
  "Metodologia anterior"               = "Legacy measure",
  "Tasa formal en bloque Predicho (%)" = "Formal rate in Predicted block (%)",
  "Expansion (%)"                      = "Expansion (%)",
  "Boxplot comparativo"                = "Comparative boxplot",
  "GLM hibrida"                        = "GLM hybrid",
  "GLM backcasting"                    = "GLM backcasting",
  "LPM hibrida"                        = "LPM hybrid",
  "LPM backcasting"                    = "LPM backcasting",
  "SLS hibrida"                        = "SLS hybrid",
  "SLS backcasting"                    = "SLS backcasting",
  "Inicio\nobservados"                 = "Start of\nobserved",

  # ── Titles/subtitles 09b ──────────────────────────────────────────────────
  "S1: Tasa observada vs. estimaciones calibradas (ocupados)" = "S1: Observed vs. calibrated rates (employed)",
  "Comparativo LPM / GLM / SLS -- 4 trimestres de validacion" = "LPM / GLM / SLS comparison -- 4 validation quarters",
  "S2: Tasa de formalidad asalariados -- comparacion con metodologia anterior" = "S2: Wage-earner formality rate -- comparison with legacy measure",
  "Zona roja = pandemia COVID | Zona naranja = 4 trims con formalidad observada" = "Red zone = COVID pandemic | Orange zone = 4 quarters with observed formality",
  "S3: Series de formalidad -- Universo Ocupados (Cal.)" = "S3: Formality series -- Employed universe (Cal.)",
  "Ocupados = Observados + Backcasting | Zona naranja = trims observados" = "Employed = Observed + Backcasting | Orange zone = observed quarters",
  "S3: Series de formalidad -- Universo PEA (Cal.)" = "S3: Formality series -- EAP universe (Cal.)",
  "PEA = Ocupados + Desocupados potenciales | Zona naranja = trims observados" = "EAP = Employed + Potential unemployed | Orange zone = observed quarters",
  "S3: Series de formalidad -- Universo Edad 18-60 (Cal.)" = "S3: Formality series -- Age 18-60 universe (Cal.)",
  "Edad 18-60 = Ocupados + Desocupados + Inactivos potenciales" = "Age 18-60 = Employed + Unemployed + Potential inactive",
  "S4: Distribucion de probabilidades predichas (muestra ocupados)" = "S4: Predicted probability distribution (employed sample)",
  "Lineas rojas = limites [0,1] | Zona fuera: cola de predicciones invalidadas" = "Red lines = [0,1] bounds | Outside zone: tail of invalid predictions",
  "Probabilidad predicha"              = "Predicted probability",
  "Probabilidad"                       = "Probability",

  # ── Titles/subtitles 09c ──────────────────────────────────────────────────
  "Cobertura hibrida por fuente (ponderada)" = "Hybrid coverage by source (weighted)",
  "Punteado = inicio pandemia | Predicho = obs nuevos via prediccion" = "Dashed = pandemic onset | Predicted = new obs via prediction",
  "Personas (millones)"                = "Persons (millions)",
  "Tasa de formalidad -- Variable hibrida" = "Formality rate -- Hybrid variable",
  "GLM: serie hibrida vs backcasting puro" = "GLM: hybrid series vs pure backcasting",
  "La serie hibrida incorpora registro historico donde disponible" = "Hybrid series incorporates historical registry where available",
  "LPM: serie hibrida vs backcasting puro" = "LPM: hybrid series vs pure backcasting",
  "SLS: serie hibrida vs backcasting puro" = "SLS: hybrid series vs pure backcasting",
  "Expansion de cobertura por trimestre (identica en LPM / GLM / SLS)" = "Coverage expansion by quarter (identical across LPM / GLM / SLS)",
  "% de obs clasificados via prediccion vs universo de medida vieja (ponderado)" = "% of obs classified via prediction vs legacy measure universe (weighted)",
  "Tasa de formalidad en el bloque Predicho por modelo" = "Formality rate in the Predicted block by model",
  "Individuos sin cobertura de medida vieja | Diferencias entre modelos visibles aqui" = "Individuals without legacy measure coverage | Model differences visible here",
  "Tasa formal en bloque Predicho (%)" = "Formal rate in Predicted block (%)",

  # ── Labels de figuras 08b (backcasting reports) ───────────────────────────
  "Observada"                          = "Observed",
  "Estimada"                           = "Estimated",
  "Estimada ocupados"                  = "Estimated employed",
  "Estimada PEA"                       = "Estimated EAP",
  "Delta ocupados vs obs."             = "Employed delta vs obs.",
  "Contribucion desocupados"           = "Unemployed contribution",
  "Diferencia (pp)"                    = "Difference (pp)",
  "Proporcion"                         = "Share",
  "Obs (miles)"                        = "Obs (thousands)",
  "Tasa"                               = "Rate",
  "Categoria"                          = "Category",
  "Universo"                           = "Universe",
  "Solo ocupados"                      = "Employed only",
  "Ocupados + Desoc. pot."             = "Employed + pot. unempl.",
  "Edad 18-60"                         = "Age 18\u201360",
  "Desoc. potencial formal"            = "Pot. formal unemployed",
  "No aplica"                          = "Not applicable",
  "Sin theta"                          = "No theta",
  "Backcasting"                        = "Backcasting",
  "Inactivo potencial formal"          = "Pot. formal inactive",
  "Pred. fuera [0,1]"                  = "Pred. outside [0,1]",
  "Observaciones por trimestre (miles)" = "Observations per quarter (thousands)",
  "Zona sombreada = 4 trimestres con formalidad observada" = "Shaded area = 4 quarters with observed formality",
  "Zona naranja = observados | Zona gris = trimestres pandemia (COVID)" = "Orange zone = observed | Grey zone = pandemic quarters (COVID)",

  # ── Labels faltantes detectados en edicion 3 (2026-04-17) ──────────────────
  "Tasa formal (%)"                    = "Formality rate (%)",
  "Tasa formal estimada (%)"           = "Estimated formality rate (%)",
  "Miles de obs."                      = "Obs (thousands)",
  "Otro"                               = "Other",
  "H\u00edbrida"                       = "Hybrid",
  "H\u00edbrida PEA"                   = "Hybrid EAP",
  "Ocupado"                            = "Employed",
  "Desocupado potencial"               = "Potential unemployed",
  "Inactivos 18-60"                    = "Inactive 18\u201360",
  "Inactivo 18-60"                     = "Inactive 18\u201360",
  "Categor\u00eda"                     = "Category"
)

# ── 3. FUNCION tr() ──────────────────────────────────────────────────────────

#' Traduce string(s) segun IDIOMA
#'
#' @param x Character vector a traducir
#' @return Character vector traducido (si IDIOMA == "en") o sin cambios (si "es")
#'
#' Si un string no tiene entrada en el diccionario, se devuelve tal cual
#' y se emite un warning (una sola vez por sesion) para detectar omisiones.
tr <- function(x) {
  if (IDIOMA == "es") return(x)

  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    # Traduccion automatica de trimestres: 2016_T4 → 2016Q4
    if (grepl("^\\d{4}_T[1-4]$", s)) {
      return(gsub("_T", "Q", s))
    }
    trad <- .DICT[[s]]
    if (!is.null(trad)) {
      trad
    } else {
      # Warning silencioso — una vez por string unico
      if (!exists(".TR_WARNED", envir = .GlobalEnv)) {
        assign(".TR_WARNED", character(0), envir = .GlobalEnv)
      }
      warned <- get(".TR_WARNED", envir = .GlobalEnv)
      if (!(s %in% warned)) {
        message(sprintf("[tr] sin traducci\u00f3n: \"%s\"", s))
        assign(".TR_WARNED", c(warned, s), envir = .GlobalEnv)
      }
      s
    }
  }, character(1), USE.NAMES = TRUE)
}

#' Traduce labels de trimestres en ejes (YYYY_T# → YYYYQ#)
#' @param x Character vector con labels de trimestres
#' @return Character vector con formato inglés
tr_quarter <- function(x) {
  gsub("_T([1-4])", "Q\\1", x)
}

# ── 4. WRAPPERS DE CONVENIENCIA ──────────────────────────────────────────────

#' labs() con traduccion automatica
#' @param ... argumentos para ggplot2::labs()
tr_labs <- function(...) {
  args <- list(...)
  args <- lapply(args, function(v) if (is.character(v)) tr(v) else v)
  do.call(labs, args)
}

#' Traduce levels de un factor
#' @param x Factor o character vector
#' @return Factor con levels traducidos (mismos datos, labels distintos)
tr_levels <- function(x) {
  if (IDIOMA == "es") return(x)
  if (is.factor(x)) {
    levels(x) <- tr(levels(x))
    x
  } else {
    tr(x)
  }
}

#' Traduce columnas especificas de un data.frame
#' @param df Data.frame
#' @param cols Character vector con nombres de columnas a traducir
#' @return Data.frame con columnas traducidas
tr_df <- function(df, cols) {
  if (IDIOMA == "es") return(df)
  for (col in cols) {
    if (col %in% names(df)) {
      if (is.factor(df[[col]])) {
        df[[col]] <- tr_levels(df[[col]])
      } else {
        df[[col]] <- tr(df[[col]])
      }
    }
  }
  df
}

# ── 5. MENSAJE ───────────────────────────────────────────────────────────────
cat(sprintf("\U0001F310 diccionario_presentacion.R cargado — IDIOMA = \"%s\" (%d entradas)\n",
            IDIOMA, length(.DICT)))
