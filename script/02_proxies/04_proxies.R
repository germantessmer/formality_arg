# =============================================================================
# [EN] 04_proxies.R -- Compute 7 proxy indicators for two latent factors (cognitive + socioemotional)
# INPUTS:  rdos/datos/03b_panel_base_completo.rds, rdos/inputs/diccionarios/00_diccionarios.rds
# OUTPUTS: rdos/datos/04_panel_con_proxies.rds
# =============================================================================
# 🌟 04_proxies_simples.R 🌟 ####

# 04_proxies_simples.R
# CAPA 2 — FEATURES: Construcción de proxies para modelo heterofactor
#
# CONTEXTO: El modelo de Sarzosa & Urzúa (2016) requiere ≥3 proxies genuinamente
# complementarias por factor latente. La versión anterior falló por dominancia
# extrema (θ_B ≈ intensidad_busqueda, R²=99.2%). Esta versión implementa un
# sistema de medición con fuentes de varianza ORTOGONALES para garantizar
# identificación según el Teorema de Kotlarski.
#
# Estrategia anti-dominancia (Sección 5 del informe técnico):
#   - θ_cog: combina fuente "Stock" (rezago), "Contexto" (clima), "Mercado
#     matrimonial" (emparejamiento selectivo) y "Outcome" (calificacion).
#   - θ_socio: combina fuente "Trayectoria" (entropía), "Preferencia" (vivienda)
#     y "Agencia" (búsqueda). Errores de medición de módulos distintos → ortogonales.
#
# PROXIES θ_COGNITIVO (4):
#   1. rezago_escolar_cohorte  — eficiencia aprendizaje relativa a cohorte
#   2. clima_educativo_hogar   — capital humano del entorno familiar
#   3. emparejamiento_selectivo — señal del mercado matrimonial
#   4. calificacion_norm       — complejidad de tarea en mercado laboral
#
# PROXIES θ_SOCIOEMOCIONAL (3):
#   5. entropia_estabilidad    — estabilidad conductual (trayectoria panel)
#   6. residual_vivienda       — preferencia temporal / paciencia
#   7. busqueda_formal         — grit y locus de control interno
#
# INPUT:  03b_panel_base_completo.rds  (output de 03b_ich.R)
# OUTPUT: 04_panel_con_proxies.rds
#
# NOTA RENDIMIENTO: Script optimizado para OpenBLAS activo (Rblas.dll reemplazado).
#   lm() en residual_vivienda usa BLAS multihilo automáticamente.
#
# AUTOR: Proyecto EPH Formalidad | FECHA: 2026-02-27

# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(tictoc)
})

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 04 - Proxies Simples")
start_time <- Sys.time()

# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  📊 SCRIPT 04 — PROXIES PARA MODELO HETEROFACTOR\n")
cat("  Sistema: 4 cognitivas + 3 socioemocionales\n")
cat(sprintf("  OpenBLAS activo: usa N_CORES = %d threads\n", N_CORES))
cat("═══════════════════════════════════════════════════════════════════\n\n")


# 🪫 1. Carga y verificación ----------------------------------------------------
cat("📂 Cargando panel base...\n")
hard_stop(file.exists(PATH_03B_PANEL_ICH),
          "No existe 03b_panel_base_completo.rds. Ejecutar 03b_ich.R primero.")

datos <- readRDS(PATH_03B_PANEL_ICH)
cat(sprintf("   ✅ %s obs × %s vars\n\n",
            format(nrow(datos), big.mark = ","), ncol(datos)))

# Verificar variables críticas
vars_req <- c("edad", "nivel_educ_obtenido2", "nivel_educ_cursado", "anio_aprobado",
              "asistencia_escuela", "id_hogar", "periodo_id", "aglomerado", "region",
              "parentesco", "sexo", "condicion_actividad", "calificacion",
              "id_individuo_hist", "ich_score", "ingreso_real_capita_familiar", "anio",
              "mas_500")           # ← documenta tamaño aglomerado (fluye desde 03a)
vars_falt <- setdiff(vars_req, names(datos))
hard_stop(length(vars_falt) == 0,
          paste("Faltan variables:", paste(vars_falt, collapse = ", ")))

# Detectar variables de búsqueda formal (métodos activos: visitar, CV, avisos, bolsa)
# Búsqueda informal (conocidos, carteles) se excluye intencionalmente —
# puede correlacionar con capital social, no con grit/agencia personal
vars_busca_formal <- intersect(
  c("busca_trabajo_entrevista", "busca_trabajo_avisos",
    "busca_trabajo_presencial", "busca_trabajo_bolsa"),
  names(datos)
)

cat(sprintf("   ✅ Variables requeridas presentes\n"))
cat(sprintf("   ✅ Métodos búsqueda formal: %d (%s)\n\n",
            length(vars_busca_formal), paste(vars_busca_formal, collapse = ", ")))
cat("   ℹ️  Niveles calificación:", paste(levels(datos$calificacion), collapse = " | "), "\n\n")

# ── Cargar diccionario educativo ───────────────────────────────────────────────
cat("📖 Cargando diccionario educativo...\n")
hard_stop(file.exists(PATH_00_DICCIONARIOS),
          "No existe 00_diccionarios.rds. Ejecutar 00_diccionarios.R primero.")

dic_00       <- readRDS(PATH_00_DICCIONARIOS)
lookup_anios <- dic_00$lookup$anios_educ   # nivel_educ_obtenido2 → años completados

# Función de normalización de key (idéntica a la de 00_diccionarios.R)
key_trim <- function(x) iconv(trimws(tolower(as.character(x))),
                              to = "ASCII//TRANSLIT")

cat(sprintf("   ✅ lookup_anios_educ: %d niveles\n\n", length(lookup_anios)))


# 🪫 PASO AUXILIAR: Años de educación continuos ---------------------------------
# Usado por proxies 1 (rezago), 2 (clima) y 3 (emparejamiento selectivo)
#
# Fórmula: años del nivel COMPLETADO (lookup) + años aprobados en nivel ACTUAL
# Ejemplo: Secundario completo (12 años) + 2 años de universidad cursados = 14
# Más preciso que usar solo el nivel discreto completado
cat("── AUXILIAR: anios_educ_cont ────────────────────────────────────────\n")

datos <- datos %>%
  mutate(
    # Años del nivel educativo más alto completado (desde diccionario)
    anios_educ_base = as.numeric(lookup_anios[key_trim(nivel_educ_obtenido2)]),
    # Años aprobados en el nivel actualmente cursado
    # Valor 99 = Ns/Nr en EPH → tratar como 0
    # Valores >10 = incoherentes dentro de un nivel → tratar como 0
    anio_aprobado_num = as.double(unclass(anio_aprobado)),
    anios_adicionales = case_when(
      is.na(anio_aprobado_num) ~ 0,
      anio_aprobado_num == 99  ~ 0,
      anio_aprobado_num > 10   ~ 0,
      TRUE                     ~ anio_aprobado_num
    ),
    # Medida continua: base completada + progreso en nivel actual
    anios_educ_cont = anios_educ_base + anios_adicionales
  )

cat(sprintf("   Rango: [%.0f, %.0f] | NA: %.1f%%\n\n",
            min(datos$anios_educ_cont, na.rm = TRUE),
            max(datos$anios_educ_cont, na.rm = TRUE),
            mean(is.na(datos$anios_educ_cont)) * 100))


# 🪫 PROXY 1: REZAGO ESCOLAR ESTANDARIZADO POR COHORTE (θ_cognitivo) -----------
#
# CONCEPTO: Dado un mismo nivel educativo final, quien lo completó en el tiempo
# teórico demostró mayor eficiencia cognitiva. El rezago simple está contaminado
# por tendencias seculares y por crisis que afectaron cohortes específicas (ej.
# 2001). La estandarización dentro de cada cohorte de nacimiento purifica estas
# tendencias → señal pura de eficiencia relativa a la propia generación.
# ENTRADA EN EL MODELO: carga negativa sobre θ_cog (más rezago = menos habilidad)
cat("── PROXY 1: rezago_escolar_cohorte ────────────────────────────────\n")

datos <- datos %>%
  mutate(
    # Cohorte de nacimiento aproximada
    cohorte_nac = anio - edad,
    # Rezago bruto: desviación respecto a trayectoria teórica
    # (6 = edad de inicio del sistema educativo argentino)
    rezago_bruto = edad - anios_educ_cont - 6,
    # Excluir menores del panel (no aplica lógica laboral)
    rezago_bruto = if_else(
      condicion_actividad == "Menor" | edad < 15, NA_real_, rezago_bruto
    )
  ) %>%
  group_by(cohorte_nac) %>%
  mutate(
    # Z-score dentro de cada cohorte:
    # elimina (a) tendencias educativas intergeneracionales
    #         (b) efectos de crisis macroeconómicas cohorte-específicas
    rezago_escolar_cohorte = safe_scale(rezago_bruto)
  ) %>%
  ungroup() %>%
  select(-rezago_bruto, -cohorte_nac)

cat(sprintf("   NA%%: %.1f%% | SD: %.3f | Rango: [%.2f, %.2f]\n\n",
            mean(is.na(datos$rezago_escolar_cohorte)) * 100,
            sd(datos$rezago_escolar_cohorte, na.rm = TRUE),
            min(datos$rezago_escolar_cohorte, na.rm = TRUE),
            max(datos$rezago_escolar_cohorte, na.rm = TRUE)))


# 🪫 PROXY 2: CLIMA EDUCATIVO DEL HOGAR (θ_cognitivo) --------------------------
#
# CONCEPTO: El entorno familiar genera externalidades de aprendizaje continuo:
# vocabulario avanzado, hábitos de lectura, aspiraciones educativas. Un hogar
# con adultos más educados revela y refuerza mayor dotación cognitiva en sus
# miembros. A diferencia del propio nivel educativo, el clima captura un
# mecanismo generador de datos distinto: el ambiente de formación.
# Fallback jerárquico garantiza cobertura casi universal:
#   hogar → aglomerado → nacional
cat("── PROXY 2: clima_educativo_hogar ─────────────────────────────────\n")

# Adultos >25: a esa edad la mayoría ya terminó su formación formal
clima_hogar <- datos %>%
  filter(edad > 25, !is.na(anios_educ_cont)) %>%
  group_by(id_hogar, periodo_id) %>%
  summarise(clima_hogar = mean(anios_educ_cont, na.rm = TRUE), .groups = "drop")

# Fallback 1: promedio del aglomerado
clima_aglo <- datos %>%
  filter(edad > 25, !is.na(anios_educ_cont)) %>%
  group_by(aglomerado, periodo_id) %>%
  summarise(clima_aglo = mean(anios_educ_cont, na.rm = TRUE), .groups = "drop")

# Fallback 2: promedio nacional del periodo
clima_nac <- datos %>%
  filter(edad > 25, !is.na(anios_educ_cont)) %>%
  group_by(periodo_id) %>%
  summarise(clima_nac = mean(anios_educ_cont, na.rm = TRUE), .groups = "drop")

datos <- datos %>%
  left_join(clima_hogar, by = c("id_hogar", "periodo_id")) %>%
  left_join(clima_aglo,  by = c("aglomerado", "periodo_id")) %>%
  left_join(clima_nac,   by = "periodo_id") %>%
  mutate(
    clima_educativo_hogar = coalesce(clima_hogar, clima_aglo, clima_nac)
  ) %>%
  select(-clima_hogar, -clima_aglo, -clima_nac)

cat(sprintf("   NA%%: %.1f%% | Media: %.2f años | SD: %.3f\n\n",
            mean(is.na(datos$clima_educativo_hogar)) * 100,
            mean(datos$clima_educativo_hogar, na.rm = TRUE),
            sd(datos$clima_educativo_hogar, na.rm = TRUE)))


# 🪫 PROXY 3: EMPAREJAMIENTO SELECTIVO (θ_cognitivo) ---------------------------
#
# CONCEPTO: La teoría del emparejamiento selectivo (Becker 1973) establece que
# los individuos forman pareja con personas de habilidades similares. La educación
# del cónyuge es una señal de la habilidad latente del individuo que trasciende
# sus propias credenciales: captura inteligencia fluida y rasgos de personalidad
# que hicieron al individuo "atractivo" en el mercado matrimonial. Los errores de
# medición (fricciones del mercado matrimonial) son ortogonales a los errores
# del propio nivel educativo → permite identificación.
# Para no-cónyuge: imputar media condicional por edad_grupo + sexo + región
cat("── PROXY 3: emparejamiento_selectivo ──────────────────────────────\n")

# Paso 1: educación del cónyuge (parentesco == "Cónyuge")
educ_conyuge <- datos %>%
  filter(parentesco == "Cónyuge", !is.na(anios_educ_cont)) %>%
  group_by(id_hogar, periodo_id) %>%
  slice(1) %>%                          # ← tomar primer cónyuge si hay >1
  ungroup() %>%
  select(id_hogar, periodo_id, educ_par_conyuge = anios_educ_cont)

# Paso 2: educación del jefe (parentesco == "Jefe")
educ_jefe <- datos %>%
  filter(parentesco == "Jefe", !is.na(anios_educ_cont)) %>%
  group_by(id_hogar, periodo_id) %>%
  slice(1) %>%                          # ← tomar primer jefe si hay >1
  ungroup() %>%
  select(id_hogar, periodo_id, educ_par_jefe = anios_educ_cont)

# Paso 3: asignar educación del par matrimonial a cada individuo
#   El jefe recibe la educación de su cónyuge y viceversa
datos <- datos %>%
  left_join(educ_conyuge, by = c("id_hogar", "periodo_id")) %>%
  left_join(educ_jefe,   by = c("id_hogar", "periodo_id")) %>%
  mutate(
    am_raw = case_when(
      parentesco == "Jefe"    ~ educ_par_conyuge,
      parentesco == "Cónyuge" ~ educ_par_jefe,
      TRUE                    ~ NA_real_
    )
  ) %>%
  select(-educ_par_conyuge, -educ_par_jefe)

pct_obs <- mean(!is.na(datos$am_raw)) * 100

# Paso 4: imputar NAs con media condicional (solteros, viudos, no-jefe/no-cónyuge)
datos <- datos %>%
  mutate(
    edad_grupo = cut(edad, breaks = c(0, 25, 35, 45, 55, 65, Inf),
                     labels = c("15-25", "26-35", "36-45", "46-55", "56-65", "65+"),
                     right = FALSE, include.lowest = TRUE)
  )

medias_am <- datos %>%
  filter(!is.na(am_raw)) %>%
  group_by(edad_grupo, sexo, region) %>%
  summarise(media_am = mean(am_raw, na.rm = TRUE), .groups = "drop")

datos <- datos %>%
  left_join(medias_am, by = c("edad_grupo", "sexo", "region")) %>%
  mutate(emparejamiento_selectivo = coalesce(am_raw, media_am)) %>%
  select(-am_raw, -media_am, -edad_grupo)

cat(sprintf("   Con par observado: %.1f%% | NA final: %.1f%% | SD: %.3f\n\n",
            pct_obs,
            mean(is.na(datos$emparejamiento_selectivo)) * 100,
            sd(datos$emparejamiento_selectivo, na.rm = TRUE)))


# 🪫 PROXY 4: CALIFICACIÓN OCUPACIONAL NORMALIZADA (θ_cognitivo) ---------------
#
# CONCEPTO: Las ocupaciones son paquetes de tareas que requieren distintos
# niveles de habilidades cognitivas. Bajo un modelo de asignación (Roy) con
# fricciones, los trabajadores con mayor θ_cog tienen ventaja comparativa en
# tareas de alta complejidad. La calificación CNO captura esta señal
# contemporánea de capacidad productiva, con un mecanismo generador DISTINTO
# al de la educación (fricción laboral vs. reporte de escolaridad) →
# errores de medición ortogonales → permite identificación.
# Variable ya taxonomizada en Capa 1 por normalizar_calificacion().
cat("── PROXY 4: calificacion_norm ──────────────────────────────────────\n")

# calificacion es factor ordenado desde Capa 1 (menor → mayor calificación)
# as.numeric() preserva el orden; safe_scale() estandariza
# Excluir niveles no informativos (no-PEA, sin experiencia, anómalos)
# Los desocupados con experiencia previa YA tienen su calificación
# coalesced en el factor desde Script 02 (calificacion_ocupado + calificacion_desocupado)
datos <- datos %>%
  mutate(
    calificacion_num = case_when(
      calificacion %in% c("No aplica (No PEA)", "Sin Calificación",
                          "Anómalo", "Ns/Nc") ~ NA_real_,
      TRUE ~ as.numeric(calificacion)
    ),
    calificacion_norm = safe_scale(calificacion_num)
  ) %>%
  select(-calificacion_num)

cat(sprintf("   NA%%: %.1f%% | SD: %.3f | Niveles: %d\n\n",
            mean(is.na(datos$calificacion_norm)) * 100,
            sd(datos$calificacion_norm, na.rm = TRUE),
            nlevels(datos$calificacion)))


# 🪫 PROXY 5: ENTROPÍA DE ESTABILIDAD LABORAL (θ_socioemocional) ---------------
#
# CONCEPTO: Los rasgos Big Five de Conscientiousness y Estabilidad Emocional se
# manifiestan en trayectorias laborales consistentes. Individuos con baja
# habilidad socioemocional exhiben mayor "turbulencia": transiciones frecuentes
# entre Ocupado, Desocupado e Inactivo. Usamos la Entropía de Shannon
# normalizada sobre 3 estados:
#   H = -Σ p_i * log2(p_i)
#   H_norm = H / log2(3)        (máxima entropía posible con 3 estados)
#   estabilidad = 1 - H_norm    (0 = caótico, 1 = siempre mismo estado)
# Se computa con TODAS las ondas disponibles del individuo → valor constante
# por individuo, luego join al panel completo.
# NOTA: no se usa formalidad_empleo como 4to estado porque solo existe en los
# últimos trimestres → rompería la cobertura histórica para backcasting.
cat("── PROXY 5: entropia_estabilidad ──────────────────────────────────\n")

entropia_ind <- datos %>%
  # Solo estados laborales válidos
  filter(condicion_actividad %in% c("Ocupado", "Desocupado", "Inactivo")) %>%
  group_by(id_individuo_hist) %>%
  summarise(
    n_obs      = n(),
    p_ocupado  = mean(condicion_actividad == "Ocupado"),
    p_desocup  = mean(condicion_actividad == "Desocupado"),
    p_inactivo = mean(condicion_actividad == "Inactivo"),
    .groups    = "drop"
  ) %>%
  mutate(
    # Entropía de Shannon: 0*log(0) = 0 por convenio matemático
    H = -(
      if_else(p_ocupado  > 0, p_ocupado  * log2(p_ocupado),  0) +
      if_else(p_desocup  > 0, p_desocup  * log2(p_desocup),  0) +
      if_else(p_inactivo > 0, p_inactivo * log2(p_inactivo), 0)
    ),
    H_norm               = H / log2(3),
    # Invertir: mayor valor = mayor estabilidad conductual
    entropia_estabilidad = 1 - H_norm
  ) %>%
  select(id_individuo_hist, entropia_estabilidad)

# Join: el valor es constante para todas las ondas del mismo individuo
datos <- datos %>%
  left_join(entropia_ind, by = "id_individuo_hist")

cat(sprintf("   NA%%: %.1f%% | Individuos con dato: %s\n",
            mean(is.na(datos$entropia_estabilidad)) * 100,
            format(nrow(entropia_ind), big.mark = ",")))
cat(sprintf("   SD: %.3f | Rango: [%.3f, %.3f]\n\n",
            sd(datos$entropia_estabilidad, na.rm = TRUE),
            min(datos$entropia_estabilidad, na.rm = TRUE),
            max(datos$entropia_estabilidad, na.rm = TRUE)))


# 🪫 PROXY 6: RESIDUAL DE CALIDAD DE VIVIENDA (θ_socioemocional) ---------------
#
# CONCEPTO: Individuos con mayor paciencia (baja tasa de descuento temporal)
# invierten más en la calidad de su vivienda dado su nivel de ingreso. Usar
# directamente el ICH score sería una proxy del ingreso, no de la habilidad.
# Solución: extraer el RESIDUAL de una regresión de calidad sobre ingreso y
# controles:  ich_score ~ log(ingreso+1) + edad + región + nivel_educ + año
# Residual > 0: invierte MÁS de lo esperado → mayor paciencia / Conscientiousness
# Residual < 0: invierte MENOS de lo esperado → mayor impaciencia
# OpenBLAS (Rblas.dll) acelera la inversión matricial del OLS automáticamente.
cat("── PROXY 6: residual_vivienda ──────────────────────────────────────\n")

# row_id ANTES del filter → datos_reg lo hereda con los índices correctos
# Esto permite un join limpio posterior sin necesidad de reconstruir claves
datos <- datos %>% mutate(row_id = row_number())

datos_reg <- datos %>%
  filter(
    !is.na(ich_score),
    !is.na(ingreso_real_capita_familiar),
    ingreso_real_capita_familiar >= 0,
    !is.na(edad),
    !is.na(region),
    !is.na(nivel_educ_obtenido2),
    !is.na(anio)
  )

cat(sprintf("   Obs para regresión: %s (%.1f%% del panel)\n",
            format(nrow(datos_reg), big.mark = ","),
            nrow(datos_reg) / nrow(datos) * 100))

fit_vivienda <- lm(
  ich_score ~ log(ingreso_real_capita_familiar + 1) +
              edad + I(edad^2) +
              region + nivel_educ_obtenido2 + factor(anio),
  data      = datos_reg,
  na.action = na.exclude
)

r2_vivienda <- summary(fit_vivienda)$r.squared
cat(sprintf("   R² regresión: %.3f\n", r2_vivienda))

# Guardar R² para que 05_reporte_proxies.R lo incluya en el contrato
saveRDS(r2_vivienda, file.path(DIR_CACHE, "04_r2_vivienda.rds"))


# Join por row_id: garantiza correspondencia exacta de residuos
datos_res <- datos_reg %>%
  mutate(residual_vivienda = residuals(fit_vivienda)) %>%
  select(row_id, residual_vivienda)

datos <- datos %>%
  left_join(datos_res, by = "row_id") %>%
  select(-row_id)

rm(fit_vivienda, datos_reg, datos_res)
gc(verbose = FALSE)

# ── Diagnóstico PRE-imputación ──────────────────────────────────────────────────
n_rv_orig    <- sum(!is.na(datos$residual_vivienda))
n_rv_na_orig <- sum( is.na(datos$residual_vivienda))
pct_orig     <- n_rv_orig    / nrow(datos) * 100
pct_na_orig  <- n_rv_na_orig / nrow(datos) * 100

cat(sprintf("   PRE-imputación: con dato = %s (%.1f%%) | NA = %s (%.1f%%)\n",
            format(n_rv_orig,    big.mark = ","), pct_orig,
            format(n_rv_na_orig, big.mark = ","), pct_na_orig))

# Guardar stats de imputación para el paper appendix
saveRDS(list(
  pct_na_income = round(pct_na_orig, 1),
  n_imputed     = n_rv_na_orig,
  min_donors    = 266L,   # from diag_estrategia_imputacion_rv.R
  median_donors = 1123L   # from diag_estrategia_imputacion_rv.R
), file.path(DIR_CACHE, "04_imputation_stats.rds"))

# 🪫 IMPUTACIÓN residual_vivienda -----------------------------------------------
#
# PROBLEMA: el OLS anterior requiere ingreso_real_capita_familiar ≠ NA.
# El 21.3% del panel no declara ingresos (fenómeno estructural EPH) → residual_vivienda = NA.
# Con solo entropia_estabilidad disponible, esos individuos incumplen n_socio ≥ 2
# en el heterofactor → sin θ_B → sin θ → 29% del training set LASSO sin score.
#
# SOLUCIÓN APROBADA: mediana | aglomerado × año × trimestre
# Fundamento: aglomerado es más preciso que región (evita agregar aglomerados
# heterogéneos); trimestre controla ciclo inflacionario (AR 2016-2025).
# Diagnóstico (diag_estrategia_imputacion_rv.R):
#   → 100% de los 383,004 casos resueltos en Nivel 1 (≥266 donantes por celda,
#     mediana 1,123). No se necesitan fallbacks.
#
# FLAG rv_imputado: TRUE para observaciones imputadas. Fluye hasta panel_con_theta.rds
# para identificar en LASSO qué θ_B fueron estimados con residuo original vs. imputado.
# Ver: NOTA_TECNICA_Imputacion_ResidualVivienda.md
cat("   ── Imputando con mediana | aglomerado × año × trimestre...\n")

# Calcular medianas donantes (solo observaciones con residuo observado)
medianas_rv <- datos %>%
  filter(!is.na(residual_vivienda)) %>%
  group_by(aglomerado, anio, trimestre) %>%
  summarise(mediana_rv_celda = median(residual_vivienda), .groups = "drop")

# Flag ANTES de coalesce (cuando residual_vivienda aún es NA para los candidatos)
# Condición: sin residuo observado + tiene ich_score (candidato real al heterofactor)
datos <- datos %>%
  left_join(medianas_rv, by = c("aglomerado", "anio", "trimestre")) %>%
  mutate(
    rv_imputado       = is.na(residual_vivienda) & !is.na(ich_score),
    residual_vivienda = coalesce(residual_vivienda, mediana_rv_celda)
  ) %>%
  select(-mediana_rv_celda)

rm(medianas_rv)
gc(verbose = FALSE)

# ── Diagnóstico POST-imputación ─────────────────────────────────────────────────
n_rv_imp     <- sum( datos$rv_imputado, na.rm = TRUE)
n_rv_na_post <- sum( is.na(datos$residual_vivienda))

cat(sprintf("   Imputados (mediana celda): %s (%.1f%%)\n",
            format(n_rv_imp, big.mark = ","),
            n_rv_imp / nrow(datos) * 100))
cat(sprintf("   NA restantes post-imputación: %s",
            format(n_rv_na_post, big.mark = ",")))
if (n_rv_na_post > 0) {
  cat(sprintf(" → individuos sin ich_score (esperado)\n"))
} else {
  cat(" ✅\n")
}
cat(sprintf("   NA%%: %.1f%% | SD: %.3f\n\n",
            mean(is.na(datos$residual_vivienda)) * 100,
            sd(datos$residual_vivienda, na.rm = TRUE)))


# 🪫 PROXY 7: BÚSQUEDA FORMAL DE EMPLEO (θ_socioemocional) ---------------------
#
# CONCEPTO: Para desocupados, la FORMA de búsqueda revela Grit y Locus de
# Control Interno. La búsqueda ACTIVA (presentarse en empresas, enviar CVs,
# consultar bolsas, publicar avisos) requiere mayor esfuerzo y autoeficacia
# que la búsqueda pasiva (esperar contactos de conocidos).
# Métodos informales (conocidos, carteles) excluidos intencionalmente: pueden
# correlacionar con capital social, no con agencia personal.
#
# NOTA CRÍTICA: valor NA (no 0) para no-desocupados. Esto evita el problema de
# dominancia de la versión anterior donde el ~95% del panel tenía intensidad=0,
# generando θ_B ≈ copia exacta de la proxy. El modelo heterofactor con FIML
# maneja NAs correctamente: entropía + residual_vivienda anclan θ_socio
# para el resto del panel.
cat("── PROXY 7: busqueda_formal ────────────────────────────────────────\n")

if (length(vars_busca_formal) > 0) {

  datos <- datos %>%
    mutate(
      # Contar métodos FORMALES activos (solo "Si"; "No corresponde" = no desocupado)
      busqueda_formal_raw = rowSums(
        across(all_of(vars_busca_formal), ~ as.integer(. == "Si")),
        na.rm = TRUE
      ),
      # NA para no-desocupados (no 0) — ver nota crítica arriba
      busqueda_formal = if_else(
        condicion_actividad == "Desocupado",
        as.numeric(busqueda_formal_raw),
        NA_real_
      )
    ) %>%
    select(-busqueda_formal_raw)

  stats_busq <- datos %>%
    filter(condicion_actividad == "Desocupado") %>%
    summarise(
      n         = n(),
      mean      = mean(busqueda_formal, na.rm = TRUE),
      sd        = sd(busqueda_formal, na.rm = TRUE),
      pct_cero  = mean(busqueda_formal == 0, na.rm = TRUE) * 100,
      pct_multi = mean(busqueda_formal >= 2, na.rm = TRUE) * 100
    )

  cat(sprintf("   Solo desocupados (n=%s): media=%.2f | SD=%.3f\n",
              format(stats_busq$n, big.mark = ","), stats_busq$mean, stats_busq$sd))
  cat(sprintf("   Sin búsqueda formal: %.1f%% | Búsqueda intensa (≥2 métodos): %.1f%%\n",
              stats_busq$pct_cero, stats_busq$pct_multi))
  cat(sprintf("   NA%% total panel: %.1f%% (esperado ~96%% = no-desocupados)\n\n",
              mean(is.na(datos$busqueda_formal)) * 100))

} else {
  datos <- datos %>% mutate(busqueda_formal = NA_real_)
  cat("   ⚠️  No se detectaron variables busca_trabajo_* formales\n\n")
}


# 🪫 LIMPIEZA: eliminar auxiliares de cálculo -----------------------------------
datos <- datos %>%
  select(-anios_educ_base, -anios_adicionales, -anios_educ_cont)


# 🪫 VALIDACIONES KOTLARSKI -----------------------------------------------------
# Criterios pre-estimación: SD, cobertura, correlaciones intra-factor
# La validación downstream (R² sobre θ) se realiza en Script 06b post-estimación
cat("══════════════════════════════════════════════════════════════════════\n")
cat("🔎 VALIDACIONES KOTLARSKI (criterios pre-estimación)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

proxies_spec <- list(
  list(var = "rezago_escolar_cohorte",   factor = "θ_cog",   sd_min = 0.1, na_max = 30),
  list(var = "clima_educativo_hogar",    factor = "θ_cog",   sd_min = 0.1, na_max = 30),
  list(var = "emparejamiento_selectivo", factor = "θ_cog",   sd_min = 0.1, na_max = 30),
  list(var = "calificacion_norm",        factor = "θ_cog",   sd_min = 0.1, na_max = 60),
  list(var = "entropia_estabilidad",     factor = "θ_socio", sd_min = 0.1, na_max = 30),
  list(var = "residual_vivienda",        factor = "θ_socio", sd_min = 0.1, na_max = 30),
  list(var = "busqueda_formal",          factor = "θ_socio", sd_min = 0.1, na_max = 98)
  # busqueda_formal: na_max=98% por diseño (solo desocupados ~4% del panel)
)

resultados_val <- map_dfr(proxies_spec, function(p) {
  x      <- datos[[p$var]]
  na_pct <- mean(is.na(x)) * 100
  sd_val <- sd(x, na.rm = TRUE)
  tibble(
    proxy   = p$var,
    factor  = p$factor,
    na_pct  = round(na_pct, 1),
    sd      = round(sd_val, 3),
    test_sd = if_else(sd_val > p$sd_min, "✅ PASS", "❌ FAIL"),
    test_na = if_else(na_pct < p$na_max, "✅ PASS", "❌ FAIL")
  )
})

print(resultados_val, n = 20)

# Correlaciones intra-factor (ningún par debe superar 0.85)
cat("\n📊 Correlaciones intra-factor:\n")
proxies_cog   <- c("rezago_escolar_cohorte", "clima_educativo_hogar",
                   "emparejamiento_selectivo", "calificacion_norm")
proxies_socio <- c("entropia_estabilidad", "residual_vivienda")
# busqueda_formal excluida del cálculo por exceso de NAs (solo desocupados)

cor_cog   <- cor(datos[proxies_cog],   use = "pairwise.complete.obs")
cor_socio <- cor(datos[proxies_socio], use = "pairwise.complete.obs")

cat(sprintf("   θ_cog   — max correlación par: %.3f\n",
            max(abs(cor_cog[lower.tri(cor_cog)]))))
cat(sprintf("   θ_socio — max correlación par (excl. busqueda_formal): %.3f\n",
            max(abs(cor_socio[lower.tri(cor_socio)]))))

n_fail <- sum(resultados_val$test_sd == "❌ FAIL" | resultados_val$test_na == "❌ FAIL")
if (n_fail > 0) {
  soft_warn(FALSE, sprintf(
    "%d proxy(es) no pasan validación Kotlarski. Revisar antes de Script 06.", n_fail))
} else {
  cat("\n✅ Todas las proxies pasan validaciones pre-estimación\n")
}

cat("\n⚠️  RECORDAR: validar R² individual de proxies sobre θ POST-estimación (Script 06b)\n")
cat("   Criterio: ningún R² > 70%, al menos 3 proxies con R² > 10% por factor\n\n")


# 🪫 GUARDADO ------------------------------------------------------------------
cat("💾 Guardando output...\n")
gc(verbose = FALSE)
saveRDS(datos, PATH_04_PANEL_PROXIES, compress = FALSE)
cat(sprintf("   ✅ Guardado: %s\n", PATH_04_PANEL_PROXIES))
cat(sprintf("   Tamaño: %s\n",     format(object.size(datos), units = "MB")))
cat(sprintf("   Dimensiones: %s obs × %s vars\n",
            format(nrow(datos), big.mark = ","), ncol(datos)))

proxies_names <- c("rezago_escolar_cohorte", "clima_educativo_hogar",
                   "emparejamiento_selectivo", "calificacion_norm",
                   "entropia_estabilidad", "residual_vivienda",
                   "busqueda_formal", "rv_imputado")
hard_stop(all(proxies_names %in% names(datos)), "Error: faltan proxies en el dataset final")
cat("   ✅ 7 proxies + flag rv_imputado confirmados en el output\n\n")


# ── Resumen final ──────────────────────────────────────────────────────────────
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("═══════════════════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 04:\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("   [✅] rezago_escolar_cohorte    — estandarizado por cohorte de nacimiento\n")
cat("   [✅] clima_educativo_hogar     — con fallback aglomerado → nacional\n")
cat("   [✅] emparejamiento_selectivo  — con imputación condicional para no-cónyuge\n")
cat("   [✅] calificacion_norm         — desde taxonomía CNO Capa 1\n")
cat("   [✅] entropia_estabilidad      — Shannon 3 estados, todas las ondas\n")
cat("   [✅] residual_vivienda         — OLS + imputación mediana | aglomerado×trimestre\n")
cat("   [✅] busqueda_formal           — NA para no-desocupados (evita dominancia)\n")
cat("   [✅] rv_imputado               — flag: TRUE = residuo imputado por celda\n")
cat("   [✅] mas_500                   — fluye desde 03a (tamaño aglomerado)\n")
cat("   [✅] Output: 04_panel_con_proxies.rds\n")
cat("───────────────────────────────────────────────────────────────────\n")
cat(sprintf("⏱️  Tiempo total: %.1f minutos\n", elapsed))
cat("───────────────────────────────────────────────────────────────────\n")
cat("🎯 SIGUIENTE PASO: Script 05 (reporte validación Capa 2)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

toc()
