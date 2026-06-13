# =============================================================================
# [EN] 01_carga_panel.R -- Load and stack quarterly EPH microdata into a historical panel (2016Q4-2025Q3)
# INPUTS:  C:/oes/eph_rdos/capa2/EPH*.RData (raw EPH microdata)
# OUTPUTS: rdos/datos/01_panel_historico_raw.rds
# =============================================================================
# 🌟 01_carga_panel.R 🌟 ####

# OBJETIVO: Cargar panel histórico completo (2016T4–2025T3) con IDs consistentes
#           y limpieza básica de edad y antigüedad.
#           NO hace taxonomía (eso va a Script 02).
# INPUTS:   C:/oes/eph_rdos/capa1/EPH*.RData  (EPH capa1 Dataverse, solo lectura)
# OUTPUTS:  rdos/datos/01_panel_historico_raw.rds
# NOTA:     La preselección de variables está guiada por dos fuentes:
#           (1) LEGACY_03a vars_core — lista final usada después de limpieza
#           (2) LEGACY_09a — requiere condicion_formalidad en panel_con_taxonomia
#           Script 03a hace la reducción definitiva; acá cargamos todo lo necesario.

# 📚 Librerías -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(tidyverse)
  library(data.table)
  library(tictoc)
})

# 🔧 Cargar configuración y funciones ------------------------------------------
source(here::here("script", "config", "parametros.R"))
source(here::here("script", "config", "funciones_comunes.R"))

# ⌛ Inicio contador de tiempo -------------------------------------------------
tic("Script 01 [Carga Panel Histórico]")
start_time <- Sys.time()
cat("═══════════════════════════════════════════════════════\n")
cat("🚀 Script 01 iniciado:", as.character(start_time), "\n")
cat("═══════════════════════════════════════════════════════\n\n")


# 🪫 1. Identificar archivos a cargar ------------------------------------------
cat("📂 Identificando archivos EPH en rango configurado...\n")

archivos_all <- list.files(RUTA_BASES,
                           pattern = "^EPH\\d{4}_T[1-4]\\.RData$",
                           full.names = TRUE)

hard_stop(length(archivos_all) > 0,
          paste0("No se encontraron archivos .RData en: ", RUTA_BASES))

df_arch <- parse_anio_trim_from_filename(archivos_all) %>%
  filter(!is.na(periodo_num),
         periodo_num >= PERIODO_INI,
         periodo_num <= PERIODO_FIN) %>%
  arrange(periodo_num)

hard_stop(nrow(df_arch) > 0,
          paste0("No hay archivos en el rango configurado (",
                 ANIO_INI, "T", TRIM_INI, " – ", ANIO_FIN, "T", TRIM_FIN, ")"))

cat("✅ Archivos detectados:", nrow(df_arch), "\n")
cat("   Cobertura:", min(df_arch$periodo_num), "→", max(df_arch$periodo_num), "\n\n")


# 🪫 2. Definir variables a conservar ------------------------------------------
cat("🔍 Definiendo variables a conservar...\n")

# Lista guiada por LEGACY_03a (vars_core) + LEGACY_09a (join panel_taxonomia)
# Principio: cargar todo lo que el pipeline usa, nada más.
# Script 03a hace la reducción definitiva (elimina vivienda raw tras calcular ICH).
# any_of() ignora silenciosamente columnas ausentes en trimestres anteriores.

vars_a_conservar <- c(

  # ── IDs de panel ──────────────────────────────────────────────────────────
  # Necesarios para construir id_individuo, id_hogar
  "codusu", "nro_hogar", "componente", "anio", "trimestre",
  "mas_500",                              # flag aglomerado grande (usado en modelos)

  # ── Demográficas ──────────────────────────────────────────────────────────
  "edad", "sexo", "aglomerado", "region", "pondera",
  "parentesco", "estado_civil", "alfabetizacion", "lugar_nacimiento",

  # ── Educación ─────────────────────────────────────────────────────────────
  "nivel_educ_obtenido2", "asistencia_escuela", "tipo_escuela",
  "nivel_educ_cursado", "anio_aprobado",

  # ── Condición de actividad y laborales ────────────────────────────────────
  "condicion_actividad",
  "seccion_ocupado", "seccion_desocupado",           # coalesce en Script 02
  "calificacion_ocupado", "calificacion_desocupado", # coalesce en Script 02
  "categoria_ocupacional",
  "tamanio_empresa",
  "nbi",
  "condicion_formalidad",                 # requerido por Script 09a (join panel_taxonomia)

  # ── Antigüedad (nombres alternativos según trimestre EPH) ─────────────────
  "antiguedad_ocup_principal", "antiguedad_ocup_indep", "antiguedad_ocup_anterior",

  # ── Hogar ─────────────────────────────────────────────────────────────────
  "miembros_hogar", "menores10", "mayores10",
  "principal_tareas_hogar", "otros_tareas_hogar",

  # ── Ingresos (ingreso_real_final se genera en Script 03a, no existe en raw) ─
  "ingreso_real_total_individual", "ingreso_total_individual",
  "ingreso_real_capita_familiar",

  # ── Vivienda RAW (necesarias para Script 03b ICH — Script 03a las elimina) ─
  "tipo_vivienda", "nro_ambientes_vivienda", "tipo_piso", "tipo_techo",
  "revestimiento_techo", "suministro_agua", "acceso_agua", "banio",
  "lugar_banio", "caract_banio", "tipo_desague", "combustible_cocina",
  "regimen_tenencia", "garage", "lavadero",

  # ── Formalidad oficial (últimos 4 trimestres — NA en el resto) ─────────────
  "formalidad_empleo",

  # ── Fuentes de vida (nombres exactos de LEGACY_03a vars_core) ─────────────
  "vive_alquiler", "vive_ganancias_negocio", "vive_renta_financiera",
  "vive_beca", "vive_cuota_alimenticia", "vive_ahorros",
  "vive_prestamos_personas", "vive_prestamos_financieros",
  "vive_financiamiento", "vive_venta_bienes", "vive_otro_ingreso"

  # busca_trabajo_* se agrega dinámicamente en la sección 3 (nombres varían entre trimestres)
)

cat("✅ Variables estáticas definidas:", length(vars_a_conservar), "\n\n")


# 🪫 3. Cargar y combinar todos los trimestres ---------------------------------
cat("📥 Cargando y combinando trimestres...\n")

lista_datos <- lapply(seq_len(nrow(df_arch)), function(i) {
  ruta <- df_arch$archivo[i]

  load(ruta)

  hard_stop(exists("datos"),
            paste0("Archivo no contiene objeto `datos`: ", basename(ruta)))

  # Normalizar nombres a minúsculas
  names(datos) <- tolower(names(datos))

  # Reproducibilidad: derivar nivel_educ_obtenido2 y condicion_formalidad desde capa1
  # (réplica de eph_full capa2). No-op si el input ya es capa2 (variables presentes).
  datos <- derivar_vars_capa2(datos)

  # busca_trabajo_* es dinámica: detectar todas las variantes presentes
  vars_busca <- names(datos)[stringr::str_detect(names(datos), "^busca_trabajo")]

  # Seleccionar variables estáticas + busca_trabajo_* (reduce memoria dramáticamente)
  datos <- datos %>%
    select(any_of(c(vars_a_conservar, vars_busca)))

  if (i %% 5 == 0) cat("   ... procesados", i, "/", nrow(df_arch), "archivos\n")

  return(datos)
})

# rbindlist: más eficiente en memoria que bind_rows; fill=TRUE maneja columnas
# ausentes entre trimestres (ej. vivienda raw no siempre está)
datos <- data.table::rbindlist(lista_datos, fill = TRUE, use.names = TRUE)
data.table::setDF(datos)
rm(lista_datos)
gc()

cat("✅ Datos combinados. Dimensiones:", format(nrow(datos), big.mark = ".", decimal.mark = ","),
    "x", ncol(datos), "\n\n")


# 🪪 4. Crear IDs consistentes -------------------------------------------------
cat("🏗️  Generando IDs únicos...\n")

datos <- datos %>%
  mutate(
    # Periodo legible
    periodo = paste0("T", as.character(trimestre), "_", anio),

    # Zapping de IDs (crítico para evitar mismatch en joins entre trimestres)
    codusu_clean   = zap_chr(codusu),
    nro_hogar_num  = as.integer(zap_num(nro_hogar)),
    componente_num = as.integer(zap_num(componente)),
    anio_clean     = as.integer(zap_num(anio)),
    trim_clean     = as.integer(zap_num(trimestre)),

    # IDs finales
    id_individuo_hist = paste(codusu_clean, nro_hogar_num, componente_num,
                              sep = "_"),
    id_individuo      = paste(codusu_clean, nro_hogar_num, componente_num,
                              anio_clean, trim_clean, sep = "-"),
    id_hogar          = paste(codusu_clean, nro_hogar_num,
                              anio_clean, trim_clean, sep = "-"),
    periodo_id        = paste0(anio_clean, "_T", trim_clean),
    periodo_num       = periodo_num(anio_clean, trim_clean)
  ) %>%
  select(-codusu_clean, -nro_hogar_num, -componente_num, -anio_clean, -trim_clean)

cat("✅ IDs generados (id_individuo, id_individuo_hist, id_hogar)\n\n")


# 🧹 5. Limpieza básica (edad, antigüedad) -------------------------------------
cat("🧹 Limpieza básica de variables...\n")

datos <- datos %>%
  mutate(
    edad       = limpiar_edad(edad),
    antiguedad = armar_antiguedad(.)
  )

cat("✅ Variables limpias: edad, antigüedad\n\n")


# 🔎 6. Validaciones de integridad ---------------------------------------------
cat("🔎 Ejecutando validaciones...\n\n")

validate_ids_unique(datos, "id_individuo")
validate_cobertura_temporal(datos, PERIODO_INI, PERIODO_FIN)

vars_criticas_01 <- c(
  "id_individuo", "id_individuo_hist", "id_hogar",
  "periodo_id", "periodo_num",
  "edad", "sexo", "condicion_actividad",
  "nivel_educ_obtenido2", "asistencia_escuela",
  "aglomerado", "region", "pondera"
)
validate_completitud(datos, vars_criticas_01, umbral_warn = 5)


# 💾 7. Guardado ---------------------------------------------------------------
cat("\n💾 Guardando resultados...\n")

saveRDS(datos, PATH_01_PANEL_RAW)

cat("✅ Guardado:", PATH_01_PANEL_RAW, "\n")
cat("   Dimensiones:", format(nrow(datos), big.mark = ".", decimal.mark = ","), "x", ncol(datos), "\n")
cat("   Tamaño en memoria:", format(object.size(datos), units = "MB"), "\n\n")


# 🧹 Limpieza de objetos intermedios -------------------------------------------
rm(list = c("archivos_all", "df_arch", "vars_a_conservar", "vars_criticas_01"))
gc()


# 📑 Checklist de salida -------------------------------------------------------
cat("═══════════════════════════════════════════════════════\n")
cat("📑 CHECKLIST SCRIPT 01:\n")
cat("═══════════════════════════════════════════════════════\n")
cat("   [✅] Panel histórico cargado (",
    ANIO_INI, "T", TRIM_INI, " – ", ANIO_FIN, "T", TRIM_FIN, ")\n", sep = "")
cat("   [✅] N observaciones:", format(nrow(datos), big.mark = ".", decimal.mark = ","), "\n")
cat("   [✅] IDs generados (id_individuo, id_individuo_hist, id_hogar)\n")
cat("   [✅] Variables limpias (edad, antigüedad)\n")
cat("   [✅] Vivienda RAW incluida (para ICH en Script 03b)\n")
cat("   [✅] condicion_formalidad incluida (para join en Script 09a)\n")
cat("   [✅] busca_trabajo_* incluida (dinámica)\n")
cat("   [✅] Validaciones OK\n")
cat("   [", if (file.exists(PATH_01_PANEL_RAW)) "✅" else "❌",
    "] Output: ", basename(PATH_01_PANEL_RAW), "\n", sep = "")
cat("═══════════════════════════════════════════════════════\n\n")

cat("🎯 SIGUIENTE PASO: Ejecutar 02_taxonomia.R\n\n")


# ⌛ Tiempo final ---------------------------------------------------------------
end_time <- Sys.time()
elapsed  <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("═══════════════════════════════════════════════════════\n")
cat("✅ Script 01 finalizado:", as.character(end_time), "\n")
cat("⏱️  Tiempo total:", round(elapsed, 1), "segundos\n")
cat("═══════════════════════════════════════════════════════\n")
toc()
