# =============================================================================
# [EN] taxonomia.R -- Taxonomy functions: map economic sector and occupational skill from raw INDEC codes
# INPUTS:  None (function definitions only)
# OUTPUTS: Functions: mapear_seccion(), normalizar_calificacion(), get_*_levels()
# =============================================================================
# ==============================================================================
# taxonomia.R
# Funciones centralizadas para mapeo de sección económica y calificación
# ==============================================================================
# 📌 OBJETIVO:
#    Proveer funciones de taxonomía reutilizables para estandarizar sección
#    económica y calificación ocupacional a lo largo del pipeline.
#    Elimina duplicación entre scripts. Se carga DESPUÉS de limpieza.R.
#
# 📦 DEPENDENCIAS (deben estar cargadas en el script principal):
#    haven, data.table, dplyr, stringr
#
# 📤 FUNCIONES EXPORTADAS:
#    · mapear_seccion()          — texto crudo INDEC → categoría estandarizada
#    · normalizar_calificacion() — calificación cruda → nivel canónico
#    · get_seccion_levels()      — vector completo de niveles canónicos de sección
#    · get_calificacion_levels() — vector completo de niveles canónicos de calificación
# ==============================================================================


#' Mapear sección económica usando diccionario exacto
#'
#' Mapea el texto crudo de sección (del INDEC) a categorías cortas
#' estandarizadas. Incluye parche regex para Servicio Doméstico (textos
#' variables entre trimestres) y fallback "Otros/Desconocido" para
#' valores no reconocidos.
#'
#' @param seccion_texto_crudo Vector character con descripción INDEC de sección
#' @param diccionarios        Lista con diccionarios (reservado para uso futuro)
#' @return                    Vector character con categorías estandarizadas
#'
#' @examples
#' seccion <- mapear_seccion(datos$seccion_ocupado)
mapear_seccion <- function(seccion_texto_crudo, diccionarios = NULL) {

  # Zap labels si viene como vector labelled haven
  stc <- as.character(haven::zap_labels(seccion_texto_crudo))

  # Mapeo exacto por texto INDEC
  out <- dplyr::case_match(
    stc,
    # AGRO Y MINERÍA
    "Agricultura, Ganadería, Caza, Silvicultura y Pesca"                         ~ "Agro y Mineria",
    "Explotación de Minas y Canteras"                                            ~ "Agro y Mineria",

    # INDUSTRIA
    "Industria Manufacturera"                                                    ~ "Industria",
    "Suministro de Electricidad, Gas, Vapor y Aire Acondicionado"                ~ "Industria",
    "Suministro de Agua; Alcantarillado, Gestión de Desechos y Act. de Saneamiento" ~ "Industria",
    "Suministro de Agua; Alcantarillado, Gestión de Desechos..."                 ~ "Industria",

    # CONSTRUCCIÓN
    "Construcción"                                                               ~ "Construccion",

    # COMERCIO
    "Comercio al por Mayor y al por Menor; Rep. de Vehículos"                    ~ "Comercio",
    "Comercio al por Mayor y al por Menor; Reparación de Vehículos Automotores y Motocicletas" ~ "Comercio",

    # TRANSPORTE
    "Transporte y Almacenamiento"                                                ~ "Transporte",

    # HOTELERÍA Y GASTRONOMÍA
    "Alojamiento y Serv. de Comidas"                                             ~ "Hoteleria y Gastronomia",
    "Alojamiento y Servicios de Comidas"                                         ~ "Hoteleria y Gastronomia",

    # INFORMÁTICA Y COMUNICACIONES
    "Información y Comunicación"                                                 ~ "Informatica y Comunicaciones",

    # FINANCIERAS E INMOBILIARIAS
    "Act. Financieras y de Seguros"                                              ~ "Financieras e Inmobiliarias",
    "Actividades Financieras y de Seguros"                                       ~ "Financieras e Inmobiliarias",
    "Act. Inmobiliarias"                                                         ~ "Financieras e Inmobiliarias",
    "Actividades Inmobiliarias"                                                  ~ "Financieras e Inmobiliarias",

    # PROFESIONALES Y ADMINISTRATIVAS
    "Act. Profesionales, Científicas y Técnicas"                                 ~ "Profesionales y Administrativas",
    "Actividades Profesionales, Científicas y Técnicas"                          ~ "Profesionales y Administrativas",
    "Act. Administrativas y Serv. de Apoyo"                                      ~ "Profesionales y Administrativas",
    "Actividades Administrativas y Servicios de Apoyo"                           ~ "Profesionales y Administrativas",

    # ADMINISTRACIÓN PÚBLICA Y DEFENSA
    "Adm. Pública y Defensa; Planes de Seguro Social Oblig."                     ~ "Admin Publica y Defensa",
    "Administración Pública y Defensa; Planes de Seguro Social Obligatorio"      ~ "Admin Publica y Defensa",

    # ENSEÑANZA
    "Enseñanza"                                                                  ~ "Enseñanza",

    # SALUD
    "Salud Humana y Serv. Sociales"                                              ~ "Salud",
    "Salud Humana y Servicios Sociales"                                          ~ "Salud",

    # SERVICIOS PERSONALES Y COMUNITARIOS
    "Artes, Entretenimiento y Recreación"                                        ~ "Servicios Personales y Comunitarios",
    "Otras Act. de Servicios"                                                    ~ "Servicios Personales y Comunitarios",
    "Otras Actividades de Servicios"                                             ~ "Servicios Personales y Comunitarios",

    # SERVICIO DOMÉSTICO (mapeo exacto; textos cortos cubiertos aquí)
    "Act. de los Hogares..."                                                     ~ "Servicio Domestico",

    # ORGANISMOS EXTRATERRITORIALES
    "Act. de Organizaciones y Órganos Extraterritoriales"                        ~ "Organismos Extraterritoriales",

    # SIN CORRESPONDENCIA
    "No corresponde"                                                             ~ NA_character_,
    .default                                                                     = NA_character_
  )

  # Parche regex: Servicio Doméstico (textos largos variables entre trimestres)
  out <- data.table::fifelse(
    is.na(out) & !is.na(stc) &
      stringr::str_detect(stc, stringr::regex("Hogares|Domestico", ignore_case = TRUE)),
    "Servicio Domestico",
    out
  )

  # Fallback: valores no reconocidos → "Otros/Desconocido"
  out <- data.table::fifelse(
    is.na(out) & !is.na(stc) & stc != "No corresponde",
    "Otros/Desconocido",
    out
  )

  return(out)
}


#' Normalizar calificación ocupacional
#'
#' Normaliza calificación cruda a niveles canónicos usando mapeo exacto
#' seguido de detección fuzzy para variantes ortográficas.
#'
#' @param calif_raw Vector character con calificación cruda (puede ser labelled)
#' @return          Vector character con niveles canónicos
normalizar_calificacion <- function(calif_raw) {

  # Zap labels + normalizar espacios
  cr  <- as.character(haven::zap_labels(calif_raw))
  cr2 <- stringr::str_squish(cr)

  CALIF_LEVELS <- c("Anómalo", "No calificado", "Ns/Nc", "Operativo", "Profesional", "Técnico")

  out <- dplyr::case_when(
    is.na(cr2)                                                                        ~ NA_character_,
    cr2 %in% CALIF_LEVELS                                                             ~ cr2,
    stringr::str_detect(cr2, stringr::regex("^no\\s*cal",               ignore_case = TRUE)) ~ "No calificado",
    stringr::str_detect(cr2, stringr::regex("^ns\\/nc$|^ns\\s*\\/\\s*nc$|^ns\\s*nc$",
                                            ignore_case = TRUE))                      ~ "Ns/Nc",
    stringr::str_detect(cr2, stringr::regex("oper",                     ignore_case = TRUE)) ~ "Operativo",
    stringr::str_detect(cr2, stringr::regex("prof",                     ignore_case = TRUE)) ~ "Profesional",
    stringr::str_detect(cr2, stringr::regex("t[eé]cn",                  ignore_case = TRUE)) ~ "Técnico",
    stringr::str_detect(cr2, stringr::regex("an[oó]m",                  ignore_case = TRUE)) ~ "Anómalo",
    TRUE                                                                              ~ cr2
  )

  # Limpiar strings vacíos residuales
  out <- dplyr::na_if(out, "")

  return(out)
}


#' Obtener niveles canónicos de sección económica
#'
#' Incluye niveles especiales para no-PEA y desocupados sin experiencia,
#' consistente con SECCION_LEVELS definido en parametros.R.
#'
#' @return Vector character con todos los niveles válidos de sección
get_seccion_levels <- function() {
  c(
    "Admin Publica y Defensa",
    "Agro y Mineria",
    "Comercio",
    "Construccion",
    "Enseñanza",
    "Financieras e Inmobiliarias",
    "Hoteleria y Gastronomia",
    "Industria",
    "Informatica y Comunicaciones",
    "Organismos Extraterritoriales",
    "Otros/Desconocido",
    "Profesionales y Administrativas",
    "Salud",
    "Servicio Domestico",
    "Servicios Personales y Comunitarios",
    "Transporte",
    "No aplica (No PEA)",       # No-PEA
    "Sin Experiencia Previa"    # Desocupados sin empleo previo
  )
}


#' Obtener niveles canónicos de calificación ocupacional
#'
#' Incluye niveles especiales para no-PEA y desocupados sin calificación,
#' consistente con CALIFICACION_LEVELS definido en parametros.R.
#'
#' @return Vector character con todos los niveles válidos de calificación
get_calificacion_levels <- function() {
  c(
    "Anómalo",
    "No calificado",
    "Ns/Nc",
    "Operativo",
    "Profesional",
    "Técnico",
    "No aplica (No PEA)",       # No-PEA
    "Sin Calificación"          # Desocupados sin experiencia previa
  )
}

# ── Fin ───────────────────────────────────────────────────────────────────────
