# =============================================================================
# [EN] derivar_capa2.R -- Reproducibility shim: derive the two capa2-only variables
#      (nivel_educ_obtenido2, condicion_formalidad) from the publicly available capa1
#      EPH files, replicating eph_full scripts 24/25 verbatim (legacy == new).
# INPUTS:  a per-quarter EPH data.frame with capa1 columns (lowercase names)
# OUTPUTS: the same data.frame with the two derived variables added (when missing)
# =============================================================================
# 🌟 derivar_capa2.R 🌟 ####
#
# OBJETIVO:
#   El paquete público distribuye las bases EPH de CAPA 1 (formato Dataverse UNR,
#   DOI 10.57715/UNR/BL85Z8). El pipeline original consumía CAPA 2, que añade
#   variables derivadas que NO se publican. Este módulo replica, a partir de capa1,
#   las DOS únicas variables capa2 que el pipeline necesita y que no existen en capa1:
#
#     · nivel_educ_obtenido2  — nivel educativo con evaluación de consistencia
#     · condicion_formalidad  — condición de formalidad/informalidad de ocupados
#
#   La lógica es una RÉPLICA EXACTA de eph_full/script/{legacy,new}/24.variables_capa2.R
#   (ambas metodologías son idénticas para estas dos variables) y las etiquetas de
#   nombre provienen de 25.etiqueta_capa2.R. Las "etiquetas de valores" son los propios
#   strings producidos por los case_when (ambas son variables de tipo character).
#
#   Dependencias en capa1 (todas presentes y verificadas, legacy y new):
#     nivel_educ_obtenido2 <- nivel_educ_obtenido, nivel_educ_cursado,
#                             finalizo_educacion, anio_aprobado
#     condicion_formalidad <- condicion_actividad, desc_jubilatorio_asalariado (PP07H),
#                             categoria_ocupacional
#
# COMPORTAMIENTO:
#   Idempotente y retrocompatible — solo deriva una variable si (a) NO está presente
#   y (b) sus dependencias SÍ están. Si el input ya es capa2 (variables presentes),
#   la función no hace nada. Asume nombres de columnas en minúscula.

derivar_vars_capa2 <- function(datos) {

  tiene <- function(v) all(v %in% names(datos))

  # ── nivel_educ_obtenido2 ────────────────────────────────────────────────────
  # Réplica verbatim de eph_full 24.variables_capa2.R (líneas 50-80)
  if (!"nivel_educ_obtenido2" %in% names(datos) &&
      tiene(c("nivel_educ_obtenido", "nivel_educ_cursado",
              "finalizo_educacion", "anio_aprobado"))) {

    datos <- dplyr::mutate(datos,
      nivel_educ_obtenido = haven::as_factor(nivel_educ_obtenido),
      nivel_educ_cursado  = haven::as_factor(nivel_educ_cursado),

      nivel_educ_obtenido2 = dplyr::case_when(
        # Condiciones originales de 'aux'
        nivel_educ_cursado == "Universitario" & finalizo_educacion == "Si" | nivel_educ_cursado == "Posgrado" ~ "Universitaria Completa",
        nivel_educ_cursado == "Universitario" & finalizo_educacion == "No" ~ "Universitaria Incompleta",
        nivel_educ_cursado == "Terciario" & finalizo_educacion == "Si" ~ "Terciario Completo",
        nivel_educ_cursado == "Terciario" & finalizo_educacion == "No" ~ "Terciario Incompleto",
        nivel_educ_obtenido == "Secundaria Completa" & finalizo_educacion == "Si" ~ "Secundaria Completa",
        nivel_educ_obtenido == "Secundaria Incompleta" & finalizo_educacion == "No" ~ "Secundaria Incompleta",
        nivel_educ_obtenido == "Secundaria Incompleta" & nivel_educ_cursado == "EGB" & finalizo_educacion == "Si" ~ "Secundaria Incompleta",
        nivel_educ_obtenido == "Primaria Completa" & finalizo_educacion == "Si" ~ "Primaria Completa",
        nivel_educ_obtenido == "Primaria Incompleta" & finalizo_educacion == "No" ~ "Primaria Incompleta",
        nivel_educ_obtenido == "Sin instrucción" & nivel_educ_cursado != "Especial" | anio_aprobado == 98 ~ "Sin instrucción",

        # Condiciones originales de 'aux2' (que se aplicaban a los NA de 'aux')
        nivel_educ_cursado %in% c("Primario", "EGB") & anio_aprobado < 7 ~ "Primaria Incompleta",
        nivel_educ_cursado %in% c("Primario", "EGB") & anio_aprobado == 7 ~ "Primaria Completa",
        nivel_educ_obtenido == "Secundaria Incompleta" & nivel_educ_cursado == "EGB" & anio_aprobado %in% 8:9 ~ "Secundaria Incompleta",
        nivel_educ_cursado == "Polimodal" & anio_aprobado < 3 ~ "Secundaria Incompleta",
        nivel_educ_cursado == "Polimodal" & anio_aprobado == 3 ~ "Secundaria Completa",
        nivel_educ_cursado == "Secundario" & anio_aprobado < 5 ~ "Secundaria Incompleta",
        nivel_educ_cursado == "Secundario" & anio_aprobado >= 5 ~ "Secundaria Completa",
        nivel_educ_obtenido == "Secundaria Completa" & nivel_educ_cursado == "EGB" & anio_aprobado %in% 8:9 ~ "Secundaria Incompleta",
        nivel_educ_obtenido == "Secundaria Completa" & nivel_educ_cursado == "Polimodal" & anio_aprobado < 3 ~ "Secundaria Incompleta",
        nivel_educ_obtenido == "Secundaria Completa" & nivel_educ_cursado == "Polimodal" & anio_aprobado == 3 ~ "Secundaria Completa",
        nivel_educ_obtenido == "Secundaria Completa" & nivel_educ_cursado == "Secundario" & anio_aprobado < 5 ~ "Secundaria Incompleta",
        nivel_educ_obtenido == "Secundaria Completa" & nivel_educ_cursado == "Secundario" & anio_aprobado >= 5 ~ "Secundaria Completa",
        TRUE ~ NA_character_ # Mantener como NA si ninguna condición se cumple
      )
    )
    attr(datos$nivel_educ_obtenido2, "label") <- "Nivel educativo con evaluación de consistencia"
  }

  # ── condicion_formalidad ────────────────────────────────────────────────────
  # Réplica verbatim de eph_full 24.variables_capa2.R (líneas 88-93)
  if (!"condicion_formalidad" %in% names(datos) &&
      tiene(c("condicion_actividad", "desc_jubilatorio_asalariado",
              "categoria_ocupacional"))) {

    datos <- dplyr::mutate(datos,
      condicion_formalidad = dplyr::case_when(
        condicion_actividad == "Ocupado" & desc_jubilatorio_asalariado == "Si" &
          categoria_ocupacional %in% c("Empleado") ~ "Formal",
        condicion_actividad == "Ocupado" & desc_jubilatorio_asalariado == "No" ~ "No formal",
        condicion_actividad == "Ocupado" & desc_jubilatorio_asalariado == "No corresponde" ~ "No corresponde"
      )
    )
    attr(datos$condicion_formalidad, "label") <- "Condición de formalidad o informalidad de ocupados"
  }

  datos
}
