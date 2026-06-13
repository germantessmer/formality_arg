# =============================================================================
# [EN] derivar_capa2.R -- Reproducibility shim: make the public capa1 EPH files match the
#      capa2 inputs the PAPER used. Derives the two capa2-only variables (nivel_educ_obtenido2,
#      condicion_formalidad) AND normalizes existing vars that capa2 transforms (sexo relabel;
#      income <=0 -> NA), replicating eph_full scripts 24/25 (legacy == new) as of the paper run.
# INPUTS:  a per-quarter EPH data.frame with capa1 columns (lowercase names)
# OUTPUTS: the same data.frame with the two derived variables added (when missing)
# =============================================================================
# 🌟 derivar_capa2.R 🌟 ####
#
# OBJETIVO:
#   El paquete público distribuye las bases EPH de CAPA 1 (formato Dataverse UNR,
#   DOI 10.57715/UNR/BL85Z8). El pipeline original consumía CAPA 2, que añade
#   variables derivadas que NO se publican Y transforma algunas que sí existen.
#   Este módulo lleva capa1 a la forma capa2 que usó el PAPER, en dos partes:
#
#   (A) DERIVA las dos variables capa2 ausentes en capa1:
#     · nivel_educ_obtenido2  — nivel educativo con evaluación de consistencia
#     · condicion_formalidad  — condición de formalidad/informalidad de ocupados
#
#   (B) NORMALIZA variables existentes que capa2 transforma y que el loader usa:
#     · sexo   — relabel "Hombre/Mujer" -> "Varones/Mujeres"
#     · ingreso_total_individual / ingreso_real_total_individual /
#       ingreso_real_capita_familiar  — regla `<= 0 -> NA` (versión del paper, pre 2026-05-22)
#   Verificado: panel capa1-fed == panel del paper (01_panel_historico_raw.rds), 84 cols.
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

  # ── sexo: relabel a forma capa2 ─────────────────────────────────────────────
  # eph_full 24.variables_capa2.R L13: recode(sexo, "Mujer"="Mujeres", "Hombre"="Varones")
  # Guard idempotente: solo actúa si están las etiquetas de capa1 (Hombre/Mujer).
  if (is.factor(datos$sexo)) {
    if ("Hombre" %in% levels(datos$sexo)) levels(datos$sexo)[levels(datos$sexo) == "Hombre"] <- "Varones"
    if ("Mujer"  %in% levels(datos$sexo)) levels(datos$sexo)[levels(datos$sexo) == "Mujer"]  <- "Mujeres"
  } else if (is.character(datos$sexo)) {
    datos$sexo[datos$sexo == "Hombre"] <- "Varones"
    datos$sexo[datos$sexo == "Mujer"]  <- "Mujeres"
  }

  # ── ingresos: regla capa2 `<= 0 -> NA` ──────────────────────────────────────
  # eph_full 24.variables_capa2.R L149-151. NOTA: se replica la versión VIGENTE
  # AL MOMENTO DEL PAPER (pre 2026-05-22), que aplica la regla también a la
  # variable per-cápita familiar (la excepción que preserva el 0 es posterior).
  # Idempotente: sobre datos capa2 (ya numéricos con NA en <=0) no cambia nada.
  for (v in c("ingreso_total_individual", "ingreso_real_total_individual",
              "ingreso_real_capita_familiar")) {
    if (v %in% names(datos)) {
      lab <- attr(datos[[v]], "label")
      x <- suppressWarnings(as.numeric(haven::zap_labels(datos[[v]])))
      x[!is.na(x) & x <= 0] <- NA_real_
      if (!is.null(lab)) attr(x, "label") <- lab
      datos[[v]] <- x
    }
  }

  datos
}
