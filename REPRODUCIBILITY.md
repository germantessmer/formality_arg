# Reproducibility note — from public `capa1` to the paper's inputs

This package is fully reproducible from the **publicly available layer-1 (`capa1`) EPH
files** distributed on the EPH–Observatorio Dataverse
([DOI 10.57715/UNR/BL85Z8](https://doi.org/10.57715/UNR/BL85Z8)). This note documents the
one place where the public input differs from the internal working data, and how the
pipeline closes that gap.

## The gap

The analysis was developed against an internal layer-2 (`capa2`) dataset that adds derived
variables and applies a handful of recodes on top of `capa1`. Only `capa1` is published.
The pipeline needs a small, well-defined set of those layer-2 transformations.

A column-by-column comparison of the panel built from `capa1` against the panel used for the
paper showed that exactly **two derived variables were missing** and **four columns needed
the layer-2 form**. Everything else in `capa1` is already what the pipeline consumes.

## The fix — `script/funciones/derivar_capa2.R`

The function `derivar_vars_capa2()` is sourced by the function loader and applied on data
load (in `01_carga_panel.R` and `00_diccionarios.R`). It is an **exact replica of the
upstream layer-2 recodes** (`eph_full` `24.variables_capa2.R`, identical across the EPH
pre-/post-2024Q4 methodologies), in two parts:

**(A) Derive the two layer-2-only variables**
- `nivel_educ_obtenido2` — education level with consistency check.
- `condicion_formalidad` — formality/informality status of the employed.

Both are reconstructed from `capa1` inputs that exist in **every** quarter (education level,
schooling, completion, approved year; activity status, pension-contribution `PP07H`,
occupational category) — so the derivation is valid across the full backcasting range.

**(B) Normalize existing variables to the layer-2 form the paper used**
- `sexo`: relabel `"Hombre"/"Mujer"` → `"Varones"/"Mujeres"` (the labels the model dummies
  and the presentation dictionary expect).
- `ingreso_total_individual`, `ingreso_real_total_individual`, `ingreso_real_capita_familiar`:
  apply the rule `<= 0 → NA` as it stood **at the time of the paper**.

The function is **idempotent and guarded**: if the input already contains these variables /
labels (i.e. it is `capa2`), it is a no-op. So the same pipeline runs unchanged on either
input.

## Validation

The panel built from `capa1` (with this shim) is **identical to the paper's reference panel**
— **0 differing cells across all 84 columns and ~1.84 million rows** (37 quarters, 2016Q4–
2025Q3). Because the rest of the pipeline is deterministic (fixed `SEED_GLOBAL`), matching
this foundational panel guarantees that all downstream proxies, models, contracts and
reported figures reproduce the paper's values by construction.
