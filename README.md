# Replication Package: From Breaks to Bridges

**Paper:** "From Breaks to Bridges: Harmonizing the New and Old Permanent Household Survey for Consistent Labor Market Series"

**Authors:** Germán Tessmer (Universidad Nacional de Rosario) and Bárbara Boggiano (Universidad Alberto Hurtado)

**Contact:** german.tessmer@unr.edu.ar | [Project page](https://germantessmer.github.io/)

---

## Overview

This repository contains the complete R pipeline to reproduce all results, figures, and tables in the paper. The pipeline constructs a bridged labour-formality series for Argentina (2016Q4--2025Q3) from EPH microdata, combining deterministic harmonization, a two-factor latent structure, LASSO-selected predictive models (LPM, GLM, SLS), and a hybrid backcasting rule.

## Requirements

- **R** >= 4.5.0
- **renv** for package management (run `renv::restore()` to install all dependencies from `renv.lock`)
- **Hardware:** 16 GB RAM recommended; 7+ CPU cores for parallel bootstrap and LOCO cross-validation
- **OS:** Developed on Windows 11; should work on Linux/macOS with minor path adjustments in `parametros.R`

## Data Sources

1. **EPH Microdata (primary):** Pre-processed panel files from the EPH--Observatorio repository.
   - DOI: [10.57715/UNR/BL85Z8](https://dataverse.unr.edu.ar/dataset.xhtml?persistentId=doi:10.57715/UNR/BL85Z8)
   - Place the `.RData` files in the directory specified by `RUTA_BASES` in `script/config/parametros.R` (default: `C:/oes/eph_rdos/capa2/`).

2. **SIPA Administrative Data:** Published by the Secretaría de Trabajo, Argentina.
   - URL: https://www.argentina.gob.ar/trabajo/estadisticas/situacion-y-evolucion-del-trabajo-registrado
   - Used only for external validation (Layer 7, script `10e`).

3. **INDEC Official Microdata:** Original EPH individual and household databases.
   - URL: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

No restricted-access or confidential data are used.

## How to Run

1. Clone this repository and open the R project.
2. Install dependencies: `renv::restore()`
3. Edit `script/config/parametros.R`:
   - Set `RUTA_BASES` to the directory containing the EPH `.RData` files.
   - Adjust `N_CORES` if needed (default: auto-detected minus 1).
4. Run the full pipeline: `source("00_master_runner.R")`

The master runner executes all layers sequentially. Total runtime is approximately 4--6 hours depending on hardware (the heterofactor estimation in Layer 3 is the bottleneck).

Individual layers can also be run independently by sourcing scripts in order.

## Pipeline Architecture (7 Layers)

| Layer | Directory | Description | Key Scripts |
|-------|-----------|-------------|-------------|
| 0 | `script/00_diccionarios/` | Variable dictionaries and crosswalks | `00_diccionarios.R` |
| 1 | `script/01_datos_base/` | Panel construction, taxonomy, income cleaning, ICH | `01` -- `03c` |
| 2 | `script/02_proxies/` | Proxy system for latent factors | `04`, `05` |
| 3 | `script/03_heterofactor/` | Two-factor latent model (FIML) | `06a` -- `06d` |
| 4 | `script/04_modelado/{LPM,GLM,SLS}/` | LASSO model estimation (3 families) | `07a` -- `07e` |
| 5 | `script/05_backcasting/{LPM,GLM,SLS}/` | Historical backcasting and hybrid construction | `08a` -- `08c` |
| 6 | `script/06_comparativo/` | Cross-model comparison | `09a` -- `09c` |
| 7 | `script/07_robusto/` | Robustness checks (15 scripts) | `10a` -- `10o` |

## Configuration

All paths, seeds, temporal parameters, and model suffixes are defined in `script/config/parametros.R`. Key settings:

- `ANIO_INI / TRIM_INI` -- `ANIO_FIN / TRIM_FIN`: Panel time range (default: 2016Q4--2025Q3)
- `N_TRIMESTRES_TRAINING`: Number of overlap quarters used for model training (default: 4)
- `SEED_GLOBAL`: Random seed for reproducibility (default: 123)
- `N_CORES`: Parallel workers for bootstrap and cross-validation

## Outputs

All outputs are written to `rdos/` (created automatically):

- `rdos/datos/` -- Processed panel files (`.rds`)
- `rdos/modelos/` -- Fitted models (`.rds`)
- `rdos/contratos/` -- Validation contracts with citable statistics (`.rds`)
- `rdos/reportes/` -- HTML diagnostic reports and CSV summaries
- `rdos/figuras/` -- All figures as PDF files

## Language Note

The R scripts are written in Spanish (comments, variable names, diagnostic messages). Each script includes a bilingual header summarizing its purpose, inputs, and outputs in English. The pipeline structure and variable naming follow a consistent convention documented in the headers and in the variable dictionary (`rdos/inputs/diccionarios/`).

## License

MIT License. See `LICENSE` for details.
