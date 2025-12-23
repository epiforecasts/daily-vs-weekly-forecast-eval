# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a research project comparing COVID-19 forecasting using daily versus weekly case data in South Africa. It evaluates predictive and computational tradeoffs using the EpiNow2 modeling framework. The analysis demonstrates that while daily data produces better fits, weekly data can achieve similar performance with greater computational costs.

## Build System

The project uses GNU Make to orchestrate the analysis pipeline. All primary workflows are managed through the Makefile.

### Key Commands

**Setup and Dependencies:**
```bash
Rscript install.R              # Initialize renv environment and restore packages
```

**Running Analyses:**
```bash
make                            # Runs default target (all_scores_panel_figs)
make local/figures/fig_panel_GP.png  # Generate results for single province (GP)
make test                       # Test with single province (default: GP, override with ONEPROV=WC)
make allextracts               # Generate all data extracts (daily/weekly for all provinces)
make all_forecasts             # Generate all forecast outputs
make all_scores                # Generate all score outputs
make all_panel_figs            # Generate all panel figures (scores + diagnostics)
```

**Paper Rendering:**
```bash
make paper                      # Render paper/paper.qmd to paper/paper.pdf
cd paper && quarto render paper.qmd  # Alternative: render directly with Quarto
```

**Override Default Province:**
```bash
make test ONEPROV=WC           # Run test pipeline for Western Cape
```

### Directory Structure

- `REFDIR` (default: `local/`): Base directory for outputs
  - `local/data/`: Processed datasets (daily/weekly extracts per province)
  - `local/figures/`: Generated figures and visualizations
  - `local/output/`: Forecast results, scores, and diagnostics
- `R/`: Core analysis scripts (reusable functions)
- `main/`: Legacy/exploratory scripts (not used in main pipeline)
- `paper/`: Quarto manuscript and bibliography
- `renv/`: R package environment (managed by renv)

## Code Architecture

### Data Flow Pipeline

The analysis follows a Make-driven pipeline with distinct stages:

1. **Data Acquisition** (`get_data.R` → `raw.csv`)
   - Downloads South African provincial COVID-19 data from DSFSI GitHub

2. **Data Import** (`R/import.R` → `intermediate.rds`)
   - Type conversion and pivoting raw CSV to long format
   - No cleaning, only structural transformation

3. **Data Extraction** (`R/extract.R` → `{daily,weekly}_{PROVINCE}.rds`)
   - Cleans data and extracts province-specific datasets
   - Creates both daily and weekly aggregations
   - Provinces: GP, WC, EC, KZN, FS, LP, MP, NC, NW, RSA (national aggregate)

4. **Forecasting** (`R/pipeline_main.R`, `R/pipeline_rescaled_weekly.R`)
   - Three forecast types per province:
     - `forecast_daily_{PROVINCE}.rds`: Daily data forecasts
     - `forecast_weekly_{PROVINCE}.rds`: Weekly data forecasts
     - `forecast_rescale_{PROVINCE}.rds`: Rescaled weekly forecasts (weeks treated as "days")
   - Uses EpiNow2 with sliding window approach (train: 70 days, test: 14 days)
   - Adaptive MCMC tuning via `ratchet_control()` function

5. **Scoring** (`R/score.R` → `score_{PROVINCE}.rds`)
   - Uses scoringutils package to compute CRPS scores
   - Compares all forecast types against both daily and weekly reference data

6. **Diagnostics** (`R/diagnostics.R` → `diagnostics_{PROVINCE}.csv`)
   - Extracts MCMC diagnostics (divergences, Rhat, ESS) from Stan fits

7. **Visualization** (`R/fig_panel_*.R`)
   - Panel figures combining scores and diagnostics across forecast types

### Key R Scripts

**Core Pipeline Components:**
- `R/pipeline_shared_inputs.R`: Shared configuration, helper functions, and EpiNow2 parameters
  - `dtextract()`: Extract and format score results
  - `join_and_score()`: Join forecasts to reference data and compute CRPS
  - `trim_leading_zero()`: Remove leading zeros from time series
  - `get_rstan_diagnostics()`: Extract MCMC diagnostics from Stan fits
  - `ratchet_control()`: Adaptive MCMC tuning (increases adapt_delta iteratively)
  - `keep_running()`: While loop control for adaptive fitting (stops when divergences < threshold or max ratchets reached)

- `R/pipeline_main.R`: Main forecasting pipeline for daily/weekly data
  - Runs epinow() in sliding windows
  - Week effect enabled for daily data, disabled for weekly

- `R/pipeline_rescaled_weekly.R`: Special pipeline for rescaled weekly data
  - Converts weekly dates to fake daily dates (treats weeks as days)
  - Rescales all time-based parameters (generation time, delays) by factor of 1/7
  - Train window: 10 "days" (actually weeks), test: 2 "days"

**Data Processing:**
- `R/import.R`: CSV import and basic structuring
- `R/extract.R`: Province extraction and data cleaning
- `R/aggregate.R`: National (RSA) aggregation from provincial data

**Evaluation:**
- `R/score.R`: CRPS calculation using scoringutils
- `R/diagnostics.R`: MCMC convergence diagnostics

**Visualization:**
- `R/fig_panel_scores.R`: Score comparison panels
- `R/fig_panel_diagnostics.R`: Diagnostic panels (Rhat, ESS, divergences)
- `R/fig_daily_vs_weekly.R`: Time series comparison plots

### EpiNow2 Configuration

**Model Parameters** (defined in `R/pipeline_shared_inputs.R`):
- Incubation period: LogNormal from epiparameter package (COVID-19)
- Generation time: Gamma(mean=7.12, sd=1.72, max=10)
- Reporting delay: LogNormal(meanlog=0.58, sdlog=0.47, max=10)
- Rt prior: LogNormal(meanlog=0.69, sdlog=0.05) ~ mean=2, sd=0.1

**Stan Options:**
- Samples: 5000
- Cores: min(detected cores - 1, 4)
- Backend: cmdstanr (if available), else rstan
- Initial adapt_delta: 0.8, max_treedepth: 10

**Adaptive Tuning:**
The pipeline implements adaptive MCMC tuning to handle convergence issues:
- Fits are re-run with increased adapt_delta if divergences/Rhat/ESS thresholds not met
- `ratchet_control()` increases adapt_delta by 25% of remaining distance to 0.99
- Max 11 ratchets (brings adapt_delta from 0.8 to 0.99)
- Stopping criteria: divergences < 0.25% of samples, Rhat < 1.01, ESS_bulk >= 400

### Makefile Pattern Rules

The Makefile uses pattern rules extensively. Key patterns:

- `${DATDIR}/daily_%.rds` and `${DATDIR}/weekly_%.rds`: Data extracts (% = province code)
- `${OUTDIR}/forecast_%.rds`: Forecasts (% = {daily,weekly}_PROVINCE)
- `${OUTDIR}/forecast_rescale_%.rds`: Rescaled weekly forecasts
- `${OUTDIR}/score_%.rds`: Scores per province
- `${FIGDIR}/fig_panel_scores_%.png`: Score panel figures

Dependencies are explicit in the Makefile; study dependency chains before modifying targets.

## Dependencies

The project uses renv for R package management. All dependencies are locked in `renv.lock`. After cloning:

1. Run `Rscript install.R` to restore the environment
2. renv will automatically activate on R session start (via `.Rprofile`)

**Key R Packages:**
- `EpiNow2`: Epidemic forecasting framework
- `data.table`: Fast data manipulation
- `scoringutils`: Forecast scoring (CRPS)
- `ggplot2`: Visualization
- `bayesplot`: MCMC diagnostics
- `epiparameter`: Epidemiological parameter database
- `cmdstanr` or `rstan`: Stan interface for Bayesian inference

## Paper

The manuscript is in `paper/paper.qmd` (Quarto format) and renders to PLOS One journal style using the `quarto-journals/plos` extension. A GitHub Actions workflow automatically renders the paper when changes are pushed to main.

## Important Notes

- Forecasting is computationally expensive (hours to days for all provinces)
- Start testing with a single province: `make local/figures/fig_panel_GP.png`
- The `main/` directory contains legacy exploratory code; active pipeline is in `R/`
- Command-line arguments in R scripts use pattern: `commandArgs(trailingOnly = TRUE)`
- Interactive mode (in RStudio/R console) uses `.args` defined at script top
- All R scripts source `R/pipeline_shared_inputs.R` for shared functions and parameters
