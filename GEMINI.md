# Gemini Context: Daily vs Weekly Forecast Evaluation

## Project Overview
This is a research project evaluating the predictive and computational tradeoffs of forecasting COVID-19 infections using daily versus weekly case data in South Africa. It employs the `EpiNow2` modelling framework to compare forecast accuracy (CRPS) and efficiency across different temporal resolutions.

The project contains both the analysis code (R) and the manuscript (Quarto/LaTeX).

## Directory Structure

*   **`R/`**: Contains the **core analysis scripts** and pipeline logic.
    *   `pipeline_main.R`: Main forecasting script (daily and weekly).
    *   `pipeline_rescaled_weekly.R`: Special pipeline treating weeks as "days".
    *   `pipeline_shared_inputs.R`: Shared functions, parameters, and configuration.
    *   `import.R`, `extract.R`: Data ingestion and processing.
    *   `score.R`, `diagnostics.R`: Evaluation and MCMC diagnostics.
    *   `fig_*.R`: Visualization scripts.
*   **`main/`**: Contains legacy or exploratory scripts. **Do not use** for the primary pipeline.
*   **`paper/`**: Contains the Quarto manuscript (`paper.qmd`) and bibliography.
*   **`local/`**: The default directory for generated outputs (data, figures, raw forecast files).
    *   `local/data/`: Processed inputs (`.rds`).
    *   `local/output/`: Raw forecast objects and scores.
    *   `local/figures/`: Final plots.
*   **`renv/`**: R environment directory (managed by `renv`).

## Build & Run Instructions

The project uses `GNU Make` to orchestrate the entire workflow.

### Setup
```bash
Rscript install.R  # Restores R packages using renv
```

### Key Commands
*   **Run full analysis (Default):**
    ```bash
    make
    ```
    *Generates all scores and panel figures.*

*   **Test run (Single Province):**
    ```bash
    make local/figures/fig_panel_GP.png
    # OR
    make test ONEPROV=WC
    ```
    *Useful for verifying changes without running the full computationally expensive pipeline.*

*   **Render Paper:**
    ```bash
    make paper
    ```
    *Compiles `paper/paper.qmd` to PDF.*

*   **Generate Forecasts:**
    ```bash
    make all_forecasts
    ```

### Pipeline Flow
1.  **Data**: `get_data.R` -> `R/import.R` -> `R/extract.R` (Splits into Daily/Weekly per province).
2.  **Forecast**: `R/pipeline_main.R` (and `rescaled`) runs `EpiNow2` with sliding windows.
    *   *Note:* Includes an adaptive "ratchet" mechanism to improve MCMC convergence.
3.  **Evaluate**: `R/score.R` computes CRPS; `R/diagnostics.R` checks Stan fit quality.
4.  **Visualize**: `R/fig_panel_*.R` aggregates results into figures.

## Development Conventions

*   **Language**: R (primary), Quarto (manuscript).
*   **Dependency Management**: `renv`. Always ensure the environment is synchronized.
*   **Data Manipulation**: `data.table` is preferred over `dplyr` for performance.
*   **Visualization**: `ggplot2`.
*   **Script Arguments**: Scripts typically handle arguments via `commandArgs(trailingOnly = TRUE)` or a defined `.args` block for interactive debugging.
*   **Output Management**: All outputs should go to `local/` (or the directory defined by `REFDIR` in Makefile).

## Important Notes
*   **Computation Time**: Full forecasting is extremely intensive (hours to days). Always prefer testing with a single province (e.g., `ONEPROV=GP`) when developing.
*   **Rescaled Pipeline**: `pipeline_rescaled_weekly.R` effectively "tricks" the model by treating weeks as days to adjust for time-scale dependent parameters.
