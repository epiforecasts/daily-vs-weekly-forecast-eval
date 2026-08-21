# Daily versus Weekly Data in Epidemiological Forecasting: Computational and Practical Implications

> **📄 View latest paper version [here](https://github.com/jamesmbaazam/daily-vs-weekly-forecast-eval/blob/main/paper/paper.pdf)**

## Background

We evaluated the predictive and computational tradeoffs of forecasting COVID-19 infections using daily vs. weekly case data in South Africa. Using a recent forecast accumulation feature modelling aggregated data in the [EpiNow2](https://epiforecasts.io/EpiNow2/) modelling framework, we compared forecast accuracy and efficiency. While daily data produced better fits overall, similar performance can be achieved with weekly data, albeit with greater computational tradeoffs. We outline a workflow to achieve comparable model fits across temporal resolutions. These findings are context-dependent and must be balanced against the value of timely, accurate public health decisions.

## Quick Start

This analysis uses [`(gnu)make`](https://www.gnu.org/software/make/manual/make.html) to orchestrate a pipeline of analysis steps, primarily using [`R`](https://www.r-project.org/), with data handling using `{data.table}`, forecasting using `{EpiNow2}`, scoring using `{scoringutils}`, and visualizations using `{ggplot2}`. Assuming `git`, `make`, and `R` are installed:

```bash
$ git clone https://github.com/epiforecasts/daily-vs-weekly-forecast-eval.git
$ cd daily-vs-weekly-forecast-eval
```

R package installation is handled automatically: any `make` target restores the `renv` environment first by running `install.R`, so there's no separate setup step. If you'd rather install packages before running anything, you can trigger the same step manually with `Rscript install.R`.

Running plain `make` builds every figure for every province (the default target, `all_figs`), which can take hours to days depending on your computing infrastructure. We recommend starting with the single-province test target instead, which defaults to province "GP" (Gauteng) but can be pointed at any other province:

```bash
$ make test
$ make test ONEPROV=WC   # or any other province code
```

<details>
<summary>Full list of Make targets</summary>

| Target | Description |
| --- | --- |
| `make` / `make all_figs` | Build every figure (scores, diagnostics panels, CRPS summary) for all provinces — the default target |
| `make test` | Build score and diagnostics panels for a single province (`ONEPROV`, default `GP`) |
| `make allextracts` | Build all daily/weekly data extracts for all provinces |
| `make all_forecasts` | Build all forecast outputs |
| `make all_scores` | Build all CRPS score outputs |
| `make all_diagnostics` | Build all MCMC diagnostics outputs |
| `make all_scores_panel_figs` | Build score panel figures for all provinces |
| `make all_diagnostics_panel_figs` | Build diagnostics panel figures for all provinces |
| `make all_dvsw_figs` | Build daily-vs-weekly comparison figures |
| `make all_crps_figs` | Build CRPS score-scatter figures for all provinces |
| `make all_provs_crps_summary_fig` | Build the combined CRPS summary figure across provinces |
| `make paper_main_text` | Render the main manuscript text to PDF |
| `make supplementary` | Render the supplementary materials to PDF |
| `make paper_full` | Render both the main text and supplementary materials |

Provinces: `GP`, `WC`, `EC`, `KZN`, `FS`, `LP`, `MP`, `NC`, `NW`, plus `RSA` (national aggregate).

</details>

## Repository Structure

- `R/`: Core analysis scripts (data import/extraction, forecasting pipelines, scoring, diagnostics, figures)
- `paper/`: Quarto manuscript, supplementary materials, and bibliography
- `local/`: Generated outputs (data extracts, figures, forecast/score/diagnostics results) — created by the pipeline, not checked in
- `renv/`: R package environment, managed by `renv`

<details>
<summary>Pipeline stages</summary>

1. **Data acquisition** — the raw South African provincial COVID-19 case data is downloaded directly from the DSFSI GitHub repository (see [Data](#data) below) into `local/data/raw.csv`.
2. **Data import** (`R/import.R`) — type conversion and pivoting from wide raw CSV to long intermediate format, no cleaning.
3. **Data extraction** (`R/extract.R`) — cleans the data and produces per-province daily and weekly extracts, plus a national (`RSA`) aggregate (`R/aggregate.R`).
4. **Forecasting** (`R/pipeline_main.R`, `R/pipeline_rescaled_weekly.R`) — runs EpiNow2 forecasts in a sliding window (70-day train, 14-day test) for daily data, weekly data, and rescaled-weekly data (weeks treated as "days" for a like-for-like comparison), with adaptive MCMC tuning to handle convergence issues.
5. **Scoring** (`R/score.R`) — computes CRPS scores against both daily and weekly reference data using `{scoringutils}`.
6. **Diagnostics** (`R/diagnostics.R`) — extracts MCMC diagnostics (divergences, Rhat, ESS) from the underlying Stan fits.
7. **Visualization** (`R/fig_panel_*.R`) — combines scores and diagnostics into comparison panels across forecast types and provinces.

</details>

## Rendering the Paper

The manuscript is written in Quarto Markdown. The recommended way to render it is via Make:

```bash
make paper_main_text   # renders paper/paper.qmd to paper/paper.pdf
make supplementary     # renders paper/supplementary.qmd to paper/supplementary.pdf
make paper_full        # renders both
```

This requires [Quarto](https://quarto.org/docs/get-started/) and a LaTeX distribution (TinyTeX is recommended: `quarto install tinytex`).

<details>
<summary>Other ways to render</summary>

**Using Quarto directly:**

```bash
cd paper
quarto render paper.qmd
```

**Using RStudio:**

1. Open `paper/paper.qmd` in RStudio (version 2022.07 or later, which includes Quarto support)
2. Click the "Render" button in the toolbar (or press `Ctrl+Shift+K` / `Cmd+Shift+K`)
3. The PDF will be generated in the `paper/` directory

**Using VS Code:**

1. Install the [Quarto extension](https://marketplace.visualstudio.com/items?itemName=quarto.quarto) for VS Code
2. Open `paper/paper.qmd` in VS Code
3. Click the "Preview" button in the toolbar, or use the command palette (`Ctrl+Shift+P` / `Cmd+Shift+P`) and select "Quarto: Render"
4. The PDF will be generated in the `paper/` directory

</details>

<details>
<summary>Automatic rendering (CI)</summary>

The repository includes a GitHub Actions workflow (`.github/workflows/render-paper.yml`, "Render Paper") that renders the paper automatically:

- **Push to `main`**: renders the paper and commits the updated PDF back to the repository, but only when the push touches files under `paper/` or the workflow file itself.
- **Pull requests**: renders the paper and uploads it as a downloadable artifact when a PR targeting `main` touches `paper/`.
- **Manual trigger**: can be run on demand from the Actions tab (`workflow_dispatch`).

Accessing rendered PDFs:
1. **From commits**: after changes are merged to main, the updated PDF is committed to `paper/paper.pdf`.
2. **From pull requests**: go to the Actions tab → select the workflow run → download the `paper-pdf` artifact.
3. **Manual runs**: Actions → "Render Paper" → "Run workflow".

The workflow installs Quarto and TinyTeX itself, so it doesn't depend on the runner having them preinstalled.

</details>

## Data

South Africa's daily confirmed COVID-19 case data comes from the South African National Institute for Communicable Diseases (NICD), collated by the Data Science for Social Impact Research Group at the University of Pretoria. The pipeline downloads daily provincial confirmed-case counts covering 5 March 2020 to 25 July 2022 directly from the [DSFSI `covid19za` repository](https://github.com/dsfsi/covid19za/blob/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv).

The "weekly" series used throughout the comparison is not an independently sourced, lower-resolution dataset — it is the same daily NICD series, resampled onto a weekly stride and differenced as part of the extraction step, so that daily and weekly forecasts can be compared on data that differ only in temporal resolution, not in source or coverage.
