# Daily versus Weekly Data in Epidemiological Forecasting: Computational and Practical Implications

## Background

We evaluated the predictive and computational tradeoffs of forecasting COVID-19 infections using daily vs. weekly case data in South Africa. Using a recent forecast accumulation feature modelling aggregated data in the [EpiNow2](https://epiforecasts.io/EpiNow2/) modelling framework, we compared forecast accuracy and efficiency. While daily data produced better fits overall, similar performance can be achieved with weekly data, albeit with greater computational tradeoffs. We outline a workflow to achieve comparable model fits across temporal resolutions. These findings are context-dependent and must be balanced against the value of timely, accurate public health decisions.

## Quick Start

This analysis uses [`(gnu)make`](https://www.gnu.org/software/make/manual/make.html) to orchestrate a pipeline of analysis steps, primarily using [`R`](https://www.r-project.org/), with data handling using `{data.table}`, forecasting using `{EpiNow2}`, scoring using `{scoringutils}`, and visualizations using `{ggplot2}`. Assuming `git`, `make`, and `R` installed.

```bash
$ git clone https://github.com/jamesmbaazam/daily-vs-weekly-forecast-eval.git
$ cd daily-vs-weekly-forecast-eval
```

You can generate the results by running ```make```, however, note that it takes several hours to days to generate the targets for all the provinces, depending on your computing infrastructure. We recommend that you instead start by generating targets for one province, say, "GP", with

 ```bash
$ make local/figures/fig_panel_GP.png
```

## Rendering the Paper

The manuscript is written in Quarto Markdown and can be rendered to PDF.

### Manual Rendering

To render the paper manually, you have two options:

**Option 1: Using Make (Recommended)**

```bash
make paper
```

This will render `paper/paper.qmd` to `paper/paper.pdf`.

**Option 2: Using Quarto directly**

```bash
cd paper
quarto render paper.qmd
```

**Option 3: Using RStudio**

1. Open `paper/paper.qmd` in RStudio
2. Click the "Render" button in the toolbar (or press `Ctrl+Shift+K` / `Cmd+Shift+K`)
3. The PDF will be generated in the `paper/` directory

**Option 4: Using VS Code**

1. Install the [Quarto extension](https://marketplace.visualstudio.com/items?itemName=quarto.quarto) for VS Code
2. Open `paper/paper.qmd` in VS Code
3. Click the "Preview" button in the toolbar or use the command palette (`Ctrl+Shift+P` / `Cmd+Shift+P`) and select "Quarto: Render"
4. The PDF will be generated in the `paper/` directory

**Prerequisites:**
- [Quarto](https://quarto.org/docs/get-started/) must be installed
- A LaTeX distribution (TinyTeX is recommended and can be installed via `quarto install tinytex`)
- For RStudio: Version 2022.07 or later (includes Quarto support)
- For VS Code: Install the Quarto extension

### Automatic Rendering with GitHub Actions

The repository includes a GitHub Actions workflow that automatically renders the paper when changes are pushed to the `main` branch or when a pull request targeting `main` modifies files in the `paper/` directory.

**Workflow Triggers:**
- **Push to main**: Automatically renders the paper and commits the PDF back to the repository
- **Pull requests**: Renders the paper and uploads it as an artifact (viewable in the Actions tab)
- **Manual trigger**: Can be triggered manually from the Actions tab using workflow_dispatch

**Accessing Rendered PDFs:**
1. **From commits**: After changes are merged to main, the updated PDF is committed to `paper/paper.pdf`
2. **From pull requests**: Navigate to the Actions tab → Select the workflow run → Download the `paper-pdf` artifact
3. **Manual runs**: Trigger the workflow from Actions → Render Paper → Run workflow

The workflow automatically installs Quarto and TinyTeX, ensuring a consistent rendering environment.

## Data:

South Africa’s  daily confirmed Covid-19 cases from the South African National Institute for Communicable Diseases (NICD) collated by the Data Science for Social Impact Research Group @ University of Pretoria. Specifically, we will make use of daily South African provincial confirmed Covid-19 cases from 5 March 2020 to 25 July 2022.  The data contains daily reported cases and deaths for each province and can be accessed [here](https://github.com/dsfsi/covid19za/blob/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv).

World Health Organization Covid-19 data on South Africa nationwide collected weekly. The [WHO weekly data](https://data.who.int/dashboards/covid19/cases?m49=710&n=o) will be considered as the lower resolution (lower quality) data in our analyses.
