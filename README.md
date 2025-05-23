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

## Data:

South Africa’s  daily confirmed Covid-19 cases from the South African National Institute for Communicable Diseases (NICD) collated by the Data Science for Social Impact Research Group @ University of Pretoria. Specifically, we will make use of daily South African provincial confirmed Covid-19 cases from 5 March 2020 to 25 July 2022.  The data contains daily reported cases and deaths for each province and can be accessed [here](https://github.com/dsfsi/covid19za/blob/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv).

World Health Organization Covid-19 data on South Africa nationwide collected weekly. The [WHO weekly data](https://data.who.int/dashboards/covid19/cases?m49=710&n=o) will be considered as the lower resolution (lower quality) data in our analyses.
