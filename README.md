# Daily versus Weekly Data in Epidemiological Forecasting: Computational and Practical Implications

In the case of COVID-19 infection incidence in South Africa, based on provincial reporting.

## Background

The COVID-19 pandemic highlighted the possibility of infectious disease forecasting, with widespread production and consumption of forecasts of cases, hospitalization, and deaths. Notionally, these forecasts informed decisions by individuals, businesses, and public health policy makers and operations. Given their popularity, we expect these methods to be used again in future epidemic and pandemics.

As such, now is the time to investigate the consequences for different deployment arrangements for these methods: they will exist as part an overall response preparation and response plan. That plan will have to make decisions balancing the practicalities and realities of governance intrinsic to public health, so we should be evaluating these methods in terms of the decisions their results would inform and the actual data they might be provided.

In this analysis, we consider how aggregation in time and space can affect the quality of forecasts.

## Quick Start

This analysis uses [`(gnu)make`](https://www.gnu.org/software/make/manual/make.html) to create a pipeline of analysis steps, primarily using [`R`](https://www.r-project.org/), with data handling using `{data.table}`, forecasting using `{EpiNow2}`, scoring using `{scoringutils}`, and visualizations using `{ggplot2}`. Assuming `git`, `make`, and `R` available, installation can be managed at command prompt with:

```bash
$ git clone git@github.com:tatendashoko/now-fore-cast
$ cd now-fore-cast
$ make install_packages
```

## Data:

South Africa’s  daily confirmed Covid-19 cases from the South African National Institute for Communicable Diseases (NICD) collated by the Data Science for Social Impact Research Group @ University of Pretoria. Specifically, we will make use of daily South African provincial confirmed Covid-19 cases from 5 March 2020 to 25 July 2022.  The data contains daily reported cases and deaths for each province and can be accessed [here](https://github.com/dsfsi/covid19za/blob/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv).

World Health Organization Covid-19 data on South Africa nationwide collected weekly. The [WHO weekly data](https://data.who.int/dashboards/covid19/cases?m49=710&n=o) will be considered as the lower resolution (lower quality) data in our analyses.
