# Post-processing helpers shared by the scoring figures and the manuscript
# summary.
#
# These deliberately live outside R/pipeline_shared_inputs.R: that file is a
# prerequisite of the forecasting rules, so editing it marks every forecast
# out of date and a subsequent `make` refits the whole pipeline. Nothing here
# affects the fitted models, so keeping it separate lets these helpers be
# changed freely.
#
# Requires data.table, which the sourcing script is expected to have loaded.

# Provinces ordered by population, with the national aggregate last. Shared so
# that figures and summaries order and label locations identically.
pop_order <- c("NC", "FS", "NW", "MP", "LP", "EC", "WC", "KZN", "GP", "RSA")

#' Load every per-province score file in a directory and bind them
#'
#' @param dir Directory holding the `score_<province>.rds` files
#'
#' @returns A single dt of all scores with a `province` column, ordered by
#' population (see `pop_order`)
#' @export
#'
#' @examples
read_scores <- function(dir) {
  fls <- list.files(dir, "score_.*\\.rds", full.names = TRUE)
  scores <- fls |>
    setNames(gsub("^.*_(.*)\\.rds$", "\\1", fls)) |>
    lapply(readRDS) |>
    rbindlist(idcol = "province")
  scores[, province := factor(province, levels = pop_order, ordered = TRUE)]
  scores[]
}

#' Summarise CRPS relative to the model trained on daily data
#'
#' Joins every non-daily forecast to the daily-trained forecast on the same
#' slide, date, evaluation resolution and province, then summarises the
#' distribution of the per-date CRPS ratio. The geometric mean is the
#' exponentiated mean of the log ratios.
#'
#' Shared by the summary figure and the manuscript summary so the plotted and
#' quoted values cannot diverge.
#'
#' @param scores A dt of scores as returned by [read_scores()]
#'
#' @returns A dt keyed by `forecast`, `data` and `province`, with the ratio
#' quantiles (`lo95`, `lo50`, `md`, `hi50`, `hi95`) and `geomean`
#' @export
#'
#' @examples
crps_ratio_summary <- function(scores) {
  scores_ref <- scores[forecast == "daily"][, .SD, .SDcols = -c("forecast")]

  scores_rel <- scores[forecast != "daily"][
    scores_ref,
    on = .(slide, date, data, province),
    nomatch = 0
  ]

  scores_rel[
    ,
    {
      qs <- quantile(crps / i.crps, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
        setNames(c("lo95", "lo50", "md", "hi50", "hi95"))
      c(qs, geomean = exp(mean(log(crps / i.crps)))) |> as.list()
    },
    by = .(forecast, data, province)
  ]
}
