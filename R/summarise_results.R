library(data.table)

# Summarise the scored forecasts and MCMC diagnostics into the values quoted in
# the manuscript, so that the Results and Discussion re-derive themselves from
# whatever the pipeline last produced.
#
# The output is a plain list of base R types (no data.table objects), so that
# paper.qmd can read it with readRDS() alone and needs no packages at render
# time.
#
# Two things are stored: `vals`, the pre-rounded scalars the prose cites, and
# `claims`, the directional statements the prose depends on. Values alone are
# not enough -- a regenerated pipeline can leave every number current while
# making the sentence around it false -- so a failing claim stops this script,
# and stops the render.

.args <- if (interactive()) {
  c(
    file.path("local", "output"),
    file.path("R", "summary_utils.R"),
    file.path("local", "output", "paper_summary.rds")
  )
} else {
  commandArgs(trailingOnly = TRUE)
}

# Load the shared post-processing helpers
source(.args[length(.args) - 1])

outdir <- .args[1]

####################################
# Rounding
####################################
# Applied here and nowhere else, so that the prose and the figures cannot round
# the same quantity differently.

# Ratios below 10 to one decimal place, above it to whole multiples
fmt_ratio <- function(x) ifelse(abs(x) >= 10, round(x), round(x, 1))
fmt_pct <- function(x) round(x, 1)
fmt_ess <- function(x) signif(x, 2)
fmt_count <- function(x) as.integer(round(x))

# Drop the data.table class so the stored tables are subsettable with base R
as_plain <- function(dt) {
  d <- as.data.frame(dt)
  for (j in names(d)) if (is.factor(d[[j]])) d[[j]] <- as.character(d[[j]])
  rownames(d) <- NULL
  d
}

####################################
# Inputs
####################################

scores <- read_scores(outdir)

dgn_files <- list.files(outdir, "diagnostics_.*\\.csv", full.names = TRUE)
dgn <- dgn_files |>
  setNames(sub("\\.csv$", "", sub("^.*diagnostics_", "", basename(dgn_files)))) |>
  lapply(fread) |>
  rbindlist(idcol = "province")
dgn[, province := factor(province, levels = pop_order, ordered = TRUE)]

# Sampling efficiency, as plotted in the diagnostics panels: tail ESS over the
# elapsed fitting time accumulated across every ratchet in the slide. A handful
# of fits recorded no elapsed time, so these are dropped and counted rather
# than silently ignored.
dgn[, ess_per_sec := ess_tail / stan_elapsed_time]

####################################
# Summary tables
####################################

# CRPS relative to the model trained on daily data, by province
crps_ratio <- crps_ratio_summary(scores)

# Sampling efficiency by province and input type
ess <- dgn[, .(
  n = .N,
  n_missing = sum(is.na(ess_per_sec)),
  min = min(ess_per_sec, na.rm = TRUE),
  q05 = quantile(ess_per_sec, 0.05, na.rm = TRUE),
  q25 = quantile(ess_per_sec, 0.25, na.rm = TRUE),
  med = median(ess_per_sec, na.rm = TRUE),
  q75 = quantile(ess_per_sec, 0.75, na.rm = TRUE),
  q95 = quantile(ess_per_sec, 0.95, na.rm = TRUE),
  max = max(ess_per_sec, na.rm = TRUE)
), by = .(province, type)]

# Adaptive refits by province and input type
ratchets <- dgn[, .(
  n_slides = .N,
  n_refit = sum(ratchets > 0),
  pct_refit = 100 * mean(ratchets > 0),
  med_ratchets = median(ratchets),
  max_ratchets = max(ratchets)
), by = .(province, type)]

# Share of dates on which the daily-trained forecast beats the weekly-trained
# one, at each evaluation resolution
head_to_head <- dcast(
  scores[forecast %in% c("daily", "weekly")],
  province + data + slide + date ~ forecast,
  value.var = "crps"
)
crps_daily_better <- head_to_head[
  , .(n = .N, pct_dates = 100 * mean(daily < weekly)),
  by = .(province, data)
]

# Spread of CRPS on the log scale, for the daily-trained forecast evaluated
# against daily observations. `oom_typical` is the 10th-to-90th-percentile
# spread in orders of magnitude; `oom_full` is the full range, which includes
# the extremes at the wave peaks.
crps_oom <- scores[
  forecast == "daily" & data == "daily" & crps > 0,
  .(
    oom_typical = diff(log10(quantile(crps, c(0.10, 0.90)))),
    oom_full = diff(log10(range(crps)))
  ),
  by = province
]

####################################
# Pooled helpers
####################################

# Province used as the worked example in the Results text and figures
example_province <- "EC"

# The "typical range" of sampling efficiency is the 5th to 95th percentile of
# all slides of all provinces for that input type; the interquartile range is
# likewise pooled.
ess_pooled <- dgn[, .(
  lo = quantile(ess_per_sec, 0.05, na.rm = TRUE),
  iqr_lo = quantile(ess_per_sec, 0.25, na.rm = TRUE),
  med = median(ess_per_sec, na.rm = TRUE),
  iqr_hi = quantile(ess_per_sec, 0.75, na.rm = TRUE),
  hi = quantile(ess_per_sec, 0.95, na.rm = TRUE),
  min = min(ess_per_sec, na.rm = TRUE)
), by = type]

ratchets_pooled <- dgn[, .(
  pct_refit = 100 * mean(ratchets > 0),
  max_ratchets = max(ratchets)
), by = type]

# Range and midpoint of the per-province CRPS ratios within each scenario
crps_scenario <- crps_ratio[, .(
  lo = min(geomean),
  med = median(geomean),
  hi = max(geomean),
  n_above_one = sum(geomean > 1)
), by = .(forecast, data)]

sc <- function(fct, dat, col) {
  crps_scenario[forecast == fct & data == dat][[col]]
}
es <- function(tp, col) ess_pooled[type == tp][[col]]
rt <- function(tp, col) ratchets_pooled[type == tp][[col]]

# Provinces in which the weekly model samples faster than the daily model
ess_med_wide <- dcast(ess, province ~ type, value.var = "med")

####################################
# Values quoted in the manuscript
####################################

vals <- list(
  # Relative forecast performance
  crps_wkly_at_daily_lo = fmt_ratio(sc("weekly", "daily", "lo")),
  crps_wkly_at_daily_med = fmt_ratio(sc("weekly", "daily", "med")),
  crps_wkly_at_daily_hi = fmt_ratio(sc("weekly", "daily", "hi")),
  crps_wkly_at_weekly_lo = fmt_ratio(sc("weekly", "weekly", "lo")),
  crps_wkly_at_weekly_med = fmt_ratio(sc("weekly", "weekly", "med")),
  crps_wkly_at_weekly_hi = fmt_ratio(sc("weekly", "weekly", "hi")),
  crps_resc_at_weekly_lo = fmt_ratio(sc("rescale", "weekly", "lo")),
  crps_resc_at_weekly_med = fmt_ratio(sc("rescale", "weekly", "med")),
  crps_resc_at_weekly_hi = fmt_ratio(sc("rescale", "weekly", "hi")),

  # Forecast performance over time
  pct_dates_daily_better = fmt_pct(
    crps_daily_better[data == "daily", 100 * sum(pct_dates * n / 100) / sum(n)]
  ),
  crps_oom_typical = round(crps_oom[province == example_province, oom_typical], 1),

  # Sampling efficiency
  ess_daily_lo = fmt_ess(es("daily", "lo")),
  ess_daily_hi = fmt_ess(es("daily", "hi")),
  ess_daily_iqr_lo = fmt_ess(es("daily", "iqr_lo")),
  ess_daily_iqr_hi = fmt_ess(es("daily", "iqr_hi")),
  ess_weekly_lo = fmt_ess(es("weekly", "lo")),
  ess_weekly_hi = fmt_ess(es("weekly", "hi")),
  ess_weekly_iqr_lo = fmt_ess(es("weekly", "iqr_lo")),
  ess_weekly_iqr_hi = fmt_ess(es("weekly", "iqr_hi")),
  ess_weekly_min = signif(es("weekly", "min"), 1),
  ess_resc_lo = fmt_ess(es("rescale", "lo")),
  ess_resc_hi = fmt_ess(es("rescale", "hi")),
  ess_resc_iqr_lo = fmt_ess(es("rescale", "iqr_lo")),
  ess_resc_iqr_hi = fmt_ess(es("rescale", "iqr_hi")),

  # Adaptive refits
  refit_pct_daily = fmt_pct(rt("daily", "pct_refit")),
  refit_pct_weekly = fmt_pct(rt("weekly", "pct_refit")),
  refit_pct_resc = fmt_pct(rt("rescale", "pct_refit")),
  refit_max_daily = fmt_count(rt("daily", "max_ratchets")),
  refit_max_weekly = fmt_count(rt("weekly", "max_ratchets")),
  refit_max_resc = fmt_count(rt("rescale", "max_ratchets")),

  # Consistency across locations
  n_prov_total = fmt_count(nrow(ess_med_wide)),
  n_prov_weekly_ess_gt_daily = fmt_count(ess_med_wide[, sum(weekly > daily)])
)

####################################
# Claims the prose depends on
####################################
# Each maps to a specific sentence, so a failure names the text to rewrite.

claim <- function(id, statement, holds, detail) {
  data.table(id = id, statement = statement, holds = holds, detail = detail)
}

claims <- rbind(
  claim(
    "C1",
    "Weekly-trained CRPS ratio exceeds 1 in every province, at both evaluation resolutions",
    crps_ratio[forecast == "weekly", all(geomean > 1)],
    sprintf(
      "%i of %i province-resolution pairs above 1",
      crps_ratio[forecast == "weekly", sum(geomean > 1)],
      crps_ratio[forecast == "weekly", .N]
    )
  ),
  claim(
    "C2",
    "Rescaled weekly CRPS ratio is more than ten times the weekly ratio at weekly resolution",
    sc("rescale", "weekly", "lo") > 10 * sc("weekly", "weekly", "hi"),
    sprintf(
      "rescaled minimum %.1f vs weekly maximum %.1f",
      sc("rescale", "weekly", "lo"), sc("weekly", "weekly", "hi")
    )
  ),
  claim(
    "C3",
    "Median refits is zero for every province and input type",
    ratchets[, all(med_ratchets == 0)],
    sprintf("maximum median across groups: %g", ratchets[, max(med_ratchets)])
  ),
  claim(
    "C4",
    "The weekly model refits more often than the daily and rescaled weekly models in every province",
    ratchets[, .(ok = pct_refit[type == "weekly"] > max(pct_refit[type != "weekly"])), by = province][, all(ok)],
    sprintf(
      "holds in %i of %i provinces",
      ratchets[, .(ok = pct_refit[type == "weekly"] > max(pct_refit[type != "weekly"])), by = province][, sum(ok)],
      uniqueN(ratchets$province)
    )
  ),
  claim(
    "C5",
    "The rescaled weekly model has the highest median sampling efficiency in every province",
    ess[, .(ok = med[type == "rescale"] > max(med[type != "rescale"])), by = province][, all(ok)],
    sprintf(
      "holds in %i of %i provinces",
      ess[, .(ok = med[type == "rescale"] > max(med[type != "rescale"])), by = province][, sum(ok)],
      uniqueN(ess$province)
    )
  ),
  claim(
    "C6",
    "The national aggregate is the worst-performing location in all three CRPS scenarios",
    crps_ratio[, .(ok = province[which.max(geomean)] == "RSA"), by = .(forecast, data)][, all(ok)],
    paste(
      crps_ratio[, .(worst = as.character(province[which.max(geomean)])), by = .(forecast, data)][
        , sprintf("%s@%s: %s", forecast, data, worst)
      ],
      collapse = "; "
    )
  ),
  claim(
    "C7",
    "The daily-trained forecast beats the weekly-trained one on most dates, at both evaluation resolutions",
    crps_daily_better[, all(pct_dates > 50)],
    sprintf("minimum share across groups: %.1f%%", crps_daily_better[, min(pct_dates)])
  ),
  claim(
    "C8",
    "The weekly model reaches lower sampling efficiency than either other model at its worst",
    es("weekly", "min") < min(es("daily", "min"), es("rescale", "min")),
    sprintf(
      "minima -- daily %.3g, weekly %.3g, rescaled %.3g",
      es("daily", "min"), es("weekly", "min"), es("rescale", "min")
    )
  ),
  claim(
    "C9",
    sprintf(
      "CRPS in %s spans at least three orders of magnitude between its extremes",
      example_province
    ),
    crps_oom[province == example_province, oom_full] >= 3,
    sprintf(
      "full log10 range: %.2f",
      crps_oom[province == example_province, oom_full]
    )
  )
)

# A broken claim means a sentence in the manuscript has become false, not that
# a number needs nudging. Stop here so that `make` fails rather than the paper
# rendering with prose the output no longer supports.
if (!all(claims$holds)) {
  stop(
    "Claims the manuscript depends on no longer hold:\n",
    paste(
      claims[holds == FALSE, sprintf("  %s: %s (%s)", id, statement, detail)],
      collapse = "\n"
    ),
    call. = FALSE
  )
}

####################################
# Output
####################################

paper_summary <- list(
  meta = list(
    generated = format(Sys.Date()),
    provinces = as.character(pop_order),
    n_provinces = fmt_count(uniqueN(dgn$province)),
    example_province = example_province,
    n_slides = as_plain(dgn[, .(n_slides = .N), by = .(province, type)]),
    ess_missing = fmt_count(dgn[, sum(is.na(ess_per_sec))])
  ),
  crps_ratio = as_plain(crps_ratio),
  ess = as_plain(ess),
  ratchets = as_plain(ratchets),
  crps_daily_better = as_plain(crps_daily_better),
  crps_oom = as_plain(crps_oom),
  vals = lapply(vals, unname),
  claims = as_plain(claims)
)

saveRDS(paper_summary, tail(.args, 1))

message(
  "Wrote ", tail(.args, 1), ": ", length(vals), " values, ",
  nrow(claims), " claims, ", paper_summary$meta$ess_missing,
  " slides without an elapsed time."
)
