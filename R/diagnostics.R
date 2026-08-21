library(EpiNow2) # We need to load this even if not needed in the script so the shared_inputs_script doesn't error
library(data.table)

.args <- if (interactive()) {
    .prov <- "GP"
    .tmp <- sprintf(
        c(file.path("local/output",
                    c("forecast_daily_%s.rds",
                      "forecast_weekly_%s.rds",
                      "forecast_rescale_%s.rds"
                    )
        ),
        file.path("local/output", "diagnostics_%s.csv")
        ),
        .prov
    )
    c(.tmp[1:length(.tmp) - 1],
        file.path("./R/pipeline_shared_inputs.R"),
      .tmp[length(.tmp)]
    )
} else {
    commandArgs(trailingOnly = TRUE)
}

# Load helper functions and shared model inputs
source(.args[length(.args) - 1])

# Extract the files
diagnostics_dt_combined <- read_bulk_and_rbind(.args[1:3], "diagnostics")

# Adaptive refit counts live alongside the run times rather than with the
# diagnostics, but are keyed identically (one row per slide per type) and
# already share stan_elapsed_time, so fold them in here. This makes the
# extract self-sufficient for both the diagnostics panel figure and the
# manuscript summary, neither of which then needs to open the forecast files.
timing_dt_combined <- read_bulk_and_rbind(.args[1:3], "timing")
timing_dt_combined[, crude_run_time_secs := as.numeric(crude_run_time, units = "secs")]

diagnostics_dt_combined <- timing_dt_combined[
  , .(type, slide, ratchets, crude_run_time_secs, last_stan_elapsed_time)
][diagnostics_dt_combined, on = .(type, slide)]

# Slide dates, taken from the weekly forecast and applied to every type, as the
# diagnostics panel figure does: the rescaled forecasts carry pseudo-daily
# dates, so the weekly series is the only one on the real calendar shared by
# all three.
slide_dates <- rbindlist(readRDS(.args[2])$forecast)[
  , .SD[1], by = "slide", .SDcols = "date"
]

diagnostics_dt_combined <- diagnostics_dt_combined[slide_dates, on = "slide"]

setcolorder(
  diagnostics_dt_combined,
  c("type", "slide", "date", "ratchets")
)

# Save as csv
write.csv(diagnostics_dt_combined, tail(.args, 1), row.names = FALSE)
