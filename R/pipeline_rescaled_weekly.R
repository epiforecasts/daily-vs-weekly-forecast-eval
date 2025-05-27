library(EpiNow2)
library(data.table)
library(parallel)
library(bayesplot)

options(mc.cores = parallel::detectCores() - 1)

.args <- if (interactive()) {
    .prov <- "GP"
    sprintf(
        c(
            "local/data/weekly_%s.rds",
            "R/pipeline_shared_inputs.R",
            "local/output/forecast_special_%s.rds"),
        .prov
    )} else commandArgs(trailingOnly = TRUE)

# inflate as.Date, because EpiNow2 seems to prefer Date over IDate
dt <- readRDS(.args[1])[, .(date = as.Date(date), confirm)][!is.na(confirm)]

source(.args[2])

# EpiNow wants to work in terms of days, so we're going to pretend
# as if weeks are days
dt[, orig_date := date]

fake_daily_dates <- seq.Date(
  from = dt$orig_date[1],
  by = "day",
  length.out = length(dt$orig_date)
)

dt$date <- fake_daily_dates

# Train and forecast windows
train_window <- 10 # 10 weeks
test_window <- 2 # 2 weeks

slides <- seq(0, dt[, .N - (train_window + test_window)], by = test_window)

# when changing units, the mean, sd, and max scale the same way
incubation_period <- LogNormal(mean = 5 / 7, sd = 1 / 7, max = 14 / 7)
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7201952/
# generation_time <- LogNormal(mean = 5.2, sd = 1.72, max = 10)
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/

# Generation period
generation_time <- Gamma(mean = 7.12 / 7, sd = 1.72 / 7, max = 10 / 7) # mean and sd are more intuitive for rescaling

# Reporting delays
reporting_delay <- LogNormal(mean = 2 / 7, sd = 1 / 7, max = 10 / 7) # mean and sd are more intuitive for rescaling

# Total delays
delay <- incubation_period + reporting_delay

# Rt prior
rt_prior <- LogNormal(meanlog = 0.69, sdlog = 0.05) # mean = 2, sd = 0.1

# Observation model
obs <- obs_opts(
  week_effect = FALSE,
  likelihood = TRUE,
  return_likelihood = FALSE
)

###############################
# Pipeline
###############################
res_dt <- lapply(slides, \(slide) {
    slice <- dt[seq_len(train_window) + slide] |> trim_leading_zero()
    # Slides for fitting are in weeks but we need to rescale back to
    # days for aligning with other scales
    slide_rescaled <- slide * 7
    # Fit model
    if (slice[, .N > test_window * 2]) {
        # diagnostics place holder to guarantee entry into while
        diagnostics <- data.table(
            divergent_transitions = 20,
            ess_bulk = 200,
            rhat = 2
        )
        ratchets <- -1
        next_stan <- stan
        stan_elapsed_time <- 0
        crude_run_time <- 0
        # Sources for while loop conditions:
        # - rhat <= 1.05: https://search.r-project.org/CRAN/refmans/rstan/html/Rhat.html AND https://arxiv.org/abs/1903.08008
        # - ess_bulk >= 400: https://search.r-project.org/CRAN/refmans/rstan/html/Rhat.html AND https://arxiv.org/abs/1903.08008
        # - divergences <= 10: all we have is that the divergences should be low, so we're assuming 10 here for now. See
        # https://mc-stan.org/learn-stan/diagnostics-warnings.html#divergent-transitions-after-warmup
        while(diagnostics$divergent_transitions > 10 &&
              diagnostics$ess_bulk < 400 &&
              diagnostics$rhat > 1.05
        ) {
            ratchets <- ratchets + 1
            # fit the model
            out <- epinow(
                data = slice,
                generation_time = generation_time_opts(generation_time),
                delays = delay_opts(delay),
                rt = rt_opts(prior = rt_prior),
                forecast = forecast_opts(horizon = test_window),
                obs = obs,
                stan = so
            )

            # Extract the diagnostic information
            diagnostics <- get_rstan_diagnostics(out$estimates$fit)
            stan_elapsed_time <- stan_elapsed_time + sum(
                rstan::get_elapsed_time(out$estimates$fit)
            )
            crude_run_time <- crude_run_time + out$timing
            next_stan <- ratchet_control(next_stan)
        }

        # Extract the forecasted cases
        forecasts <- out$estimates$samples[
            variable == "reported_cases" & type == "forecast",
            .(date, sample, value, slide = slide)
        ]
        # Extract the diagnostic information
        diagnostics <- get_rstan_diagnostics(out$estimates$fit)
        diagnostics <- diagnostics[, slide := slide_rescaled]
        # Extract and append stan's internal timing of the model fitting process.
        stan_elapsed_time <- sum(rstan::get_elapsed_time(out$estimates$fit))
        diagnostics <- diagnostics[, "stan_elapsed_time" := stan_elapsed_time] #  NB: NEEDS REVIEW: Currently computes total time taken for warmup and sampling for all chains.
        # Extract the crude timing measured by epinow()
        crude_run_time <- out$timing
        # Combine the forecast, timing and diagnostics
        res_dt <- data.table(
            forecast = list(forecasts),
            timing = list(
                data.table(
                    slide = slide_rescaled,
                    crude_run_time = crude_run_time,
                    stan_elapsed_time = stan_elapsed_time
                )
            ),
            diagnostics = list(diagnostics)
        )
    } else {
        empty_forecast <- data.table(
            date = dt[train_window + slide, date + seq_len(test_window)],
            sample = NA_integer_, value = NA_integer_, slide = slide_rescaled
        )
        res_dt <- data.table(
            forecast = list(empty_forecast),
            timing = list(data.table(
                slide = slide_rescaled,
                crude_run_time = lubridate::as.duration(NA),
                stan_elapsed_time = lubridate::as.duration(NA))
            ),
            diagnostics = list(data.table(
                slide = slide_rescaled,
                "samples" = NA,
                "max_rhat" = NA,
                "divergent_transitions" = NA,
                "per_divergent_transitions" = NA,
                "max_treedepth" = NA,
                "no_at_max_treedepth" = NA,
                "per_at_max_treedepth" = NA,
                "ess_basic" = NA,
                "ess_bulk" = NA,
                "ess_tail" = NA,
                "rhat" = NA
            )
            )
        )
    }
}) |> rbindlist()

# Reach into res_dt and update forecast as follows:
# - replace the fake dates with orig_dates by doing a merge on date
# - Remove "confirm" and "date" which was fake
res_dt[,
       forecast := lapply(forecast, function(x) {
           merge(x, dt)
       })]

res_dt[,
       forecast := lapply(forecast, function(x) {
           x[, date := orig_date
           ][, `:=`(
               orig_date = NULL,
               confirm = NULL
           )]
       })]

# Save output
res_dt |> saveRDS(tail(.args, 1))
