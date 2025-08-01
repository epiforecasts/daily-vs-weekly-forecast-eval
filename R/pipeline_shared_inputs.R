
library(EpiNow2) # Needed to setup EpiNow2 modules

####################################
# Functions
####################################
#' Extract columns
#'
#' @param dt The results data.table
#' @param fct The forecast scale. One of c("daily", "weekly", "rescale").
#' @param dat The reference data scale. One of c("daily", "weekly")
#'
#' @returns
#' @export
#'
#' @examples
dtextract <- function(dt, fct, dat) {
    dt[,
       .(date, crps, forecast = fct, data = dat),
       by = slide
    ]
}

#' Join forecasts to reference data and score forecasts
#'
#' @param fore_dt Forecasts dt
#' @param ref_dt Data dt
#'
#' @returns
#' @export
#'
#' @examples
join_and_score <- function(fore_dt, ref_dt) {
    fore_dt[
        ref_dt, on = .(date), .(sample, slide, date, prediction, true_value),
        nomatch = 0
    ][,
      csum := cumsum(prediction), by = .(sample, slide)
    ][!is.na(true_value),
      .(date, prediction = c(csum[1], diff(csum)), true_value),
      by = .(sample, slide)
    ] |>
        as_forecast_sample(
            predicted = "prediction",
            observed = "true_value",
            sample_id = "sample"
        ) |>
        score(metrics = list("crps" = crps_sample))
}
#' Trim leading zeros
#'
#' @param init_dt the raw dt
#'
#' @returns
#' @export
#'
#' @examples
trim_leading_zero <- function (init_dt) {

	first_non_zero <- init_dt[, which.max(confirm != 0)]
	if (first_non_zero == 1) {
		return(init_dt)
	} else {
		# while the first non-NA value is a zero, drop that and all leading values
		while(init_dt[!is.na(confirm)][1, confirm == 0]) {
			init_dt <- init_dt[-(1:which.max(confirm == 0))]
		}

		return(init_dt)
	}
}

#' @title Get rstan diagnostics
#' @description
#' Summarise the diagnostic information contained in a `<stanfit>` object. If
#' the object is not a stanfit object, return a data.table with NA values.
#' This function is adapted from the `{epidist}` R package in
#' https://github.com/epinowcast/epidist/pull/175/files
#'
#' @param fit A stanfit object
#'
#' @return A data.table containing the summarised diagnostics
get_rstan_diagnostics <- function(fit) {
	if (inherits(fit, "stanfit") | inherits(fit, "CmdStanFit")) {
		np <- bayesplot::nuts_params(fit)
		divergence_data <- subset(np, Parameter == "divergent__")
		treedepth_data <- subset(np, Parameter == "treedepth__")
		posterior_summary <- posterior::summarize_draws(
		    fit, c(posterior::default_convergence_measures(), "ess_basic")
		) |> subset(variable != "lp__")

		# Calculate the different types of ess (basic, bulk, and tail)
		fit_ess_basic <- min(posterior_summary$ess_basic, na.rm = TRUE)
		fit_ess_bulk <- min(posterior_summary$ess_bulk, na.rm = TRUE)
		fit_ess_tail <- min(posterior_summary$ess_tail, na.rm = TRUE)

		diagnostics <- data.table(
			"samples" = nrow(np) / length(unique(np$Parameter)),
			"max_rhat" = round(max(posterior_summary$rhat, na.rm = TRUE), 3),
			"divergent_transitions" = sum(divergence_data$Value),
			"per_divergent_transitions" = mean(divergence_data$Value),
			"max_treedepth" = max(treedepth_data$Value),
			"ess_basic" = fit_ess_basic,
			"ess_bulk" = fit_ess_bulk,
			"ess_tail" = fit_ess_tail
		)
		diagnostics[, no_at_max_treedepth :=
									sum(treedepth_data$Value == max_treedepth)
		][, per_at_max_treedepth := no_at_max_treedepth / samples]
	} else {
		diagnostics <- data.table(
			"samples" = NA,
			"max_rhat" = NA,
			"divergent_transitions" = NA,
			"per_divergent_transitions" = NA,
			"max_treedepth" = NA,
			"no_at_max_treedepth" = NA,
			"per_at_max_treedepth" = NA,
			"ess_basic" = NA,
			"ess_bulk" = NA,
			"ess_tail" = NA
		)
	}
	return(diagnostics[])
}

elapsed_time <- function(fit) {
    if (inherits(fit, "stanfit")) {
        rstan::get_elapsed_time(fit) |> apply(1, sum) |> max()
    } else if (inherits(fit, "CmdStanFit")) {
        fit$time()$chains[, c("warmup", "sampling")] |> apply(1, sum) |> max()
    } else stop("error in get elapsed_time")
}

#' Define new parameter values for tuning the model
#'
#' @param stan_cfg Current stan parameter values
#'
#' @returns New stan parameter values
#' @export
#'
#' @examples
ratchet_control <- function(stan_cfg) if (stan_cfg$backend == "rstan") within(stan_cfg, {
    control <- within(control, {
        # "Increasing adapt_delta beyond 0.99 and max_treedepth beyond 12 is seldom useful." (Source: https://mc-stan.org/learn-stan/diagnostics-warnings.html#bulk-and-tail-ess)
        adapt_delta <- min(0.990, adapt_delta + (1 - adapt_delta) * 0.25)
    })
}) else if (stan_cfg$backend == "cmdstanr") within(stan_cfg, {
    adapt_delta <- min(0.990, adapt_delta + (1 - adapt_delta) * 0.25)
}) else stop("backend error")

#' Load forecasts, diagnostics, or timings, bind by row, and add the type id
#' as a column
#'
#' @param files Vector of file paths
#' @param out_type String; the output type. One of c("forecasts", "timing",
#' "diagnostics")
#'
#' @returns A single dt with all results bound together with a type column
#' identifying the forecast scale
#' @export
#'
#' @examples
read_bulk_and_rbind <- function(files, out_type) {
    # Extract the target labels
    target_labels <- gsub("^([^_]+)_([^_]+)_([^.]+)\\.rds$", "\\2", files)
    files |>
        setNames(target_labels) |> # Must always make sure the inputs are in that order
        lapply(readRDS) |>
        lapply(\(obj) {
            rbindlist(obj[[out_type]])
        }) |>
        rbindlist(idcol = "type", fill = TRUE)
}

####################################
# Inputs
####################################

# Starting stan controls to be retuned
control_opts <- list(
	adapt_delta = 0.8,
	max_treedepth = 10,
	stepsize = 0.1
)

# EpiNow2 stan options
stan <- stan_opts(
    samples = 5000,
    control = control_opts,
    cores = min(parallel::detectCores() - 1, 4),
    backend = if (require(cmdstanr)) "cmdstanr" else "rstan"
)

# Train and forecast windows for rescaled data
train_window_rescaled <- 10 # 10 weeks
test_window_rescaled <- 2 # 2 weeks

# Train and forecast windows for daily and weekly data
train_window <- 7*10
test_window <- 7*2

# While loop controls
# Rationale for while loop conditions:
# - divergences <= 2: all we have is that the divergences should be low, so we're using a more realistic value based on fitting the data many times and not achieving low enough divergences. See
# https://mc-stan.org/learn-stan/diagnostics-warnings.html#divergent-transitions-after-warmup
# - To prevent the loop from running forever, we also stop refitting after a specified number of ratchets and return/process the last fit.
# - By our computation, we need 11 ratchets to bump up adapt_delta from 0.88 to 0.99 in 0.25 increments of the previous value
#  R code:
#' initial_delta <- 0.8
# target_delta <- 0.99
# adapt_delta <- initial_delta
# steps <- 0
# adapt_delta_vec <- initial_delta
#
# while (adapt_delta < target_delta) {
#     adapt_delta <- min(0.990, adapt_delta + (1 - adapt_delta) * 0.25)
#     steps <- steps + 1
#     adapt_delta_vec <- append(adapt_delta_vec, adapt_delta)
# }
#
# steps
# adapt_delta_vec
# divergent_transitions_limit <- 0.0025 # .25 percent of the samples
# ratchets_limit <- 11

keep_running <- function(
    dgn, rs, dlimit = 5000 * 0.0025, rlimit = 11, rhatlim = 1.01, essmin = 100
) {

    passingmcmc <- c(
        dgn$divergent_transitions < dlimit,
        dgn$rhat < rhatlim,
        dgn$ess_tail > essmin
    ) |> sum()

    (passingmcmc < 2) && (rs < rlimit)
}
