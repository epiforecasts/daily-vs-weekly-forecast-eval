
# DATA

## NICD data

# province_data <- read_csv(here("data/province_data.csv"))


# setDT(province_data)
options(mc.cores = 4)

province_data_filtered <- province_data %>%
  filter(province != "Unknown") %>%
  select(date, province, incidence, cumulative_cases)


incubation_period <- LogNormal(mean = 5, sd = 1, max = 14)
# generation_time <- LogNormal(mean = 5.2, sd = 1.72, max = 10) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7201952/
generation_time <- Gamma(mean = 7.12, sd = 1.72, max=10) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/
reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10)

delay <- incubation_period + reporting_delay
rt_prior <- list(mean = 3, sd = 0.1)

reported_province_cases <- function(province_name, start_day=1, end_day){
  cases <- province_data_filtered %>%
    filter(province == province_name) %>%
    slice(start_day:end_day) %>%
    select(date, incidence) %>%
    rename(confirm = incidence)
  return(cases)
}

reported_eastern_cape_cases <- reported_province_cases("Northern Cape", end_day = 60)

def <- epinow(reported_eastern_cape_cases,
  generation_time = generation_time_opts(generation_time),
  delays = delay_opts(delay),
  rt = rt_opts(prior = rt_prior),
  horizon = 14,
)

actual_eastern_cape_cases <- reported_province_cases("Northern Cape", end_day = 60+14)

# plot(def)

data <- summary(def, output = "estimated_reported_cases")
# data <- summary(def, type = "parameters", params = "infections")
data[, date := as.Date(date)]
actual_eastern_cape_cases[, date := as.Date(date)]


# Add actual cases to forecast data
data[actual_eastern_cape_cases, on = "date", actual_cases := i.confirm]
ggplot(data, aes(x = date)) +
  # Shaded area for 90% confidence interval
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90, fill = "90% CI"), alpha = 0.2) +
  # Shaded area for 50% confidence interval
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 0.4) +
  # Shaded area for 20% confidence interval
  geom_ribbon(aes(ymin = lower_20, ymax = upper_20, fill = "20% CI"), alpha = 0.6) +
  # Line for the median
  geom_line(aes(y = median), color = "blue") +
  geom_line(aes(y = actual_cases), linetype = "dashed") +
  geom_vline(xintercept = min(data$date) + 60, linetype = "dashed") +
  geom_point(aes(y = actual_cases)) +
  labs(title = "New Reports per Day",
       x = "Date",
       y = "New reports per day",
       fill = "Confidence Interval") +
  scale_fill_manual(values = c("90% CI" = "purple", "50% CI" = "orange", "20% CI" = "green")) +
  theme_minimal() +
  # scale_y_log10() +
  theme(legend.position = "bottom")

# Scoring Using CRPS
forecast_data <- select(def[["estimates"]][["samples"]], c("date", "sample", "value"))

forecast_data[actual_eastern_cape_cases, on = "date", actual_cases := i.confirm]
forecast_data[, model := "daily"]

forecast_data <- forecast_data[sample > 250] %>%
  rename(prediction = value, true_value = actual_cases)

forecaster <- forecast_data %>%
             group_by(date, sample) %>%
             summarize(prediction = mean(prediction, na.rm = TRUE), .groups = 'drop') %>%
          
setDT(forecaster)
forecaster[actual_eastern_cape_cases, on = "date", true_value := i.confirm]
forecaster[, model := "daily"] 

daily_forecaster <- forecaster %>% filter(true_value != " Invalid Number") %>% select(-rn)

daily_scored <- score(daily_forecaster)
score_summary <- summarise_scores(daily_scored)

# Scoring Using Quantiles
forecast_long <- melt(data, id.vars = c("date", "actual_cases"), 
                    measure.vars = c("median", "lower_20", "lower_50", "lower_90", "upper_20", "upper_50", "upper_90"), 
                    variable.name = "type", value.name = "value")

forecast_long[, quantile := fifelse(type == "median", 0.5,
                            fifelse(type == "lower_90", 0.05,
                            fifelse(type == "lower_50", 0.25,
                            fifelse(type == "lower_20", 0.4,
                            fifelse(type == "upper_20", 0.6,
                            fifelse(type == "upper_50", 0.75,
                            fifelse(type == "upper_90", 0.95, NA_real_)))))))]
forecast_long[, prediction := value]
forecast_long[, true_value := actual_cases]
forecast_long[, model := "daily"]

# Filter out rows with NA quantile values if any
forecast_long <- forecast_long[!is.na(quantile)]

# Evaluate the forecasts
daily_scored <- select(forecast_long, c("date", "quantile", "prediction", "true_value", "model"))
daily_scores <- score(daily_scored, metrics = NULL)

# Summarize the scores
score_summary <- summarise_scores(daily_scores)

# Print the score summary
print(score_summary)
