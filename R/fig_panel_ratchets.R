library(data.table)
library(ggplot2)
library(patchwork)

.args <- if (interactive()) {
    .prov <- "WC"
    sprintf(
        c(
            file.path("local", "data", c("daily_%s.rds", "weekly_%s.rds")), # cases
            file.path("local", "output",
                      c("forecast_daily_%s.rds",
                        "forecast_weekly_%s.rds",
                        "forecast_rescale_%s.rds"
                      )
            ), # forecasts (also contains timing)
            file.path("local", "figures", "fig_panel_ratchets_%s.png")
        ), # ratchets
        .prov
    )} else commandArgs(trailingOnly = TRUE)

# Load the raw data
# Cases
daily_cases <- readRDS(.args[1])
weekly_cases <- readRDS(.args[2])

# Function to read forecasts and timings and rbind
read_bulk_and_rbind <- function(files, out_type) {
    # Extract the forecast target labels
    forecast_targets <- gsub("^([^_]+)_([^_]+)_([^.]+)\\.rds$", "\\2", files)
    # Replace "special" with "rescale"; old name -> new name
    forecast_targets <- ifelse(forecast_targets == "special", "rescale", forecast_targets)
    setNames(files, forecast_targets) |> # Must always make sure the inputs are in that order 
        lapply(readRDS) |>
        lapply(\(obj) rbindlist(obj[[out_type]])) |>
        rbindlist(idcol = "type", fill = TRUE)
}

# Forecasts
forecasts_dt <- read_bulk_and_rbind(.args[3:5], "forecast")

# Get the slides and their dates for merging with the other data
slide_dates_dictionary <- forecasts_dt[type == "weekly", .SD[1], by = "slide", .SDcols = c("date")]

# Runtimes
rachets_dt <- read_bulk_and_rbind(.args[3:5], "timing")

# Add dates
rachets_dt <- rachets_dt[
    slide_dates_dictionary,
    on = "slide"
]

#####
#Plots
####

# Cases plot
cases_plt <- ggplot() +
    geom_point(data = daily_cases,
               aes(x = date, y = confirm),
               size = 0.5
    ) +
    geom_segment(
        aes(x = date - 6.5, xend = date + 0.5, y = confirm/7, yend = confirm/7),
        data = weekly_cases[!is.na(confirm)],
        color = "firebrick"
    ) +
    scale_y_log10(
        "Daily Incidence (log10)", sec.axis = sec_axis(
            ~ . * 7, name = "Weekly Incidence (log10)"
        )
    ) +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

cases_plt

# Ratchets
ratchets_plot <- ggplot(data = daily_cases) +
    geom_blank(
        aes(x = date, y = max(confirm))
    ) +
    geom_col(
        data = rachets_dt,
        aes(x = date, y = ratchets, fill = type),
        position = position_dodge2()
    ) +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Date", y = "rachets")

panel_fig <- (cases_plt/ratchets_plot) &
    plot_layout(ncol = 1, guides = "collect", axes = "collect_x") &
    plot_annotation(title = paste(daily_cases$province[1])) &
    theme_minimal() &
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

ggsave(tail(.args, 1), panel_fig, bg = "white", width = 12, height = 6)
