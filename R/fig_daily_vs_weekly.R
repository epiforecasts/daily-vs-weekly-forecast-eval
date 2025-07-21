
library(data.table)
library(ggplot2)

.args <- if (interactive()) {
    .prov <- "GP"
    sprintf(
        c(
            file.path("local", "data", "daily_%s.rds"),
            file.path("local", "data", "weekly_%s.rds"),
            file.path("local", "figures", "daily_vs_weekly_%s.png")
        ), .prov
    )} else commandArgs(trailingOnly = TRUE)

daily_dt <- readRDS(.args[1])
weekly_dt <- readRDS(.args[2])

p <- ggplot() +
    geom_point(
        aes(x = date, y = confirm),
        data = daily_dt, size = 0.5
    ) +
    geom_segment(
        aes(x = date - 6.5, xend = date + 0.5, y = confirm/7, yend = confirm/7),
        data = weekly_dt[!is.na(confirm)],
        color = "firebrick"
    ) +
    scale_y_continuous(
        "Daily Incidence", sec.axis = sec_axis(
            ~ . * 7, name = "Weekly Incidence"
        )
    ) +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    # theme_minimal()
    NULL

ggsave(tail(.args, 1), p, bg = "white", width = 12, height = 6)
