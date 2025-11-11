library(data.table)
library(ggplot2)
library(ggh4x)
library(geomtextpath)
library(patchwork)

# data = observation resolution
# forecast = training data resolution

.args <- if (interactive()) {
    c(
        file.path("local", "output"), # scores
        file.path("local", "figures", "score_scatter_%s.png") # diagnostics
    )
} else commandArgs(trailingOnly = TRUE)

fls <- list.files(.args[1], "score_.*\\.rds", full.names = TRUE)

# Scores
scores <- fls |> setNames(gsub("^.*_(.*)\\.rds$", "\\1", fls)) |>
    lapply(readRDS) |>
    rbindlist(idcol = "province")

## TODO currently aggregating scores via mean - probably just keep the actual dates?
# scores[data == "daily" & forecast == "daily", date := date - 6 ]
# scores[forecast != "rescale", slide := slide / 14L]
# scores[forecast == "rescale", slide := slide / 14L]

slide_counts <- scores[forecast == "daily", .(tot = .N), by = .(data)]

monthlabs <- strsplit("JFMAMJJASOND", "")[[1]]

yearextract <- function(dates, force = 2, showmonth = 1) {
    yrs <- year(dates) %% 100
    show <- month(dates) == showmonth
    show[force] <- TRUE
    return(ifelse(show, sprintf("\n'%s", yrs), "\n "))
}

scores_ref <- scores[forecast == "daily"][, .SD, .SDcols = -c("forecast")]

scores_rel <- scores[forecast != "daily"][scores_ref, on = .(slide, date, data, province), nomatch = 0]
scores_rel[, pos := 1:.N, by = .(data, forecast)]

geomean_dt <- scores_rel[,.(geomean = exp(mean(log(crps/i.crps)))), by=.(forecast, data, province)]

rel_plot <- ggplot(data = geomean_dt[slide_counts, on = .(data)]) +
    aes(
        x = as.integer(interaction(forecast, data)) - 0.25 + as.integer(factor(province))/20,
        y = geomean,
        color = province
    ) +
    theme_minimal() +
    geom_point() +
    geom_hline(
        mapping = aes(yintercept = yint),
        data = \(dt) dt[1, .(yint = 1)],
        linetype = "dashed"
    ) +
    geom_text(aes(x = 1.25, y = ratio, label = perf), \(dt) dt[, .(ratio = c(10, 1/10), perf = c("worse", "better"))], vjust = 0.5, hjust = 0, inherit.aes = FALSE) +
    coord_cartesian(ylim = 10^c(-2.5, 4), xlim = c(1, 4.75), expand = FALSE) +
    scale_x_continuous(NULL, breaks = 2:4, labels = c(
        "vs. Aggregated Weekly Training, Daily Test",
        "vs. Weekly Scale, Weekly Test",
        "vs. Aggregated Weekly Training, Weekly Test"
    )) +
    scale_y_continuous(
        "relative CRPS against training on Daily data",
        transform = "log10",
        breaks = 10^c(-2:4), minor_breaks = NULL,
        labels = \(b) fifelse(b < 1, sprintf("1/%ix", as.integer(1/b)), sprintf("%ix", as.integer(b)))
    ) + scale_color_discrete(
        name = NULL
    ) + theme(
        legend.position = "inside", legend.position.inside = c(0.5, 0.2),
        legend.direction = "horizontal"
    )


ggsave(tail(.args, 1), rel_plot, bg = "white", width = 12, height = 6)
