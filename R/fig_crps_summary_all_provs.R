library(data.table)
library(ggplot2)
library(ggh4x)
library(geomtextpath)
library(patchwork)

# data = observation resolution
# forecast = training data resolution

.args <- if (interactive()) {
    c(
        file.path("local", "output"),
        file.path("R", "summary_utils.R"),
        file.path("local", "figures", "fig_crps_summary_all_provs.png")
    )
} else commandArgs(trailingOnly = TRUE)

# Load the shared post-processing helpers
source(.args[length(.args) - 1])

# Scores
scores <- read_scores(.args[1])

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

geomean_dt <- crps_ratio_summary(scores)

rel_plot <- ggplot(data = geomean_dt[slide_counts, on = .(data)]) +
    aes(
        x = as.integer(interaction(forecast, data)) - 0.25 + as.integer(province)/20,
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
    geom_text(
        aes(x = 1.5, y = ratio, label = perf),
        \(dt) dt[, .(ratio = c(10/3, 3/10), perf = c("worse", "better"))],
        vjust = 0.5, hjust = 0,
        inherit.aes = FALSE
    ) +
    coord_cartesian(
        ylim = 10^c(-1, 3), xlim = c(1.5, 4.5), expand = FALSE
    ) +
    scale_x_continuous(NULL, breaks = 2:4, labels = c(
        "vs. Aggregated Weekly Training\n& Daily Test",
        "vs. Weekly Rescale\n& Weekly Test",
        "vs. Aggregated Weekly Training\n& Weekly Test"
    ), expand = expansion(), position = "top") +
    scale_y_continuous(
        "relative CRPS against training on Daily data",
        transform = "log10",
        breaks = 10^c(-2:4), minor_breaks = NULL,
        labels = \(b) fifelse(b < 1, sprintf("1/%ix", as.integer(1/b)), sprintf("%ix", as.integer(b)))
    ) + scale_color_discrete(
        name = NULL, breaks = pop_order
    ) + theme(
        legend.position = "bottom",
        legend.position.inside = c(0.5, 0.2),
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 14)
    )

# box_plot <- ggplot(scores_rel) +
#     aes(
#         x = interaction(forecast, data),
#         y = crps/i.crps,
#         group = interaction(forecast, data, province),
#         color = province
#     ) +
#     theme_minimal() +
#     geom_boxplot(position = "dodge") +
#     geom_hline(
#         mapping = aes(yintercept = yint),
#         data = \(dt) dt[1, .(yint = 1)],
#         linetype = "dashed"
#     ) +
#     geom_text(
#         aes(x = 1.5, y = ratio, label = perf),
#         \(dt) dt[, .(ratio = c(10/3, 3/10), perf = c("worse", "better"))],
#         vjust = 0.5, hjust = 0,
#         inherit.aes = FALSE
#     ) +
#     coord_cartesian(
#         ylim = 10^c(-1, 3), expand = FALSE
#     ) +
#     scale_x_discrete(position = "top") +
#     scale_y_continuous(
#         "relative CRPS against training on Daily data",
#         transform = "log10",
#         breaks = 10^c(-2:4), minor_breaks = NULL,
#         labels = \(b) fifelse(b < 1, sprintf("1/%ix", as.integer(1/b)), sprintf("%ix", as.integer(b)))
#     ) + scale_color_discrete(
#         name = NULL, breaks = pop_order
#     ) + theme(
#         legend.position = "inside", legend.position.inside = c(0.5, 0.2),
#         legend.direction = "horizontal",
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.x = element_text(size = 14)
#     )

if (interactive()) print(rel_plot)

ggsave(tail(.args, 1), rel_plot, bg = "white", width = 12, height = 6)
