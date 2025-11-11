library(data.table)
library(ggplot2)
library(ggh4x)
library(geomtextpath)
library(patchwork)

# data = observation resolution
# forecast = training data resolution

.args <- if (interactive()) {
    .prov <- "GP"
    sprintf(c(
        file.path("local", "output", "score_%s.rds"), # scores
        file.path("local", "figures", "score_scatter_%s.png") # diagnostics
    ), .prov)
} else commandArgs(trailingOnly = TRUE)

# Scores
scores <- readRDS(.args[1])
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

score_plt <- ggplot(data = scores) +
	geom_line(
	  aes(x = date, y = crps, color = forecast, group = interaction(slide, forecast, data))
	) +
	geom_blank(
		aes(x = date, y = 10^4), data = \(dt) dt[, {
			rd <- round(range(date), "month")
			rd[2] <- round(rd[2] + 32, "month") - 1
			.(date = rd)
		}, by = .(data)]
	) +
	geom_text(
		aes(x = date, y = pos, label = forecast, color = forecast),
		data = \(dt) dt[,{
			d <- max(date) + 1
			.(date = d, pos = crps[.N])
		}, by = .(data, forecast)],
		hjust = 0
	) + geom_blank(
	    aes(x = date + 30, y = pos),
	    data = \(dt) dt[,{
	        d <- max(date) + 1
	        .(date = d, pos = crps[.N])
	    }, by = .(data, forecast)],
	) +
  facet_nested("CRPS, by Forecasting Target,\ngrouped by forecast horizons" + data ~ ., switch = "y") +
  scale_x_date(
  	NULL, date_breaks = "month",
  	labels = \(b) sprintf("%s%s", monthlabs[month(b)], yearextract(b)),
  	minor_breaks = NULL, expand = c(0, 0)
  ) +
  scale_y_log10(
  #    labels = \(b) parse(text = sprintf("10^%i", as.integer(log10(b))))
  ) +
  scale_color_brewer(guide = "none", na.translate = FALSE, palette = "Dark2") +
  theme_minimal() +
  theme(
  	axis.text.x = element_text(hjust = 0, vjust = 2),
  	strip.placement = "outside"
  ) +
  labs(y = NULL, linetype = "Data", color = "Forecast Training\nData Resolution")

scores_ref <- scores[forecast == "daily"][, .SD, .SDcols = -c("forecast")]

scores_rel <- scores[forecast != "daily"][scores_ref, on = .(slide, date, data), nomatch = 0]
scores_rel[, pos := 1:.N, by = .(data, forecast)]

rel_plot <- ggplot(data = scores_rel[slide_counts, on = .(data)]) +
    aes(
    	x = as.integer(interaction(forecast, data)) - 0.25 + (pos/tot)/2,
    	y = crps/i.crps
    ) +
    theme_minimal() +
    geom_point() +
    geom_labelsegment(
        aes(
            x = as.integer(interaction(forecast, data))-0.3,
            xend = as.integer(interaction(forecast, data))+0.5,
            y = geomean, yend = geomean, label = sprintf("~%.0fx",geomean)
        ),
        data = \(dt) dt[,.(geomean = exp(mean(log(crps/i.crps)))), by=.(forecast, data)],
        color = "red", hjust = 1.1, fill = alpha("white", 0.9)
    ) +
    geom_hline(
        mapping = aes(yintercept = yint),
        data = \(dt) dt[1, .(yint = 1)],
        linetype = "dashed"
    ) +
    geom_text(aes(x = 1.25, y = ratio, label = perf), \(dt) dt[, .(ratio = c(10, 1/10), perf = c("worse", "better"))], vjust = 0.5, hjust = 0) +
    coord_cartesian(ylim = 10^c(-2.5, 4), xlim = c(1, 4.75), expand = FALSE) +
    scale_x_continuous(NULL, breaks = 2:4, labels = c(
        "vs. Weekly Training, Daily Test",
        "vs. Weekly Scale, Weekly Test",
        "vs. Weekly Training, Weekly Test"
    )) +
    #scale_x_discrete(
    #    NULL,
        #label = \(b) lapply(
        #    strsplit(b,".",fixed = TRUE),
        #    \(spl) sprintf("%s data => %s target", spl[1], spl[2])
        #)
    #) +
    scale_y_continuous(
        "relative CRPS against training on Daily data",
        transform = "log10",
        breaks = 10^c(-2:4), minor_breaks = NULL,
        labels = \(b) fifelse(b < 1, sprintf("1/%ix", as.integer(1/b)), sprintf("%ix", as.integer(b)))
    )


ggsave(tail(.args, 1), rel_plot, bg = "white", width = 12, height = 6)
