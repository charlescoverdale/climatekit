# Figure generator for the climatekit R Journal paper.
# Run: RSTUDIO_PANDOC=/Applications/quarto/bin/tools Rscript paper/make_figures.R

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ggplot2)
  library(showtext)
  library(scales)
})

font_add("HelveticaNeue",
         regular = "/System/Library/Fonts/Helvetica.ttc",
         bold = "/System/Library/Fonts/Helvetica.ttc",
         italic = "/System/Library/Fonts/Helvetica.ttc")
showtext_auto()
showtext_opts(dpi = 300)

fig_dir <- "paper/figures"
tab_dir <- "paper/tables"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)

ok_blue   <- "#0072B2"
ok_orange <- "#E69F00"
ok_green  <- "#009E73"
ok_red    <- "#D55E00"
ok_purple <- "#CC79A7"
ok_sky    <- "#56B4E9"
ok_grey   <- "#999999"

fam <- "HelveticaNeue"

theme_wp <- function(base_size = 10) {
  theme_bw(base_size = base_size, base_family = fam) +
    theme(
      plot.title = element_blank(), plot.subtitle = element_blank(),
      plot.caption = element_blank(), panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      axis.line = element_line(linewidth = 0.35, colour = "grey25"),
      axis.ticks = element_line(linewidth = 0.35, colour = "grey25"),
      axis.ticks.length = unit(2.5, "pt"),
      axis.text = element_text(size = base_size, colour = "grey20"),
      axis.title = element_text(size = base_size, colour = "grey20"),
      legend.position = "bottom", legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1, family = fam),
      legend.key.height = unit(10, "pt"),
      legend.key.width = unit(22, "pt"),
      legend.spacing.x = unit(10, "pt"),
      legend.margin = margin(4, 0, 0, 0),
      plot.margin = margin(6, 10, 6, 6)
    )
}

tex_esc <- function(x) gsub("_", "\\\\_", as.character(x))

# -----------------------------------------------------------------------------
# Synthetic 10-year daily weather panel for a temperate site (London-like).
# -----------------------------------------------------------------------------
set.seed(20260419)
start_date <- as.Date("2015-01-01")
end_date   <- as.Date("2024-12-31")
dates <- seq(start_date, end_date, by = "day")
n <- length(dates)
doy <- as.POSIXlt(dates)$yday + 1

# Temperature: seasonal cycle centred on ~11C with a 10C amplitude.
# Slight warming trend of +0.4C per decade.
year_frac <- (as.numeric(dates) - as.numeric(start_date)) / 365.25
base_cycle <- 11 + 10 * sin(2 * pi * (doy - 80) / 365.25)
warming <- 0.04 * year_frac
noise <- rnorm(n, 0, 2.5)
tavg <- base_cycle + warming + noise
# Diurnal swing 5 to 10C depending on season; cooler days in winter.
swing <- 5 + 3 * cos(2 * pi * (doy - 1) / 365.25) + rnorm(n, 0, 0.6)
tmin <- tavg - swing / 2
tmax <- tavg + swing / 2

# Precipitation: binary wet/dry + lognormal amount when wet.
wet_prob <- 0.38 - 0.1 * sin(2 * pi * (doy - 170) / 365.25)  # wetter in winter
wet <- rbinom(n, 1, wet_prob)
amount <- wet * rlnorm(n, meanlog = 0.6, sdlog = 1.1)
amount[amount > 80] <- 80
precip <- round(amount, 1)

# Relative humidity (for heat index), loosely anti-correlated with temperature.
humidity <- pmin(100, pmax(30, 70 - 0.8 * (tavg - 11) + rnorm(n, 0, 6)))

# -----------------------------------------------------------------------------
# Figure 1: annual temperature indices (frost days, summer days, GDD).
# -----------------------------------------------------------------------------
frost <- ck_frost_days(tmin, dates, period = "annual")
summer <- ck_summer_days(tmax, dates, period = "annual")
gdd  <- ck_growing_degree_days(tavg, dates, base = 10, period = "annual")

yr <- function(d) as.numeric(format(as.Date(d), "%Y"))
df1 <- rbind(
  data.frame(year = yr(frost$period),  value = frost$value,
             series = "Frost days (T_min < 0)"),
  data.frame(year = yr(summer$period), value = summer$value,
             series = "Summer days (T_max >= 25)"),
  data.frame(year = yr(gdd$period),    value = gdd$value / 10,
             series = "Growing degree days / 10 (base 10)")
)
df1$series <- factor(df1$series,
  levels = c("Frost days (T_min < 0)",
             "Summer days (T_max >= 25)",
             "Growing degree days / 10 (base 10)"))

p1 <- ggplot(df1, aes(x = year, y = value,
                       colour = series, linetype = series)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2, alpha = 0.9) +
  scale_colour_manual(values = c(
    "Frost days (T_min < 0)" = ok_blue,
    "Summer days (T_max >= 25)" = ok_red,
    "Growing degree days / 10 (base 10)" = ok_green)) +
  scale_linetype_manual(values = c(
    "Frost days (T_min < 0)" = "solid",
    "Summer days (T_max >= 25)" = "longdash",
    "Growing degree days / 10 (base 10)" = "dotted")) +
  scale_x_continuous(breaks = seq(2015, 2024, 2)) +
  labs(x = NULL, y = "Index value (days, or GDD/10)") +
  guides(colour = guide_legend(nrow = 2,
                               override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(nrow = 2)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig1_temperature.pdf"),
       p1, width = 5.5, height = 3.4, device = cairo_pdf)

cat(sprintf("fig1: mean frost = %.0f, summer = %.0f, GDD = %.0f\n",
            mean(frost$value), mean(summer$value), mean(gdd$value)))

# -----------------------------------------------------------------------------
# Figure 2: monthly precipitation totals with heavy-precip events.
# -----------------------------------------------------------------------------
total <- ck_total_precip(precip, dates, period = "monthly")
heavy <- ck_heavy_precip(precip, dates, period = "monthly", threshold = 10)

# Replace synthetic precipitation with real Central Park precip, 1950-2024.
cp_precip <- read.csv("paper/data/ny_central_park.csv")
cp_precip$date <- as.Date(cp_precip$date)
# Annual totals and heavy-precip day counts (>10 mm).
cp_total <- ck_total_precip(cp_precip$prcp, cp_precip$date, period = "annual")
cp_heavy <- ck_heavy_precip(cp_precip$prcp, cp_precip$date,
                             period = "annual", threshold = 10)

yr2 <- function(d) as.numeric(format(as.Date(d), "%Y"))
df2 <- rbind(
  data.frame(year = yr2(cp_total$period), value = cp_total$value,
             series = "Annual total precipitation (mm)"),
  data.frame(year = yr2(cp_heavy$period), value = cp_heavy$value * 50,
             series = "Heavy precip days (>10 mm), x50 for scale"))
df2$series <- factor(df2$series,
  levels = c("Annual total precipitation (mm)",
             "Heavy precip days (>10 mm), x50 for scale"))

p2 <- ggplot(df2, aes(x = year, y = value,
                       colour = series, linetype = series)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.4,
              linetype = "dotted", show.legend = FALSE) +
  geom_line(linewidth = 0.55, alpha = 0.85) +
  geom_point(size = 1.2, alpha = 0.75) +
  scale_colour_manual(values = c(
    "Annual total precipitation (mm)" = ok_blue,
    "Heavy precip days (>10 mm), x50 for scale" = ok_red)) +
  scale_linetype_manual(values = c(
    "Annual total precipitation (mm)" = "solid",
    "Heavy precip days (>10 mm), x50 for scale" = "longdash")) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  labs(x = NULL, y = "Millimetres (or days x 50)") +
  guides(colour = guide_legend(nrow = 2,
                               override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(nrow = 2)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig2_precip.pdf"),
       p2, width = 5.5, height = 3.2, device = cairo_pdf)

# Trend in heavy-precip days per decade.
heavy_trend <- coef(lm(value ~ yr2(period), data = cp_heavy))[2] * 10
total_trend <- coef(lm(value ~ yr2(period), data = cp_total))[2] * 10
cat(sprintf("fig2 NY: total trend %+.0f mm/decade; heavy trend %+.2f days/decade\n",
            total_trend, heavy_trend))

# -----------------------------------------------------------------------------
# Figure 3: SPI drought index at 3-month and 12-month scales.
# -----------------------------------------------------------------------------
spi3  <- ck_spi(precip, dates, scale = 3)
spi12 <- ck_spi(precip, dates, scale = 12)

df3 <- rbind(
  data.frame(date = as.Date(spi3$period),  value = spi3$value,  series = "SPI-3"),
  data.frame(date = as.Date(spi12$period), value = spi12$value, series = "SPI-12")
)
df3$series <- factor(df3$series, levels = c("SPI-3", "SPI-12"))
df3 <- df3[!is.na(df3$value), ]

p3 <- ggplot(df3, aes(x = date, y = value,
                       colour = series, linetype = series)) +
  geom_hline(yintercept = c(-1.5, -1, 0, 1, 1.5), linewidth = 0.3,
             colour = "grey70", linetype = "dotted") +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("SPI-3" = ok_blue, "SPI-12" = ok_red)) +
  scale_linetype_manual(values = c("SPI-3" = "solid",
                                    "SPI-12" = "longdash")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  labs(x = NULL, y = "Standardised Precipitation Index (SPI)") +
  guides(colour = guide_legend(nrow = 1,
                               override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig3_spi.pdf"),
       p3, width = 5.5, height = 3.0, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 4: Huglin viticultural index across years.
# -----------------------------------------------------------------------------
hg <- ck_huglin(tmin, tmax, dates, lat = 51.5)  # London latitude

df4 <- data.frame(year = yr(hg$period), huglin = hg$value)

# Huglin classes for wine varieties (approximate):
# HI-3: <1500  (too cool), 1500-1800 (I), 1800-2100 (II), 2100-2400 (III), ...
classes <- data.frame(
  ymin = c(0, 1500, 1800, 2100, 2400),
  ymax = c(1500, 1800, 2100, 2400, 3200),
  label = c("Too cool", "Cool (HI-3)", "Temperate", "Warm", "Hot")
)

p4 <- ggplot(df4, aes(x = year, y = huglin)) +
  geom_rect(data = classes,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE,
            fill = c("grey92", "grey88", "grey84", "grey80", "grey76")) +
  geom_line(colour = ok_blue, linewidth = 0.8) +
  geom_point(colour = ok_blue, size = 2.5) +
  scale_x_continuous(breaks = seq(2015, 2024, 2)) +
  scale_y_continuous(limits = c(1200, 2400),
                     breaks = seq(1200, 2400, 300)) +
  labs(x = NULL, y = "Huglin index") +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig4_huglin.pdf"),
       p4, width = 5.5, height = 3.2, device = cairo_pdf)

cat(sprintf("fig4: Huglin range %.0f to %.0f over 10 years\n",
            min(hg$value), max(hg$value)))

# -----------------------------------------------------------------------------
# Figure 5: 12-month SPI for Central Park, 1950 to 2024 (real NOAA data).
# Shows multi-decade drought and pluvial episodes using ck_spi() on the same
# GHCND record used elsewhere in the paper.
# -----------------------------------------------------------------------------
cp_pre <- read.csv("paper/data/ny_central_park.csv")
cp_pre$date <- as.Date(cp_pre$date)

spi12 <- ck_spi(cp_pre$prcp, cp_pre$date, scale = 12)
df5 <- data.frame(date = spi12$period, spi = spi12$value)
df5 <- df5[!is.na(df5$spi), ]
df5$sign <- ifelse(df5$spi >= 0, "Wet (SPI > 0)", "Dry (SPI < 0)")

p5 <- ggplot(df5, aes(x = date)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = c(-1, 1), colour = "grey60",
             linewidth = 0.25, linetype = "dashed") +
  geom_col(aes(y = spi, fill = sign), width = 30, show.legend = TRUE) +
  scale_fill_manual(name = NULL,
                    values = c("Wet (SPI > 0)" = ok_blue,
                               "Dry (SPI < 0)" = ok_red)) +
  annotate("text", x = as.Date("1966-06-01"), y = -2.7,
           label = "1960s NE drought",
           size = 2.8, colour = "grey25", family = fam) +
  annotate("text", x = as.Date("2002-01-01"), y = -2.4,
           label = "2001-02",
           size = 2.8, colour = "grey25", family = fam) +
  annotate("text", x = as.Date("2011-09-01"), y = 2.5,
           label = "2011 wet year",
           size = 2.8, colour = "grey25", family = fam) +
  scale_x_date(breaks = seq(as.Date("1950-01-01"),
                             as.Date("2025-01-01"),
                             by = "10 years"),
               date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-3, 3, 1),
                     limits = c(-3, 3.2),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = "12-month SPI (standard deviations)") +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig5_spi_central_park.pdf"),
       p5, width = 5.5, height = 3.2, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 6: real NOAA Central Park data (1950-2024), three temperature indices.
# -----------------------------------------------------------------------------
cp <- read.csv("paper/data/ny_central_park.csv")
cp$date <- as.Date(cp$date)

cp_frost  <- ck_frost_days(cp$tmin, cp$date, period = "annual")
cp_summer <- ck_summer_days(cp$tmax, cp$date, period = "annual")
cp_gdd    <- ck_growing_degree_days(
  (cp$tmin + cp$tmax) / 2, cp$date, base = 10, period = "annual")

df6 <- rbind(
  data.frame(year = yr(cp_frost$period),  value = cp_frost$value,
             series = "Frost days"),
  data.frame(year = yr(cp_summer$period), value = cp_summer$value,
             series = "Summer days"),
  data.frame(year = yr(cp_gdd$period),    value = cp_gdd$value / 20,
             series = "Growing degree days / 20")
)
df6$series <- factor(df6$series,
  levels = c("Frost days", "Summer days", "Growing degree days / 20"))

p6 <- ggplot(df6, aes(x = year, y = value,
                       colour = series, linetype = series)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.4,
              alpha = 0.4, linetype = "dotted", show.legend = FALSE) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  geom_point(size = 1.1, alpha = 0.7) +
  scale_colour_manual(values = c(
    "Frost days" = ok_blue,
    "Summer days" = ok_red,
    "Growing degree days / 20" = ok_green)) +
  scale_linetype_manual(values = c(
    "Frost days" = "solid",
    "Summer days" = "longdash",
    "Growing degree days / 20" = "dotdash")) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), expand = c(0.01, 0)) +
  labs(x = NULL, y = "Index value (days, or GDD/20)") +
  guides(colour = guide_legend(nrow = 1,
                               override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig6_central_park.pdf"),
       p6, width = 5.5, height = 3.2, device = cairo_pdf)

# Linear trend: per decade change.
trend <- function(x) {
  df <- data.frame(year = yr(x$period), value = x$value)
  coef(lm(value ~ year, data = df))[["year"]] * 10
}
cat(sprintf("fig6: frost trend = %.1f days/decade; summer = %.1f; GDD = %.0f\n",
            trend(cp_frost), trend(cp_summer), trend(cp_gdd)))

# -----------------------------------------------------------------------------
# Table: index family count and representative function names.
# -----------------------------------------------------------------------------
fam_rows <- list(
  c("Temperature", "10", "ck_frost_days, ck_summer_days, ck_growing_degree_days, ck_heating_degree_days, ck_cooling_degree_days, ck_tropical_nights, ck_ice_days, ck_diurnal_range, ck_growing_season, ck_warm_spell"),
  c("Precipitation", "8", "ck_total_precip, ck_dry_days, ck_wet_days, ck_heavy_precip, ck_very_heavy_precip, ck_max_1day_precip, ck_max_5day_precip, ck_precip_intensity"),
  c("Drought", "3", "ck_spi, ck_spei, ck_pet"),
  c("Agroclimatic", "5", "ck_huglin, ck_winkler, ck_branas, ck_first_frost, ck_last_frost"),
  c("Comfort", "4", "ck_wind_chill, ck_heat_index, ck_humidex, ck_fire_danger")
)

tab_lines <- c(
  "\\begin{tabular}{lrl}",
  "\\toprule",
  "Index family & Count & Representative functions \\\\",
  "\\midrule"
)
for (r in fam_rows) {
  # Truncate the representative-function string so the column doesn't overrun.
  reps <- r[[3]]
  if (nchar(reps) > 48) reps <- paste0(substr(reps, 1, 43), "\\ldots")
  tab_lines <- c(tab_lines,
    sprintf("%s & %s & \\texttt{%s} \\\\",
            r[[1]], r[[2]], tex_esc(reps)))
}
tab_lines <- c(tab_lines, "\\midrule", "Total & 30 & \\\\",
               "\\bottomrule", "\\end{tabular}")
writeLines(tab_lines, file.path(tab_dir, "families.tex"))

cat("\n--- done ---\n")
