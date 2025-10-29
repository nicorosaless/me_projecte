suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(forecast)
})

# Silence NSE notes for R CMD check / linters
utils::globalVariables(c(
  "group", "PCTPOV", "POV", "POP", "RACE_NAME", "time",
  "ymin", "ymax", "start", "end",
  "baseline", "peak", "peak_year", "n_base", "n_win", "delta_pp"
))

## Paths & Output --------------------------------------------------------------
csv_path <- file.path("timeseries", "data", "cps_asec_poverty_1959_2024.csv")

out_dir <- file.path("timeseries", "output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## Helper: Recession Windows ---------------------------------------------------
recessions <- tibble::tribble(
  ~name,     ~start, ~end,
  "Reagan",   1980L,  1983L,
  "DotCom",   2001L,  2003L,
  "GreatRec", 2008L,  2010L,
  "COVID19",  2020L,  2021L
)

## Load & Harmonize Groups -----------------------------------------------------
dat_raw <- read_csv(csv_path, show_col_types = FALSE)

# Keep only US totals
dat <- dat_raw %>%
  filter(us == 1)

# We want a consistent set of groups across time
# Priority lists handle category changes across years.
group_priority <- tibble::tribble(
  ~group,     ~RACE_NAME,                                  ~priority,
  # All
  "All",      "All Races",                                 1L,
  # White (pref non-Hispanic if available in later years)
  "White",    "White Alone, Not Hispanic",                  1L,
  "White",    "White Alone",                                2L,
  "White",    "White",                                      3L,
  # Black
  "Black",    "Black Alone",                                1L,
  "Black",    "Black Alone or in Combination",              2L,
  "Black",    "Black",                                      3L,
  # Hispanic
  "Hispanic", "Hispanic (of any race)",                    1L,
  # Asian
  "Asian",    "Asian Alone",                                1L,
  "Asian",    "Asian Alone or in Combination",              2L,
  "Asian",    "Asian and Pacific Islander",                 3L
)

# Attach group and priority, pick best match per year and group
harmonized <- dat %>%
  inner_join(group_priority, by = "RACE_NAME") %>%
  arrange(time, group, priority) %>%
  group_by(time, group) %>%
  slice_min(order_by = priority, n = 1, with_ties = FALSE) %>%
  ungroup()

# Keep the essential columns, one row per year x group
harmonized <- harmonized %>%
  select(time, group, PCTPOV, POV, POP, RACE_NAME)

# Wide table for quick comparisons
wide <- harmonized %>%
  select(time, group, PCTPOV) %>%
  distinct() %>%
  tidyr::pivot_wider(names_from = group, values_from = PCTPOV)

# Save a cleaned copy
write_csv(harmonized, file.path(out_dir, "poverty_harmonized.csv"))

## Plot: Time Series with Recession Shading -----------------------------------
plot_ts_with_recessions <- function(df, value_col = "PCTPOV", title = "Poverty rate by group (% of people in poverty)") {
  # Shading data for ggplot
  rects <- recessions %>% mutate(ymin = -Inf, ymax = Inf)
  ggplot(df, aes(x = .data$time, y = .data[[value_col]], color = .data$group)) +
    geom_rect(data = rects, inherit.aes = FALSE,
              aes(xmin = .data$start, xmax = .data$end, ymin = .data$ymin, ymax = .data$ymax),
              fill = "grey85", color = NA, alpha = 0.5) +
    geom_line(linewidth = 0.9) +
    scale_y_continuous("Poverty rate (%)", limits = c(0, NA), labels = label_number(accuracy = 0.1)) +
    scale_x_continuous("Year", breaks = scales::pretty_breaks()) +
    scale_color_brewer(palette = "Set1") +
    ggtitle(title) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
}

p_all <- plot_ts_with_recessions(harmonized, value_col = "PCTPOV")
ggsave(filename = file.path(out_dir, "poverty_ts_by_group.png"), plot = p_all, width = 11, height = 6, dpi = 150)

## Recession Impact: Who was hit hardest? -------------------------------------
compute_recession_impacts <- function(df_grouped, baseline_years = 3) {
  # df_grouped: cols time, group, PCTPOV
  out <- list()
  for (i in seq_len(nrow(recessions))) {
    win <- recessions[i,]
    start <- win$start; end <- win$end
    base_years <- seq.int(start - baseline_years, start - 1L)
    win_years  <- seq.int(start, end)

  base_df <- df_grouped %>% filter(.data$time %in% base_years)
  win_df  <- df_grouped %>% filter(.data$time %in% win_years)

    # compute baseline (mean of previous N years) and window peak
    summ <- base_df %>%
      group_by(.data$group) %>%
      summarise(baseline = mean(.data$PCTPOV, na.rm = TRUE), n_base = sum(!is.na(.data$PCTPOV)), .groups = "drop") %>%
      inner_join(
        win_df %>% group_by(.data$group) %>% summarise(peak = max(.data$PCTPOV, na.rm = TRUE),
                                                 peak_year = .data$time[which.max(.data$PCTPOV)],
                                                 n_win = sum(!is.na(.data$PCTPOV)), .groups = "drop"),
        by = "group"
      ) %>%
      mutate(window = win$name, delta_pp = .data$peak - .data$baseline) %>%
      # drop where baseline or peak is NA or counts too small
      filter(is.finite(.data$baseline), is.finite(.data$peak), .data$n_base >= 2, .data$n_win >= 1) %>%
      arrange(desc(.data$delta_pp))

    out[[win$name]] <- summ
  }

  bind_rows(out)
}

impact <- compute_recession_impacts(harmonized)
write_csv(impact, file.path(out_dir, "recession_impact_summary.csv"))

# Print a console summary
cat("\n===== Recession Impact: Largest increases in poverty rate (pp) =====\n")
impact %>% group_by(window) %>% slice_max(order_by = delta_pp, n = 3, with_ties = FALSE) %>% print(n = 20)

## Time Series Modeling (All Races) — like 04_ts_estimate.R -------------------
# We follow the class pattern:
# - Log transform for variance stabilization
# - Inspect ACF/PACF of differenced log series
# - Fit 2-3 ARIMA candidates with d=1
# - Check coefficient significance (|coef/se| > 2), compare AIC, pick model
# - Validate residuals (homoscedasticity, normality, independence)
# - Predict on log scale, back-transform via exp()

series_all <- harmonized %>% filter(.data$group == "All") %>% arrange(.data$time)
summary(series_all)
table(series_all$time)
start_year <- min(series_all$time)

ts_all <- ts(series_all$PCTPOV, start = start_year, frequency = 1)

length(ts_all)

plot((ts_all))
# Log transform and differencing for exploration
lng_all   <- log(ts_all)
d1lng_all <- diff(lng_all)

# ACF & PACF of differenced logs (as in class scripts)
par(mfrow = c(1,2))

acf(d1lng_all, ylim = c(-1,1), lag.max = 40, main = "ACF (d1 log All Races)")
pacf(d1lng_all, ylim = c(-1,1), lag.max = 40, main = "PACF (d1 log All Races)")
dev.off()

plot(d1lng_all)
# Candidate models (mirror 04_ts_estimate.R pattern)
all.arima1 <- arima(d1lng_all, order = c(3,1,0))
all.arima2 <- arima(d1lng_all, order = c(2,1,0))
# Coefficient significance ratios
rat1 <- round(abs(all.arima1$coef / sqrt(diag(all.arima1$var.coef))), 2)
rat2 <- round(abs(all.arima2$coef / sqrt(diag(all.arima2$var.coef))), 2)

rat1
rat2


# Choose by AIC (record both)
aic1 <- AIC(all.arima1); aic2 <- AIC(all.arima2)
mod_def <- if (aic2 <= aic1) all.arima2 else all.arima1
mod_def

rat3 <- round(abs(mod_def$coef / sqrt(diag(mod_def$var.coef))), 2)
rat3

# Validation — like class scripts (04):

resid <- mod_def$residuals

par(mfrow = c(2,2), mar = c(3,3,3,3))

plot(resid, main = "Residuals")
abline(h = c(0, -3*sd(resid), 3*sd(resid)), lty = c(1,3,3), col = c(1,4,4))

scatter.smooth(sqrt(abs(resid)), main = "Square Root of Absolute residuals", lpars = list(col=2))

qqnorm(resid); qqline(resid, col = 2, lwd = 2)
hist(resid, breaks = 10, freq = FALSE, main = "Residuals histogram")
curve(dnorm(x, mean = mean(resid), sd = sd(resid)), col = 2, add = TRUE)
dev.off()

# Independence diagnostics and consolidated checks
tsdiag(mod_def, gof.lag = 20)
dev.off()
forecast::checkresiduals(mod_def)

# Prediction (n.ahead = 10) and back-transform
pred   <- predict(mod_def, n.ahead = 10)
pr_log <- pred$pred
se_log <- pred$se
li_log <- pr_log - 1.96 * se_log
ls_log <- pr_log + 1.96 * se_log

li <- ts(exp(li_log), start = end(ts_all)[1] + 1, freq = 1)
pr <- ts(exp(pr_log), start = end(ts_all)[1] + 1, freq = 1)
ls <- ts(exp(ls_log), start = end(ts_all)[1] + 1, freq = 1)

png(file.path(out_dir, "all_forecast.png"), width = 1000, height = 500)
par(mfrow = c(1,1))
ts.plot(ts_all, li, ls, pr,
  lty = c(1,2,2,1), col = c("black","blue","blue","red"),
  xlab = "Year", ylab = "Poverty rate (%)",
  main = "All Races: Forecast with 95% intervals")
dev.off()

# Save model comparison summary (coefs, AIC, chosen model)
sink(file.path(out_dir, "all_model_summary.txt"))
cat("All Races ARIMA model comparison (log scale, d=1)\n\n")
cat("Model 1: ARIMA(3,1,0)\n"); print(all.arima1); cat("\nRatios: \n"); print(rat1); cat("\nAIC:", aic1, "\n\n")
cat("Model 2: ARIMA(2,1,0)\n"); print(all.arima2); cat("\nRatios: \n"); print(rat2); cat("\nAIC:", aic2, "\n\n")
cat("Chosen model (by AIC):\n\n"); print(mod_def)
sink()

## Optional: Per-group quick ARIMA fits (not saved), feel free to expand -------
# By default we fully modelled All Races as per class examples. You can repeat
# the same modelling block for White, Black, Hispanic, and Asian if needed.

## Extra outputs to support documentation -------------------------------------
# 1) Serie original vs. log-transformada (All)
png(file.path(out_dir, "all_original_vs_log.png"), width = 1000, height = 450)
par(mfrow = c(1,2))
plot(ts_all, main = "PCTPOV (All)", xlab = "Año", ylab = "Tasa de pobreza (%)")
plot(lng_all, main = "log(PCTPOV) (All)", xlab = "Año", ylab = "log tasa de pobreza")
dev.off()

# 2) checkresiduals plot saved as PNG
png(file.path(out_dir, "all_checkresiduals.png"), width = 900, height = 600)
forecast::checkresiduals(mod_def)
dev.off()

# 3) Matriz (tabla ancha) de impactos por grupo x crisis (delta_pp)
impact_matrix <- impact %>%
  select(.data$group, .data$window, .data$delta_pp) %>%
  tidyr::pivot_wider(names_from = .data$window, values_from = .data$delta_pp)
write_csv(impact_matrix, file.path(out_dir, "impact_matrix.csv"))

# incremento en pp por grupo y crisis
impact$group <- factor(impact$group, levels = sort(unique(impact$group)))
pb <- ggplot(impact, aes(x = .data$group, y = .data$delta_pp, fill = .data$group)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f", .data$delta_pp)), vjust = -0.3, size = 3) +
  facet_wrap(~ .data$window, scales = "free_y") +
  scale_y_continuous("Incremento (pp)", labels = label_number(accuracy = 0.1)) +
  scale_x_discrete("Grupo") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  ggtitle("Incremento de pobreza por crisis (pp sobre baseline)") +
  theme_minimal(base_size = 12)
ggsave(filename = file.path(out_dir, "impact_bars.png"), plot = pb, width = 11, height = 6, dpi = 150)

# matriz de impactos (pp) por grupo x crisis
imp_hm <- impact %>%
  select(.data$group, .data$window, .data$delta_pp) %>%
  mutate(group = factor(.data$group, levels = sort(unique(.data$group))),
         window = factor(.data$window, levels = c("Reagan","DotCom","GreatRec","COVID19")))
phm <- ggplot(imp_hm, aes(x = .data$window, y = .data$group, fill = .data$delta_pp)) +
  geom_tile() +
  geom_text(aes(label = ifelse(is.finite(.data$delta_pp), sprintf("%.1f", .data$delta_pp), "")), color = "black", size = 3) +
  scale_fill_gradient(name = "pp", low = "#f7fbff", high = "#08306b", na.value = "#eeeeee") +
  xlab("Crisis") + ylab("Grupo") +
  ggtitle("Heatmap de incrementos (pp) por crisis y grupo") +
  theme_minimal(base_size = 12)
ggsave(filename = file.path(out_dir, "impact_heatmap.png"), plot = phm, width = 7, height = 5, dpi = 150)