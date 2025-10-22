# This script analyzes poverty spikes by demographic group during four downturns:
# - 1980s Reagan Recession (window: 1980–1983; baseline: 1977–1979)
# - 2001 Dot-com Crash (window: 2001–2003; baseline: 1997–2000)
# - 2008–2010 Great Recession (window: 2008–2010; baseline: 2005–2007)
# - 2020 COVID-19 (window: 2020; baseline: 2017–2019)
#
# Data: timeseries/data/cps_asec_poverty_1959_2024.csv
# Uses PCTPOV (poverty rate, percent) by RACE_NAME and year.
#
# Outputs (created in timeseries/outputs/):
# - option3_spikes.csv: table of baseline, peak, and spike per group and recession
# - option3_spikes_top3.csv: top 3 hardest-hit groups per recession
# - option3_spike_bars.png: bar chart of absolute spike by group (faceted by recession)
# - option3_timeseries.png: time-series of poverty rates with shaded recession windows
# - ts_* artifacts: ACF/PACF, residual diagnostics, fit+forecast, model summary
#
# How to run (from repository root):
#   Rscript timeseries/analysis/timeseries.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(forcats)
  library(stringr)
  library(purrr)
  library(tibble)
  library(rlang)
  library(forecast)  # for BoxCox, auto.arima, etc.
})

# For R CMD check / linters (dplyr NSE)
utils::globalVariables(c(
  "year", "group", "pov_rate", "baseline_rate", "peak_rate", "spike_abs", "spike_rel",
  "peak_year", "window_start", "window_end", "baseline_start", "baseline_end", "recession",
  "xmin", "xmax", "ymin", "ymax"
))

# Ensure folders exist
invisible(dir.create("timeseries/outputs", recursive = TRUE, showWarnings = FALSE))

# Load data
csv_path <- file.path("timeseries", "data", "cps_asec_poverty_1959_2024.csv")
stopifnot(file.exists(csv_path))
raw <- readr::read_csv(csv_path, show_col_types = FALSE)

# Harmonize demographic group labels across years
normalize_group <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    stringr::str_detect(x, regex("All Races", ignore_case = TRUE)) ~ "All",
    stringr::str_detect(x, regex("Hispanic", ignore_case = TRUE)) ~ "Hispanic",
    stringr::str_detect(x, regex("White Alone, Not Hispanic", ignore_case = TRUE)) ~ "White (NH)",
    stringr::str_detect(x, regex("^White$|White Alone", ignore_case = TRUE)) ~ "White",
    stringr::str_detect(x, regex("^Black|Black Alone", ignore_case = TRUE)) ~ "Black",
    stringr::str_detect(x, regex("Asian and Pacific Islander|Asian Alone", ignore_case = TRUE)) ~ "Asian",
    stringr::str_detect(x, regex("American Indian and Alaska Native", ignore_case = TRUE)) ~ "AIAN",
    stringr::str_detect(x, regex("Two or More Races", ignore_case = TRUE)) ~ "Two+",
    TRUE ~ x
  )
}

# Clean and aggregate (handle duplicates within year/group by averaging)
clean <- raw %>%
  dplyr::transmute(
    year = as.integer(time),
    group = normalize_group(RACE_NAME),
    pov_rate = as.numeric(PCTPOV)
  ) %>%
  dplyr::filter(!is.na(.data$year), !is.na(.data$group), !is.na(.data$pov_rate)) %>%
  dplyr::group_by(.data$year, .data$group) %>%
  dplyr::summarise(pov_rate = mean(.data$pov_rate, na.rm = TRUE), .groups = "drop")

# Define recessions and baselines
recessions <- tibble::tribble(
  ~recession,                      ~baseline_start, ~baseline_end, ~window_start, ~window_end,
  "1980s Reagan Recession",                1977L,          1979L,         1980L,       1983L,
  "2001 Dot-com Crash",                    1997L,          2000L,         2001L,       2003L,
  "2008–2010 Great Recession",             2005L,          2007L,         2008L,       2010L,
  "2020 COVID-19",                          2017L,          2019L,         2020L,       2020L
)

compute_spikes <- function(df, rec_row) {
  stopifnot(nrow(rec_row) == 1)
  baseline <- df %>% dplyr::filter(.data$year >= rec_row$baseline_start, .data$year <= rec_row$baseline_end)
  window   <- df %>% dplyr::filter(.data$year >= rec_row$window_start,   .data$year <= rec_row$window_end)

  base_by_group <- baseline %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(baseline_rate = mean(.data$pov_rate, na.rm = TRUE), .groups = "drop")

  peak_by_group <- window %>%
    dplyr::group_by(.data$group) %>%
    dplyr::slice_max(order_by = .data$pov_rate, n = 1, with_ties = FALSE) %>%
    dplyr::transmute(group = .data$group, peak_rate = .data$pov_rate, peak_year = .data$year)

  base_by_group %>%
    dplyr::inner_join(peak_by_group, by = "group") %>%
    dplyr::mutate(
      spike_abs = .data$peak_rate - .data$baseline_rate,
      spike_rel = ifelse(is.finite(.data$baseline_rate) & .data$baseline_rate > 0, .data$spike_abs / .data$baseline_rate, NA_real_),
      recession = rec_row$recession,
      window_start = rec_row$window_start,
      window_end   = rec_row$window_end,
      baseline_start = rec_row$baseline_start,
      baseline_end   = rec_row$baseline_end
    ) %>%
    dplyr::relocate(.data$recession)
}

# Run for all recessions
results <- purrr::map_dfr(seq_len(nrow(recessions)), function(i) compute_spikes(clean, recessions[i, ]))

# Save full results
readr::write_csv(results %>% dplyr::arrange(recession, dplyr::desc(spike_abs)), file.path("timeseries", "outputs", "option3_spikes.csv"))

# Top 3 hardest-hit per recession (by absolute spike)
top3 <- results %>%
  dplyr::group_by(recession) %>%
  dplyr::slice_max(order_by = spike_abs, n = 3, with_ties = FALSE) %>%
  dplyr::arrange(recession, dplyr::desc(spike_abs)) %>%
  dplyr::ungroup()
readr::write_csv(top3, file.path("timeseries", "outputs", "option3_spikes_top3.csv"))

# Print concise summary
cat("\nTop 3 hardest-hit groups by recession (absolute spike in pct points):\n\n")
print(top3 %>% dplyr::select(recession, group, baseline_rate, peak_rate, spike_abs, peak_year))

# Visualization: bar charts of spikes by group
p_bars <- results %>%
  dplyr::mutate(group = forcats::fct_reorder(group, spike_abs)) %>%
  ggplot2::ggplot(aes(x = group, y = spike_abs, fill = group)) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ recession, scales = "free_y") +
  ggplot2::labs(
    title = "Poverty spike vs pre-recession baseline by demographic group",
    subtitle = "Spike = (peak during window) – (baseline mean)",
    x = NULL, y = "Spike (percentage points)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(filename = file.path("timeseries", "outputs", "option3_spike_bars.png"), plot = p_bars, width = 11, height = 7, dpi = 150)

# Visualization: long-run time series with shaded recession windows
major_groups <- c("All", "White (NH)", "White", "Black", "Hispanic", "Asian", "AIAN", "Two+")
ts_df <- clean %>% dplyr::filter(group %in% major_groups)

# Prepare rectangles for shading
rects <- recessions %>%
  dplyr::transmute(recession, xmin = window_start, xmax = window_end, ymin = -Inf, ymax = Inf)

p_ts <- ggplot2::ggplot(ts_df, ggplot2::aes(x = year, y = pov_rate, color = group)) +
  ggplot2::geom_rect(data = rects, inherit.aes = FALSE,
                     ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                     fill = "grey85", alpha = 0.4, color = NA) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggplot2::labs(
    title = "US Poverty Rate by Demographic Group (CPS ASEC)",
    subtitle = "Shaded areas mark recession windows used in analysis",
    x = NULL, y = "Poverty Rate (%)", color = "Group"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(filename = file.path("timeseries", "outputs", "option3_timeseries.png"), plot = p_ts, width = 12, height = 6.5, dpi = 150)

# Optional: quick answer to the research question (by absolute spike)
cat("\nWho was hit hardest (by absolute spike)?\n\n")
quick <- top3 %>% dplyr::group_by(recession) %>% dplyr::slice_max(order_by = spike_abs, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
print(quick %>% dplyr::select(recession, group, spike_abs, peak_year))

# ------------------------------------------------------------------------------
# Section: TS modeling like 01/02 (ACF/PACF, ARIMA, Forecasts)
# ------------------------------------------------------------------------------
# Build annual ts per group, inspect ACF/PACF pre/post differencing, fit ARIMA, forecast

# Choose groups to model (adjust as desired)
groups_to_model <- intersect(
  c("All", "White (NH)", "White", "Black", "Hispanic", "Asian", "AIAN", "Two+"),
  unique(clean$group)
)

# Helper to save ACF/PACF plots
save_acf_pacf <- function(x, file, max_lag = 40, main_suffix = "") {
  grDevices::png(filename = file, width = 1100, height = 500, res = 120)
  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar), add = TRUE)
  par(mfrow = c(1,2))
  acf(x, lag.max = max_lag, ylim = c(-1,1), main = paste("ACF", main_suffix))
  pacf(x, lag.max = max_lag, ylim = c(-1,1), main = paste("PACF", main_suffix))
  grDevices::dev.off()
}

# Fit ARIMA per group and save outputs
model_summaries <- purrr::map_dfr(groups_to_model, function(g) {
  df_g <- clean %>% dplyr::filter(group == g) %>% dplyr::arrange(year)
  y    <- df_g$pov_rate
  yrs  <- df_g$year

  ts_y <- stats::ts(y, start = min(yrs, na.rm = TRUE), frequency = 1)

  lambda <- tryCatch(forecast::BoxCox.lambda(ts_y), error = function(e) 1)
  ts_bc  <- if (is.finite(lambda)) forecast::BoxCox(ts_y, lambda) else ts_y

  d <- forecast::ndiffs(ts_bc)

  out_prefix <- file.path("timeseries", "outputs", paste0("ts_", gsub("[^A-Za-z0-9]+", "_", g)))
  save_acf_pacf(ts_bc,   paste0(out_prefix, "_acf_pacf_raw.png"),   main_suffix = "(BoxCox)")
  ts_stat <- if (d > 0) diff(ts_bc, differences = d) else ts_bc
  save_acf_pacf(ts_stat, paste0(out_prefix, "_acf_pacf_diff.png"),  main_suffix = paste0("(BoxCox, d=", d, ")"))

  fit <- forecast::auto.arima(
    ts_bc, d = d, seasonal = FALSE, stepwise = FALSE, approximation = FALSE, biasadj = FALSE
  )

  h <- 5
  fc <- forecast::forecast(fit, h = h)

  grDevices::png(filename = paste0(out_prefix, "_fit_forecast.png"), width = 1200, height = 600, res = 130)
  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar), add = TRUE)
  plot(fc, main = paste0("ARIMA fit and forecast: ", g, " (BoxCox λ=", round(lambda, 3), ", d=", d, ")"),
       ylab = "Poverty rate (transformed)", xlab = "Year")
  grid()
  grDevices::dev.off()

  resids <- stats::residuals(fit)
  k      <- length(stats::coef(fit))
  lb_lag <- max(10, 2 * k); lb_lag <- min(lb_lag, max(2, length(resids) - 1))
  lb_test <- tryCatch(stats::Box.test(resids, lag = lb_lag, type = "Ljung-Box", fitdf = k), error = function(e) NULL)

  save_acf_pacf(resids, paste0(out_prefix, "_residuals_acf_pacf.png"), main_suffix = "(residuals)")

  # Build ARIMA(p,d,q) string robustly
  p <- fit$arma[1]; q <- fit$arma[2]
  tibble::tibble(
    group = g,
    start_year = start(ts_y)[1],
    end_year = end(ts_y)[1],
    n_obs = length(ts_y),
    lambda = as.numeric(lambda),
    d = d,
    arima = paste0("ARIMA(", p, ",", d, ",", q, ")"),
    aicc = fit$aicc,
    bic = fit$bic,
    sigma2 = fit$sigma2,
    lb_stat = if (!is.null(lb_test)) as.numeric(lb_test$statistic) else NA_real_,
    lb_pval = if (!is.null(lb_test)) as.numeric(lb_test$p.value)   else NA_real_,
    forecast_h = h
  )
})

readr::write_csv(model_summaries, file.path("timeseries", "outputs", "ts_model_summaries.csv"))

cat("\nTime-series modeling completed (ARIMA per group). Outputs written to timeseries/outputs/:\n",
    "- option3_*.csv/png\n",
    "- ts_<group>_acf_pacf_raw.png (BoxCox)\n",
    "- ts_<group>_acf_pacf_diff.png (differenced)\n",
    "- ts_<group>_residuals_acf_pacf.png\n",
    "- ts_<group>_fit_forecast.png\n",
    "- ts_model_summaries.csv\n", sep = "")