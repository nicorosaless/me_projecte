################################################################################
#
# Option 3 — Economic Crisis Impact on Poverty (Time Series)
#
# This script follows the workflow from class scripts 01_ts_intro.R,
# 02_ts_simula.R, 03_ts_estimate.R, and 04_ts_estimate.R to:
#   1) Load and harmonize CPS ASEC poverty time series data by demographic group
#   2) Explore and visualize series with recession shading
#   3) Build and validate a (S)ARIMA model for a selected series (All Races)
#   4) Forecast and back-transform (BoxCox)
#   5) Quantify which groups were hit hardest in each recession window
#
# Data source in repo: timeseries/data/cps_asec_poverty_1959_2024.csv
# Key fields: time (year), PCTPOV (poverty rate, percent), RACE_NAME, us
#
# How to run (from repo root):
#   - Ensure packages installed: forecast, dplyr, tidyr, ggplot2, readr, scales
#   - Set working directory to repo root (getwd() ends with me_projecte)
#   - source("timeseries/timeseries.R")
# Outputs will be written to: timeseries/output/
################################################################################

## Packages -------------------------------------------------------------------
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
csv_path1 <- file.path("timeseries", "data", "cps_asec_poverty_1959_2024.csv")
csv_path2 <- file.path("data", "cps_asec_poverty_1959_2024.csv") # fallback if sourcing from timeseries/
if (file.exists(csv_path1)) {
  csv_path <- csv_path1
} else if (file.exists(csv_path2)) {
  csv_path <- csv_path2
} else {
  stop("Could not locate cps_asec_poverty_1959_2024.csv. Expected under timeseries/data/ or data/.")
}

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

start_year <- min(series_all$time, na.rm = TRUE)
ts_all <- ts(series_all$PCTPOV, start = start_year, frequency = 1)

# Log transform and differencing for exploration
lng_all   <- log(ts_all)
d1lng_all <- diff(lng_all)

# ACF & PACF of differenced logs (as in class scripts)
png(file.path(out_dir, "all_ACF_PACF_d1log.png"), width = 900, height = 400)
par(mfrow = c(1,2))
acf(d1lng_all, ylim = c(-1,1), lag.max = 40, main = "ACF (d1 log All Races)")
pacf(d1lng_all, ylim = c(-1,1), lag.max = 40, main = "PACF (d1 log All Races)")
dev.off()

# Candidate models (mirror 04_ts_estimate.R pattern)
all.arima1 <- arima(lng_all, order = c(3,1,0))
all.arima2 <- arima(lng_all, order = c(2,1,0))

# Coefficient significance ratios
rat1 <- round(abs(all.arima1$coef / sqrt(diag(all.arima1$var.coef))), 2)
rat2 <- round(abs(all.arima2$coef / sqrt(diag(all.arima2$var.coef))), 2)

# Choose by AIC (record both)
aic1 <- AIC(all.arima1); aic2 <- AIC(all.arima2)
mod_def <- if (aic2 <= aic1) all.arima2 else all.arima1

# Validation — like class scripts (04):
resid <- mod_def$residuals
png(file.path(out_dir, "all_residual_diagnostics.png"), width = 1000, height = 700)
par(mfrow = c(2,2), mar = c(3,3,3,3))
plot(resid, main = "Residuals")
abline(h = c(0, -3*sd(resid), 3*sd(resid)), lty = c(1,3,3), col = c(1,4,4))
scatter.smooth(sqrt(abs(resid)), main = "Square Root of Abs residuals", lpars = list(col = 2))
qqnorm(resid); qqline(resid, col = 2, lwd = 2)
hist(resid, breaks = 10, freq = FALSE, main = "Residuals histogram")
curve(dnorm(x, mean = mean(resid), sd = sd(resid)), col = 2, add = TRUE)
dev.off()

# Independence diagnostics and consolidated checks
png(file.path(out_dir, "all_tsdiag.png"), width = 1000, height = 500)
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

# 4) Barras: incremento en pp por grupo y crisis
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

# 5) Heatmap: matriz de impactos (pp) por grupo x crisis
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

# 6) Resumen de armonización: qué etiqueta original se usó por grupo y años
mapping_summary <- harmonized %>%
  group_by(.data$group, .data$RACE_NAME) %>%
  summarise(start_year = min(.data$time), end_year = max(.data$time), n_years = dplyr::n_distinct(.data$time), .groups = "drop") %>%
  arrange(.data$group, .data$RACE_NAME, .data$start_year)
write_csv(mapping_summary, file.path(out_dir, "mapping_summary.csv"))

# 7) Índice de figuras y tablas recomendadas (ES)
fig_index <- file.path(out_dir, "figures_index_es.txt")
sink(fig_index)
cat("Figuras y tablas recomendadas para el documento:\n\n")
cat("1) trends: timeseries/output/poverty_ts_by_group.png — Series 1959–2024 por grupo con sombreado de recesiones.\n")
cat("2) transform: timeseries/output/all_original_vs_log.png — Serie original vs. log-transformada (All).\n")
cat("3) id: timeseries/output/all_ACF_PACF_d1log.png — ACF/PACF de la serie log‑diferenciada (identificación).\n")
cat("4) resid: timeseries/output/all_residual_diagnostics.png — Panel de residuos (escala, QQ, histograma).\n")
cat("5) indep: timeseries/output/all_tsdiag.png — Independencia (tsdiag).\n")
cat("6) check: timeseries/output/all_checkresiduals.png — Comprobación consolidada de residuos.\n")
cat("7) forecast: timeseries/output/all_forecast.png — Predicción 10 años con IC 95%.\n")
cat("8) bars: timeseries/output/impact_bars.png — Barras por crisis con incremento (pp) por grupo.\n")
cat("9) heatmap: timeseries/output/impact_heatmap.png — Heatmap de incrementos (pp) grupo×crisis.\n")
cat("10) impact_table: timeseries/output/recession_impact_summary.csv — Tabla detallada (baseline, pico, año pico, delta_pp).\n")
cat("11) impact_matrix: timeseries/output/impact_matrix.csv — Matriz grupo×crisis con delta_pp.\n")
cat("12) mapping: timeseries/output/mapping_summary.csv — Resumen de armonización (etiquetas originales por años).\n")
sink()

## Report generation -----------------------------------------------------------
# Create a narrative report tying outputs to the research question.
generate_report <- function() {
  report_path <- file.path(out_dir, "report.txt")

  # Prep impact highlights
  impact_top <- impact %>%
    group_by(.data$window) %>%
    slice_max(order_by = .data$delta_pp, n = 3, with_ties = FALSE) %>%
    arrange(.data$window, desc(.data$delta_pp)) %>%
    ungroup()

  # Helper: format a small ranked list per window
  fmt_window <- function(win_name) {
    x <- impact_top %>% filter(window == win_name)
    if (nrow(x) == 0) return(paste0("No data available for ", win_name, ".\n"))
    paste0(
      sprintf("%s — top 3 poverty spikes (pp increase vs pre-window baseline):\n", win_name),
      paste0(sprintf("  %s: +%.1f pp (peak %.1f%% in %d)",
                     x$group, x$delta_pp, x$peak, x$peak_year), collapse = "\n"),
      "\n\n"
    )
  }

  # Coefficient significance text
  sig_coefs <- function(ratios) {
    if (is.null(ratios) || length(ratios) == 0) return("(none)")
    sig <- ratios[ratios > 2]
    if (length(sig) == 0) return("(none)")
    paste0(names(sig), " (", round(sig,2), ")", collapse = ", ")
  }

  sink(report_path)
  cat("Executive Summary\n\n")
  cat("This analysis examines U.S. poverty rates (CPS ASEC, 1959–2024) across key demographic groups to assess which groups were hit hardest during the Reagan Recession (1980–1983), the Dot‑com Crash (2001–2003), the Great Recession (2008–2010), and COVID‑19 (2020–2021). Using the class ARIMA workflow on the All Races series (log transform, d=1), we benchmark overall dynamics and quantify spikes by comparing recession peaks to pre‑window baselines.\n\n")

  cat("Dataset Selection\n\n")
  cat("- Source: CPS ASEC poverty (timeseries/data/cps_asec_poverty_1959_2024.csv)\n")
  cat("- Variables used: year (time), poverty rate (PCTPOV), demographic categories (RACE_NAME)\n")
  cat("- Harmonized groups: All, White, Black, Hispanic, Asian (mapped across category changes over time)\n")
  cat("- US totals only (us == 1). Cleaned file saved: timeseries/output/poverty_harmonized.csv\n\n")

  cat("Research Questions\n\n")
  cat("Which demographic groups were hit hardest by each recession? We define 'hit hardest' as the largest percentage‑point (pp) increase from a 3‑year baseline prior to the recession window to the peak within the window. Windows:\n")
  cat("- Reagan: 1980–1983\n- Dot‑com: 2001–2003\n- Great Recession: 2008–2010\n- COVID‑19: 2020–2021\n\n")

  cat("Time Series Modeling\n\n")
  cat("- Series modeled (annual): All Races poverty rate (PCTPOV)\n")
  cat("- Transform: log; Differencing: d = 1 (to remove trend)\n")
  cat("- Candidate models fit: ARIMA(3,1,0) and ARIMA(2,1,0)\n")
  cat(sprintf("- AICs: ARIMA(3,1,0) = %.2f; ARIMA(2,1,0) = %.2f. Chosen by lower AIC.\n", aic1, aic2))
  cat(sprintf("- Significant coefficients (|coef/se|>2):\n  ARIMA(3,1,0): %s\n  ARIMA(2,1,0): %s\n\n",
              sig_coefs(rat1), sig_coefs(rat2)))
  cat("Recommended figures:\n")
  cat("- timeseries/output/poverty_ts_by_group.png — Trends by group with recession shading\n")
  cat("- timeseries/output/all_ACF_PACF_d1log.png — ACF/PACF of differenced logs (model identification)\n")
  cat("- timeseries/output/all_residual_diagnostics.png — Residual distribution and scale checks\n")
  cat("- timeseries/output/all_tsdiag.png — Independence diagnostics (ACF of residuals, Ljung‑Box)\n")
  cat("- timeseries/output/all_forecast.png — Forecast with 95% intervals on original scale\n\n")

  cat("Results and Testing\n\n")
  cat("- Recession impact (top three groups by pp increase, baseline = prior 3 years):\n\n")
  cat(fmt_window("Reagan"))
  cat(fmt_window("DotCom"))
  cat(fmt_window("GreatRec"))
  cat(fmt_window("COVID19"))
  cat("- Full table: timeseries/output/recession_impact_summary.csv\n")
  cat("- Residual checks: see all_residual_diagnostics.png and all_tsdiag.png; consolidated check via checkresiduals(mod_def) was run.\n\n")

  # Conclusions: one-liner per window (top-1)
  concl <- impact_top %>% group_by(.data$window) %>% slice_max(order_by = .data$delta_pp, n = 1, with_ties = FALSE) %>% ungroup()
  cat("Conclusions\n\n")
  if (nrow(concl) > 0) {
    apply(concl, 1, function(r) {
      cat(sprintf("- %s: %s experienced the largest spike (+%.1f pp, peak %.1f%% in %s).\n",
                  r[["window"]], r[["group"]], as.numeric(r[["delta_pp"]]), as.numeric(r[["peak"]]), r[["peak_year"]]))
    })
    cat("\n")
  } else {
    cat("- Insufficient data to determine top groups by window.\n\n")
  }
  cat("Use the recommended figures above to illustrate overall trends, model identification/validation, and the forecast, and cite the CSV summary for exact values per group/window.\n")
  sink()
}

generate_report()

cat("\nDone. Outputs written to:", normalizePath(out_dir), "\n")
