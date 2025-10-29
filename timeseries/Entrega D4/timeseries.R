library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forecast)

csv_path <- file.path("timeseries", "data", "cps_asec_poverty_1959_2024.csv")
dat_raw <- read_csv(csv_path, show_col_types = FALSE)

dat <- dat_raw %>% filter(us == 1)

out_dir <- file.path("timeseries", "output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#harmonitzacio de grups racials
group_priority <- tibble::tribble(
  ~group,     ~RACE_NAME,                                  ~priority,
  "All",      "All Races",                                 1L,
  "White",    "White Alone, Not Hispanic",                  1L,
  "White",    "White Alone",                                2L,
  "White",    "White",                                      3L,
  "Black",    "Black Alone",                                1L,
  "Black",    "Black Alone or in Combination",              2L,
  "Black",    "Black",                                      3L,
  "Hispanic", "Hispanic (of any race)",                    1L,
  "Asian",    "Asian Alone",                                1L,
  "Asian",    "Asian Alone or in Combination",              2L,
  "Asian",    "Asian and Pacific Islander",                 3L
)

#assignar grup i prioritat, seleccionem millor match per any
harmonized <- dat %>%
  inner_join(group_priority, by = "RACE_NAME") %>%
  arrange(time, group, priority) %>%
  group_by(time, group) %>%
  slice_min(order_by = priority, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(time, group, PCTPOV, POV, POP, RACE_NAME)

wide <- harmonized %>%
  select(time, group, PCTPOV) %>%
  distinct() %>%
  pivot_wider(names_from = group, values_from = PCTPOV)

#guardem dades harmonitzades
write_csv(harmonized, file.path(out_dir, "poverty_harmonized.csv"))
write_csv(wide, file.path(out_dir, "mapping_summary.csv"))

# Serie temporal per grup amb ombrejat de recessions

#recessions
recessions <- tibble::tribble(
  ~name,     ~start, ~end,
  "Reagan",   1980L,  1983L,
  "DotCom",   2001L,  2003L,
  "GreatRec", 2008L,  2010L,
  "COVID19",  2020L,  2021L
)

#grafic amb recessions ombrejades
rects <- recessions %>% mutate(ymin = -Inf, ymax = Inf)

p <- ggplot(harmonized, aes(x = time, y = PCTPOV, color = group)) +
  geom_rect(data = rects, inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
            fill = "grey85", color = NA, alpha = 0.5) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous("Poverty rate (%)", limits = c(0, NA)) +
  scale_x_continuous("Year", breaks = scales::pretty_breaks()) +
  scale_color_brewer(palette = "Set1") +
  ggtitle("Poverty rate by group (% of people in poverty)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave(filename = file.path(out_dir, "poverty_ts_by_group.png"), 
       plot = p, width = 11, height = 6, dpi = 150)

# impacte de les recessions: increment en pp sobre baseline

#calcul per cada recessio
impact_list <- list()

for (i in seq_len(nrow(recessions))) {
  win <- recessions[i,]
  start_yr <- win$start
  end_yr   <- win$end
  
  # Baseline: 3 anys previs
  base_years <- seq.int(start_yr - 3, start_yr - 1)
  win_years  <- seq.int(start_yr, end_yr)
  
  base_df <- harmonized %>% filter(time %in% base_years)
  win_df  <- harmonized %>% filter(time %in% win_years)
  
  # mitjana i maxim per grup
  summ <- base_df %>%
    group_by(group) %>%
    summarise(baseline = mean(PCTPOV, na.rm = TRUE), 
              n_base = sum(!is.na(PCTPOV)), .groups = "drop") %>%
    inner_join(
      win_df %>% 
        group_by(group) %>% 
        summarise(peak = max(PCTPOV, na.rm = TRUE),
                  peak_year = time[which.max(PCTPOV)],
                  n_win = sum(!is.na(PCTPOV)), .groups = "drop"),
      by = "group"
    ) %>%
    mutate(window = win$name, delta_pp = peak - baseline) %>%
    filter(is.finite(baseline), is.finite(peak), n_base >= 2, n_win >= 1) %>%
    arrange(desc(delta_pp))
  
  impact_list[[win$name]] <- summ
}

impact <- bind_rows(impact_list)
write_csv(impact, file.path(out_dir, "recession_impact_summary.csv"))

#resum a consola
cat("\n===== Recession Impact: Largest increases in poverty rate (pp) =====\n")
impact %>% 
  group_by(window) %>% 
  slice_max(order_by = delta_pp, n = 3, with_ties = FALSE) %>% 
  print(n = 20)

#matriu d'impacte
impact_matrix <- impact %>%
  select(group, window, delta_pp) %>%
  pivot_wider(names_from = window, values_from = delta_pp)
write_csv(impact_matrix, file.path(out_dir, "impact_matrix.csv"))

# grafic de barres per crisis
impact$group <- factor(impact$group, levels = sort(unique(impact$group)))

pb <- ggplot(impact, aes(x = group, y = delta_pp, fill = group)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f", delta_pp)), vjust = -0.3, size = 3) +
  facet_wrap(~ window, scales = "free_y") +
  scale_y_continuous("Incremento (pp)") +
  scale_x_discrete("Grupo") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  ggtitle("Incremento de pobreza por crisis (pp sobre baseline)") +
  theme_minimal(base_size = 12)
ggsave(filename = file.path(out_dir, "impact_bars.png"), 
       plot = pb, width = 11, height = 6, dpi = 150)

# heatmap d'impacte
imp_hm <- impact %>%
  select(group, window, delta_pp) %>%
  mutate(group = factor(group, levels = sort(unique(group))),
         window = factor(window, levels = c("Reagan","DotCom","GreatRec","COVID19")))

phm <- ggplot(imp_hm, aes(x = window, y = group, fill = delta_pp)) +
  geom_tile() +
  geom_text(aes(label = ifelse(is.finite(delta_pp), sprintf("%.1f", delta_pp), "")), 
            color = "black", size = 3) +
  scale_fill_gradient(name = "pp", low = "#f7fbff", high = "#08306b", na.value = "#eeeeee") +
  xlab("Crisis") + ylab("Grupo") +
  ggtitle("Heatmap de incrementos (pp) por crisis y grupo") +
  theme_minimal(base_size = 12)
ggsave(filename = file.path(out_dir, "impact_heatmap.png"), 
       plot = phm, width = 7, height = 5, dpi = 150)

# Modelitzacio ARIMA: All Races

#serie All Races
series_all <- harmonized %>% 
  filter(group == "All") %>% 
  arrange(time)

start_year <- min(series_all$time)
ts_all <- ts(series_all$PCTPOV, start = start_year, frequency = 1)

lnts_all   <- log(ts_all)
d1lnts_all <- diff(lnts_all)

#plots exploratoris
par(mfrow=c(2,2))
plot(ts_all,       main = 'PCTPOV (All)')
plot(lnts_all,     main = 'log(PCTPOV)')
plot(d1lnts_all,   main = 'd1 log(PCTPOV)')
dev.off()

#ACF i PACF de la serie diferenciada
par(mfrow=c(1,2))
acf(d1lnts_all, ylim = c(-1,1), lag.max = 40, main = "ACF (d1 log All Races)")
pacf(d1lnts_all, ylim = c(-1,1), lag.max = 40, main = "PACF (d1 log All Races)")
dev.off()

# Models ARIMA candidats

#ARIMA(3,1,0)
mod1 <- arima(lnts_all, order = c(3,1,0))
ratios1 <- round(abs(mod1$coef / sqrt(diag(mod1$var.coef))), 2)
ratios1
ratios1 > 2

#ARIMA(2,1,0)
mod2 <- arima(lnts_all, order = c(2,1,0))
ratios2 <- round(abs(mod2$coef / sqrt(diag(mod2$var.coef))), 2)
ratios2
ratios2 > 2

#comparacio AIC
AIC(mod1)
AIC(mod2)

# Validacio del model

#homoscedasticitat
resid <- mod1$residuals

par(mfrow=c(2,2), mar=c(3,3,3,3))
plot(resid, main="Residuals")
abline(h = c(0, -3*sd(resid), 3*sd(resid)), lty = c(1,3,3), col=c(1,4,4))
scatter.smooth(sqrt(abs(resid)), 
               main="Square Root of Absolute residuals",
               lpars = list(col=2))

#normalitat
qqnorm(resid)
qqline(resid, col=2, lwd=2)
hist(resid, breaks = 10, freq=FALSE, main = "Residuals histogram")
curve(dnorm(x, mean = mean(resid), sd = sd(resid)), col=2, add=TRUE)
dev.off()

#independencia
tsdiag(mod1, gof.lag = 20)
dev.off()

#tsdiag
png(file.path(out_dir, "all_tsdiag.png"), width = 900, height = 700)
tsdiag(mod1, gof.lag = 20)
dev.off()

#checkresiduals
png(file.path(out_dir, "all_checkresiduals.png"), width = 900, height = 600)
checkresiduals(mod1)
dev.off()

# Prediccio

#calcul de les prediccions
pred   <- predict(mod1, n.ahead = 10)
pr_log <- pred$pred
se_log <- pred$se

#intervals de confianca (log scale)
li_log <- pr_log - 1.96 * se_log
ls_log <- pr_log + 1.96 * se_log

#desfer logaritmes
li <- ts(exp(li_log), start = end(ts_all)[1] + 1, freq = 1)
pr <- ts(exp(pr_log), start = end(ts_all)[1] + 1, freq = 1)
ls <- ts(exp(ls_log), start = end(ts_all)[1] + 1, freq = 1)

#grafic
par(mfrow=c(1,1))
ts.plot(ts_all, li, ls, pr,
        lty = c(1,2,2,1), 
        col = c("black","blue","blue","red"),
        xlab = "Year", ylab = "Poverty rate (%)",
        main = "All Races: Forecast with 95% intervals")

#guardem prediccio
png(file.path(out_dir, "all_forecast.png"), width = 1000, height = 500)
par(mfrow=c(1,1))
ts.plot(ts_all, li, ls, pr,
        lty = c(1,2,2,1), 
        col = c("black","blue","blue","red"),
        xlab = "Year", ylab = "Poverty rate (%)",
        main = "All Races: Forecast with 95% intervals")
dev.off()

#guardem plots diagnostics
png(file.path(out_dir, "all_original_vs_log.png"), width = 1000, height = 450)
par(mfrow=c(1,2))
plot(ts_all, main = "PCTPOV (All)", xlab = "Any", ylab = "Tasa de pobresa (%)")
plot(lnts_all, main = "log(PCTPOV) (All)", xlab = "Any", ylab = "log tasa de pobresa")
dev.off()

png(file.path(out_dir, "all_ACF_PACF_d1log.png"), width = 900, height = 450)
par(mfrow=c(1,2))
acf(d1lnts_all, ylim = c(-1,1), lag.max = 40, main = "ACF (d1 log All Races)")
pacf(d1lnts_all, ylim = c(-1,1), lag.max = 40, main = "PACF (d1 log All Races)")
dev.off()

png(file.path(out_dir, "all_residual_diagnostics.png"), width = 1000, height = 800)
par(mfrow=c(2,2), mar=c(3,3,3,3))
plot(resid, main="Residuals")
abline(h = c(0, -3*sd(resid), 3*sd(resid)), lty = c(1,3,3), col=c(1,4,4))
scatter.smooth(sqrt(abs(resid)), 
               main="Square Root of Absolute residuals",
               lpars = list(col=2))
qqnorm(resid)
qqline(resid, col=2, lwd=2)
hist(resid, breaks = 10, freq=FALSE, main = "Residuals histogram")
curve(dnorm(x, mean = mean(resid), sd = sd(resid)), col=2, add=TRUE)
dev.off()

#resum de models
sink(file.path(out_dir, "all_model_summary.txt"))
cat("All Races ARIMA model comparison (log scale, d=1)\n\n")
cat("Model 1: ARIMA(3,1,0)\n")
print(mod1)
cat("\nRatios (coef/se):\n")
print(ratios1)
cat("\nAIC:", AIC(mod1), "\n\n")

cat("Model 2: ARIMA(2,1,0)\n")
print(mod2)
cat("\nRatios (coef/se):\n")
print(ratios2)
cat("\nAIC:", AIC(mod2), "\n\n")
sink()

cat("\n===== Script finalitzat =====\n")
cat("Outputs guardats a:", out_dir, "\n\n")