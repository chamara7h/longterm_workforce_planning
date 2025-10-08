library(tidyverse)
library(scales)
library(ggthemes)
library(viridis)
library(showtext)

font_add_google("Assistant", "C:/Windows/Fonts/Assistant-Regular.ttf")
showtext_auto()

# Read data ---------------------------------------------------------------

scen <- read_rds('results/sd_scenario.rds')

base <- read_rds('results/sd_base.rds')

# Outcome + levers
ycol <- "balance"
lever_cols <- c("v_ucas","intl_rate","recr_mult","grad_rate")

# Focus year (2028 if present)
focus_year <- if (any(scen$year == 2028)) 2028 else max(scen$year, na.rm = TRUE)

# Region order
default_order <- c("East of England","London","Midlands",
                   "North East and Yorkshire","North West",
                   "South East","South West")
present <- default_order[default_order %in% unique(scen$nhse_region_name)]
region_levels <- if (length(present)) present else sort(unique(scen$nhse_region_name))


# National fan chart (baseline vs scenario envelope) ----------------------

# National totals per scenario-year
nat_scen <- scen |>
  group_by(year, scenario_id) |>
  summarise(national_balance = sum(.data[[ycol]], na.rm = TRUE), .groups = "drop")

# Quantiles per year
fan_df <- nat_scen |>
  group_by(year) |>
  summarise(
    p05    = quantile(national_balance, 0.05, na.rm = TRUE),
    p25    = quantile(national_balance, 0.25, na.rm = TRUE),
    median = median(national_balance, na.rm = TRUE),
    p75    = quantile(national_balance, 0.75, na.rm = TRUE),
    p95    = quantile(national_balance, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# Baseline national line
base_nat <- base |>
  group_by(year) |>
  summarise(baseline_national = sum(.data[[ycol]], na.rm = TRUE), .groups = "drop")

ggplot(fan_df, aes(x = year)) +
  geom_ribbon(aes(ymin = p05, ymax = p95, fill = "5–95%"), alpha = 0.25) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "25–75%"), alpha = 0.45) +
  geom_line(aes(y = median, colour = "Median", linetype = 'Median'), size = 1) +
  geom_line(
    data = base_nat,
    aes(y = baseline_national, colour = "Business as usual", linetype = "Business as usual"),
    size = 1
  ) +
  geom_hline(yintercept = 0, size = 0.3) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "5–95%"  = "grey80",
      "25–75%" = "grey40"
    )
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Median"   = "#D55E00",
      "Business as usual" = "#0072B2"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Business as usual" = "dashed",
      'Median' = 'solid'
    )
  ) +
  labs(
    x       = "Year",
    y       = "Surplus/ shortfall (Nurse headcount)"
  ) +
  scale_x_continuous(
    breaks = seq(min(base$year), max(base$year), by = 1)
  ) +
  theme_few(base_family = "Assistant", base_size = 22) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "lightgrey", fill = NA),
    panel.spacing    = unit(0.5, "lines"),
    legend.position  = "right",
    legend.key.width = unit(2, "line")
  )


# Regiona fan chart (baseline vs scenario envelope) ----------------------

# Regional totals per scenario-year
reg_scen <- scen |>
  group_by(year,nhse_region_name, scenario_id) |>
  summarise(regional_balance = sum(.data[[ycol]], na.rm = TRUE), .groups = "drop")

# Quantiles per year
fan_df <- reg_scen |>
  group_by(year, nhse_region_name) |>
  summarise(
    p05    = quantile(regional_balance, 0.05, na.rm = TRUE),
    p25    = quantile(regional_balance, 0.25, na.rm = TRUE),
    median = median(regional_balance, na.rm = TRUE),
    p75    = quantile(regional_balance, 0.75, na.rm = TRUE),
    p95    = quantile(regional_balance, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# Baseline national line
base_reg <- base |>
  group_by(year, nhse_region_name) |>
  summarise(baseline_reg = sum(.data[[ycol]], na.rm = TRUE), .groups = "drop")

ggplot(fan_df, aes(x = year)) +
  geom_ribbon(aes(ymin = p05, ymax = p95, fill = "5–95%"), alpha = 0.25) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "25–75%"), alpha = 0.45) +
  geom_line(aes(y = median, colour = "Median", linetype = 'Median'), size = 1) +
  geom_line(
    data = base_reg,
    aes(y = baseline_reg, colour = "Business as usual", linetype = "Business as usual"),
    size = 1
  ) +
  geom_hline(yintercept = 0, size = 0.3) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "5–95%"  = "grey80",
      "25–75%" = "grey40"
    )
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Median"   = "#D55E00",
      "Business as usual" = "#0072B2"
    )
  ) +
  facet_wrap(~nhse_region_name, scales = 'free') +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Business as usual" = "dashed",
      'Median' = 'solid'
    )
  ) +
  labs(
    x       = "Year",
    y       = "Surplus/ shortfall (Nurse headcount)"
  ) +
  scale_x_continuous(
    breaks = seq(min(base$year), max(base$year), by = 1)
  ) +
  theme_few(base_family = "Assistant", base_size = 22) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "lightgrey", fill = NA),
    panel.spacing    = unit(0.5, "lines"),
    legend.position  = "right",
    legend.key.width = unit(2, "line")
  )

# Regional boxplots (2028 distributions across 256 scenarios) -------------

reg_scen_2028 <- scen |>
  filter(year == focus_year) |>
  group_by(nhse_region_name, scenario_id) |>                    
  summarise(value = mean(.data[[ycol]], na.rm = TRUE), .groups = "drop") |>
  mutate(nhse_region_name = factor(nhse_region_name, levels = region_levels))

ggplot(reg_scen_2028, aes(x = nhse_region_name, y = value)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_discrete(guide = guide_axis(angle = 25)) +
  labs(title = glue::glue("Regional Distribution across 256 Scenarios — {focus_year}"),
       x = NULL, y = paste0(str_to_title(ycol), " (", focus_year, ")")) +
  theme_few(base_family = "Assistant", base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "lightgrey", fill = NA),
    panel.spacing    = unit(0.5, "lines"),
    legend.position  = "right",
    legend.key.width = unit(2, "line")
  )


# Regional heatmap (median by year) ---------------------------------------

heat_df <- scen |>
  group_by(nhse_region_name, year) |>
  summarise(median_balance = mean(.data[[ycol]], na.rm = TRUE), .groups = "drop") |>
  mutate(nhse_region_name = factor(nhse_region_name, levels = region_levels))

ggplot(heat_df, aes(x = year, y = nhse_region_name, fill = median_balance)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Median balance") +
  labs(title = "Regional Median Balance by Year (Scenarios)", x = NULL, y = NULL) +
  scale_x_continuous(
    breaks = seq(min(base$year), max(base$year), by = 1)
  ) +
  theme_few(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "lightgrey", fill = NA),
    panel.spacing    = unit(0.5, "lines"),
    legend.position  = "right",
    legend.key.width = unit(2, "line")
  )


# supply vs demand & balance ------------------------------------

# Supply and demand

cb2 <- c(
  "Supply projection"  = "#E69F00",  
  "Demand projection" = "#0072B2"   
)

base |>
  # filter(year != 2022) |> 
  group_by(year) |>
  summarise(
    stock  = sum(stock,  na.rm = TRUE),
    demand = sum(demand, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = stock,  colour = "Supply projection"),  size = 1) +
  geom_line(aes(y = demand, colour = "Demand projection"), size = 1,
            linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(base$year), max(base$year), by = 1)) +
  scale_color_manual(
    name   = "Scenario",
    values = cb2
  ) +
  labs(y = "Headcount", x = "Year") +
  theme_few(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "lightgrey", fill = NA),
    panel.spacing    = unit(0.5, "lines"),
    legend.position  = "right",
    legend.key.width = unit(2, "line")
  )


# Balance (surplus/shortage)
base |>
  filter(year != 2022) |> 
  ggplot(aes(year, balance, fill = nhse_region_name)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9, name = "Region") +
  scale_x_continuous(
    breaks = seq(min(base$year), max(base$year), by = 1)
  ) +
  labs(y = "Surplus (+) / Shortage (–)",
       x = 'Year') +
  theme_few(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "lightgrey", fill = NA),
    panel.spacing    = unit(0.5, "lines"),
    legend.position  = "right",
    legend.key.width = unit(2, "line")
  )


