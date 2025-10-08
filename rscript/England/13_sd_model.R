# sd_flow_full_model

library(tidyverse)
library(purrr)

# Read inputs ----------------------------------------------------------

# Initial nurse stock (headcount)
stock_init   <- read_csv("system_dynamics/nurse_stock_eng.csv")  
joiners_obs  <- read_csv("system_dynamics/joiners_stock_eng.csv")  
leavers_obs  <- read_csv("system_dynamics/leavers_stock_eng.csv")  

# UCAS: initial acceptances and growth rates (Table 4)
ucas_rates   <- read_csv("system_dynamics/ucas_acceptance_rate_eng.csv") |>
  rename(ucas_growth = average_ucas_acceptance_rate)

# UCAS initial acceptance counts (year 0, i.e. 2022)
ucas_init    <- tibble(
  nhse_region_name = c("East of England","London","Midlands",
                       "North East and Yorkshire","North West",
                       "South East","South West"),
  ucas0 = c(565, 990, 1080, 905, 970, 505, 325)
)

# Demand forecasts (base case) from your predictions CSV
demand_fcst <- read_csv("system_dynamics/predictions_eng.csv") |>
  select(year, starts_with("`"), ends_with("_base case")) |>
  pivot_longer(-year,
               names_to = "tmp", values_to = "demand"
  ) |>
  separate(tmp, into = c("nhse_region_name","scenario"), sep = "_") |>
  filter(scenario == "base case") |>
  select(-scenario)

# Tidy joiners/leavers to long -----------------------------------------

joiners_long <- joiners_obs |>
  pivot_longer(-year, names_to="nhse_region_name", values_to="AllJoiners")

leavers_long <- leavers_obs |>
  pivot_longer(-year, names_to="nhse_region_name", values_to="Leavers")

flows <- full_join(joiners_long, leavers_long,
                   by = c("year","nhse_region_name"))

# Parameter constants --------------------------------------------------

grad_rate    <- 0.48    # graduate joiner rate (48%) 
intl_rate    <- 0.005   # international joiner rate (0.5%) 
recr_mult    <- 1       # recruitment multiplier 

# Simulation function per region ---------------------------------------

simulate_region <- function(df, init_stock, u0, v_ucas, dem) {
  yrs   <- sort(df$year)
  n     <- length(yrs)
  
  # pre-allocate
  stock <- numeric(n)
  su    <- numeric(n)  # GraduateJoiners
  si    <- numeric(n)  # InternationalJoiners
  so    <- numeric(n)  # OtherRecruitments
  tj    <- numeric(n)  # TotalJoiners
  demand <- numeric(n)
  bal   <- numeric(n)
  
  for (i in seq_along(yrs)) {
    y <- yrs[i]
    
    # GraduateJoiners: using UCAS base and growth
    if (i == 1) {
      su[i] <- u0 * (1 + v_ucas) * grad_rate
    } else {
      # assume UCAS grows at v_ucas each year from u0
      u_prev  <- u0 * (1 + v_ucas)^(i - 1)
      su[i]   <- u_prev * (1 + v_ucas) * grad_rate
    }
    
    # InternationalJoiners: obs AllJoiners * intl_rate
    si[i]  <- df$AllJoiners[i] * intl_rate
    
    # OtherRecruitments: residual
    so[i]  <- df$AllJoiners[i] - su[i] - si[i]
    
    # TotalJoiners (with multiplier)
    tj[i]  <- (su[i] + si[i] + so[i]) * recr_mult
    
    # Stock update
    if (i == 1) {
      stock[i] <- init_stock
    } else {
      stock[i] <- stock[i - 1] + tj[i] - df$Leavers[i]
    }
    
    # Demand & balance
    if (i == 1) {
      demand[i] <- init_stock    # base‐year demand = stock
      bal[i]    <- 0
    } else {
      this_dem <- dem$demand[dem$year == y]
      demand[i] <- if (length(this_dem)==1) this_dem else NA_real_
      bal[i]    <- stock[i] - demand[i]
    }
  }
  
  # return a tibble with all series
  tibble(
    year                 = yrs,
    nhse_region_name     = df$nhse_region_name[1],
    graduate_joiners      = su,
    international_joiners = si,
    other_recruitments    = so,
    total_joiners         = tj,
    leavers              = df$Leavers,
    stock                = stock,
    demand               = demand,
    balance              = bal
  )
}



# Run simulation over all regions --------------------------------------

regions <- unique(flows$nhse_region_name)

results <- map_dfr(regions, function(region) {
  df_region <- flows |> filter(nhse_region_name == region)
  
  init_hc <- stock_init |>
    filter(nhse_region_name == region) |>
    pull(hc)
  
  u0 <- ucas_init |>
    filter(nhse_region_name == region) |>
    pull(ucas0)
  
  v_ucas <- ucas_rates |>
    filter(nhse_region_name == region) |>
    pull(ucas_growth)
  
  dem <- demand_fcst |>
    filter(nhse_region_name == region)
  
  simulate_region(
    df      = df_region,
    init_stock = init_hc,
    u0      = u0,
    v_ucas  = v_ucas,
    dem     = dem
  )
})

write_rds(results, 'results/sd_base.rds')

write.csv(results, 'results/sd_base.csv', row.names = F)

# Scenario testing --------------------------------------------------------

# Revised sim function

simulate_region <- function(df, init_stock, u0, v_ucas,
                            dem,
                            intl_rate, recr_mult, grad_rate) {
  yrs   <- sort(df$year)
  n     <- length(yrs)
  
  # pre-allocate
  stock <- numeric(n)
  su    <- numeric(n)
  si    <- numeric(n)
  so    <- numeric(n)
  tj    <- numeric(n)
  demand<- numeric(n)
  bal   <- numeric(n)
  
  for (i in seq_along(yrs)) {
    y <- yrs[i]
    
    # GraduateJoiners
    if (i == 1) {
      su[i] <- u0 * (1 + v_ucas) * grad_rate
    } else {
      u_prev <- u0 * (1 + v_ucas)^(i - 1)
      su[i]  <- u_prev * (1 + v_ucas) * grad_rate
    }
    
    # InternationalJoiners
    si[i]  <- df$AllJoiners[i] * intl_rate
    
    # OtherRecruitments
    so[i]  <- max(df$AllJoiners[i] - su[i] - si[i], 0)
    
    # TotalJoiners
    tj[i]  <- (su[i] + si[i] + so[i]) * recr_mult
    
    # Stock update
    if (i == 1) {
      stock[i] <- init_stock
    } else {
      stock[i] <- stock[i - 1] + tj[i] - df$Leavers[i]
    }
    
    # Demand & balance
    if (i == 1) {
      demand[i] <- init_stock
      bal[i]    <- 0
    } else {
      this_dem   <- dem$demand[dem$year == y]
      demand[i]  <- if (length(this_dem)==1) this_dem else NA_real_
      bal[i]     <- stock[i] - demand[i]
    }
  }
  
  tibble(
    year                 = yrs,
    nhse_region_name     = df$nhse_region_name[1],
    graduate_joiners     = su,
    international_joiners= si,
    other_recruitments   = so,
    total_joiners        = tj,
    leavers              = df$Leavers,
    stock                = stock,
    demand               = demand,
    balance              = bal
  )
}


# scenario grid 
scenarios <- expand_grid(
  scenario_id = seq_len(4*4*4*4),
  v_ucas      = c(0.00, 0.10, 0.20, 0.30),
  intl_rate   = c(0.003, 0.005, 0.007, 0.01),
  recr_mult   = c(1.0,   1.1,   1.2,   1.3),
  grad_rate   = c(0.40,  0.45,  0.50,  0.55)
) |> 
  filter(scenario_id == 1) |> 
  mutate(scenario_id = row_number())

# package region inputs
region_inputs <- flows %>%
  split(.$nhse_region_name) |>
  imap_dfr(~ tibble(
    nhse_region_name = .y,
    df_region        = list(.x),
    init_hc          = stock_init   |> filter(nhse_region_name == .y) |> pull(hc),
    u0               = ucas_init    |> filter(nhse_region_name == .y) |> pull(ucas0),
    dem              = list(demand_fcst |> filter(nhse_region_name == .y))
  ))

# cross join
scen_by_region <- crossing(scenarios, region_inputs)

# run sim
all_scenarios <- scen_by_region |> 
  mutate(
    sim = pmap(
      list(df         = df_region,
           init_stock = init_hc,
           u0         = u0,
           v_ucas     = v_ucas,
           dem        = dem,
           intl_rate  = intl_rate,
           recr_mult  = recr_mult,
           grad_rate  = grad_rate),
      simulate_region
    )
  ) |>
  select(-df_region, -init_hc, -u0, -dem, -nhse_region_name) |>  
  unnest(sim)

write_rds(all_scenarios, 'results/sd_scenario.rds')

write.csv(all_scenarios, 'results/sd_scenario.csv', row.names = FALSE)







