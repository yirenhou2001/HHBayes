library(devtools)
#library(pbapply)

remove.packages("Household.Transmission.Chain.Data.Analysis")
.rs.restartR()
devtools::document()
meg = devtools::check()
meg$notes
meg$warnings
meg$errors

devtools::install()
devtools::build_manual()
library(Household.Transmission.Chain.Data.Analysis)


################################# Example 1 #################################
# Create seasonal forcing (constant for simplicity)
seasonal_forcing <- list(
  adult   = rep(0.1, 365),
  child   = rep(0.1, 365),
  elderly = rep(0.1, 365),
  toddler = rep(0.1, 365)
)

# Run simulation + estimation
result <- GenSyn(
  n_households          = 50,
  seasonal_forcing_list = seasonal_forcing,
  max_days              = 365,
  stan_chains           = 2,
  stan_iter             = 100,
  stan_warmup           = 1,
  stan_cores            = 2
)

# View estimated parameters
print(result$postprocessing)

# Access individual plots
result$plot_list$daily
result$plot_list$weekly
result$plot_list$timeline
result$plot_list$sar


################################# Example 2 #################################
# Define seasonal forcing for each role
seasonal_forcing_list <- list(
  adult   = rep(0.1, 365),
  child   = rep(0.1, 365),
  elderly = rep(0.1, 365),
  toddler = rep(0.1, 365)
)

# Simulate 100 households and estimate parameters
result <- GenSyn(

  n_households = 100,
  seasonal_forcing_list = seasonal_forcing_list,
  max_days = 365,


  # Transmission parameters (these are the "true" values for simulation)
  beta1 = 0.3,              # Baseline transmission
  beta2 = 0.05,             # Viral load contribution
  phi_by_role = c(adult = 1.0, child = 7.0, toddler = 7.0, elderly = 4.0),
  kappa_by_role = c(adult = 1.0, child = 1.5, toddler = 1.5, elderly = 1.0),

  # Stan settings
  stan_chains  = 1,
  stan_iter    = 100,
  stan_warmup  = 1,
  stan_control = list(adapt_delta = 0.99, max_treedepth = 15),
  stan_cores   = 1,

  # Plotting
  print_plots = FALSE
)

# View results
print(result)


################################# Example 3 #################################
test_data <- data.frame(
  HH = c(1, 1, 1, 1, 1, 1,       # Household 1, person 1
         1, 1, 1, 1, 1, 1,       # Household 1, person 2
         2, 2, 2, 2, 2, 2),      # Household 2, person 1
  individual_ID = c(1, 1, 1, 1, 1, 1,
                    2, 2, 2, 2, 2, 2,
                    1, 1, 1, 1, 1, 1),
  role = c(rep("adult", 6),
           rep("child", 6),
           rep("adult", 6)),
  test_date = c(1, 3, 5, 7, 9, 11,
                1, 3, 5, 7, 9, 11,
                1, 3, 5, 7, 9, 11),
  infection_status = c(0, 0, 1, 1, 1, 0,   # Person 1: infected days 5-9
                       0, 0, 0, 1, 1, 1,   # Person 2: infected days 7-11
                       0, 1, 1, 1, 0, 0)   # Person 3: infected days 3-7
)

# Create seasonal forcing
T_max <- 15
seasonal_forcing_list <- list(
  adult = rep(1, T_max), child = rep(1, T_max),
  elderly = rep(1, T_max), toddler = rep(1, T_max)
)

# Analyze
result <- TransmissionChainAnalysis(
  user_data = test_data,
  seasonal_forcing_list = seasonal_forcing_list,
  max_days = T_max,

  # Specify viral load (vl) source when supplying your own data
  vl_source = "none",

  stan_chains  = 1,
  stan_iter    = 100,
  stan_warmup  = 1,
  stan_control = list(adapt_delta = 0.95, max_treedepth = 12),
  stan_cores   = 1
)

print(result$postprocessing)
