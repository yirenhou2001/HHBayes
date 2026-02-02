# tests/testthat/test-stan-integration.R
# Integration test that actually runs Stan
# This ONLY runs locally or in CI, NEVER on CRAN

test_that("GenSyn runs Stan model end-to-end", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  # Skip if Stan compilation fails (e.g., no C++ toolchain)
  skip_if_not(
    tryCatch({
      requireNamespace("rstan", quietly = TRUE)
      TRUE
    }, error = function(e) FALSE),
    "rstan not available"
  )

  set.seed(42)

  # Very minimal settings for speed
  seasonal_forcing <- list(
    adult = rep(0.1, 20),
    child = rep(0.1, 20),
    elderly = rep(0.1, 20),
    toddler = rep(0.1, 20)
  )

  # This actually compiles and runs Stan!
  result <- suppressWarnings(
    GenSyn(
    n_households = 3,
    max_days = 20,
    seasonal_forcing_list = seasonal_forcing,
    stan_chains = 1,
    stan_iter = 50,    # Absolute minimum
    stan_warmup = 25,
    stan_refresh = 0,  # Suppress Stan output
    stan_cores = 1,
    print_plots = FALSE,
    plots = character(0)
    )
  )

  # Verify structure

  expect_s3_class(result, "GenSynResult")
  expect_true("fit" %in% names(result$results))
  expect_true(inherits(result$results$fit, "stanfit"))

  # Verify posterior summary exists
  expect_true(nrow(result$postprocessing) > 0)
})


test_that("TransmissionChainAnalysis runs Stan model end-to-end", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  T_max <- 15

  df <- data.frame(
    hh_id = c("HH1", "HH1", "HH2", "HH2"),
    person_id = c(1, 2, 1, 2),
    role = c("adult", "child", "adult", "elderly"),
    infection_time = c(3, 6, 2, 5),
    infectious_start = c(4, 7, 3, 6),
    infectious_end = c(8, 10, 7, 9),
    infection_resolved = c(10, 12, 9, 11)
  )

  seasonal_forcing <- list(
    adult = rep(1, T_max),
    child = rep(1, T_max),
    elderly = rep(1, T_max),
    toddler = rep(1, T_max)
  )

  result <- suppressWarnings(
    TransmissionChainAnalysis(
    user_data = df,
    max_days = T_max,
    seasonal_forcing_list = seasonal_forcing,
    stan_chains = 1,
    stan_iter = 50,
    stan_warmup = 25,
    stan_refresh = 0,
    stan_cores = 1
    )
  )

  expect_s3_class(result, "TransmissionChainResult")
  expect_true("fit" %in% names(result$results))
  expect_true(inherits(result$results$fit, "stanfit"))
})
