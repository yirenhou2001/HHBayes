# tests/testthat/test-simulation.R
# Tests for simulation functions - minimal, fast tests only

test_that("simulate_households returns correct structure", {
  # Use very short simulation for speed
  set.seed(42)
  sf <- list(
    adult = rep(0.1, 10), child = rep(0.1, 10),
    elderly = rep(0.1, 10), toddler = rep(0.1, 10)
  )

  result <- simulate_households(
    n_households = 2,
    max_days = 10,
    seasonal_forcing_list = sf
  )

  expect_type(result, "list")
  expect_named(result, c("hh_df", "households", "diagnostic_df"))
  expect_s3_class(result$hh_df, "data.frame")
  expect_type(result$households, "list")
  expect_length(result$households, 2)
})

test_that("simulate_households creates expected columns", {
  set.seed(123)
  sf <- list(
    adult = rep(0.1, 10), child = rep(0.1, 10),
    elderly = rep(0.1, 10), toddler = rep(0.1, 10)
  )

  result <- simulate_households(n_households = 1, max_days = 10, seasonal_forcing_list = sf)

  expected_cols <- c("hh_id", "person_id", "role", "infection_time",
                     "infectious_start", "infectious_end", "detection_time")
  expect_true(all(expected_cols %in% names(result$hh_df)))
})

test_that("simulate_households respects max_days", {
  set.seed(42)
  max_days_val <- 15
  sf <- list(
    adult = rep(0.1, max_days_val), child = rep(0.1, max_days_val),
    elderly = rep(0.1, max_days_val), toddler = rep(0.1, max_days_val)
  )

  result <- simulate_households(n_households = 2, max_days = max_days_val, seasonal_forcing_list = sf)

  valid_times <- result$hh_df$infection_time[!is.na(result$hh_df$infection_time)]
  if (length(valid_times) > 0) {
    expect_true(all(valid_times <= max_days_val))
    expect_true(all(valid_times >= 1))
  }
})

test_that("simulate_households handles Ct testing mode", {
  set.seed(42)
  sf <- list(
    adult = rep(0.1, 10), child = rep(0.1, 10),
    elderly = rep(0.1, 10), toddler = rep(0.1, 10)
  )

  result <- simulate_households(
    n_households = 1, max_days = 10,
    seasonal_forcing_list = sf,
    viral_testing = "Ct"
  )

  expect_s3_class(result$hh_df, "data.frame")
})

test_that("diagnostic_df has correct columns when created", {
  set.seed(42)
  sf <- list(
    adult = rep(0.1, 10), child = rep(0.1, 10),
    elderly = rep(0.1, 10), toddler = rep(0.1, 10)
  )

  result <- simulate_households(n_households = 2, max_days = 10, seasonal_forcing_list = sf)

  if (nrow(result$diagnostic_df) > 0) {
    expected <- c("hh_id", "person_id", "role", "day_index", "test_result")
    expect_true(all(expected %in% names(result$diagnostic_df)))
    expect_true(all(result$diagnostic_df$test_result %in% c(0, 1)))
  }
})

test_that("household members have valid roles", {
  set.seed(42)
  sf <- list(
    adult = rep(0.1, 10), child = rep(0.1, 10),
    elderly = rep(0.1, 10), toddler = rep(0.1, 10)
  )

  result <- simulate_households(n_households = 3, max_days = 10, seasonal_forcing_list = sf)

  valid_roles <- c("adult", "child", "toddler", "elderly")
  expect_true(all(result$hh_df$role %in% valid_roles))
})
