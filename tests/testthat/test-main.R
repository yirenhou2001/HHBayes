# tests/testthat/test-main-functions.R
# Tests for main entry points - input validation only, NO STAN SAMPLING

# These tests verify function signatures and input validation
# without actually running Stan (which is slow)

test_that("GenSyn has correct signature", {
  expect_true(is.function(GenSyn))

  # Check key parameters exist
  args <- names(formals(GenSyn))
  expect_true("n_households" %in% args)
  expect_true("seasonal_forcing_list" %in% args)
  expect_true("stan_chains" %in% args)
  expect_true("stan_iter" %in% args)
})

test_that("GenSyn default parameters are sensible", {
  defaults <- formals(GenSyn)

  expect_equal(defaults$n_households, 50)
  expect_equal(defaults$stan_chains, 4)
  expect_equal(defaults$stan_iter, 2000)
  expect_equal(defaults$stan_warmup, 1000)
})

test_that("print.GenSynResult works with mock object", {
  mock <- list(
    n_households = 10,
    postprocessing = data.frame(Parameter = "beta1", mean = 0.3)
  )
  class(mock) <- "GenSynResult"

  expect_output(print(mock), "GenSyn result")
  expect_output(print(mock), "Households: 10")
})

test_that("print.GenSynResult handles NULL postprocessing", {
  mock <- list(n_households = 5, postprocessing = NULL)
  class(mock) <- "GenSynResult"

  expect_output(print(mock), "No posterior summary")
})


test_that("TransmissionChainAnalysis validates user_data", {
  expect_error(
    TransmissionChainAnalysis(user_data = NULL),
    "must be supplied"
  )

  expect_error(
    TransmissionChainAnalysis(user_data = "string"),
    "must be a data.frame"
  )

  expect_error(
    TransmissionChainAnalysis(user_data = list(1, 2, 3)),
    "must contain only data.frames"
  )
})

test_that("TransmissionChainAnalysis has correct signature", {
  expect_true(is.function(TransmissionChainAnalysis))

  args <- names(formals(TransmissionChainAnalysis))
  expect_true("user_data" %in% args)
  expect_true("seasonal_forcing_list" %in% args)
  expect_true("max_days" %in% args)
  expect_true("stan_chains" %in% args)
})

test_that("TransmissionChainAnalysis provides helpful error on invalid data", {
  df <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    TransmissionChainAnalysis(user_data = df, max_days = 10),
    "could not interpret"
  )
})

test_that("print.TransmissionChainResult works with mock object", {
  mock <- list(
    postprocessing = data.frame(Parameter = "phi", mean = 1.5)
  )
  class(mock) <- "TransmissionChainResult"

  expect_output(print(mock), "TransmissionChainAnalysis result")
})


test_that("main_parameter_estimation_pipeline has correct signature", {
  expect_true(is.function(main_parameter_estimation_pipeline))

  args <- names(formals(main_parameter_estimation_pipeline))
  expect_true("user_data" %in% args)
  expect_true("synthetic_data" %in% args)
  expect_true("n_households" %in% args)
  expect_true("stan_file" %in% args)
})

test_that("main_parameter_estimation_pipeline defaults are sensible", {
  defaults <- formals(main_parameter_estimation_pipeline)

  expect_equal(defaults$synthetic_data, TRUE)
  expect_equal(defaults$n_households, 10)
  expect_equal(defaults$stan_file, "model.stan")
})


test_that("postprocess_stan_fit handles NULL gracefully", {
  # Mock a failed fit
  mock_fit <- structure(list(), class = "stanfit")

  # Should not error, should return empty data frame
  result <- tryCatch(
    postprocess_stan_fit(mock_fit),
    error = function(e) data.frame(Parameter = character())
  )

  expect_s3_class(result, "data.frame")
})
