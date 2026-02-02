# tests/testthat/test-helpers.R
# Unit tests for helper functions - NO STAN REQUIRED

test_that("g_rescaled returns correct shape", {
  result <- g_rescaled(1:10, peak_day = 5, width = 2)

  expect_length(result, 10)
  expect_true(all(result >= 0 & result <= 1))
  expect_equal(max(result), 1)
  expect_equal(which.max(result), 5)
})

test_that("g_rescaled handles empty input", {
  result <- g_rescaled(numeric(0), peak_day = 5, width = 2)
  expect_length(result, 0)
})

test_that("g_rescaled handles single value", {
  result <- g_rescaled(5, peak_day = 5, width = 2)
  expect_equal(result, 1)
})


test_that("simulate_viral_load_trajectory returns finite values", {

  t <- 0:14
  result <- simulate_viral_load_trajectory(
    t, v_p = 4.14, t_p = 5.09, lambda_g = 2.31, lambda_d = 2.71
  )

  expect_length(result, 15)
  expect_true(all(is.finite(result)))
})

test_that("simulate_viral_load_trajectory peaks near t_p", {
  t <- 0:20
  result <- simulate_viral_load_trajectory(
    t, v_p = 5, t_p = 7, lambda_g = 2, lambda_d = 2
  )

  peak_idx <- which.max(result)
  expect_true(abs(t[peak_idx] - 7) <= 2)
})


test_that("simulate_Ct_trajectory returns expected shape", {
  t <- 0:14
  result <- simulate_Ct_trajectory(
    t, Cpeak = 33, r = 1.5, d = 1.2, t_peak = 5
  )

  expect_length(result, 15)
  expect_true(all(is.finite(result)))

  # Ct should be lowest near t_peak
  min_idx <- which.min(result)
  expect_true(abs(t[min_idx] - 5) <= 1)
})


test_that("draw_random_VL_params returns correct structure", {
  result <- draw_random_VL_params("adult")

  expect_type(result, "list")
  expect_named(result, c("v_p", "t_p", "lambda_g", "lambda_d"))
})

test_that("draw_random_VL_params works for all roles", {
  for (role in c("adult", "child", "toddler", "elderly")) {
    result <- draw_random_VL_params(role)
    expect_length(result, 4)
  }
})

test_that("draw_random_VL_params errors on unknown role", {
  expect_error(draw_random_VL_params("teenager"), "Unknown role")
})


test_that("draw_random_Ct_params returns correct structure", {
  result <- draw_random_Ct_params("child")

  expect_type(result, "list")
  expect_named(result, c("Cpeak", "r", "d", "t_peak"))
})

test_that("draw_random_Ct_params errors on unknown role", {
  expect_error(draw_random_Ct_params("alien"), "Unknown role")
})


test_that("generate_household_roles returns valid roles", {
  set.seed(42)
  valid_roles <- c("adult", "child", "toddler", "elderly")

  for (i in 1:5) {
    roles <- generate_household_roles()
    expect_true(all(roles %in% valid_roles))
    expect_true(length(roles) >= 2)
    expect_true(sum(roles == "adult") >= 1)
  }
})

test_that("generate_household_roles respects profile", {
  set.seed(123)

  profile <- list(
    prob_single_parent = 0,
    prob_siblings = c(1, 0, 0),
    prob_elderly = c(1, 0, 0)
  )

  roles <- generate_household_roles(profile)
  expect_false("elderly" %in% roles)
  expect_false("toddler" %in% roles)
})


test_that(".norm_role normalizes common variants", {
  input <- c("Adult", "CHILD", "infant", "baby", "parent",
             "mother", "father", "elder", "grandparent", "sibling", "kid")

  result <- .norm_role(input)

  expected <- c("adult", "child", "toddler", "toddler", "adult",
                "adult", "adult", "elderly", "elderly", "child", "child")
  expect_equal(result, expected)
})

test_that(".norm_role handles whitespace", {
  result <- .norm_role(c("  adult  ", "child\t"))
  expect_equal(result, c("adult", "child"))
})


test_that("default_VL_params has expected structure", {
  expect_type(default_VL_params, "list")
  expect_named(default_VL_params, c("adult", "child", "toddler", "elderly"))

  for (role in names(default_VL_params)) {
    expect_named(default_VL_params[[role]], c("v_p", "t_p", "lambda_g", "lambda_d"))
  }
})

test_that("default_Ct_params has expected structure", {
  expect_type(default_Ct_params, "list")
  expect_true(all(c("adult", "child", "toddler", "elderly") %in% names(default_Ct_params)))
})

test_that("default_household_profile probabilities sum to 1", {
  expect_equal(sum(default_household_profile$prob_siblings), 1)
  expect_equal(sum(default_household_profile$prob_elderly), 1)
})
