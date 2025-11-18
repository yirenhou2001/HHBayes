test_that("simulate_multiple_households_comm returns expected structure", {
  skip_on_cran()
  set.seed(1)

  sim <- simulate_multiple_households_comm(
    n_households = 3, alpha_comm_by_role = 5e-3,
    max_days = 15, verbose = FALSE
  )

  expect_type(sim, "list")
  expect_true(all(c("hh_df", "households") %in% names(sim)))
  expect_s3_class(sim$hh_df, "data.frame")
  expect_true(length(sim$households) == 3)

  req_cols <- c("hh_id","person_id","role","infection_time",
                "infectious_start","infectious_end","detection_time",
                "infection_resolved")
  expect_true(all(req_cols %in% names(sim$households[[1]])))

  # Attributes
  expect_true(!is.null(attr(sim$households[[1]], "test_days")) ||
                !is.null(attr(sim$households[[1]], "params")))
})

test_that("simulate_one_household_comm honors max_days and role set", {
  set.seed(123)
  hh <- simulate_one_household_comm(
    hh_id = "HHX",
    roles = c("adult","adult","child","elderly"),
    alpha_comm_by_role = 2e-3,
    max_days = 12,
    perfect_detection = TRUE
  )
  expect_s3_class(hh, "data.frame")
  expect_true(max(hh$infection_resolved, na.rm = TRUE) <= 12)
  expect_setequal(unique(hh$role), c("adult","child","elderly"))
  expect_true(is.integer(attr(hh, "test_days")))
})
