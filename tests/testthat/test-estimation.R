test_that("running_parameter_estimation returns finite parameter matrix with names", {
  skip_on_cran()
  set.seed(42)

  # tiny person-day dataset (no covariates)
  dat <- data.table::data.table(
    agegrp2 = 0L, agegrp3 = 1L, agegrp4 = 0L,
    n_inf_infant = 0L, n_inf_sibling = 1L, n_inf_adult = 0L, n_inf_elder = 0L,
    event = c(0L,1L,0L,0L,1L)
  )

  est <- running_parameter_estimation(
    long_dt = dat, n_runs = 2,
    lambda = 0.01, lambda0 = 0.2, lambda_alpha = 0.5,
    delta0_true = qlogis(0.002), alpha0_true = qlogis(0.2),
    standardize_covariates = FALSE, verbose = FALSE
  )

  expect_true(is.matrix(est))
  expect_equal(nrow(est), 2L)
  expect_true(all(is.finite(est)))
  expect_true(all(nchar(colnames(est)) > 0))
})
