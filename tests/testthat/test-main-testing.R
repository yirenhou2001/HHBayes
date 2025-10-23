test_that("GenSyn returns the expected structure with tiny settings", {
  skip_on_cran()
  set.seed(123)

  out <- GenSyn(
    n_households = 3,
    n_runs = 1,
    Covariates = TRUE,
    postprocessing = TRUE,
    data_summary = FALSE
  )

  # top-level structure
  expect_type(out, "list")
  expect_true(all(c("results", "postprocessing") %in% names(out)))

  # results structure
  res <- out$results
  expect_true(all(c("person_day", "estimates") %in% names(res)))
  expect_s3_class(res$person_day, "data.frame")
  expect_true(is.matrix(res$estimates) || is.data.frame(res$estimates))

  # key columns in person_day
  needed_cols <- c(
    "agegrp2","agegrp3","agegrp4",
    "n_inf","n_inf_infant","n_inf_sibling","n_inf_adult","n_inf_elder",
    "cases","event","ID_indiv","ID_hh","day"
  )
  expect_true(all(needed_cols %in% names(res$person_day)))

  # post-processing exists and is data.frame-like
  expect_true(is.null(out$postprocessing) == FALSE)
  expect_true(is.data.frame(out$postprocessing) || is.matrix(out$postprocessing))
})

test_that("GenSyn errors if synthetic_data is FALSE with guidance", {
  skip_on_cran()
  expect_error(
    GenSyn(synthetic_data = FALSE),
    regexp = "TransmissionChainAnalysis",
    fixed  = FALSE
  )
})

test_that("GenSyn handles covariate options without error", {
  skip_on_cran()
  set.seed(42)

  out <- GenSyn(
    n_households = 2,
    n_runs = 1,
    Covariates = TRUE,
    Covariates_list = c("Vaccination status", "Antibody Level"),
    day_series_covariates = TRUE,
    postprocessing = FALSE
  )

  pd <- out$results$person_day
  expect_s3_class(pd, "data.frame")

  # At least the base columns must be present; covariate columns may vary by generation,
  # so we only check that the table is non-empty and has the required base columns.
  expect_gt(nrow(pd), 0)
  base_cols <- c("ID_hh","ID_indiv","day","event","cases")
  expect_true(all(base_cols %in% names(pd)))
})
