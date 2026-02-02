# tests/testthat/test-plots.R
# Tests for plotting functions - NO STAN REQUIRED

test_that(".rsv_plot_daily returns ggplot", {
  hh1 <- data.frame(
    hh_id = "HH1", person_id = 1:3, role = c("adult", "child", "elderly"),
    infection_time = c(5, 10, NA)
  )
  hh2 <- data.frame(
    hh_id = "HH2", person_id = 1:2, role = c("adult", "toddler"),
    infection_time = c(3, 7)
  )

  result <- .rsv_plot_daily(list(hh1, hh2))
  expect_s3_class(result, "ggplot")
})

test_that(".rsv_plot_weekly returns ggplot", {
  hh1 <- data.frame(
    hh_id = "HH1", person_id = 1:3, role = c("adult", "child", "elderly"),
    infection_time = c(5, 15, 25)
  )

  result <- .rsv_plot_weekly(list(hh1))
  expect_s3_class(result, "ggplot")
})

test_that(".rsv_plot_timeline returns ggplot", {
  hh1 <- data.frame(
    hh_id = "HH1", person_id = 1:2, role = c("adult", "child"),
    infection_time = c(5, 10), detection_time = c(6, 11)
  )

  result <- .rsv_plot_timeline(list(hh1))
  expect_s3_class(result, "ggplot")
})

test_that(".rsv_plot_timeline respects max_hh", {
  households <- lapply(1:20, function(i) {
    data.frame(
      hh_id = paste0("HH", i), person_id = 1:2, role = c("adult", "child"),
      infection_time = c(i, i + 5), detection_time = c(i + 1, i + 6)
    )
  })

  result <- .rsv_plot_timeline(households, max_hh = 5)
  expect_s3_class(result, "ggplot")
})

test_that(".rsv_plot_sar_by_index_vl returns ggplot", {
  hh1 <- data.frame(
    hh_id = "HH1", person_id = 1:3, role = c("adult", "child", "elderly"),
    infection_time = c(5, 10, NA), detection_time = c(6, 11, NA)
  )
  hh1$viral_loads_test_days <- list(c("6" = 4.5), c("11" = 3.2), numeric(0))

  hh2 <- data.frame(
    hh_id = "HH2", person_id = 1:2, role = c("adult", "child"),
    infection_time = c(3, 8), detection_time = c(4, 9)
  )
  hh2$viral_loads_test_days <- list(c("4" = 5.5), c("9" = 4.0))

  result <- .rsv_plot_sar_by_index_vl(list(hh1, hh2))
  expect_s3_class(result, "ggplot")
})

test_that(".make_transmission_plots returns named list", {
  hh1 <- data.frame(
    hh_id = "HH1", person_id = 1:2, role = c("adult", "child"),
    infection_time = c(5, 10), detection_time = c(6, 11)
  )
  hh1$viral_loads_test_days <- list(c("6" = 4.5), c("11" = 3.2))

  mock_results <- list(households = list(hh1))

  result <- .make_transmission_plots(mock_results, which = c("daily", "weekly"), print = FALSE)

  expect_type(result, "list")
})

test_that(".make_transmission_plots handles 'all'", {
  hh1 <- data.frame(
    hh_id = "HH1", person_id = 1:2, role = c("adult", "child"),
    infection_time = c(5, 10), detection_time = c(6, 11)
  )
  hh1$viral_loads_test_days <- list(c("6" = 4.5), c("11" = 3.2))

  result <- .make_transmission_plots(list(households = list(hh1)), which = "all", print = FALSE)
  expect_type(result, "list")
})

test_that(".make_transmission_plots handles missing households", {
  result <- .make_transmission_plots(
    list(raw_simulation = NULL, households = NULL),
    which = "daily", print = FALSE
  )

  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("plots handle empty infection times", {
  hh <- data.frame(
    hh_id = "HH1", person_id = 1:2, role = c("adult", "child"),
    infection_time = c(NA, NA)
  )

  result <- .rsv_plot_daily(list(hh))
  expect_s3_class(result, "ggplot")
})

test_that("plots handle single household", {
  hh <- data.frame(
    hh_id = "HH1", person_id = 1, role = "adult",
    infection_time = 5, detection_time = 6
  )

  expect_s3_class(.rsv_plot_daily(list(hh)), "ggplot")
  expect_s3_class(.rsv_plot_weekly(list(hh)), "ggplot")
})
