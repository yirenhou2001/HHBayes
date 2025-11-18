#' TransmissionChainAnalysis: analyze user-supplied household data
#'
#' Runs the end-to-end workflow on \emph{user-provided} data using either the
#' legacy MLE path (\code{estimation_method = "mle"}) or the RSV/VL Stan path
#' (\code{estimation_method = "stan"}). This function does not simulate data;
#' it expects input in tabular form (see Details).
#'
#' @param user_data A data.frame or a list of data.frames describing household testing
#'   or episode data (see Details for accepted shapes). Lists are row-bound internally.
#' @param estimation_method One of \code{c("mle","stan")}. Valid pairs are
#'   legacy+MLE (\code{"mle"}) and RSV/VL+Stan (\code{"stan"}).
#' @param data_summary Logical; print/retain a brief summary in print methods (see Value).
#' @param postprocessing Logical; for MLE, compute a post-processing table; for Stan, this is an alias of the posterior summary.
#' @param plots Character vector of plot names to build when compatible data are present:
#'   \code{c("daily","weekly","timeline","sar")} or \code{"all"}.
#' @param print_plots Logical; print plots if produced.
#' @param index_vl_column Optional character; name of the viral-load column used by SAR-by-VL plotting. Defaults to \code{"vl_test"} when present.
#' @param n_runs Integer; number of repeated estimation runs (MLE path only).
#' @param day_series_covariates Logical; include day-series covariates (MLE path).
#' @param series_cols Optional character vector; selected day-series columns (MLE path).
#' @param comm_covariate_cols,hh_covariate_cols Optional character vectors of likelihood-level covariates (MLE path).
#' @param hh_by_role Logical; fit separate household effects by role (MLE path).
#' @param hh_role_covariate_cols Optional named list of role-specific covariates (MLE path).
#' @param standardize_covariates Logical; standardize covariates (MLE path).
#' @param lambda_comm,lambda_hh Numeric; ridge penalties for community and household covariates (MLE path).
#' @param start_date,end_date \code{Date} study window.
#' @param latent_par,report_par,infect_par Lists of gamma parameters (shape, scale) used by imputation (MLE path).
#' @param start_par Numeric vector; optimizer starting values (MLE path).
#' @param lambda,lambda0,lambda_alpha Numeric; optimizer penalties (MLE path).
#' @param delta0_true,alpha0_true Numeric anchors on the logit scale (MLE path).
#' @param true_values Optional named numeric vector of reference/true values for post-processing (MLE path).
#' @param seasonal_forcing_list Optional named list of role vectors for forcing (Stan path).
#' @param max_days Integer; maximum modeled days (Stan path).
#' @param stan_file Path to a Stan model file.
#' @param stan_chains,stan_iter,stan_warmup Integers; Stan sampling controls.
#' @param stan_control List; passed to \code{rstan::sampling(..., control = ...)}.
#' @param stan_init Character or function; Stan initialization.
#' @param stan_refresh Integer; Stan refresh rate.
#' @param stan_cores Integer; CPU cores for Stan.
#' @param vl_mode One of \code{c("auto","from_long")} indicating how to derive VL for Stan path.
#' @param vl_source One of \code{c("column","simulate","none")} indicating the VL source for Stan path.
#' @param vl_column Optional character; the VL column when \code{vl_source="column"} (Stan path).
#' @param role_levels Character vector; canonical role levels for normalization on the Stan path.
#'
#' @details
#' \strong{Accepted input shapes}:
#' \itemize{
#'   \item Long testing table with columns like \code{HH}, \code{individual_ID}, \code{role},
#'         \code{test_date}, \code{infection_status}, and optionally a VL column.
#'   \item Per-person episode table with \code{hh_id}, \code{person_id}, \code{role},
#'         \code{infection_time}, \code{infectious_start}, \code{infectious_end},
#'         and optionally a viral-load trajectory.
#' }
#' Only two engine/method pairs are supported: legacy+MLE and RSV/VL+Stan.
#'
#' @return An object of class \code{"TransmissionChainResult"} containing:
#' \itemize{
#'   \item \code{$results}: engine-specific internals (MLE: summaries/person-day/estimates;
#'         Stan: stan_data, fit, posterior_summary).
#'   \item \code{$postprocessing}: MLE summary table or Stan posterior summary.
#'   \item \code{$plot_list}: named list of ggplot objects when built.
#'   \item \code{$data_summary}: flag indicating whether print methods preview summaries.
#' }
#'
#' @seealso \code{\link{main_parameter_estimation_pipeline}}, \code{\link{postprocessing_estimates}}
#'
#' @examples
#' \dontrun{
#' # MLE path requires full legacy preprocessing; shown here schematically:
#' # res <- TransmissionChainAnalysis(user_data = your_long_df, estimation_method = "mle")
#' }
#' @export
TransmissionChainAnalysis <- function(
    user_data,

    estimation_method = c("mle","stan"),

    data_summary   = FALSE,
    postprocessing = TRUE,

    plots          = c("daily","weekly","timeline","sar"),
    print_plots    = FALSE,
    index_vl_column = "vl_test",

    # repetitions (MLE path only)
    n_runs = 10,

    # summarization / person-day options (MLE path)
    day_series_covariates = TRUE,
    series_cols           = NULL,

    # likelihood covariate mapping (MLE path)
    comm_covariate_cols    = NULL,
    hh_covariate_cols      = NULL,
    hh_by_role             = FALSE,
    hh_role_covariate_cols = NULL,
    standardize_covariates = TRUE,

    # penalties (MLE path)
    lambda_comm = 0.01,
    lambda_hh   = 0.01,

    # analysis window
    start_date = as.Date("2024-09-21"),
    end_date   = as.Date("2025-04-17"),

    # imputation gamma params (MLE path)
    latent_par = list(shape = 2, scale = 1),
    report_par = list(shape = 1, scale = 1.5),
    infect_par = list(shape = 3, scale = 2),

    # optimizer penalties / anchors (MLE path)
    start_par    = c(-6, 0.02, -2, rep(0, 6)),
    lambda       = 0.01,
    lambda0      = 0.2,
    lambda_alpha = 5,
    delta0_true  = qlogis(0.002),
    alpha0_true  = qlogis(0.2),

    # post-processing reference values (MLE)
    true_values = NULL,

    # RSV/VL engine inputs (Stan path)
    seasonal_forcing_list = NULL,
    max_days = 365,

    # Stan controls
    stan_file    = "HH_parameter_estimation2.stan",
    stan_chains = 1, stan_iter = 100, stan_warmup = 100,
    stan_control = list(adapt_delta = 0.99, max_treedepth = 20),
    stan_init = "random", stan_refresh = 50, stan_cores = 4,

    # Stan: how to obtain viral loads from user long data (if needed)
    vl_mode   = c("auto","from_long"),
    vl_source = c("column","simulate","none"),
    vl_column = NULL,
    role_levels = c("adult","child","elderly","toddler")
) {
  if (is.null(user_data)) {
    stop("`user_data` must be supplied to TransmissionChainAnalysis().", call. = FALSE)
  }

  # Allow list of data.frames but not other list types
  if (is.list(user_data) && !is.data.frame(user_data)) {
    ok <- all(vapply(user_data, is.data.frame, logical(1)))
    if (!ok) {
      stop("If `user_data` is a list, it must contain only data.frames.", call. = FALSE)
    }
    user_data <- data.table::rbindlist(user_data, use.names = TRUE, fill = TRUE)
  }
  if (!is.data.frame(user_data)) {
    stop("`user_data` must be a data.frame (or a list of data.frames).", call. = FALSE)
  }

  estimation_method <- match.arg(estimation_method)
  vl_mode   <- match.arg(vl_mode)
  vl_source <- match.arg(vl_source)

  # ------------------------------------------------------------------
  # Path 1: Stan (RSV/VL engine)
  # ------------------------------------------------------------------
  if (estimation_method == "stan") {

    # Normalize roles (long-format user_data)
    if ("role" %in% names(user_data)) {
      if (exists(".norm_role", mode = "function")) {
        user_data$role <- .norm_role(user_data$role)
      }
    }

    households <- tryCatch(
      prepare_stan_households_from_user_data(
        user_data   = user_data,
        role_levels = role_levels,
        vl_mode     = vl_mode,
        vl_source   = vl_source,
        vl_column   = vl_column,
        start_date  = start_date,
        end_date    = end_date
      ),
      error = function(e) {
        msg <- paste0(
          "TransmissionChainAnalysis (estimation_method = 'stan') could not interpret `user_data`.\n\n",
          "Please ensure that your data are either:\n",
          "  (a) a long household-testing table with columns such as HH, individual_ID,\n",
          "      role, test_date, infection_status (and optionally a viral-load column);\n",
          "  or\n",
          "  (b) a per-person table with columns such as hh_id, person_id, role,\n",
          "      infection_time, infectious_start, infectious_end, and optionally\n",
          "      vl_full_trajectory.\n\n",
          "Original error from prepare_stan_households_from_user_data():\n  ",
          conditionMessage(e)
        )
        stop(msg, call. = FALSE)
      }
    )

    stan_data <- build_stan_household_arrays(
      households = households,
      T_max = max_days,
      seasonal_forcing_list = seasonal_forcing_list
    )

    fit <- run_household_stan(
      stan_data,
      stan_file = stan_file,
      chains    = stan_chains,
      iter      = stan_iter,
      warmup    = stan_warmup,
      control   = stan_control,
      init      = stan_init,
      refresh   = stan_refresh,
      cores     = stan_cores
    )

    posterior_summary <- postprocess_stan_fit(fit)

    # For consistency with GenSyn: top-level postprocessing == posterior_summary
    postprocessing_tbl <- posterior_summary

    # Optional plots: we need summarized_data + person_day. If you later wire in
    # a helper like .stan_make_summary_from_households(), you can drop it here.

    out <- list(
      call              = match.call(),
      estimation_method = estimation_method,
      data_summary      = data_summary,
      results = list(
        raw_simulation    = households,
        stan_data         = stan_data,
        fit               = fit,
        posterior_summary = posterior_summary
      ),
      postprocessing = postprocessing_tbl,
      plots          = list()
    )
    class(out) <- "TransmissionChainResult"
    return(out)
  }

  # ------------------------------------------------------------------
  # Path 2: Legacy + MLE
  # ------------------------------------------------------------------
  user_data_mle <- .validate_user_data_mle(user_data, start_date = start_date)

  results <- main_parameter_estimation_pipeline(
    user_data      = user_data_mle,
    synthetic_data = FALSE,
    engine         = "legacy",
    estimation_method = "mle",
    n_households   = NA_integer_,  # ignored when synthetic_data = FALSE
    n_runs         = n_runs,
    hh.size        = NA_integer_,  # ignored
    tests.per.week = NA_integer_,  # ignored

    day_series_covariates = day_series_covariates,
    series_cols           = series_cols,
    comm_covariate_cols    = comm_covariate_cols,
    hh_covariate_cols      = hh_covariate_cols,
    hh_by_role             = hh_by_role,
    hh_role_covariate_cols = hh_role_covariate_cols,
    standardize_covariates = standardize_covariates,
    lambda_comm = lambda_comm,
    lambda_hh   = lambda_hh,

    start_date = start_date,
    end_date   = end_date,

    latent_par = latent_par,
    report_par = report_par,
    infect_par = infect_par,

    start_par    = start_par,
    lambda       = lambda,
    lambda0      = lambda0,
    lambda_alpha = lambda_alpha,
    delta0_true  = delta0_true,
    alpha0_true  = alpha0_true,

    # RSV/VL args unused in legacy+mle path
    seasonal_forcing_list = NULL,
    max_days              = max_days,
    stan_file             = stan_file,
    stan_chains           = stan_chains,
    stan_iter             = stan_iter,
    stan_warmup           = stan_warmup,
    stan_control          = stan_control,
    stan_init             = stan_init,
    stan_refresh          = stan_refresh,
    stan_cores            = stan_cores
  )

  # Post-processing table for MLE
  post_tbl <- NULL
  if (isTRUE(postprocessing) && !is.null(results$estimates) && is.matrix(results$estimates)) {
    post_tbl <- postprocessing_estimates(results$estimates, true_values = true_values)
  }

  # Optional plots (requires summarized_data + person_day)
  plot_list <- list()
  if (!is.null(results$summarized_data) &&
      !is.null(results$person_day) &&
      length(plots)) {
    which_plots <- if (identical(plots, "all")) c("daily","weekly","sar","timeline") else plots
    plot_list <- .make_transmission_plots(
      results,
      which           = which_plots,
      print           = print_plots,
      index_vl_column = index_vl_column
    )
  }

  out <- list(
    call              = match.call(),
    estimation_method = estimation_method,
    data_summary      = data_summary,
    results           = results,
    postprocessing    = post_tbl,
    plots             = plots,
    plot_list         = plot_list
  )
  class(out) <- "TransmissionChainResult"
  out
}



#' Print a TransmissionChainResult
#'
#' Nicely prints sections available in a \code{TransmissionChainResult} returned by
#' \code{\link{TransmissionChainAnalysis}}. For MLE, shows the post-processing
#' table (if present) and, optionally, a preview of the individual-level summary.
#' For Stan, shows the Stan posterior summary (or a preview) when available.
#'
#' @param x A \code{TransmissionChainResult} object.
#' @param ... Passed to or from other methods (unused).
#'
#' @return \code{x}, returned invisibly.
#'
#' @seealso \code{\link{TransmissionChainAnalysis}}
#' @exportS3Method print TransmissionChainResult
print.TransmissionChainResult <- function(x, ...) {
  cat("TransmissionChainAnalysis result\n\n")
  cat("Estimation method: ", x$estimation_method, "\n\n", sep = "")

  ## Post-processing / posterior summary
  if (!is.null(x$postprocessing)) {
    cat("--- Post-processing / summary of estimates ---\n")
    print(x$postprocessing)
  } else {
    cat("(No post-processing table stored in `x$postprocessing`.)\n")
  }

  ## MLE-specific data summary preview
  if (x$estimation_method == "mle") {
    if (isTRUE(x$data_summary) && !is.null(x$results$summarized_data)) {
      cat("\n--- Individual-level summary (first 10 rows) ---\n")
      print(utils::head(x$results$summarized_data, 10))
    } else if (!isTRUE(x$data_summary)) {
      cat("\n(Data summary not requested; call TransmissionChainAnalysis(..., data_summary = TRUE)\n",
          "to print a preview of `results$summarized_data` here.)\n", sep = "")
    } else if (is.null(x$results$summarized_data)) {
      cat("\n(Data summary was requested but `results$summarized_data` is NULL.)\n")
    }
  }

  ## Stan-specific preview (posterior_summary is already in postprocessing)
  if (x$estimation_method == "stan") {
    if (isTRUE(x$data_summary) && !is.null(x$postprocessing)) {
      cat("\n--- Stan posterior summary (first 10 rows) ---\n")
      print(utils::head(x$postprocessing, 10))
    } else if (!isTRUE(x$data_summary)) {
      cat("\n(Posterior summary stored in `x$postprocessing`)\n")
    }
  }

  if (x$estimation_method == "mle") {
    cat("\nPlots are only available with dataset for 'stan' estimation method.\n")
  }
  if (x$estimation_method == "stan") {
    cat("\nData summary are only available with dataset for 'mle' estimation method.\n")
  }
  invisible(x)
}
