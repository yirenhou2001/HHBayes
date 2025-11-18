#' GenSyn: household simulation & estimation wrapper
#'
#' Runs \code{\link{main_parameter_estimation_pipeline}()} to either simulate
#' (or ingest) household data, fit via the legacy MLE path or the RSV/VL+Stan
#' path, and assemble a \code{"GenSynResult"}.
#'
#' @param n_households Integer; number of households to simulate when \code{synthetic_data=TRUE}.
#' @param n_runs Integer; number of repeated MLE runs (legacy path only).
#' @param data_summary Logical (legacy+MLE only); if \code{TRUE}, per-individual summary is retained/printed.
#' @param print_plots Logical; print plots if produced.
#' @param plots Character vector of plot names (\code{"daily"}, \code{"weekly"}, \code{"sar"}, \code{"timeline"}) or \code{"all"}.
#' @param engine Character; one of \code{c("legacy","rsv_vl")}.
#' @param estimation_method Character; one of \code{c("mle","stan")}.
#' @param index_vl_column Character; viral-load column name for plotting on Stan path (default \code{"vl_test"}).
#' @param user_data Optional user data (see \code{\link{main_parameter_estimation_pipeline}}); ignored if \code{synthetic_data=TRUE}.
#' @param synthetic_data Logical; simulate data internally.
#' @param hh.size Integer; household size used in simulation.
#' @param tests.per.week Integer; tests per person per week (legacy engine).
#' @param Covariates Logical; include synthetic covariates (legacy).
#' @param Covariates_list Character vector; names of covariates (legacy).
#' @param Covariate_specifications Optional list describing covariate generation (legacy).
#' @param day_series_covariates Logical; include day-series covariates (legacy+MLE).
#' @param series_cols Optional character vector; selected day-series columns (legacy+MLE).
#' @param comm_covariate_cols,hh_covariate_cols Optional character vectors; likelihood-level covariates (legacy+MLE).
#' @param hh_by_role Logical; fit separate HH effects by role (legacy+MLE).
#' @param hh_role_covariate_cols Optional named list of role-specific covariates (legacy+MLE).
#' @param standardize_covariates Logical; standardize covariates (legacy+MLE).
#' @param lambda_comm,lambda_hh Numeric; ridge penalties for community/household covariates (legacy+MLE).
#' @param p.comm.base.infant.fix,p.comm.multiplier.sibling,p.comm.multiplier.parent,p.comm.multiplier.elder Numeric; community risk knobs (legacy simulator).
#' @param p.hh.base.infant,p.hh.multiplier.sibling,p.hh.multiplier.parent,p.hh.multiplier.elder Numeric; within-HH knobs (legacy simulator).
#' @param p.imm.base.sibling,p.imm.base.parent,p.imm.base.elder,partial.immunity.infant,partial.immunity.sibling,partial.immunity.parent,partial.immunity.elder Numeric; immunity knobs (legacy simulator).
#' @param duration.latent,duration.infect.inf,multiplier.dur.sibpar Numeric; timing knobs (legacy simulator).
#' @param p.detect Numeric; detection probability (legacy simulator).
#' @param amplitude,phase Numeric; seasonality controls (legacy simulator).
#' @param start_date,end_date \code{Date}; study window.
#' @param latent_par,report_par,infect_par Lists of gamma parameters (shape, scale) for imputation (legacy+MLE).
#' @param start_par Numeric vector; optimizer starting values (legacy+MLE).
#' @param lambda,lambda0,lambda_alpha Numeric; optimizer penalties (legacy+MLE).
#' @param delta0_true,alpha0_true Numeric; reference values on logit scale (legacy+MLE).
#' @param seasonal_forcing_list Optional named list of role vectors; seasonal forcing (RSV/VL+Stan).
#' @param max_days Integer; maximum simulated days (RSV/VL+Stan).
#' @param stan_file Path to a Stan model file.
#' @param stan_chains,stan_iter,stan_warmup Integers; Stan sampling controls.
#' @param stan_control List; Stan \code{control} list.
#' @param stan_init Character or function; Stan initialization.
#' @param stan_refresh Integer; Stan refresh rate.
#' @param stan_cores Integer; CPU cores for Stan.
#'
#' @return A \code{"GenSynResult"} list with elements: \code{$call}, \code{$engine},
#' \code{$estimation_method}, \code{$n_households}, \code{$n_runs}, \code{$results},
#' \code{$postprocessing}, \code{$plot_list}.
#'
#' @details Arguments are forwarded to \code{\link{main_parameter_estimation_pipeline}()}; some are ignored depending on \code{engine}/\code{estimation_method}.
#' @seealso \code{\link{main_parameter_estimation_pipeline}}, \code{\link{postprocessing_estimates}}
#' @examples
#' \donttest{
#' # Lightweight MLE example (no Stan):
#' set.seed(1)
#' fit <- GenSyn(
#'   n_households = 3,
#'   engine = "legacy", estimation_method = "mle",
#'   n_runs = 2, print_plots = FALSE
#' )
#' class(fit)
#' }
#' \dontrun{
#' # Heavier Stan example (do not run on CRAN):
#' seasonal_forcing_list <- list(
#'   adult=rep(1,60), child=rep(1,60), elderly=rep(1,60), toddler=rep(1,60)
#' )
#' fit2 <- GenSyn(
#'   n_households=2,
#'   engine="rsv_vl", estimation_method="stan",
#'   seasonal_forcing_list=seasonal_forcing_list, max_days=60,
#'   stan_chains=1, stan_iter=200, stan_warmup=100, stan_cores=1
#' )
#' }
#' @export
GenSyn <- function(
    n_households       = 10,
    n_runs             = 10,
    data_summary       = FALSE,
    print_plots        = TRUE,
    plots              = c("daily","weekly","timeline","sar"),
    engine             = c("legacy","rsv_vl"),
    estimation_method  = c("mle","stan"),
    index_vl_column = "vl_test",


    # ---- data source / simulation flags ----
    user_data          = NULL,
    synthetic_data     = TRUE,

    # ---- simulation knobs (legacy + RSV/VL) ----
    hh.size            = sample(3:7, 1),
    tests.per.week     = 1,

    # ---- synthetic covariates (legacy) ----
    Covariates              = FALSE,
    Covariates_list         = c("Vaccination status", "Antibody Level"),
    Covariate_specifications = NULL,

    # ---- person-day options (legacy + MLE) ----
    day_series_covariates = TRUE,
    series_cols           = NULL,

    # ---- likelihood covariates (legacy + MLE) ----
    comm_covariate_cols      = NULL,
    hh_covariate_cols        = NULL,
    hh_by_role               = FALSE,
    hh_role_covariate_cols   = NULL,
    standardize_covariates   = TRUE,
    lambda_comm              = 0.01,
    lambda_hh                = 0.01,

    # ---- community / HH knobs (legacy engine) ----
    p.comm.base.infant.fix   = 0.002,
    p.comm.multiplier.sibling = 1,
    p.comm.multiplier.parent  = 1,
    p.comm.multiplier.elder   = 1,

    p.hh.base.infant         = 0.2,
    p.hh.multiplier.sibling  = 5.267686e-01,
    p.hh.multiplier.parent   = 8.008933e-01,
    p.hh.multiplier.elder    = 6.008933e-01,

    p.imm.base.sibling       = 1e-10,
    p.imm.base.parent        = 1e-10,
    p.imm.base.elder         = 1e-10,

    partial.immunity.infant  = 1e-10,
    partial.immunity.sibling = 1e-10,
    partial.immunity.parent  = 1e-10,
    partial.immunity.elder   = 1e-10,

    duration.latent          = 1,
    duration.infect.inf      = 2,
    multiplier.dur.sibpar    = 0.5,
    p.detect                 = 0.999,

    # ---- seasonality / window ----
    amplitude = 2.65810 * 0,
    phase     = -0.408,
    start_date = as.Date("2024-09-21"),
    end_date   = as.Date("2025-04-17"),

    # ---- imputation params (legacy + MLE) ----
    latent_par = list(shape = 2, scale = 1),
    report_par = list(shape = 1, scale = 1.5),
    infect_par = list(shape = 3, scale = 2),

    # ---- optimizer (legacy + MLE) ----
    start_par   = c(-6, 0.02, -2, rep(0, 6)),
    lambda      = 0.01,
    lambda0     = 0.2,
    lambda_alpha = 5,
    delta0_true  = qlogis(0.002),
    alpha0_true  = qlogis(0.2),

    # ---- RSV/VL inputs ----
    seasonal_forcing_list = NULL,
    max_days              = 365,

    # ---- Stan controls (RSV/VL + Stan) ----
    stan_file    = "../inst/stan/HH_parameter_estimation2.stan",
    stan_chains  = 1,
    stan_iter    = 100,
    stan_warmup  = 100,
    stan_control = list(adapt_delta = 0.99, max_treedepth = 20),
    stan_init    = "random",
    stan_refresh = 50,
    stan_cores   = 4
) {


  engine            <- match.arg(engine)
  estimation_method <- match.arg(estimation_method)

  # ------------------------------------------------------------------
  # Run the main pipeline (no ellipsis; all args explicit)
  # ------------------------------------------------------------------
  results <- main_parameter_estimation_pipeline(
    user_data          = user_data,
    synthetic_data     = synthetic_data,
    engine             = engine,
    estimation_method  = estimation_method,
    n_households       = n_households,
    n_runs             = n_runs,
    hh.size            = hh.size,
    tests.per.week     = tests.per.week,
    Covariates         = Covariates,
    Covariates_list    = Covariates_list,
    Covariate_specifications = Covariate_specifications,
    day_series_covariates    = day_series_covariates,
    series_cols              = series_cols,
    comm_covariate_cols      = comm_covariate_cols,
    hh_covariate_cols        = hh_covariate_cols,
    hh_by_role               = hh_by_role,
    hh_role_covariate_cols   = hh_role_covariate_cols,
    standardize_covariates   = standardize_covariates,
    lambda_comm              = lambda_comm,
    lambda_hh                = lambda_hh,
    p.comm.base.infant.fix   = p.comm.base.infant.fix,
    p.comm.multiplier.sibling = p.comm.multiplier.sibling,
    p.comm.multiplier.parent  = p.comm.multiplier.parent,
    p.comm.multiplier.elder   = p.comm.multiplier.elder,
    p.hh.base.infant          = p.hh.base.infant,
    p.hh.multiplier.sibling   = p.hh.multiplier.sibling,
    p.hh.multiplier.parent    = p.hh.multiplier.parent,
    p.hh.multiplier.elder     = p.hh.multiplier.elder,
    p.imm.base.sibling        = p.imm.base.sibling,
    p.imm.base.parent         = p.imm.base.parent,
    p.imm.base.elder          = p.imm.base.elder,
    partial.immunity.infant   = partial.immunity.infant,
    partial.immunity.sibling  = partial.immunity.sibling,
    partial.immunity.parent   = partial.immunity.parent,
    partial.immunity.elder    = partial.immunity.elder,
    duration.latent           = duration.latent,
    duration.infect.inf       = duration.infect.inf,
    multiplier.dur.sibpar     = multiplier.dur.sibpar,
    p.detect                  = p.detect,
    amplitude                 = amplitude,
    phase                     = phase,
    start_date                = start_date,
    end_date                  = end_date,
    latent_par                = latent_par,
    report_par                = report_par,
    infect_par                = infect_par,
    start_par                 = start_par,
    lambda                    = lambda,
    lambda0                   = lambda0,
    lambda_alpha              = lambda_alpha,
    delta0_true               = delta0_true,
    alpha0_true               = alpha0_true,
    seasonal_forcing_list     = seasonal_forcing_list,
    max_days                  = max_days,
    stan_file                 = stan_file,
    stan_chains               = stan_chains,
    stan_iter                 = stan_iter,
    stan_warmup               = stan_warmup,
    stan_control              = stan_control,
    stan_init                 = stan_init,
    stan_refresh              = stan_refresh,
    stan_cores                = stan_cores
  )

  if (engine == "legacy" && estimation_method == "mle") {
    if (isTRUE(data_summary) && !is.null(results$summarized_data)) {
      summarized_data <- results$summarized_data
    }
  }

  # ------------------------------------------------------------------
  # Define the top-level `postprocessing` element
  # ------------------------------------------------------------------
  if (engine == "rsv_vl" && estimation_method == "stan") {
    # EXACTLY what you requested:
    #   result$postprocessing == result$results$posterior_summary
    postprocessing <- results$posterior_summary
    summarized_data <- NULL
  } else if (engine == "legacy" && estimation_method == "mle") {
    if (!is.null(results$estimates)) {
      postprocessing <- postprocessing_estimates(results$estimates)
    } else {
      postprocessing <- NULL
    }
  } else {
    postprocessing <- NULL
  }

  plot_list <- list()
  if (engine == "rsv_vl" && estimation_method == "stan" && length(plots)) {
    plot_list <- .make_transmission_plots(
      results,
      which = plots,
      print = print_plots,
      index_vl_column = index_vl_column
    )
  }
  # (Optional plotting for legacy path could go here, if you still want it.)

  # ------------------------------------------------------------------
  # Assemble and return GenSynResult object
  # ------------------------------------------------------------------
  out <- list(
    call              = match.call(),
    engine            = engine,
    estimation_method = estimation_method,
    n_households      = n_households,
    n_runs            = n_runs,
    data_summary      = data_summary,   # <- important
    plots             = plots,
    plot_list         = plot_list,
    results           = results,
    postprocessing    = postprocessing
  )
  class(out) <- "GenSynResult"
  out
}



#' Print method for \code{GenSynResult}
#'
#' Formats a \code{GenSynResult} for display. Prints engine/method,
#' basic run info, any stored post-processing table, and (legacy+MLE only)
#' a short individual-level preview when \code{x$data_summary} is \code{TRUE}.
#'
#' @param x A \code{GenSynResult} object (from \code{\link{GenSyn}}).
#' @param ... Unused.
#'
#' @return \code{x}, invisibly.
#'
#' @seealso \code{\link{GenSyn}}
#'
#' @exportS3Method print GenSynResult
print.GenSynResult <- function(x, ...) {
  cat("GenSyn result\n\n")
  cat("Engine: ", x$engine,
      "   Estimation: ", x$estimation_method, "\n", sep = "")
  cat("Households: ", x$n_households,
      "   Runs: ", x$n_runs, "\n\n", sep = "")

  ## Post-processing summary
  if (!is.null(x$postprocessing)) {
    cat("--- Post-processing of estimates ---\n")
    print(x$postprocessing)
  } else {
    cat("(No post-processing summary stored in `x$postprocessing`.)\n")
  }

  ## Optional data summary
  if (x$engine == "legacy" && x$estimation_method == "mle") {
    if (isTRUE(x$data_summary) && !is.null(x$results$summarized_data)) {
      cat("\n--- Individual-level summary (first 10 rows) ---\n")
      print(utils::head(x$results$summarized_data, 10))
    } else if (!isTRUE(x$data_summary)) {
      cat("\n(Data summary not requested; call GenSyn(..., data_summary = TRUE) ",
          "to print a preview here.)\n", sep = "")
    } else if (is.null(x$results$summarized_data)) {
      cat("\n(Data summary requested but `results$summarized_data` is NULL.)\n")
    }
  } else if (x$engine == "rsv_vl" && x$estimation_method == "stan") {
      cat("\n(Posterior summary stored in `x$postprocessing`)\n")
    }
  if (x$estimation_method == "mle") {
    cat("\nPlots are only available with dataset for 'stan' estimation method.\n")
  }

  if (x$estimation_method == "stan") {
    cat("\nData summary are only available with dataset for 'mle' estimation method.\n")
  }
  invisible(x)
}
