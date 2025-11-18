#' Main parameter estimation pipeline (simulate or analyze)
#'
#' End-to-end workflow for household transmission estimation via legacy MLE
#' or RSV/VL+Stan. Works with simulated or user-provided data.
#'
#' @param user_data Optional; data.frame or list of data.frames as described in
#'   \code{\link{TransmissionChainAnalysis}}.
#' @param synthetic_data Logical; if \code{TRUE}, simulate data internally.
#' @param engine Character; \code{"legacy"} or \code{"rsv_vl"}.
#' @param estimation_method Character; \code{"mle"} or \code{"stan"} (valid pairs:
#'   \code{legacy+mle}, \code{rsv_vl+stan}).
#' @param n_households,n_runs Integers; simulation size and number of repeated runs (MLE only).
#' @param hh.size,tests.per.week Integers; simulation controls (legacy).
#' @param Covariates Logical; include synthetic covariates (legacy).
#' @param Covariates_list Character vector; covariate names (legacy).
#' @param Covariate_specifications Optional list; covariate generation controls (legacy).
#' @param day_series_covariates Logical; include day-series covariates (legacy+MLE).
#' @param series_cols Optional character vector; day-series columns (legacy+MLE).
#' @param comm_covariate_cols,hh_covariate_cols Optional character vectors; likelihood-level covariates (legacy+MLE).
#' @param hh_by_role Logical; fit separate HH effects by role (legacy+MLE).
#' @param hh_role_covariate_cols Optional named list; role-specific covariates (legacy+MLE).
#' @param standardize_covariates Logical; standardize covariates (legacy+MLE).
#' @param lambda_comm,lambda_hh Numeric; ridge penalties (legacy+MLE).
#' @param p.comm.base.infant.fix,p.comm.multiplier.sibling,p.comm.multiplier.parent,p.comm.multiplier.elder Numeric; community knobs (legacy).
#' @param p.hh.base.infant,p.hh.multiplier.sibling,p.hh.multiplier.parent,p.hh.multiplier.elder Numeric; within-HH knobs (legacy).
#' @param p.imm.base.sibling,p.imm.base.parent,p.imm.base.elder,partial.immunity.infant,partial.immunity.sibling,partial.immunity.parent,partial.immunity.elder Numeric; immunity knobs (legacy).
#' @param duration.latent,duration.infect.inf,multiplier.dur.sibpar Numeric; timing knobs (legacy).
#' @param p.detect Numeric; detection probability (legacy).
#' @param amplitude,phase Numeric; seasonality controls (legacy).
#' @param start_date,end_date \code{Date}; analysis window.
#' @param latent_par,report_par,infect_par Lists (shape, scale) for imputation (legacy+MLE).
#' @param start_par Numeric; optimizer start (legacy+MLE).
#' @param lambda,lambda0,lambda_alpha Numeric; optimizer penalties (legacy+MLE).
#' @param delta0_true,alpha0_true Numeric; reference values on logit scale (legacy+MLE).
#' @param seasonal_forcing_list Optional role-named list for seasonality (RSV/VL+Stan).
#' @param max_days Integer; maximum days (RSV/VL+Stan).
#' @param stan_file Path to a Stan model file.
#' @param stan_chains,stan_iter,stan_warmup Integers; Stan sampling controls.
#' @param stan_control List; Stan \code{control} list.
#' @param stan_init Character or function; Stan initialization.
#' @param stan_refresh Integer; Stan refresh.
#' @param stan_cores Integer; CPU cores for Stan.
#'
#' @details Enforces valid engine/estimation pairs; other combinations error.
#'
#' @return For legacy+MLE: list with \code{raw_simulation}, \code{summarized_data},
#' \code{person_day}, \code{estimates}, and \code{postprocessing}.
#' For RSV/VL+Stan: list with \code{raw_simulation}, \code{stan_data}, \code{fit},
#' \code{posterior_summary} (also in \code{postprocessing}), and \code{NULL} placeholders for
#' \code{summarized_data}/\code{person_day}.
#'
#' @seealso \code{\link{TransmissionChainAnalysis}}, \code{\link{postprocessing_estimates}},
#'   \code{\link{build_stan_household_arrays}}, \code{\link{run_household_stan}}
main_parameter_estimation_pipeline <- function(
    user_data = NULL,
    synthetic_data = TRUE,

    # Engines & estimation (valid pairs only: legacy+mle OR rsv_vl+stan)
    engine = c("legacy","rsv_vl"),
    estimation_method = c("mle","stan"),

    n_households = 10,
    n_runs = 10,

    hh.size = sample(3:7,1),
    tests.per.week = 1,

    # Synthetic covariates (legacy)
    Covariates = FALSE,
    Covariates_list = c("Vaccination status", "Antibody Level"),
    Covariate_specifications = NULL,

    # Person-day options (legacy+mle)
    day_series_covariates = TRUE,
    series_cols = NULL,

    # Likelihood covariates (legacy+mle)
    comm_covariate_cols = NULL,
    hh_covariate_cols   = NULL,
    hh_by_role          = FALSE,
    hh_role_covariate_cols = NULL,
    standardize_covariates = TRUE,
    lambda_comm = 0.01,
    lambda_hh   = 0.01,

    # Community/HH knobs (legacy)
    p.comm.base.infant.fix = 0.002,
    p.comm.multiplier.sibling = 1,
    p.comm.multiplier.parent = 1,
    p.comm.multiplier.elder = 1,

    p.hh.base.infant = 0.2,
    p.hh.multiplier.sibling = 5.267686e-01,
    p.hh.multiplier.parent  = 8.008933e-01,
    p.hh.multiplier.elder   = 6.008933e-01,

    p.imm.base.sibling = 1e-10,
    p.imm.base.parent  = 1e-10,
    p.imm.base.elder   = 1e-10,

    partial.immunity.infant = 1e-10,
    partial.immunity.sibling = 1e-10,
    partial.immunity.parent  = 1e-10,
    partial.immunity.elder   = 1e-10,

    duration.latent = 1,
    duration.infect.inf = 2,
    multiplier.dur.sibpar = 0.5,
    p.detect = 0.999,

    # Seasonality / window
    amplitude = 2.65810*0,
    phase = -0.408,
    start_date = as.Date("2024-09-21"),
    end_date   = as.Date("2025-04-17"),

    # Imputation params (legacy+mle)
    latent_par = list(shape = 2, scale = 1),
    report_par = list(shape = 1, scale = 1.5),
    infect_par = list(shape = 3, scale = 2),

    # Optimizer (legacy+mle)
    start_par = c(-6, 0.02, -2, rep(0, 6)),
    lambda = 0.01,
    lambda0 = 0.2,
    lambda_alpha = 5,
    delta0_true = qlogis(0.002),
    alpha0_true = qlogis(0.2),

    # RSV/VL inputs
    seasonal_forcing_list = NULL,
    max_days = 365,

    # Stan controls (rsv_vl+stan)
    stan_file = "HH_parameter_estimation2.stan",
    stan_chains = 1, stan_iter = 100, stan_warmup = 100,
    stan_control = list(adapt_delta = 0.99, max_treedepth = 20),
    stan_init = "random", stan_refresh = 50, stan_cores = 4
) {
  engine <- match.arg(engine)
  estimation_method <- match.arg(estimation_method)

  # Enforce valid engine/estimation pairs
  valid_pair <- (engine == "legacy"  && estimation_method == "mle") ||
    (engine == "rsv_vl" && estimation_method == "stan")
  if (!valid_pair) {
    stop(sprintf(
      "Invalid combination: engine='%s' with estimation_method='%s'. Valid pairs: (legacy+mle) OR (rsv_vl+stan).",
      engine, estimation_method
    ), call. = FALSE)
  }

  # ------------------------------------------------------------------
  # Data generation / ingestion
  # ------------------------------------------------------------------
  if (synthetic_data) {
    if (engine == "legacy") {
      raw_dt <- simulate_households(
        n_households = n_households,
        hh.size      = hh.size,
        tests.per.week = tests.per.week,
        simulation_function = generate_synthetic_data_one,
        engine = "legacy",
        start_date = start_date, end_date = end_date,
        Covariates = Covariates,
        Covariates_list = Covariates_list,
        Covariate_specifications = Covariate_specifications,
        p.comm.base.infant.fix = p.comm.base.infant.fix,
        p.comm.multiplier.sibling = p.comm.multiplier.sibling,
        p.comm.multiplier.parent  = p.comm.multiplier.parent,
        p.comm.multiplier.elder   = p.comm.multiplier.elder,
        p.hh.base.infant = p.hh.base.infant,
        p.hh.multiplier.sibling = p.hh.multiplier.sibling,
        p.hh.multiplier.parent  = p.hh.multiplier.parent,
        p.hh.multiplier.elder   = p.hh.multiplier.elder,
        p.imm.base.sibling = p.imm.base.sibling,
        p.imm.base.parent  = p.imm.base.parent,
        p.imm.base.elder   = p.imm.base.elder,
        partial.immunity.infant  = partial.immunity.infant,
        partial.immunity.sibling = partial.immunity.sibling,
        partial.immunity.parent  = partial.immunity.parent,
        partial.immunity.elder   = partial.immunity.elder,
        duration.latent     = duration.latent,
        duration.infect.inf = duration.infect.inf,
        multiplier.dur.sibpar = multiplier.dur.sibpar,
        p.detect = p.detect,
        amplitude = amplitude, phase = phase
      )
    } else { # rsv_vl + stan (synthetic)
      raw_dt <- simulate_households(
        n_households = n_households,
        hh.size      = hh.size,
        tests.per.week = tests.per.week, # ignored by RSV/VL engine
        engine = "rsv_vl",
        seasonal_forcing_list = seasonal_forcing_list,
        start_date = start_date, end_date = end_date,
        max_days = max_days
      )
    }
  } else {
    # Non-synthetic data path (currently only meaningful for legacy+mle;
    # rsv_vl+stan from user_data is handled below via normalization).
    if (engine == "legacy") {
      if (is.null(user_data)) {
        stop("For non-synthetic legacy analysis, `user_data` must be supplied.", call. = FALSE)
      }
      raw_dt <- dataframe_to_household_list(user_data)
    } else {
      # Pass straight through; we'll normalize shapes in the rsv_vl+stan branch.
      raw_dt <- user_data
      if (is.null(raw_dt)) {
        stop("Non-synthetic rsv_vl+stan path requires `user_data`.", call. = FALSE)
      }
    }
  }

  # ------------------------------------------------------------------
  # Branch: legacy + mle
  # ------------------------------------------------------------------
  if (engine == "legacy" && estimation_method == "mle") {
    dt <- summarize_individuals(
      raw_dt = raw_dt, study_start = start_date, study_end = end_date,
      day_series_covariates = day_series_covariates, series_cols = series_cols
    )

    dt <- infectious_time_imputation(
      dt = dt, study_start = start_date,
      latent_par = latent_par, report_par = report_par, infect_par = infect_par
    )

    tmax <- as.integer(end_date - start_date)
    cases_t_raw <- pmax(0, round(30 * sin(2 * pi * (0:tmax) / 365) + rnorm(tmax + 1, 0, 5)))
    cases_t <- (cases_t_raw - min(cases_t_raw)) / (max(cases_t_raw) - min(cases_t_raw))

    need_cols <- unique(na.omit(unlist(c(
      comm_covariate_cols %||% character(0),
      hh_covariate_cols   %||% character(0),
      if (is.list(hh_role_covariate_cols)) unlist(hh_role_covariate_cols) else NULL
    ))))

    long <- build_person_day_table(dt, tmax, cases_t, covariate_cols = need_cols)

    theta_mat <- running_parameter_estimation(
      long_dt = long, n_runs = n_runs, start_par = start_par,
      lambda = lambda, lambda0 = lambda0, lambda_alpha = lambda_alpha,
      delta0_true = delta0_true, alpha0_true = alpha0_true,
      comm_covariate_cols = comm_covariate_cols,
      hh_covariate_cols   = hh_covariate_cols,
      hh_by_role          = hh_by_role,
      hh_role_covariate_cols = hh_role_covariate_cols,
      standardize_covariates = standardize_covariates,
      lambda_comm = lambda_comm, lambda_hh = lambda_hh
    )

    # NEW: legacy postprocessing based on repeated MLE runs
    postprocessing <- postprocessing_estimates(theta_mat)

    return(list(
      raw_simulation = raw_dt,
      summarized_data = dt,
      person_day = long,
      estimates = theta_mat,
      postprocessing = postprocessing
    ))
  }

  # ------------------------------------------------------------------
  # Branch: rsv_vl + stan
  # ------------------------------------------------------------------
  if (engine == "rsv_vl" && estimation_method == "stan") {
    # Normalize whatever we got into a list of per-HH data.frames
    households <- .normalize_households_input(raw_dt)

    # SAFETY CHECK: ensure required columns exist in every HH
    req <- c("infection_time","infectious_start","infectious_end","infection_resolved")
    bad <- which(!vapply(households, function(hh) all(req %in% names(hh)), logical(1)))
    if (length(bad)) {
      stop(sprintf(
        "Households %s missing required columns: %s",
        paste(head(bad, 5), collapse = ", "),
        paste(req, collapse = ", ")
      ), call. = FALSE)
    }

    stan_data <- build_stan_household_arrays(
      households = households,
      T_max = 365L,
      seasonal_forcing_list = seasonal_forcing_list,
      alpha_comm_by_role = 5e-3,
      beta1 = .2, beta2 = .6, V_ref = 1e3,
      reference_phi = 1, reference_kappa = 1
    )

    fit <- run_household_stan(
      stan_data,
      stan_file = stan_file,
      chains = stan_chains, iter = stan_iter, warmup = stan_warmup,
      control = stan_control, init = stan_init, refresh = stan_refresh, cores = stan_cores
    )

    posterior_summary <- postprocess_stan_fit(fit)
    postprocessing <- posterior_summary

    return(list(
      raw_simulation    = households,
      stan_data         = stan_data,
      fit               = fit,
      posterior_summary = posterior_summary,
      postprocessing    = postprocessing,
      summarized_data   = NULL,
      person_day        = NULL
    ))
  }
}



#' Summarize and compare estimates against reference values
#'
#' Computes mean, SD, SE across runs and (optionally) bias and relative bias
#' versus supplied reference (true) values for each parameter.
#'
#' @param theta_mat Numeric matrix. Parameter estimates from multiple runs
#'   (e.g., output of \code{running_parameter_estimation()} or a Stan summary).
#'   Must be non-empty.
#' @param true_values Optional named numeric vector of reference values. Names
#'   should match columns in \code{theta_mat}. Unmatched names are ignored; missing
#'   references yield \code{NA} bias/relative bias.
#'
#' @return A \code{data.table} with one row per parameter and columns:
#' \itemize{
#'   \item \code{Parameter}: column name from \code{theta_mat}
#'   \item \code{Estimate}: mean across runs
#'   \item \code{SD}: standard deviation across runs
#'   \item \code{SE}: standard error (\code{SD / sqrt(n\_runs)})
#'   \item \code{True}: supplied reference value (if any)
#'   \item \code{Bias}: \code{Estimate - True}
#'   \item \code{RelBias}: \code{Bias / |True|} (NA when \code{True} is 0 or missing)
#'   \item \code{Block}, \code{Role}: simple parameter grouping labels for readability
#' }
#'
#' @details
#' Rows with incomplete estimates are dropped using the first column as a sentinel.
#' If \code{theta_mat} lacks column names, generic names \code{"par1"}, \code{"par2"}, … are assigned.
#' The number of retained runs is stored in the \code{"n_runs"} attribute of the returned table.
#'
#' This helper understands both the legacy parameter naming (e.g., \code{delta0},
#' \code{alpha0}, \code{gamma2}, \code{beta2}, \code{theta_comm_*}, \code{theta_hh_*})
#' and the new RSV/seasonality framework parameters (e.g., \code{phi_*}, \code{kappa_*}).
postprocessing_estimates <- function(theta_mat, true_values = NULL) {
  if (is.null(theta_mat) || !is.matrix(theta_mat) || ncol(theta_mat) == 0)
    stop("`theta_mat` must be a non-empty matrix.")

  # Keep complete runs (using first column as sentinel)
  keep <- stats::complete.cases(theta_mat[, 1])
  theta_mat <- theta_mat[keep, , drop = FALSE]
  n_runs <- nrow(theta_mat)

  # Column names fallback
  cn <- colnames(theta_mat)
  if (is.null(cn)) {
    cn <- paste0("par", seq_len(ncol(theta_mat)))
    colnames(theta_mat) <- cn
  }

  # Summary statistics
  mean_est <- colMeans(theta_mat)
  sd_est   <- apply(theta_mat, 2, stats::sd)
  se_est   <- sd_est / sqrt(max(n_runs, 1L))

  # Merge true values when supplied
  true_vec <- rep(NA_real_, length(cn)); names(true_vec) <- cn
  if (!is.null(true_values)) {
    overlap <- intersect(names(true_values), cn)
    true_vec[overlap] <- true_values[overlap]
  }
  bias <- mean_est - true_vec
  rel_bias <- ifelse(is.finite(true_vec) & true_vec != 0, bias / abs(true_vec), NA_real_)

  # Parameter classifier (legacy + RSV/seasonality)
  normalize_role <- function(x) {
    x <- tolower(trimws(as.character(x)))

    # map various synonyms to the 4 canonical buckets
    x[x == "infant"]  <- "toddler"
    x[x == "baby"]    <- "toddler"

    x[x == "sibling"] <- "child"
    x[x == "kid"]     <- "child"
    # already "child" stays "child"

    x[x == "parent"]  <- "adult"
    x[x == "mother"]  <- "adult"
    x[x == "father"]  <- "adult"
    # already "adult" stays "adult"

    x[x == "elder"]   <- "elderly"
    x[x == "grandparent"] <- "elderly"
    # already "elderly" stays "elderly"

    x
  }

  classify_param <- function(p) {
    # RSV/seasonality phi_* (susceptibility) and kappa_* (infectivity)
    if (grepl("^phi(_|$)", p)) {
      role <- normalize_role(sub("^phi_?", "", p))
      return(c("Seasonality/Community (phi)", role))
    }
    if (grepl("^kappa(_|$)", p)) {
      role <- normalize_role(sub("^kappa_?", "", p))
      return(c("Household infectivity (kappa)", role))
    }

    # Legacy blocks
    if (p %in% c("delta0","gamma2","gamma3","gamma4")) return(c("Community (legacy)", ""))
    if (p %in% c("alpha0","z_sib","z_ad","z_el"))      return(c("Household (legacy)", ""))
    if (grepl("^theta_comm_", p))                       return(c("Community covariate", ""))
    if (grepl("^theta_hh_inf_", p))                     return(c("HH covariate (role)", "infant"))
    if (grepl("^theta_hh_sib_", p))                     return(c("HH covariate (role)", "child"))
    if (grepl("^theta_hh_ad_",  p))                     return(c("HH covariate (role)", "adult"))
    if (grepl("^theta_hh_el_",  p))                     return(c("HH covariate (role)", "elderly"))
    if (grepl("^theta_hh_",     p))                     return(c("HH covariate (shared)", ""))
    return(c("Other", ""))
  }

  blk_role <- t(vapply(cn, classify_param, character(2)))
  block <- blk_role[, 1]
  role  <- blk_role[, 2]

  summary_dt <- data.table::data.table(
    Parameter = cn,
    Estimate  = as.numeric(mean_est),
    SD        = as.numeric(sd_est),
    SE        = as.numeric(se_est),
    True      = as.numeric(true_vec),
    Bias      = as.numeric(bias),
    RelBias   = as.numeric(rel_bias),
    Block     = block,
    Role      = role
  )
  attr(summary_dt, "n_runs") <- n_runs
  return(summary_dt)
}
