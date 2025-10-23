#' Main parameter estimation pipeline
#'
#' End-to-end pipeline that (i) obtains household testing data (either simulated
#' or user-supplied), (ii) summarizes individuals, (iii) imputes infection
#' timelines, (iv) expands to a person-day table, and (v) runs repeated
#' maximum-likelihood estimation with penalties.
#'
#' @param user_data List or data.frame. User-provided observations. If a list,
#'   it should contain one data frame per household; if a single data.frame,
#'   it must contain at least columns \code{HH}, \code{individual_ID}, \code{role},
#'   \code{test_date}, \code{infection_status}, and \code{community_risk}.
#'   Ignored when \code{synthetic_data = TRUE}. Default \code{NULL}.
#' @param synthetic_data Logical. If \code{TRUE} (default), simulate data using
#'   \code{\link{generate_synthetic_data_standardized}} via
#'   \code{\link{simulate_households}}; otherwise, convert \code{user_data} with
#'   \code{\link{dataframe_to_household_list}}.
#' @param n_households Integer. Number of households to simulate (used only when
#'   \code{synthetic_data = TRUE}). Default 10.
#' @param n_runs Integer. Number of repeated estimation runs. Default 10.
#'
#' @param hh.size Integer. Household size for simulation. Default \code{sample(3:7,1)}.
#' @param tests.per.week Integer. Tests per person per week (1–3). Default 1.
#'
#' @param Covariates Logical. If \code{TRUE}, generate additional covariates in simulation.
#' @param Covariates_list Character vector. Names of covariates to generate.
#' @param Covariate_specifications List. Per-covariate specifications; see
#'   \code{\link{generate_synthetic_data_one}} for schema and defaults.
#'
#' @param day_series_covariates Logical. If \code{TRUE}, build day-series list-columns
#'   for covariates during summarization (see \code{\link{summarize_individuals}}).
#' @param series_cols Character vector or \code{NULL}. Names (pre-normalization) of covariates
#'   to include as day-series; if \code{NULL}, uses all detected covariates.
#'
#' @param comm_covariate_cols Character vector. Column names included in the community
#'   risk model (no intercept).
#' @param hh_covariate_cols Character vector. Household covariates shared across roles.
#' @param hh_by_role Logical. If \code{TRUE}, allow role-specific household covariates.
#' @param hh_role_covariate_cols List. Named list with elements \code{infant},
#'   \code{sibling}, \code{adult}, \code{elder}, each a character vector of columns;
#'   falls back to \code{hh_covariate_cols} when missing.
#' @param standardize_covariates Logical. Z-score non-binary numeric columns in model matrices.
#' @param lambda_comm,lambda_hh Numeric. L2 penalties for community and household
#'   covariate coefficients. Defaults 0.01.
#'
#' @param p.comm.base.infant.fix Numeric. Baseline community infection probability per day for infants.
#' @param p.comm.multiplier.sibling,p.comm.multiplier.parent,p.comm.multiplier.elder
#'   Numeric multipliers applied to \code{p.comm.base.infant.fix} by role.
#'
#' @param p.hh.base.infant Numeric. Baseline within-household transmission probability per infectious infant contact (per day).
#' @param p.hh.multiplier.sibling,p.hh.multiplier.parent,p.hh.multiplier.elder
#'   Multipliers relative to infant baseline by role.
#'
#' @param p.imm.base.sibling,p.imm.base.parent,p.imm.base.elder Numeric. Baseline
#'   (day-1) immunity probabilities by role.
#' @param partial.immunity.infant,partial.immunity.sibling,partial.immunity.parent,partial.immunity.elder
#'   Numeric. Breakthrough scaling (0–1) if previously immune, by role.
#'
#' @param duration.latent Integer. Mean latent period (days).
#' @param duration.infect.inf Integer. Mean infectious duration for infants (days).
#'   Other roles use \code{duration.infect.inf * multiplier.dur.sibpar}.
#' @param multiplier.dur.sibpar Numeric. Infectious duration multiplier for non-infant roles.
#' @param p.detect Numeric. Detection probability on testing days.
#'
#' @param amplitude,phase Numeric. Seasonality parameters (cosine forcing) for community risk.
#' @param start_date,end_date Date. Simulation/analysis window (inclusive of \code{start_date}).
#'
#' @param latent_par,report_par,infect_par Lists with elements \code{shape} and
#'   \code{scale} for the latent delay, reporting delay, and infectious period gammas.
#'
#' @param start_par Numeric vector. Initial parameter vector. If not tailored to the
#'   covariate layout, it is resized internally when passed to the optimizer.
#' @param lambda Numeric. Base L2 penalty for \code{gamma} (age) and \code{z_*} (role offsets).
#' @param lambda0,lambda_alpha Numeric. Penalties pulling \code{delta0} and \code{alpha0}
#'   toward \code{delta0_true} and \code{alpha0_true}.
#' @param delta0_true,alpha0_true Numeric anchors for the intercept penalties (logit scale).
#'
#' @return A list with elements:
#' \itemize{
#'   \item \code{raw_simulation}: list of household data frames (simulated or user-provided).
#'   \item \code{summarized_data}: individual-level summary table
#'     (from \code{\link{summarize_individuals}} then \code{\link{infectious_time_imputation}}).
#'   \item \code{person_day}: long person-day table (from \code{\link{build_person_day_table}}).
#'   \item \code{estimates}: \eqn{n\_runs \times p} matrix of parameter estimates
#'     (from \code{\link{running_parameter_estimation}}).
#' }
#'
#' @details
#' When \code{synthetic_data = TRUE}, data are produced by
#' \code{\link{generate_synthetic_data_standardized}} via
#' \code{\link{simulate_households}}. When \code{FALSE}, \code{user_data} is converted
#' with \code{\link{dataframe_to_household_list}}. Covariate names are normalized to
#' snake_case during summarization.
#'
#' Community intensity \code{cases_t} used by the person-day builder is generated
#' internally as a smoothed, scaled seasonal signal over the analysis window.
#'
#' @seealso
#' \code{\link{simulate_households}},
#' \code{\link{generate_synthetic_data_standardized}},
#' \code{\link{summarize_individuals}},
#' \code{\link{infectious_time_imputation}},
#' \code{\link{build_person_day_table}},
#' \code{\link{running_parameter_estimation}},
#' \code{\link{postprocessing_estimates}}
#'
#' @examples
#' \dontrun{
#' res <- main_parameter_estimation_pipeline(
#'   synthetic_data = TRUE,
#'   n_households = 20, n_runs = 5,
#'   hh.size = 4, tests.per.week = 2,
#'   delta0_true = qlogis(0.002), alpha0_true = qlogis(0.2)
#' )
#' str(res$estimates)
#' }
#'
main_parameter_estimation_pipeline <- function(
    user_data = NULL,          # User has the option to provide their own data

    synthetic_data = TRUE,     # Default to generate synthetic data
    n_households = 10,
    n_runs = 10,

    hh.size = sample(3:7,1),
    tests.per.week = 1,

    # Either user's data already include covariates of interest
    # Or user can specify the covariates to be generated in the synthetic data
    Covariates = FALSE,
    Covariates_list = c("Vaccination status", "Antibody Level"),
    Covariate_specifications = NULL,

    day_series_covariates = TRUE,
    series_cols = NULL,

    # Likelihood covariate mapping
    comm_covariate_cols = NULL,        # e.g., c("cases","vaccination_status_mode")
    hh_covariate_cols   = NULL,        # e.g., c("vaccination_status_mode","bmi_mean")
    hh_by_role          = FALSE,       # set TRUE for role-specific HH effects
    hh_role_covariate_cols = NULL,     # list(infant=..., sibling=..., adult=..., elder=...)
    standardize_covariates = TRUE,

    lambda_comm = 0.01,
    lambda_hh   = 0.01,

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
    amplitude = 2.65810*0,
    phase = -0.408,
    start_date = as.Date("2024-09-21"),
    end_date   = as.Date("2025-04-17"),

    latent_par = list(shape = 2, scale = 1),
    report_par = list(shape = 1, scale = 1.5),
    infect_par = list(shape = 3, scale = 2),

    start_par = c(-6, 0.02, -2, rep(0, 6)),
    lambda = 0.01,
    lambda0 = 0.2,
    lambda_alpha = 5,
    delta0_true = qlogis(0.002),
    alpha0_true = qlogis(0.2)
) {

  ##############################################################################################################################################
  #                                                                                                                                            #
  #    If synthetic_data = TRUE, then data will be generated by generate_synthetic_data_standardized function for n_households                 #
  #                                                                                                                                            #
  ##############################################################################################################################################

  if (isTRUE(synthetic_data)) {
    sim_func <- generate_synthetic_data_one
    raw_dt <- simulate_households(
      n_households = n_households,
      simulation_function = sim_func,
      hh.size,
      tests.per.week = tests.per.week,

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

      partial.immunity.infant = partial.immunity.infant,
      partial.immunity.sibling = partial.immunity.sibling,
      partial.immunity.parent  = partial.immunity.parent,
      partial.immunity.elder   = partial.immunity.elder,

      duration.latent = duration.latent,
      duration.infect.inf = duration.infect.inf,
      multiplier.dur.sibpar = multiplier.dur.sibpar,
      p.detect = p.detect,
      amplitude = amplitude,
      phase = phase,
      start_date = start_date,
      end_date   = end_date,

      Covariates = Covariates,
      Covariates_list = Covariates_list,
      Covariate_specifications = Covariate_specifications
    )
  } else {
    raw_dt <- dataframe_to_household_list(user_data)
  }

  ##########################################################################
  #                                                                        #
  #                    Summarize individuals                               #
  #                                                                        #
  ##########################################################################
  dt <- summarize_individuals(raw_dt = raw_dt, study_start = start_date, study_end = end_date)

  ##########################################################################
  #                                                                        #
  #                   Impute infectious time                               #
  #                                                                        #
  ##########################################################################
  dt <- infectious_time_imputation(dt = dt, study_start = start_date,
                                   latent_par = latent_par,
                                   report_par = report_par,
                                   infect_par = infect_par)

  ########################################################################################
  #                                                                                      #
  #              Build person day table (long format), including covariates              #
  #                                                                                      #
  ########################################################################################
  tmax <- as.integer(end_date - start_date)
  cases_t_raw <- pmax(0, round(30 * sin(2 * pi * (0:tmax) / 365) + rnorm(tmax + 1, 0, 5)))
  cases_t <- (cases_t_raw - min(cases_t_raw)) / (max(cases_t_raw) - min(cases_t_raw))

  need_cols <- unique(na.omit(unlist(c(
    comm_covariate_cols %||% character(0),
    hh_covariate_cols   %||% character(0),
    if (is.list(hh_role_covariate_cols)) unlist(hh_role_covariate_cols) else NULL
  ))))

  long <- build_person_day_table(dt, tmax, cases_t, covariate_cols = need_cols)

  ##########################################################################
  #                                                                        #
  #                      Running parameter estimation                      #
  #                                                                        #
  ##########################################################################
  theta_mat <- running_parameter_estimation(
    long_dt = long, n_runs = n_runs, start_par = start_par,
    lambda = lambda, lambda0 = lambda0, lambda_alpha = lambda_alpha,
    delta0_true = delta0_true, alpha0_true = alpha0_true,

    comm_covariate_cols = comm_covariate_cols,
    hh_covariate_cols   = hh_covariate_cols,
    hh_by_role          = hh_by_role,
    hh_role_covariate_cols = hh_role_covariate_cols,
    standardize_covariates = standardize_covariates,
    lambda_comm = lambda_comm,
    lambda_hh   = lambda_hh
  )

  list(
    raw_simulation = raw_dt,
    summarized_data = dt,
    person_day = long,
    estimates = theta_mat
  )
}




#' Summarize and compare estimates against reference values
#'
#' Computes mean, SD, SE across runs and (optionally) bias and relative bias
#' versus supplied reference (true) values for each parameter.
#'
#' @param theta_mat Numeric matrix. Parameter estimates from multiple runs
#'   (e.g., output of \code{\link{running_parameter_estimation}}). Must be non-empty.
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
#' @seealso \code{\link{main_parameter_estimation_pipeline}},
#'   \code{\link{running_parameter_estimation}}
#'
#' @examples
#' \dontrun{
#' est <- matrix(rnorm(50), nrow = 10)
#' colnames(est) <- c("delta0","gamma2","alpha0","z_sib","theta_comm_x")
#' postprocessing_estimates(est, true_values = c(delta0 = -9.5, alpha0 = -1.2))
#' }
#'
postprocessing_estimates <- function(theta_mat, true_values = c(
  # Community infection
  delta0 = log(7.148217e-05),                        # ≈ -9.546
  gamma2 = log(7.148217e-05 * 4.331956e+00) - log(7.148217e-05), # sibling multiplier
  gamma3 = log(7.148217e-05 * 1.835466e+00) - log(7.148217e-05), # parent multiplier
  gamma4 = log(7.148217e-05 * 2) - log(7.148217e-05),            # elder multiplier

  # Household transmission
  alpha0 = log(0.2888953),                           # ≈ -1.242
  beta2  = log(0.2888953 * 0.5267686) - log(0.2888953), # sibling / infant
  beta3  = log(0.2888953 * 0.8008933) - log(0.2888953), # parent  / infant
  beta4  = log(0.2888953 * 0.6008933) - log(0.2888953)  # elder   / infant
)) {
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
  se_est   <- sd_est / sqrt(n_runs)

  # Merge true values when supplied
  true_vec <- rep(NA_real_, length(cn)); names(true_vec) <- cn
  if (!is.null(true_values)) {
    overlap <- intersect(names(true_values), cn)
    true_vec[overlap] <- true_values[overlap]
  }
  bias <- mean_est - true_vec
  rel_bias <- ifelse(is.finite(true_vec) & true_vec != 0, bias / abs(true_vec), NA_real_)

  # Classify parameters for readability
  classify_param <- function(p) {
    if (p %in% c("delta0","gamma2","gamma3","gamma4")) return(c("Community",""))
    if (p %in% c("alpha0","z_sib","z_ad","z_el"))      return(c("Household",""))
    if (grepl("^theta_comm_", p))                       return(c("Community covariate",""))
    if (grepl("^theta_hh_inf_", p))                     return(c("HH covariate (role)","infant"))
    if (grepl("^theta_hh_sib_", p))                     return(c("HH covariate (role)","sibling"))
    if (grepl("^theta_hh_ad_",  p))                     return(c("HH covariate (role)","adult"))
    if (grepl("^theta_hh_el_",  p))                     return(c("HH covariate (role)","elder"))
    if (grepl("^theta_hh_",     p))                     return(c("HH covariate (shared)",""))
    return(c("Other",""))
  }

  blk_role <- t(vapply(cn, classify_param, character(2)))
  block <- blk_role[,1]; role <- blk_role[,2]

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

