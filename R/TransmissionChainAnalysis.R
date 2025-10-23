#' Estimate transmission parameters from user data
#'
#' Runs the full parameter-estimation pipeline on user-provided long-format testing data.
#' By default, prints a post-processing table (mean estimates, SD/SE, bias, relative bias).
#' Optionally prints the per-individual summary produced by the pipeline.
#'
#' @section Required columns in \code{user_data}:
#' \itemize{
#'   \item \code{HH} — household identifier (integer)
#'   \item \code{individual_ID} — individual identifier within household (integer)
#'   \item \code{role} — one of \code{"infant"}, \code{"sibling"}, \code{"adult"}, \code{"elder"}
#'   \item \code{test_date} — test day (integer or \code{Date}; \code{Date} will be converted to relative day)
#'   \item \code{infection_status} — 0/1 infectious status at \code{test_date}
#'   \item \code{community_risk} — numeric community infection intensity at \code{test_date}
#' }
#' Any additional columns are treated as candidate covariates and may be included in the likelihood
#' via \code{comm_covariate_cols}, \code{hh_covariate_cols}, or \code{hh_role_covariate_cols}.
#'
#' @param user_data Data frame (preferred) with the required columns listed above; alternatively,
#'   a list of per-household data frames which will be row-bound. Must not be \code{NULL}.
#' @param data_summary Logical. If \code{TRUE}, prints \code{results$summarized_data}. Default \code{FALSE}.
#' @param postprocessing Logical. If \code{TRUE} (default), prints \code{\link{postprocessing_estimates}}.
#'
#' @param n_runs Integer. Number of repeated estimation runs. Default \code{10}.
#'
#' @param day_series_covariates Logical. If \code{TRUE}, builds day-series list-columns for covariates in the person–day table. Default \code{TRUE}.
#' @param series_cols Character vector of covariate names (pre-normalization) for which to build day-series; if \code{NULL}, all detected covariates are used.
#'
#' @param comm_covariate_cols Character vector of community-model covariates (no intercept).
#' @param hh_covariate_cols Character vector of household-model covariates shared across roles.
#' @param hh_by_role Logical. If \code{TRUE}, use role-specific household covariates.
#' @param hh_role_covariate_cols Named list with elements \code{infant}, \code{sibling}, \code{adult}, \code{elder}
#'   providing covariate names per role when \code{hh_by_role=TRUE}.
#' @param standardize_covariates Logical. Z-score non-binary numeric columns in model matrices. Default \code{TRUE}.
#'
#' @param lambda_comm,lambda_hh Numeric. L2 penalties for community and household covariate coefficients.
#'
#' @param start_date,end_date \code{Date}. Study window used for summarization and imputation if \code{test_date} is provided as \code{Date}.
#'
#' @param latent_par,report_par,infect_par Lists with Gamma(\code{shape}, \code{scale}) parameters for imputation of latent delay,
#'   reporting delay, and infectious period.
#'
#' @param start_par Numeric vector of initial parameters (auto-expanded if length mismatches model).
#' @param lambda Base L2 penalty for slope-like parameters (age multipliers and household role offsets).
#' @param lambda0 Penalty anchoring \code{delta0} near \code{delta0_true}.
#' @param lambda_alpha Penalty anchoring \code{alpha0} near \code{alpha0_true}.
#' @param delta0_true,alpha0_true Numeric anchors (logit scale) for community baseline and household baseline.
#'
#' @param true_values Optional named numeric vector of reference values. Names
#'   should match columns in \code{theta_mat}. Unmatched names are ignored; missing
#'   references yield \code{NA} bias/relative bias.
#' @return (Invisibly) a list with elements:
#' \itemize{
#'   \item \code{results}: Output of \code{\link{main_parameter_estimation_pipeline}} (summaries, person-day, estimates).
#'   \item \code{postprocessing}: The table returned by \code{\link{postprocessing_estimates}} (or \code{NULL} if \code{postprocessing=FALSE}).
#' }
#'
#' @details This wrapper calls \code{\link{main_parameter_estimation_pipeline}} with \code{synthetic_data=FALSE}
#' and passes \code{user_data} through \code{\link{dataframe_to_household_list}} internally. If \code{user_data} is a list
#' of per-household data frames, it is row-bound before processing. Character covariates are internally factorized
#' via model matrices when used in the likelihood.
#'
#' @seealso \code{\link{GenSyn}}, \code{\link{main_parameter_estimation_pipeline}},
#'   \code{\link{data_summarization}}, \code{\link{postprocessing_estimates}}
#'
#' @examples
#' \dontrun{
#' # Suppose df is your long-format dataset with required columns:
#' fit <- TransmissionChainAnalysis(
#'   user_data = df,
#'   n_runs = 20,
#'   comm_covariate_cols = c("cases"),
#'   hh_covariate_cols   = c("vaccination_status_mode")
#' )
#' fit$postprocessing
#' }
#' @export
TransmissionChainAnalysis <- function(
    user_data,
    data_summary   = FALSE,
    postprocessing = TRUE,

    n_runs = 10,

    #Covariate handling of user data in pipeline
    day_series_covariates = TRUE,
    series_cols           = NULL,

    comm_covariate_cols    = NULL,   # e.g., c(vaccination_status_mode")
    hh_covariate_cols      = NULL,   # e.g., c("vaccination_status_mode","bmi_mean")
    hh_by_role             = FALSE,
    hh_role_covariate_cols = NULL,   # list(infant=..., sibling=..., adult=..., elder=...)
    standardize_covariates = TRUE,

    #Regularization for covariate effects
    lambda_comm = 0.01,
    lambda_hh   = 0.01,

    #Study window (used in summaries/imputation)
    start_date = as.Date("2024-09-21"),
    end_date   = as.Date("2025-04-17"),

    #Imputed delays
    latent_par = list(shape = 2, scale = 1),
    report_par = list(shape = 1, scale = 1.5),
    infect_par = list(shape = 3, scale = 2),

    #Optimization (starting values & penalties)
    start_par    = c(-6, 0.02, -2, rep(0, 6)),
    lambda       = 0.01,
    lambda0      = 0.2,
    lambda_alpha = 5,
    delta0_true  = qlogis(0.002),
    alpha0_true  = qlogis(0.2),
    true_values = NA


) {

  #Validate and normalize user data
  if (is.null(user_data)) {
    stop("`user_data` must be supplied (data.frame or list of per-household data.frames).", call. = FALSE)
  }

  #If a list of HH data.frames is provided, try to rbind them
  if (is.list(user_data) && !is.data.frame(user_data)) {
    ok <- all(vapply(user_data, is.data.frame, logical(1)))
    if (!ok) stop("`user_data` list must contain only data.frames.", call. = FALSE)
    user_data <- data.table::rbindlist(user_data, use.names = TRUE, fill = TRUE)
  }

  if (!is.data.frame(user_data)) {
    stop("`user_data` must be a data.frame (or a list of data.frames).", call. = FALSE)
  }

  #Run the main pipeline with synthetic_data = FALSE
  results <- main_parameter_estimation_pipeline(
    user_data      = user_data,
    synthetic_data = FALSE,
    n_runs         = n_runs,

    start_date = start_date,
    end_date   = end_date,

    day_series_covariates = day_series_covariates,
    series_cols           = series_cols,

    comm_covariate_cols    = comm_covariate_cols,
    hh_covariate_cols      = hh_covariate_cols,
    hh_by_role             = hh_by_role,
    hh_role_covariate_cols = hh_role_covariate_cols,
    standardize_covariates = standardize_covariates,

    lambda_comm = lambda_comm,
    lambda_hh   = lambda_hh,

    latent_par = latent_par,
    report_par = report_par,
    infect_par = infect_par,

    start_par    = start_par,
    lambda       = lambda,
    lambda0      = lambda0,
    lambda_alpha = lambda_alpha,
    delta0_true  = delta0_true,
    alpha0_true  = alpha0_true
  )

  #Optional: print data summary prepared by the pipeline
  if (isTRUE(data_summary)) {
    cat("\n--- Data summary ---\n")
    print(results$summarized_data)
  }

  # Optional: post-processing (uses defaults embedded in postprocessing_estimates)
  postprocessing_est <- NULL
  if (isTRUE(postprocessing)) {
    cat("\n--- Post-processing of estimates ---\n")
    postprocessing_est <- postprocessing_estimates(results$estimates, true_values = true_values)
    print(postprocessing_est)
  }

  invisible(list(results = results, postprocessing = postprocessing_est))
}
