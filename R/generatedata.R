#' (Deprecated) generate_synthetic_data_standardized
#'
#' Deprecated alias for \code{\link{simulate_households}}.
#'
#' @description
#' This function is retained for backward compatibility. It forwards its arguments
#' to \code{\link{simulate_households}} and emits a deprecation notice.
#'
#' @inheritParams simulate_households
#'
#' @return See \code{\link{simulate_households}}.
#' @seealso \code{\link{simulate_households}}
#' @examples
#' \dontrun{
#' sims <- generate_synthetic_data_standardized(
#'   n_households = 5,
#'   simulation_function = generate_synthetic_data_one,
#'   hh.size = 4,
#'   tests.per.week = 2
#' )
#' length(sims); head(sims[[1]])
#' }
generate_synthetic_data_standardized <- function(n_households,
                                                 hh.size = sample(3:7,1),
                                                 tests.per.week,
                                                 p.comm.base.infant.fix = 0.001,
                                                 p.comm.multiplier.sibling = 1,
                                                 p.comm.multiplier.parent = 1,
                                                 p.comm.multiplier.elder = 1,
                                                 p.hh.base.infant = 0.1,
                                                 p.hh.multiplier.sibling = 1,
                                                 p.hh.multiplier.parent = 1,
                                                 p.hh.multiplier.elder = 1,
                                                 p.imm.base.sibling = 1e-10,
                                                 p.imm.base.parent = 1e-10,
                                                 p.imm.base.elder = 1e-10,
                                                 partial.immunity.infant = 1e-10,
                                                 partial.immunity.sibling = 1e-10,
                                                 partial.immunity.parent = 1e-10,
                                                 partial.immunity.elder = 1e-10,
                                                 duration.latent = 2,
                                                 duration.infect.inf = 3,
                                                 multiplier.dur.sibpar = 0.5,
                                                 p.detect = 0.999,
                                                 amplitude = 0,
                                                 phase = -0.408,
                                                 start_date = as.Date("2024-09-21"),
                                                 end_date = as.Date("2025-04-17"),
                                                 Covariates = FALSE,
                                                 Covariates_list = c("Vaccination status", "Antibody Level"),
                                                 Covariate_specifications = NULL) {

  lapply(seq_len(n_households), function(i) {
    generate_synthetic_data_one(
      household_id = i,
      hh.size = hh.size,
      tests.per.week = tests.per.week,
      p.comm.base.infant.fix = p.comm.base.infant.fix,
      p.comm.multiplier.sibling = p.comm.multiplier.sibling,
      p.comm.multiplier.parent = p.comm.multiplier.parent,
      p.comm.multiplier.elder = p.comm.multiplier.elder,
      p.hh.base.infant = p.hh.base.infant,
      p.hh.multiplier.sibling = p.hh.multiplier.sibling,
      p.hh.multiplier.parent = p.hh.multiplier.parent,
      p.hh.multiplier.elder = p.hh.multiplier.elder,
      p.imm.base.sibling = p.imm.base.sibling,
      p.imm.base.parent = p.imm.base.parent,
      p.imm.base.elder = p.imm.base.elder,
      partial.immunity.infant = partial.immunity.infant,
      partial.immunity.sibling = partial.immunity.sibling,
      partial.immunity.parent = partial.immunity.parent,
      partial.immunity.elder = partial.immunity.elder,
      duration.latent = duration.latent,
      duration.infect.inf = duration.infect.inf,
      multiplier.dur.sibpar = multiplier.dur.sibpar,
      p.detect = p.detect,
      amplitude = amplitude,
      phase = phase,
      start_date = start_date,
      end_date = end_date,
      Covariates = Covariates,
      Covariates_list = Covariates_list,
      Covariate_specifications = Covariate_specifications
    )
  })
}
