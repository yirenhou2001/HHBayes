#' Simulate households (legacy or RSV/VL engine)
#'
#' Generates synthetic household data for downstream MLE or RSV/VL–Stan
#' pipelines. Returns per-household data frames (legacy) or a long test-day
#' table with an attached per-household list (RSV/VL).
#'
#' @param n_households Integer; number of households.
#' @param hh.size Integer or vector; household size(s).
#' @param tests.per.week Integer; testing frequency (legacy).
#' @param engine Character; one of \code{c("legacy","rsv_vl")}.
#' @param simulation_function Function used by the legacy engine to simulate one
#'   household (defaults to \code{generate_synthetic_data_one}).
#' @param Covariates Logical; include synthetic covariates (legacy).
#' @param Covariates_list Character vector; names of covariates (legacy).
#' @param Covariate_specifications Optional list; covariate generation controls (legacy).
#' @param amplitude,phase Numeric seasonality knobs (legacy/upstream only).
#' @param start_date,end_date \code{Date} study window (RSV/VL path and inference).
#' @param max_days Optional integer; horizon to stamp as attribute
#'   \code{"test_days"} on each household.
#' @param p.comm.base.infant.fix Numeric; base community risk (infant; legacy).
#' @param p.comm.multiplier.sibling Numeric; community multiplier (sibling; legacy).
#' @param p.comm.multiplier.parent Numeric; community multiplier (parent; legacy).
#' @param p.comm.multiplier.elder Numeric; community multiplier (elder; legacy).
#' @param p.hh.base.infant Numeric; base HH transmission (infant; legacy).
#' @param p.hh.multiplier.sibling Numeric; HH multiplier (sibling; legacy).
#' @param p.hh.multiplier.parent Numeric; HH multiplier (parent; legacy).
#' @param p.hh.multiplier.elder Numeric; HH multiplier (elder; legacy).
#' @param p.imm.base.sibling Numeric; baseline immunity (sibling; legacy).
#' @param p.imm.base.parent Numeric; baseline immunity (parent; legacy).
#' @param p.imm.base.elder Numeric; baseline immunity (elder; legacy).
#' @param partial.immunity.infant Numeric; partial immunity (infant; legacy).
#' @param partial.immunity.sibling Numeric; partial immunity (sibling; legacy).
#' @param partial.immunity.parent Numeric; partial immunity (parent; legacy).
#' @param partial.immunity.elder Numeric; partial immunity (elder; legacy).
#' @param duration.latent Integer; latent duration (legacy).
#' @param duration.infect.inf Integer; infectious duration (legacy).
#' @param multiplier.dur.sibpar Numeric; duration multiplier for sibling/parent (legacy).
#' @param p.detect Numeric; detection probability per test (legacy).
#' @param seasonal_forcing_list Optional named list (\code{adult, child, elderly, toddler})
#'   for RSV/VL downstream use.
#'
#' @return If \code{engine="legacy"}: a list of per-household data frames (each may
#'   carry \code{attr(., "test_days")}). If \code{engine="rsv_vl"}: a long test-day
#'   data frame with attribute \code{"households"} holding the per-household list.
simulate_households <- function(
    n_households,
    hh.size,
    tests.per.week = 1,
    engine = c("legacy","rsv_vl"),

    # legacy single-household simulator (one HH at a time)
    simulation_function = generate_synthetic_data_one,

    # legacy covariates
    Covariates = FALSE,
    Covariates_list = c("Vaccination status", "Antibody Level"),
    Covariate_specifications = NULL,

    # seasonality / dates
    amplitude = 0,
    phase = 0,
    start_date = as.Date("2024-09-21"),
    end_date   = as.Date("2025-04-17"),

    # horizon tag for downstream grid (used to set attr 'test_days')
    max_days = NULL,

    # legacy infection / immunity / testing knobs
    p.comm.base.infant.fix     = 0.002,
    p.comm.multiplier.sibling  = 1,
    p.comm.multiplier.parent   = 1,
    p.comm.multiplier.elder    = 1,

    p.hh.base.infant           = 0.2,
    p.hh.multiplier.sibling    = 0.5267686,
    p.hh.multiplier.parent     = 0.8008933,
    p.hh.multiplier.elder      = 0.6008933,

    p.imm.base.sibling         = 1e-10,
    p.imm.base.parent          = 1e-10,
    p.imm.base.elder           = 1e-10,
    partial.immunity.infant    = 1e-10,
    partial.immunity.sibling   = 1e-10,
    partial.immunity.parent    = 1e-10,
    partial.immunity.elder     = 1e-10,

    duration.latent            = 1,
    duration.infect.inf        = 2,
    multiplier.dur.sibpar      = 0.5,
    p.detect                   = 0.999,

    # RSV/VL extras
    seasonal_forcing_list = NULL
) {
  engine <- match.arg(engine)

  # utility: call fn with only the formals it accepts
  call_with_formals <- function(fn, args) {
    fm <- names(formals(fn))
    do.call(fn, args[intersect(names(args), fm)])
  }

  # utility: ensure minimal columns + names
  ensure_min_cols <- function(hh_df) {
    # common person id name harmonization
    if ("person_id" %in% names(hh_df) && !"individual_ID" %in% names(hh_df)) {
      hh_df$individual_ID <- hh_df$person_id
    } else if (!"individual_ID" %in% names(hh_df)) {
      hh_df$individual_ID <- seq_len(nrow(hh_df))
    }
    need <- c("individual_ID", "role", "infection_status", "infectious_start", "infectious_end")
    for (nm in need) if (!nm %in% names(hh_df)) hh_df[[nm]] <- NA
    hh_df$infection_status <- as.integer(ifelse(is.na(hh_df$infection_status), 0L, hh_df$infection_status))
    hh_df
  }

  if (engine == "legacy") {
    # loop over households; inject the right id arg if needed
    households <- vector("list", n_households)

    # inspect which id name the simulator expects
    sim_formals <- names(formals(simulation_function))
    id_arg <- if ("household_id" %in% sim_formals) "household_id"
    else if ("hh_id" %in% sim_formals) "hh_id"
    else NULL

    for (h in seq_len(n_households)) {
      args_h <- list(
        hh.size = hh.size,
        tests.per.week = tests.per.week,
        Covariates = Covariates,
        Covariates_list = Covariates_list,
        Covariate_specifications = Covariate_specifications,
        amplitude = amplitude, phase = phase,
        start_date = start_date, end_date = end_date,

        p.comm.base.infant.fix    = p.comm.base.infant.fix,
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
        p.detect                  = p.detect
      )
      if (!is.null(id_arg)) args_h[[id_arg]] <- h

      hh_df <- call_with_formals(simulation_function, args_h)
      hh_df <- ensure_min_cols(hh_df)

      # stamp test_days if we can infer a horizon
      td <- if (!is.null(max_days)) as.integer(max_days) else {
        # try to infer from infectious_end / infection_resolved
        cand <- suppressWarnings(as.integer(c(hh_df$infectious_end, hh_df$infection_resolved)))
        cmax <- suppressWarnings(max(cand, na.rm = TRUE))
        if (is.finite(cmax) && cmax > 0) cmax else as.integer(end_date - start_date + 1L)
      }
      if (is.finite(td) && td > 0) attr(hh_df, "test_days") <- td

      households[[h]] <- hh_df
    }

    return(households)
  }

  # RSV/VL path: produce households via the RSV/VL engine, then a long table
  sim <- simulate_multiple_households_comm(
    n_households = n_households,
    max_days = if (is.null(max_days)) 40L else as.integer(max_days),
    seasonal_forcing_list = seasonal_forcing_list
    # (other exposed RSV/VL knobs can be threaded here as needed)
  )
  households <- sim$households
  long_tests <- households_to_long_tests(households, seasonal_forcing_list)
  # ensure 6th core col exists for episode utils downstream
  if (!"community_risk" %in% names(long_tests)) long_tests$community_risk <- NA_real_
  # attach the per-HH list for Stan branch
  attr(long_tests, "households") <- households
  return(long_tests)
}



#' Transmission potential as a function of viral load
#'
#' Computes a saturating transmission potential on \[0,1] from log10 viral load.
#'
#' @param VL Numeric vector; log10 viral load.
#' @param n,p_v,K,Vm,sai Numeric scalars; model constants.
#'
#' @return Numeric vector of transmission potentials.
p_transmission <- function(VL, n, p_v, K, Vm, sai) {
  pot <- 1 - exp(-p_v * sai * Vm * VL^n / (VL^n + K^n))
  pot[is.na(pot)] <- 0
  pot
}



#' Gaussian-like infectivity profile (normalized)
#'
#' Bell-shaped infectivity profile scaled to have maximum 1.
#'
#' @param t Integer vector of times (e.g., days since infectious onset).
#' @param peak_day Integer; peak day.
#' @param width Numeric; spread around the peak.
#'
#' @return Numeric vector in \[0,1] of length \code{t}.
g_rescaled <- function(t, peak_day, width) {
  if (length(t) == 0) return(numeric(0))
  vals <- exp(-0.5 * ((t - peak_day)/width)^2)
  mx <- max(vals, na.rm = TRUE)
  if (!is.finite(mx) || mx <= 0) return(vals)
  vals / mx
}



#' Simulate a log10 viral-load trajectory
#'
#' Generates a unimodal log10 viral-load curve given peak level/day and
#' growth/decay rates.
#'
#' @param t Integer vector; days relative to infection.
#' @param v_p Numeric; peak log10 VL.
#' @param t_p Integer; day of peak.
#' @param lambda_g,lambda_d Numeric; growth/decay rates.
#'
#' @return Numeric vector of log10 VL at times \code{t}.
simulate_viral_load_trajectory <- function(t, v_p, t_p, lambda_g, lambda_d) {
  vt <- 2 * 10^v_p / (exp(-lambda_g * (t - t_p)) + exp(lambda_d * (t - t_p)))
  log10(vt)
}



#' Default viral-load parameter means by role
#'
#' Role-specific means used to generate viral-load trajectories.
#'
#' @format A named list with elements \code{adult}, \code{child}, \code{toddler},
#'   \code{elderly}; each is a list with \code{v_p}, \code{t_p}, \code{lambda_g},
#'   \code{lambda_d}.
#' @keywords internal
#' @noRd
VL_params <- list(
  adult   = list(v_p = 4.14, t_p = 5.09, lambda_g = 2.31, lambda_d = 2.71),
  child   = list(v_p = 5.84, t_p = 3.09, lambda_g = 2.82, lambda_d = 1.01),
  toddler = list(v_p = 5.84, t_p = 3.09, lambda_g = 2.82, lambda_d = 1.01),
  elderly = list(v_p = 2.95, t_p = 5.10, lambda_g = 3.15, lambda_d = 0.87)
)



#' Draw role-specific viral-load parameters
#'
#' Returns the parameter set used to generate a viral-load trajectory for a role.
#'
#' @param role Character; one of \code{"adult"}, \code{"child"}, \code{"toddler"}, \code{"elderly"}.
#' @return A list with \code{v_p}, \code{t_p}, \code{lambda_g}, \code{lambda_d}.
#' @keywords internal
#' @noRd
draw_random_VL_params <- function(role) {
  p <- VL_params[[role]]
  if (is.null(p)) stop("Unknown role in VL_params: ", role)
  list(v_p = p$v_p, t_p = p$t_p, lambda_g = p$lambda_g, lambda_d = p$lambda_d)
}



#' Cosine seasonal multiplier (non-negative)
#'
#' Cosine-based seasonal multiplier bounded below by 0.
#'
#' @param day Integer vector of days.
#' @param peak_day Integer; day of maximum.
#' @param amplitude Numeric; cosine amplitude.
#' @param period Integer; period length.
#'
#' @return Numeric vector of multipliers.
seasonal_cosine <- function(day, peak_day = 350, amplitude = 0.8, period = 365) {
  pmax(1 + amplitude * cos(2 * pi * (day - peak_day) / period), 0)
}



#' Generate a simple household role composition
#'
#' Samples a household (size 3–5) with at least one child and two adults,
#' optionally adding elderly/toddler roles.
#'
#' @return Character vector of role labels.
generate_household_roles <- function() {
  n <- sample(3:5, 1)
  roles <- c(rep("child", 1), rep("adult", 2))
  remaining <- n - length(roles)
  if (remaining > 0) {
    n_elderly <- sample(0:min(2, remaining), 1)
    roles <- c(roles, rep("elderly", n_elderly))
    remaining <- remaining - n_elderly
  }
  if (remaining > 0) {
    roles <- c(roles, sample(c("toddler"), remaining, replace = TRUE))
  }
  sample(roles)
}



#' Simulate one household (RSV/VL engine)
#'
#' Simulates infection events, infectious periods, detection, and VL trajectories
#' for a single household over a fixed horizon.
#'
#' @param hh_id Household identifier.
#' @param roles Character vector of roles (one per person).
#' @param alpha_comm_by_role Numeric; baseline community hazard (scalar).
#' @param beta1,beta2,delta,rho,V_ref Numeric; transmission constants.
#' @param phi_by_role,kappa_by_role Named numeric vectors; susceptibility and
#'   infectivity multipliers by role.
#' @param latent_shape,latent_scale,infectious_shape,infectious_scale Numeric;
#'   gamma parameters for latent/infectious durations.
#' @param peak_day,width Numeric; infectivity profile controls.
#' @param max_days Integer; simulation horizon (days).
#' @param test_weekly_before_detection Logical; weekly testing before first detection.
#' @param perfect_detection Logical; perfect detection on test days.
#' @param contact_mat Optional square contact matrix (no self-contact).
#' @param verbose Logical; print progress.
#' @param seasonal_forcing_list Optional list of length 4 with role-specific
#'   seasonal multipliers (length \code{max_days}).
#'
#' @return A data frame with columns
#'   \code{hh_id}, \code{person_id}, \code{role}, \code{infection_time},
#'   \code{infectious_start}, \code{infectious_end}, \code{detection_time},
#'   \code{infection_resolved}, plus list-columns \code{vl_full_trajectory},
#'   \code{viral_loads_test_days}. Attributes \code{test_days} and \code{params}
#'   are attached.
simulate_one_household_comm <- function(
    hh_id,
    roles,
    alpha_comm_by_role,
    beta1 = 0.06, beta2 = 1e-7, delta = 1, rho = 3, V_ref = 1e7,
    phi_by_role = c(adult = 1.0, child = 1.1, elderly = 0.8, toddler = 1.2),
    kappa_by_role = c(adult = 1.0, child = 1.1, elderly = 0.7, toddler = 1.2),
    latent_shape = 2, latent_scale = 1,
    infectious_shape = 2, infectious_scale = 2,
    peak_day = 2, width = 2,
    max_days = 40,
    test_weekly_before_detection = TRUE,
    perfect_detection = TRUE,
    contact_mat = NULL,
    verbose = FALSE,
    seasonal_forcing_list = NULL
) {
  n <- length(roles)
  infection_time <- infectious_start <- infectious_end <- detection_time <- infection_resolved <- rep(NA_integer_, n)
  vl_trajs <- vector("list", n)
  viral_loads_log10 <- vector("list", n)

  # durations
  latent_period <- pmax(1, ceiling(stats::rgamma(n, shape = latent_shape, scale = latent_scale)))
  infectious_period <- pmax(1, ceiling(stats::rgamma(n, shape = infectious_shape, scale = infectious_scale)))

  # contact matrix
  if (is.null(contact_mat)) {
    contact_mat <- matrix(1, n, n); diag(contact_mat) <- 0
  }

  # infectivity profile (Gaussian-like, normalized)
  g_profile <- exp(-0.5 * ((1:max_days - peak_day)/width)^2)
  g_profile <- g_profile / max(g_profile)

  household_detected <- FALSE

  # simulate days 1..max_days
  for (t in 1:max_days) {
    test_today <- if (test_weekly_before_detection && !household_detected) ((t - 1) %% 7 == 0) else TRUE

    # attempt infection for susceptibles
    for (j in seq_len(n)) {
      if (!is.na(infection_time[j]) && infection_time[j] < t) next

      # household hazard from infectors i -> j
      lambda_house_j <- 0
      for (i in seq_len(n)) {
        if (i == j) next
        if (is.na(infection_time[i]) || is.na(infectious_start[i]) || is.na(infectious_end[i])) next
        if (infectious_start[i] >= t) next

        tau <- t - infectious_start[i]
        gval <- if (tau >= 1 && tau <= length(g_profile)) g_profile[tau] else 0

        if (is.null(vl_trajs[[i]])) {
          t_seq <- seq(infection_time[i], infection_resolved[i])
          t_rel <- t_seq - infection_time[i]
          p <- draw_random_VL_params(roles[i])
          vl_trajs[[i]] <- simulate_viral_load_trajectory(t_rel, p$v_p, p$t_p, p$lambda_g, p$lambda_d)
        }

        rel_day_idx <- t - infection_time[i] + 1
        log10V <- if (rel_day_idx >= 1 && rel_day_idx <= length(vl_trajs[[i]])) vl_trajs[[i]][rel_day_idx] else -5
        V_it <- ifelse(is.na(log10V), -5, log10V)

        scaling_n <- (1 / max(n, 1))^delta
        term1 <- beta1 * gval
        term2 <- beta2 * p_transmission(VL = V_it, n = 3.91, p_v = .83e-2, K = 5.39, Vm = 6.59, sai = 4.57)
        term1[is.na(term1)] <- 0; term2[is.na(term2)] <- 0
        h_ij_t <- scaling_n * (term1 + term2)

        lambda_house_j <- lambda_house_j + kappa_by_role[roles[i]] * h_ij_t * contact_mat[j, i]
      }

      # community + household hazard
      if (is.null(seasonal_forcing_list)) {
        season_mult_j <- 1
      } else {
        season_mult_j <- seasonal_forcing_list[[roles[j]]][t]
      }
      alpha_comm_j <- alpha_comm_by_role * season_mult_j
      lambda_jt <- phi_by_role[roles[j]] * (alpha_comm_j + lambda_house_j)
      lambda_jt <- pmin(pmax(lambda_jt, 0), 1e6)
      p_jt <- 1 - exp(-lambda_jt)
      p_jt <- pmin(pmax(p_jt, 1e-20), 1 - 1e-20)

      if (is.na(infection_time[j]) && stats::runif(1) < p_jt) {
        infection_time[j]   <- t
        infectious_start[j] <- pmin(infection_time[j] + latent_period[j], max_days)
        infectious_end[j]   <- pmin(infectious_start[j] + infectious_period[j] - 1, max_days)
        infection_resolved[j] <- pmin(infectious_end[j] + 1, max_days)

        t_seq <- seq(infection_time[j], infection_resolved[j])
        t_rel <- t_seq - infection_time[j]
        p <- draw_random_VL_params(roles[j])
        vl_trajs[[j]] <- simulate_viral_load_trajectory(t_rel, p$v_p, p$t_p, p$lambda_g, p$lambda_d)
      }
    }

    # detection
    if (test_today) {
      for (i in seq_len(n)) {
        if (is.na(infection_time[i]) || is.na(infectious_start[i]) || is.na(infectious_end[i])) next
        if (t < infectious_start[i] || t > infectious_end[i]) next
        if (is.na(detection_time[i]) && perfect_detection) {
          detection_time[i] <- t
          household_detected <- TRUE
        }
      }
    }
  }

  # ensure detection assigned for any infected without it
  for (i in seq_len(n)) {
    if (!is.na(infection_time[i]) && is.na(detection_time[i])) {
      detection_time[i] <- if (!is.na(infectious_start[i]) && infectious_start[i] <= max_days) infectious_start[i] else infection_time[i]
    }
  }

  # test schedule: weekly until first detection, then daily
  first_det <- if (all(is.na(detection_time))) NA_integer_ else min(detection_time, na.rm = TRUE)
  test_days <- integer(0)
  for (d in seq_len(max_days)) {
    if (!is.na(first_det)) {
      if (d < first_det) { if ((d - 1) %% 7 == 0) test_days <- c(test_days, d) } else test_days <- c(test_days, d)
    } else {
      if ((d - 1) %% 7 == 0) test_days <- c(test_days, d)
    }
  }

  # test-day VLs (NA for non-infectious days)
  for (i in seq_len(n)) {
    if (is.null(vl_trajs[[i]]) || all(is.na(vl_trajs[[i]]))) {
      viral_loads_log10[[i]] <- -5
    } else {
      vals <- vapply(test_days, function(dd) {
        if (is.na(infectious_start[i]) || is.na(infectious_end[i])) return(-5)
        if (dd < infectious_start[i] || dd > infectious_end[i]) return(-5)
        rel_day <- dd - infection_time[i] + 1
        if (rel_day <= length(vl_trajs[[i]])) vl_trajs[[i]][rel_day] else -5
      }, numeric(1))
      names(vals) <- as.character(test_days)
      viral_loads_log10[[i]] <- vals
    }
  }

  hh_df <- data.frame(
    hh_id = hh_id,
    person_id = seq_len(n),
    role = roles,
    infection_time = ifelse(is.infinite(infection_time), NA_integer_, infection_time),
    infectious_start = ifelse(is.infinite(infectious_start), NA_integer_, infectious_start),
    infectious_end = ifelse(is.infinite(infectious_end), NA_integer_, infectious_end),
    detection_time = ifelse(is.infinite(detection_time), NA_integer_, detection_time),
    infection_resolved = ifelse(is.infinite(infection_resolved), NA_integer_, infection_resolved),
    stringsAsFactors = FALSE
  )
  hh_df$vl_full_trajectory   <- vl_trajs
  hh_df$viral_loads_test_days <- viral_loads_log10
  attr(hh_df, "test_days") <- test_days
  attr(hh_df, "params") <- list(beta1 = beta1, beta2 = beta2, delta = delta, rho = rho, V_ref = V_ref)
  hh_df
}



#' Simulate multiple households (RSV/VL engine)
#'
#' Runs \code{\link{simulate_one_household_comm}} across households and returns
#' both the stacked data frame and the per-household list.
#'
#' @param n_households Integer; number of households.
#' @inheritParams simulate_one_household_comm
#'
#' @return A list with:
#' \describe{
#'   \item{\code{hh_df}}{Stacked data frame across households.}
#'   \item{\code{households}}{List of per-household data frames.}
#' }
simulate_multiple_households_comm <- function(
    n_households = 200,
    alpha_comm_by_role = 5e-3,
    beta1 = 0.06, beta2 = 1e-7, delta = 1, rho = 3, V_ref = 1e7,
    phi_by_role = c(adult = 1.0, child = 1.1, elderly = 0.8, toddler = 1.2),
    kappa_by_role = c(adult = 1.0, child = 1.1, elderly = 0.7, toddler = 1.2),
    latent_shape = 2, latent_scale = 1,
    infectious_shape = 2, infectious_scale = 2,
    peak_day = 2, width = 2,
    max_days = 40, verbose = FALSE,
    seasonal_forcing_list = NULL
) {
  households <- vector("list", n_households)
  for (h in seq_len(n_households)) {
    roles <- generate_household_roles()
    households[[h]] <- simulate_one_household_comm(
      hh_id = paste0("HH", h), roles = roles,
      alpha_comm_by_role = alpha_comm_by_role,
      beta1 = beta1, beta2 = beta2, delta = delta, rho = rho, V_ref = V_ref,
      phi_by_role = phi_by_role, kappa_by_role = kappa_by_role,
      latent_shape = latent_shape, latent_scale = latent_scale,
      infectious_shape = infectious_shape, infectious_scale = infectious_scale,
      peak_day = peak_day, width = width,
      max_days = max_days, verbose = verbose,
      seasonal_forcing_list = seasonal_forcing_list
    )
  }
  hh_df <- do.call(rbind, households)
  rownames(hh_df) <- NULL
  list(hh_df = hh_df, households = households)
}



#' Normalize role labels to child, toddler, adult, elderly
#'
#' Maps common variants (e.g., \code{"infant" -> "toddler"}, \code{"sibling" -> "child"}).
#'
#' @param x Character vector of role labels.
#' @return Character vector of normalized labels.
#' @keywords internal
.norm_role <- function(x) {
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



#' Extract index-case roles from simulated households
#'
#' Returns the role of the earliest infected individual (index case) per household.
#'
#' @param households List of household data frames with \code{infection_time} and \code{role}.
#' @return Character vector of index roles (or \code{NA_character_} if no infections).
#' @keywords internal
.get_index_roles_from_households <- function(households) {
  sapply(households, function(hh) {
    if (all(is.na(hh$infection_time))) return(NA_character_)
    tmin <- min(hh$infection_time, na.rm = TRUE)
    # choose the earliest infected person; if tie, first in row order
    idxs <- which(!is.na(hh$infection_time) & hh$infection_time == tmin)
    hh$role[idxs[1]]
  })
}



#' Validate user-supplied long data for legacy MLE
#'
#' Ensures the presence of required columns and coerces \code{test_date} to
#' integer day indices when given as \code{Date}.
#'
#' @param user_data Long table with one row per person-day.
#' @param start_date \code{Date} defining day 1 when \code{test_date} is a \code{Date}.
#' @return A validated/coerced data frame.
#' @keywords internal
.validate_user_data_mle <- function(user_data, start_date) {
  required <- c("HH", "individual_ID", "role",
                "test_date", "infection_status", "community_risk")
  missing <- setdiff(required, names(user_data))

  if (length(missing) > 0L) {
    stop(
      paste0(
        "For estimation_method = 'mle', `user_data` must be a long household-testing table\n",
        "with one row per person-day and at least the columns:\n",
        "  HH, individual_ID, role, test_date, infection_status, community_risk.\n",
        "The following required columns are missing:\n  ",
        paste(missing, collapse = ", "), "\n\n",
        "Please reformat your data so that each row is one individual on one test_date,\n",
        "with infection_status (0/1) and community_risk recorded."
      ),
      call. = FALSE
    )
  }

  # Coerce test_date if it's a Date
  if (inherits(user_data$test_date, "Date")) {
    if (missing(start_date) || is.null(start_date)) {
      stop("When `test_date` is a Date, please supply `start_date` to define day 1.",
           call. = FALSE)
    }
    user_data$test_date <- as.integer(user_data$test_date - start_date) + 1L
  }

  user_data
}


#' Normalize various household inputs into a per-household list
#'
#' Accepts (i) an object with \code{$households}, (ii) a list of data frames,
#'
#' or (iii) a long data frame with a recognizable household id column, and
#' returns a list of per-household data frames.
#'
#' @param obj Mixed household representation.
#' @return List of per-household data frames.
#' @keywords internal
.normalize_households_input <- function(obj) {
  # NEW: honor a per-HH list stored as an attribute on a long test table
  hh_attr <- attr(obj, "households", exact = TRUE)
  if (!is.null(hh_attr)) {
    if (!length(hh_attr) || !all(vapply(hh_attr, is.data.frame, logical(1)))) {
      stop("Attribute 'households' exists but is not a list of data.frames.", call. = FALSE)
    }
    return(hh_attr)
  }

  # Case A: simulator-style list with $households
  if (is.list(obj) && !is.data.frame(obj) && !is.null(obj$households)) {
    hh <- obj$households
    if (!length(hh) || !all(vapply(hh, is.data.frame, logical(1)))) {
      stop("`$households` exists but is not a list of data.frames.", call. = FALSE)
    }
    return(hh)
  }

  # Case B: already a list of per-HH data.frames
  if (is.list(obj) && length(obj) && all(vapply(obj, is.data.frame, logical(1)))) {
    return(obj)
  }

  # Case C: a single long data.frame/tibble/data.table -> split by a household id column
  if (inherits(obj, c("data.frame", "tbl_df", "tbl", "data.table"))) {
    df <- as.data.frame(obj)

    # Accept multiple common names for household id
    candidates <- c("hh_id","HH","household","household_id","householdID","hh","hid")
    key <- candidates[candidates %in% names(df)]
    if (length(key) == 0L) {
      stop(
        "RSV/VL+Stan received a data.frame but cannot find a household id column.\n",
        "Please include one of: hh_id, HH, household, household_id, householdID, hh, hid.",
        call. = FALSE
      )
    }
    key <- key[1]  # first match

    # Create a clean character hh_id, then split
    df$hh_id <- as.character(df[[key]])
    households <- split(df, df$hh_id, drop = TRUE)

    # Keep names stable
    nm <- unique(df$hh_id)
    names(households) <- nm
    return(households)
  }

  stop(
    "RSV/VL+Stan path expects (i) list(hh_df=..., households=...), or ",
    "(ii) a list of per-household data.frames, or ",
    "(iii) a long data.frame with a household id column.",
    call. = FALSE
  )
}
