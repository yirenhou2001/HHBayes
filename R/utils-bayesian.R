#' Prepare per-household inputs for the Stan RSV/VL model
#'
#' Converts user data to a list of per-household data frames required by the
#' Stan RSV/VL pipeline. Accepts either a long test-day table or a per-person
#' episodes table.
#'
#' @param user_data Either:
#'   \itemize{
#'     \item \emph{Long format:} columns \code{HH}, \code{individual_ID},
#'           \code{role}, \code{test_date}, \code{infection_status}
#'           (optionally a VL column); or
#'     \item \emph{Per-person format:} columns \code{hh_id}, \code{person_id},
#'           \code{role}, \code{infection_time}, \code{infectious_start},
#'           \code{infectious_end} (optionally \code{vl_full_trajectory}).
#'   }
#' @param role_levels Character vector of allowed roles after normalization,
#'   e.g., \code{c("adult","child","elderly","toddler")}.
#' @param vl_mode Character; currently used only to trigger trajectory building
#'   from long data (\code{"from_long"}).
#' @param vl_source Character; one of \code{"none"}, \code{"column"}, \code{"simulate"}
#'   (applies to long-format input).
#' @param vl_column Optional name of the VL column when \code{vl_source = "column"}.
#' @param start_date,end_date Optional \code{Date} bounds to convert \code{test_date}
#'   to day indices (long format).
#'
#' @return A named list of data frames (one per household) with columns
#'   \code{hh_id}, \code{person_id}, \code{role}, \code{infection_time},
#'   \code{infectious_start}, \code{infectious_end}, \code{detection_time},
#'   \code{infection_resolved}, and \code{vl_full_trajectory} (list-column).
prepare_stan_households_from_user_data <- function(
    user_data,
    role_levels = c("adult","child","elderly","toddler"),
    vl_mode   = c("from_long","auto"),
    vl_source = c("none","column","simulate"),
    vl_column = NULL,
    start_date = NULL,
    end_date   = NULL
) {
  vl_mode   <- match.arg(vl_mode)
  vl_source <- match.arg(vl_source)

  # -----------------------------
  # Helper: normalize roles
  # -----------------------------
  norm_role_vec <- function(x) {
    x <- tolower(as.character(x))
    x[x == "infant"]  <- "toddler"
    x[x == "baby"]    <- "toddler"
    x[x == "sibling"] <- "child"
    x[x == "kid"]     <- "child"
    x[x == "parent"]  <- "adult"
    x[x == "mother"]  <- "adult"
    x[x == "father"]  <- "adult"
    x[x == "elder"]   <- "elderly"
    x[x == "grandparent"] <- "elderly"
    x
  }

  # ================================================================
  # Case 1: per-person episodes table (Stan-ready)
  # ================================================================
  req_person_core <- c("hh_id","person_id","role",
                       "infection_time","infectious_start","infectious_end","infection_resolved")
  if (inherits(user_data, "data.frame") && all(req_person_core %in% names(user_data))) {
    df <- data.frame(user_data, stringsAsFactors = FALSE)

    # normalize roles and validate
    df$role <- norm_role_vec(df$role)
    bad_roles <- setdiff(unique(df$role), role_levels)
    if (length(bad_roles)) {
      stop("Unknown role labels remain in per-person data: ",
           paste(bad_roles, collapse = ", "), call. = FALSE)
    }

    # make detection_time optional -> default to infectious_start (or infection_time)
    if (!"detection_time" %in% names(df)) {
      df$detection_time <- ifelse(is.finite(df$infectious_start), df$infectious_start, df$infection_time)
    }

    # ensure vl_full_trajectory list-col exists (placeholder if absent)
    if (!"vl_full_trajectory" %in% names(df)) {
      vl_list <- vector("list", nrow(df))
      for (i in seq_len(nrow(df))) {
        it <- df$infection_time[i]; ir <- df$infection_resolved[i]
        if (is.na(it) || is.na(ir) || ir < it) vl_list[[i]] <- numeric(0) else vl_list[[i]] <- rep(-5, ir - it + 1L)
      }
      df$vl_full_trajectory <- vl_list
    }

    # split to list of per-HH data.frames
    df$hh_id     <- as.character(df$hh_id)
    df$person_id <- as.integer(df$person_id)
    df          <- df[order(df$hh_id, df$person_id), , drop = FALSE]
    households  <- split(df, df$hh_id, drop = TRUE)
    return(households)
  }

  # ================================================================
  # Case 2: long-format testing table
  # ================================================================
  required_long <- c("HH","individual_ID","role","test_date","infection_status")
  if (!all(required_long %in% names(user_data))) {
    stop(
      "Long-format user_data is missing required columns: ",
      paste(setdiff(required_long, names(user_data)), collapse = ", "),
      "\nIf you intended to pass a per-person episodes table, ensure it has columns:\n",
      paste(req_person_core, collapse = ", "),
      call. = FALSE
    )
  }

  dt <- data.table::as.data.table(user_data)

  # coerce & normalize
  dt[, HH            := as.character(HH)]
  dt[, individual_ID := as.integer(individual_ID)]
  dt[, role          := norm_role_vec(role)]
  dt[, infection_status := as.integer(infection_status)]

  # test_date -> integer day
  if (inherits(dt$test_date, "Date")) {
    if (is.null(start_date)) start_date <- min(dt$test_date, na.rm = TRUE)
    dt[, day := as.integer(test_date - start_date) + 1L]
  } else {
    dt[, day := as.integer(test_date)]
  }

  # role check
  bad_roles <- setdiff(unique(dt$role), role_levels)
  if (length(bad_roles)) {
    stop("Unknown role labels remain in long data: ",
         paste(bad_roles, collapse = ", "), call. = FALSE)
  }

  # collapse to per-person episodes
  person <- dt[, {
    infected_days <- day[infection_status == 1L]
    if (length(infected_days) == 0L) {
      list(
        infection_time     = NA_integer_,
        infectious_start   = NA_integer_,
        infectious_end     = NA_integer_,
        detection_time     = NA_integer_,
        infection_resolved = NA_integer_
      )
    } else {
      it <- min(infected_days)
      ie <- max(infected_days)
      list(
        infection_time     = it,
        infectious_start   = it,
        infectious_end     = ie,
        detection_time     = it,
        infection_resolved = ie + 1L
      )
    }
  }, by = .(hh_id = HH, person_id = individual_ID, role)]

  person <- as.data.frame(person, stringsAsFactors = FALSE)

  # attach a vl_full_trajectory list-col
  vl_list <- vector("list", nrow(person))
  if (vl_source == "none") {
    for (i in seq_len(nrow(person))) {
      it <- person$infection_time[i]; ir <- person$infection_resolved[i]
      if (is.na(it) || is.na(ir) || ir < it) vl_list[[i]] <- numeric(0) else vl_list[[i]] <- rep(-5, ir - it + 1L)
    }
  } else if (vl_source == "column") {
    if (is.null(vl_column) || !vl_column %in% names(dt)) {
      stop("vl_source = 'column' but `vl_column` not found in user_data.", call. = FALSE)
    }
    max_day <- max(dt$day, na.rm = TRUE)
    for (i in seq_len(nrow(person))) {
      sub <- dt[dt$HH == person$hh_id[i] & dt$individual_ID == person$person_id[i], ]
      traj <- rep(-5, max_day)
      if (nrow(sub)) {
        idx  <- sub$day
        vals <- sub[[vl_column]]
        ok   <- !is.na(idx) & idx >= 1 & idx <= max_day
        traj[idx[ok]] <- vals[ok]
      }
      vl_list[[i]] <- traj
    }
  } else if (vl_source == "simulate") {
    for (i in seq_len(nrow(person))) {
      it <- person$infection_time[i]; ir <- person$infection_resolved[i]
      if (is.na(it) || is.na(ir) || ir < it) {
        vl_list[[i]] <- numeric(0)
      } else {
        t_rel <- 0:as.integer(ir - it)
        rp <- draw_random_VL_params(person$role[i])
        vl_list[[i]] <- simulate_viral_load_trajectory(
          t = t_rel,
          v_p      = rp$v_p,
          t_p      = rp$t_p,
          lambda_g = rp$lambda_g,
          lambda_d = rp$lambda_d
        )
      }
    }
  }

  person$vl_full_trajectory <- vl_list

  # split to per-HH list for Stan
  person$hh_id     <- as.character(person$hh_id)
  person$person_id <- as.integer(person$person_id)
  person$role      <- factor(person$role, levels = role_levels)
  person <- person[order(person$hh_id, person$person_id), , drop = FALSE]
  households <- split(person, person$hh_id, drop = TRUE)
  households
}



#' Attach viral-load trajectories to person-level data
#'
#' Creates or fills a \code{vl_full_trajectory} list-column for each person over
#' their infectious span using VL from a long table, simulated VL, or a sentinel.
#'
#' @param hh_long Long test-day data (may be \code{NULL} if simulating); must
#'   include \code{HH}, \code{individual_ID}, \code{test_date} and the VL column
#'   when \code{vl_source = "column"}.
#' @param hh_person Person-level data frame with infection/imputation columns.
#' @param user_data Original user input (not modified; passed for convenience).
#' @param vl_source One of \code{"column"}, \code{"simulate"}, \code{"none"}.
#' @param vl_column Column name with log10 VL when \code{vl_source = "column"}.
#' @param start_date,end_date Study window (\code{Date}); used for alignment.
#'
#' @return \code{hh_person} with a \code{vl_full_trajectory} list-column added.
attach_vl_full_trajectory_from_long <- function(
    hh_long,                # long test-day df (may be NULL if simulating)
    hh_person,              # person-level households df being built
    user_data,              # original user_data for convenience
    vl_source = c("column","simulate","none"),
    vl_column = NULL,
    start_date, end_date
) {
  vl_source <- match.arg(vl_source)
  n <- nrow(hh_person)
  vl_list <- vector("list", n)

  # Map roles for simulation if needed
  role_vec <- tolower(hh_person$role)

  for (i in seq_len(n)) {
    inf_t <- hh_person$infection_time[i]
    inf_start <- hh_person$infectious_start[i]
    inf_end   <- hh_person$infectious_end[i]

    if (is.na(inf_t) || is.na(inf_start) || is.na(inf_end) || inf_end < inf_start) {
      vl_list[[i]] <- numeric(0)
      next
    }

    days <- seq(inf_t, inf_end)
    len  <- length(days)

    if (vl_source == "column") {
      if (is.null(vl_column) || !vl_column %in% names(hh_long)) {
        stop("vl_source='column' requires `vl_column` present in the long data.", call. = FALSE)
      }
      # Match this person's long rows and pull VLs on those test days
      rows_i <- hh_long$HH == hh_person$hh_id[i] &
        hh_long$individual_ID == hh_person$person_id[i]
      sub <- hh_long[rows_i, c("test_date", vl_column)]
      names(sub) <- c("test_date","vl")
      # initialize with sentinel -5 (no VL)
      traj <- rep(-5, len)
      if (nrow(sub)) {
        on_span <- sub$test_date >= inf_start & sub$test_date <= inf_end
        if (any(on_span)) {
          idx <- match(sub$test_date[on_span], days)
          traj[idx] <- as.numeric(sub$vl[on_span])
          traj[!is.finite(traj)] <- -5
        }
      }
      vl_list[[i]] <- traj

    } else if (vl_source == "simulate") {
      # Use your RSV/VL trajectory generator
      t_rel <- seq(0, len)  # 0..len
      p <- draw_random_VL_params(role_vec[i])
      traj <- simulate_viral_load_trajectory(t_rel, p$v_p, p$t_p, p$lambda_g, p$lambda_d)
      traj <- traj[seq_len(len)]
      traj[!is.finite(traj)] <- -5
      vl_list[[i]] <- traj

    } else { # "none"
      vl_list[[i]] <- rep(-5, len)
    }
  }

  hh_person$vl_full_trajectory <- vl_list
  hh_person
}



#' Normalize role labels to child, toddler, adult, elderly
#'
#' Maps common synonyms (\code{infant->toddler}, \code{sibling->child},
#' \code{parent->adult}, \code{elder->elderly}) and returns a factor with
#' consistent levels.
#'
#' @param x Character vector of role labels.
#' @return A factor with levels \code{c("child","toddler","adult","elderly")}.
normalize_roles <- function(x) {
  trans <- c(infant = "toddler", sibling = "child", parent = "adult", elder = "elderly")
  y <- ifelse(x %in% names(trans), unname(trans[x]), x)
  factor(y, levels = c("child","toddler","adult","elderly"))
}



#' Convert simulator households to a long test-day table
#'
#' Flattens simulator output (list of per-household data frames) into a long
#' table with one row per person-day.
#'
#' @param households List of household data frames from the simulator (each may
#'   carry attributes such as \code{test_days} and per-person
#'   \code{viral_loads_test_days}).
#' @param seasonal_forcing_list Optional named list of role-specific forcing
#'   series (\code{adult}, \code{child}, \code{elderly}, \code{toddler}).
#'
#' @return A data frame with columns
#'   \code{HH}, \code{individual_ID}, \code{role}, \code{test_date},
#'   \code{infection_status}, \code{community_risk}, \code{vl_test}.
households_to_long_tests <- function(households, seasonal_forcing_list = NULL) {
  empty_out <- data.frame(
    HH = integer(0),
    individual_ID = integer(0),
    role = character(0),
    test_date = integer(0),
    infection_status = integer(0),
    community_risk = numeric(0),
    vl_test = numeric(0),
    stringsAsFactors = FALSE
  )
  if (is.null(households) || length(households) == 0L) return(empty_out)

  out_list <- vector("list", length(households))

  for (h in seq_along(households)) {
    hh <- households[[h]]
    if (is.null(hh) || nrow(hh) == 0L) {
      out_list[[h]] <- empty_out[0, ]
      next
    }

    need_cols <- c("individual_ID","role","infection_status",
                   "infectious_start","infectious_end")
    for (nm in need_cols) if (!nm %in% names(hh)) hh[[nm]] <- NA

    has_comm_risk <- "community_risk" %in% names(hh)
    has_vl_full   <- "vl_full_trajectory" %in% names(hh)
    has_vl_days   <- "viral_loads_test_days" %in% names(hh)

    hh$infection_status <- as.integer(ifelse(is.na(hh$infection_status), 0L, hh$infection_status))

    test_days_attr <- attr(hh, "test_days", exact = TRUE)
    candidates <- c(
      suppressWarnings(as.integer(test_days_attr)),
      suppressWarnings(as.integer(hh$infectious_end)),
      suppressWarnings(as.integer(hh$infection_resolved))
    )
    max_days <- suppressWarnings(max(candidates, na.rm = TRUE))
    if (!is.finite(max_days) || is.na(max_days) || max_days < 1L) max_days <- 1L
    max_days <- as.integer(max_days)

    rows <- vector("list", nrow(hh) * max_days)
    k <- 0L
    for (i in seq_len(nrow(hh))) {
      id_i   <- hh$individual_ID[i]
      role_i <- hh$role[i]

      s_i <- suppressWarnings(as.integer(hh$infectious_start[i]))
      e_i <- suppressWarnings(as.integer(hh$infectious_end[i]))
      status1 <- isTRUE(hh$infection_status[i] == 1L)

      comm_i <- if (has_comm_risk) suppressWarnings(as.numeric(hh$community_risk[i])) else NA_real_

      vl_full_i <- if (has_vl_full) hh$vl_full_trajectory[[i]] else NULL
      vl_days_i <- if (has_vl_days) hh$viral_loads_test_days[[i]] else NULL

      for (d in seq_len(max_days)) {
        in_window <- (!is.na(s_i) && !is.na(e_i) && d >= s_i && d <= e_i)
        infect_today <- as.integer(status1 && in_window)

        vl_val <- NA_real_
        if (!is.null(vl_days_i)) {
          nm <- as.character(d)
          if (!is.null(names(vl_days_i)) && nm %in% names(vl_days_i)) {
            vl_val <- suppressWarnings(as.numeric(vl_days_i[[nm]]))
          }
        } else if (!is.null(vl_full_i) && length(vl_full_i) >= d) {
          vl_val <- suppressWarnings(as.numeric(vl_full_i[d]))
        }

        k <- k + 1L
        rows[[k]] <- list(
          HH = as.integer(h),
          individual_ID = as.integer(id_i),
          role = as.character(role_i),
          test_date = as.integer(d),
          infection_status = infect_today,
          community_risk = comm_i,
          vl_test = vl_val
        )
      }
    }

    hh_long <- do.call(rbind.data.frame, rows)
    out_list[[h]] <- hh_long
  }

  out <- do.call(rbind.data.frame, out_list)
  rownames(out) <- NULL
  out$HH               <- as.integer(out$HH)
  out$individual_ID    <- as.integer(out$individual_ID)
  out$role             <- as.character(out$role)
  out$test_date        <- as.integer(out$test_date)
  out$infection_status <- as.integer(out$infection_status)
  out$community_risk   <- as.numeric(out$community_risk)
  out$vl_test          <- as.numeric(out$vl_test)
  out
}


#' Built-in minimal Stan models
#'
#' Returns a small set of embedded Stan programs for testing or fallback use.
#'
#' @return Named list of character strings containing Stan code.
#' @keywords internal
.builtin_stan_models <- function() {
  list(
    "household_min" = "
data {
  int<lower=1> N;            // number of observations
  vector[N] y;               // toy response
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(0,5);
  sigma ~ normal(0,2);
  y ~ normal(mu, sigma);
}
generated quantities {
  real phi_adult   = normal_rng(0,1);
  real phi_child   = normal_rng(0,1);
  real phi_elderly = normal_rng(0,1);
  real phi_toddler = normal_rng(0,1);
  real kappa_adult   = normal_rng(0,1);
  real kappa_child   = normal_rng(0,1);
  real kappa_elderly = normal_rng(0,1);
  real kappa_toddler = normal_rng(0,1);
}
"
  )
}

#' Resolve Stan model source (file or code)
#'
#' Chooses between direct Stan code, a file path, a packaged file via
#' \code{system.file()}, a \code{builtin:<key>} reference, or a tiny fallback.
#'
#' @param stan_file Character or \code{NULL}. Path, \code{builtin:<key>}, or packaged file name.
#' @param stan_code Character or \code{NULL}. Stan program text.
#' @param package_name Character or \code{NULL}. Package name for \code{system.file()} lookup.
#'
#' @return List with elements \code{file} (normalized path or \code{NULL})
#'   and \code{code} (Stan text or \code{NULL}).
#' @keywords internal
.resolve_stan_model <- function(stan_file = NULL, stan_code = NULL, package_name = NULL) {
  # (A) Direct code provided
  if (!is.null(stan_code) && nzchar(stan_code)) {
    return(list(file = NULL, code = stan_code))
  }

  # (B) File path provided and exists
  if (!is.null(stan_file) && nzchar(stan_file) && file.exists(stan_file)) {
    return(list(file = normalizePath(stan_file), code = NULL))
  }

  # (C) system.file lookup (inst/stan/<file>) if a package name is known
  if (!is.null(stan_file) && nzchar(stan_file) && !is.null(package_name)) {
    sysf <- tryCatch(system.file("stan", stan_file, package = package_name), error = function(e) "")
    if (!is.null(sysf) && nzchar(sysf) && file.exists(sysf)) {
      return(list(file = normalizePath(sysf), code = NULL))
    }
  }

  # (D) builtin:<key>
  if (!is.null(stan_file) && grepl("^builtin:", stan_file)) {
    key <- sub("^builtin:", "", stan_file)
    built <- .builtin_stan_models()[[key]]
    if (!is.null(built)) return(list(file = NULL, code = built))
  }

  # (E) CHANGED: Stop with a clear error instead of using a fallback
  # This prevents the "variable y does not exist" error
  if (is.null(stan_file)) {
    stop("No 'stan_file' or 'stan_code' was provided.")
  } else {
    stop(paste0(
      "Stan file not found: '", stan_file, "'\n",
      "Current working directory: ", getwd(), "\n",
      "Please provide the full absolute path or check your file name."
    ))
  }
}

#' Build a role-specific seasonal forcing matrix
#'
#' Stacks role-specific forcing series into a \eqn{T \times 4} matrix for Stan.
#'
#' @param seasonal_forcing_list Named list with entries \code{adult}, \code{child},
#'   \code{elderly}, \code{toddler}; each a numeric vector of length \eqn{\ge} \code{T_max}.
#' @param T_max Integer; number of rows (days).
#'
#' @return A numeric matrix with columns \code{adult}, \code{child},
#'   \code{elderly}, \code{toddler} and \code{T_max} rows.
seasonal_forcing_to_series <- function(seasonal_forcing_list, T_max = 365) {
  stopifnot(all(c("adult","child","elderly","toddler") %in% names(seasonal_forcing_list)))
  stopifnot(length(seasonal_forcing_list$adult) >= T_max)
  cbind(
    adult   = seasonal_forcing_list$adult[1:T_max],
    child   = seasonal_forcing_list$child[1:T_max],
    elderly = seasonal_forcing_list$elderly[1:T_max],
    toddler = seasonal_forcing_list$toddler[1:T_max]
  )
}

#' Default seasonal forcing (per role)
#'
#' Builds per-role seasonal forcing vectors of length \code{T_max}. If
#' \code{amplitude = 0}, forcing is flat at 1; otherwise a cosine pattern.
#'
#' @param T_max Integer. Number of days.
#' @param amplitude,phase Numeric. Cosine amplitude and phase.
#'
#' @return List with numeric vectors for roles \code{adult}, \code{child},
#'   \code{elderly}, \code{toddler}.
#' @keywords internal
.make_default_seasonal_forcing <- function(T_max, amplitude = 0, phase = 0) {
  # flat = 1 if amplitude = 0; otherwise cosine shape (same for all roles)
  day <- seq_len(T_max)
  base <- if (isTRUE(all.equal(amplitude, 0))) rep(1, T_max) else
    seasonal_cosine(day, peak_day = (365 + round(phase * 365 / (2*pi))) %% 365, amplitude = amplitude, period = 365)
  list(
    adult   = base,
    child   = base,
    elderly = base,
    toddler = base
  )
}

#' Validate or repair seasonal forcing
#'
#' Ensures the seasonal forcing list has all required role entries and that
#' each role vector has length \code{T_max} (recycled or trimmed as needed).
#' Falls back to \code{.make_default_seasonal_forcing()} when incomplete.
#'
#' @param sfl List (possibly incomplete) of role vectors.
#' @param T_max Integer. Target length.
#' @param amplitude,phase Numeric. Passed to the default builder if needed.
#'
#' @return A complete seasonal forcing list with roles
#'   \code{adult}, \code{child}, \code{elderly}, \code{toddler}.
#' @keywords internal
.validate_and_fix_seasonal_forcing <- function(sfl, T_max, amplitude = 0, phase = 0) {
  need_names <- c("adult","child","elderly","toddler")
  if (is.null(sfl) || !all(need_names %in% names(sfl))) {
    sfl <- .make_default_seasonal_forcing(T_max, amplitude = amplitude, phase = phase)
  }
  # coerce each role vector to length T_max (recycle or trim)
  for (nm in need_names) {
    v <- sfl[[nm]]
    if (is.null(v)) v <- rep(1, T_max)
    if (length(v) < T_max) v <- rep(v, length.out = T_max)
    if (length(v) > T_max) v <- v[seq_len(T_max)]
    sfl[[nm]] <- as.numeric(v)
  }
  sfl
}



#' Assemble Stan data arrays for the household model
#'
#' Produces the \code{data} list for Stan from simulator households or a
#' person-level frame, including infectious indicators, infection days, VL
#' arrays, household membership, and seasonal forcing.
#'
#' @param households Either a list of per-household data frames (each with
#'   \code{vl_full_trajectory}) or a single person-level data frame with
#'   \code{hh_id}, \code{role}, \code{infection_time}, \code{infectious_start},
#'   \code{infectious_end}, \code{infection_resolved}, \code{vl_full_trajectory}.
#' @param T_max Integer; time horizon (days).
#' @param seasonal_forcing_list Named list of role-specific forcing series.
#' @param alpha_comm_by_role,beta1,beta2,V_ref,reference_phi,reference_kappa
#'   Scalars controlling hazards and reference levels.
#' @param g_peak_day,g_width Scalars controlling the infectivity profile.
#'
#' @return A named list suitable for \code{data=} in Stan containing
#'   \code{N, T, H, R, delta, hh_id, role_id, Y, I, V, V_term, alpha_comm_by_role,
#'   max_infectious, hh_size, hh_max_size, hh_members, g_profile, V_ref, beta1,
#'   beta2, reference_phi, reference_kappa, seasonal_forcing_mat}.
#'
#' @details
#' If VL is absent or sentinel-only, \code{V_term} is zero and the VL-mediated
#' component is effectively disabled.
build_stan_household_arrays <- function(
    households,
    T_max = 365,
    seasonal_forcing_list,
    alpha_comm_by_role = 5e-3,
    beta1 = .2, beta2 = .6, V_ref = 1e3,
    reference_phi = 1, reference_kappa = 1,
    g_peak_day = 2, g_width = 2
) {
  # ---- helpers ----
  .norm_roles <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x[x %in% c("infant","baby")] <- "toddler"
    x[x %in% c("sibling","kid","child")] <- "child"
    x[x %in% c("parent","mother","father","adult")] <- "adult"
    x[x %in% c("elder","elderly","grandparent")] <- "elderly"
    ok <- c("adult","child","elderly","toddler")
    x[!x %in% ok] <- NA_character_
    x
  }
  .ensure_sf_mat <- function(sfl, T_max) {
    role_order <- c("adult","child","elderly","toddler")
    default_vec <- rep(1, T_max)
    if (is.null(sfl) || !is.list(sfl)) {
      sfl <- setNames(replicate(4, default_vec, simplify = FALSE), role_order)
    }
    out <- lapply(role_order, function(nm) {
      v <- sfl[[nm]]; if (is.null(v)) v <- default_vec
      v <- as.numeric(v); if (!length(v)) v <- default_vec
      if (length(v) < T_max) v <- rep(v, length.out = T_max)
      if (length(v) > T_max) v <- v[seq_len(T_max)]
      v
    })
    cbind(adult = out[[1]], child = out[[2]], elderly = out[[3]], toddler = out[[4]])
  }
  .clamp_int <- function(x, lo, hi) pmin(pmax(as.integer(x), lo), hi)

  # ---- accept list or data.frame ----
  if (is.data.frame(households)) {
    all_individuals <- households
  } else if (is.list(households) && length(households) > 0L &&
             all(vapply(households, is.data.frame, logical(1)))) {
    all_individuals <- dplyr::bind_rows(households)
  } else {
    stop("`households` must be either a data.frame or a non-empty list of data.frames.")
  }
  if (!nrow(all_individuals)) stop("Stan data builder: no individuals found.")

  # ---- IDs & roles ----
  if (!("hh_id" %in% names(all_individuals))) {
    if ("HH" %in% names(all_individuals)) {
      all_individuals$hh_id <- all_individuals$HH
    } else {
      stop("Stan data builder: need `hh_id` or `HH`.")
    }
  }
  if (!("role" %in% names(all_individuals))) stop("Stan data builder: need `role` column.")
  all_individuals$role <- .norm_roles(all_individuals$role)
  role_levels <- c("adult","child","elderly","toddler")
  if (any(is.na(all_individuals$role)))
    stop("Stan data builder: some roles not in {adult, child, elderly, toddler}.")

  # ---- coerce time cols ----
  for (nm in c("infection_time","infectious_start","infectious_end","infection_resolved")) {
    if (!nm %in% names(all_individuals)) {
      all_individuals[[nm]] <- NA_integer_
    } else {
      all_individuals[[nm]] <- suppressWarnings(as.integer(all_individuals[[nm]]))
    }
  }

  # ---- sizes ----
  all_individuals$hh_id <- factor(all_individuals$hh_id)
  hh_id <- as.integer(all_individuals$hh_id)
  N <- nrow(all_individuals)
  H <- length(levels(all_individuals$hh_id))
  T <- as.integer(T_max)
  role_id <- match(all_individuals$role, role_levels)
  if (any(is.na(role_id))) stop("Stan data builder: role_id mapping failed.")

  # ---- Y (N x T) ----
  Y <- matrix(0L, nrow = N, ncol = T)
  for (i in seq_len(N)) {
    s <- all_individuals$infectious_start[i]
    e <- all_individuals$infectious_end[i]
    if (!is.na(s) && !is.na(e)) {
      s_idx <- .clamp_int(s, 1L, T)
      e_idx <- .clamp_int(e, 1L, T)
      if (e_idx >= s_idx) Y[i, s_idx:e_idx] <- 1L
    }
  }

  # ---- I (N x T) ----
  I <- matrix(0L, nrow = N, ncol = T)
  for (i in seq_len(N)) {
    s <- all_individuals$infection_time[i]
    if (!is.na(s) && s >= 1L && s <= T) I[i, as.integer(s)] <- 1L
  }

  # ---- V (N x T) from vl_full_trajectory ----
  V <- matrix(0, nrow = N, ncol = T)
  if ("vl_full_trajectory" %in% names(all_individuals)) {
    for (i in seq_len(N)) {
      traj <- all_individuals$vl_full_trajectory[[i]]
      if (is.null(traj) || !length(traj)) next
      inf_time <- all_individuals$infection_time[i]
      inf_end  <- all_individuals$infection_resolved[i]
      if (is.na(inf_time) || is.na(inf_end)) next
      days_seq <- seq.int(inf_time, inf_end)
      valid_len <- min(length(traj), length(days_seq), T - as.integer(inf_time) + 1L)
      if (valid_len < 1L) next
      days_fill <- as.integer(days_seq[seq_len(valid_len)])
      vals_fill <- as.numeric(traj[seq_len(valid_len)])
      vals_fill[!is.finite(vals_fill) | vals_fill < 0] <- 0
      keep <- days_fill >= 1L & days_fill <= T
      if (any(keep)) V[i, days_fill[keep]] <- vals_fill[keep]
    }
  }

  # ---- V_term (N x T) ----
  V_term <- matrix(0, nrow = N, ncol = T)
  if (any(V > 0)) {
    n_exp <- 3.91; p_v <- 0.83e-2; K <- 5.39; Vm <- 6.59; sai <- 4.57
    powV  <- V^n_exp
    denom <- powV + K^n_exp
    idx   <- denom > 0
    V_term[idx] <- 1 - exp(-sai * Vm * p_v * powV[idx] / denom[idx])
  }

  # ---- max_infectious: **length N** (per person)  --------------------------
  max_infectious <- ifelse(
    is.na(all_individuals$infectious_start) | is.na(all_individuals$infectious_end),
    0L,
    as.integer(all_individuals$infectious_end - all_individuals$infectious_start + 1L)
  )
  max_infectious <- as.integer(max_infectious)  # <- THIS is what Stan expects in your model

  # ---- household membership ----
  hh_members_list <- split(seq_len(N), hh_id)
  hh_size <- vapply(hh_members_list, length, integer(1))
  hh_max_size <- max(hh_size)
  # pad with 1L (your working script), Stan will use indices up to hh_size[h]
  hh_members <- matrix(1L, nrow = H, ncol = hh_max_size)
  for (h in seq_len(H)) {
    hh_members[h, 1:hh_size[h]] <- hh_members_list[[h]]
  }

  # ---- infectivity profile ----
  g_profile <- exp(-0.5 * ((seq_len(T) - g_peak_day) / g_width)^2)
  g_profile <- g_profile / max(g_profile)

  # ---- seasonal forcing (T x 4) ----
  sf_mat <- .ensure_sf_mat(seasonal_forcing_list, T_max = T)
  sf_mat <- sf_mat[, c("adult","child","elderly","toddler"), drop = FALSE]

  # ---- package for Stan ----
  list(
    N = N,
    T = T,
    H = H,
    R = length(role_levels),
    delta = 1,
    hh_id = hh_id,
    role_id = as.integer(role_id),
    Y = Y,
    I = I,
    V = V,
    V_term = V_term,
    alpha_comm_by_role = alpha_comm_by_role,
    max_infectious = max_infectious,     # <- length N
    hh_size = as.integer(hh_size),
    hh_max_size = as.integer(hh_max_size),
    hh_members = hh_members,             # H x hh_max_size, 1-indexed, padded with 1
    g_profile = as.numeric(g_profile),   # length T
    V_ref = V_ref,
    beta1 = beta1,
    beta2 = beta2,
    reference_phi = reference_phi,
    reference_kappa = reference_kappa,
    seasonal_forcing_mat = sf_mat        # T x 4
  )
}



#' Run the Stan household model
#'
#' Thin wrapper around \code{rstan::stan()} (or compiled model via
#' \code{rstan::stan_model()}) with reasonable defaults.
#'
#' @param stan_data Named list as returned by
#'   \code{\link{build_stan_household_arrays}}.
#' @param stan_file Path to a Stan model file (or \code{"builtin:<key>"}).
#' @param chains,iter,warmup Stan MCMC settings.
#' @param control List passed to \code{rstan::stan()} (e.g., \code{adapt_delta},
#'   \code{max_treedepth}).
#' @param init,refresh,cores Passed through to \code{rstan::sampling()}.
#' @param stan_code Optional character string with Stan program code.
#' @param package_name Optional package name for \code{system.file("stan", ...)} lookup.
#'
#' @return A \code{stanfit} object.
run_household_stan <- function(
    stan_data,
    stan_file   = "HH_parameter_estimation2.stan",
    chains      = 4, iter = 2000, warmup = 1000,
    control     = list(adapt_delta = 0.99, max_treedepth = 20),
    init        = "random", refresh = 50, cores = 4,
    # NEW: optional direct code and package name for system.file lookup
    stan_code   = NULL,
    package_name = NULL
) {
  # Resolve model source
  res <- .resolve_stan_model(stan_file = stan_file, stan_code = stan_code, package_name = package_name)

  # Prefer cmdstanr if you use it; otherwise rstan:
  if (requireNamespace("rstan", quietly = TRUE)) {
    # Compile
    sm <- if (!is.null(res$code)) {
      rstan::stan_model(model_code = res$code, verbose = FALSE)
    } else {
      rstan::stan_model(file = res$file, verbose = FALSE)
    }

    # Sample
    op <- options(mc.cores = cores); on.exit(options(op), add = TRUE)
    fit <- rstan::sampling(
      sm, data = stan_data, chains = chains, iter = iter, warmup = warmup,
      control = control, init = init, refresh = refresh
    )
    return(fit)
  } else {
    stop("Package 'rstan' not available. Please install rstan or adapt to cmdstanr.")
  }
}



#' Tidy posterior summary for role multipliers
#'
#' Extracts posterior summaries for \code{phi_by_role[]} and \code{kappa_by_role[]}
#' from a \code{stanfit}.
#'
#' @param fit A \code{stanfit} object returned by \code{\link{run_household_stan}}.
#'
#' @return A data frame with columns:
#'   \code{Parameter}, \code{Estimate}, \code{SD}, \code{Lower CI}, \code{Median},
#'   \code{Upper CI}.
postprocess_stan_fit <- function(fit) {
  s <- tryCatch(rstan::summary(fit)$summary, error = function(e) NULL)
  if (is.null(s) || !nrow(s)) return(data.frame(Parameter=character()))
  tab <- as.data.frame(s)
  tab$Parameter <- rownames(tab); rownames(tab) <- NULL
  names(tab) <- sub("^X2[.]5[.]$",  "2.5%",  names(tab))
  names(tab) <- sub("^X50[.]$",     "50%",   names(tab))
  names(tab) <- sub("^X97[.]5[.]$", "97.5%", names(tab))
  keep <- intersect(c("Parameter","mean","sd","2.5%","50%","97.5%"), names(tab))
  tab[, keep, drop = FALSE]
}
