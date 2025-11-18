#' Prepare inputs for transmission plots
#'
#' Validates and standardizes \code{results$summarized_data} and (optionally)
#' locates a viral-load column in \code{results$person_day}. Adds a normalized
#' role factor (\code{role_std}) and returns the pieces plotters need.
#'
#' @param results List from the pipeline; must contain \code{summarized_data}
#'   and may contain \code{person_day}.
#' @param index_vl_column Optional character. Viral-load column name to use
#'   from \code{person_day}. If \code{NULL}, falls back to \code{"vl_test"} when present.
#'
#' @return List with elements: \code{sd} (standardized summary data),
#'   \code{pd} (\code{person_day} or \code{NULL}), \code{vl_col} (chosen VL column
#'   or \code{NULL}), and \code{ok} (\code{TRUE}/\code{FALSE}).
#'
#' @details Expects columns in \code{sd}: \code{HH}, \code{individual_ID},
#'   \code{role}, \code{inf_date}, \code{inf_start_date}, \code{inf_end_date}.
#'   Creates \code{role_std} with levels \code{child}, \code{toddler},
#'   \code{adult}, \code{elderly}.
#' @keywords internal
.compute_plot_inputs <- function(results, index_vl_column = NULL) {
  # Use summarized_data produced by the pipeline
  sd <- results$summarized_data
  if (is.null(sd) || !nrow(sd)) return(list(sd = NULL, ok = FALSE))

  # Expected columns (from your new summarizers)
  # HH, individual_ID, role, inf_date, inf_start_date, inf_end_date,
  # infection.detected.start (if available)
  needed <- c("HH","individual_ID","role","inf_date","inf_start_date","inf_end_date")
  if (any(!needed %in% names(sd))) return(list(sd = NULL, ok = FALSE))

  # Standardize role labels a bit
  role_map <- c(infant = "toddler", sibling = "child", parent  = "adult", elder = "elderly")
  sd$role_std <- tolower(as.character(sd$role))
  sd$role_std <- ifelse(sd$role_std %in% names(role_map), role_map[sd$role_std], sd$role_std)
  sd$role_std <- factor(sd$role_std, levels = c("child","toddler","adult","elderly"))

  # Try to bring in a VL column, if user provides one (long tables often name it 'vl_test')
  # We'll use it only for the SAR-by-VL plot to find index VL on index detection day.
  vl_col <- NULL
  if (!is.null(index_vl_column) && index_vl_column %in% names(results$person_day)) {
    vl_col <- index_vl_column
  } else if ("vl_test" %in% names(results$person_day)) {
    vl_col <- "vl_test"
  }

  list(sd = sd, pd = results$person_day, vl_col = vl_col, ok = TRUE)
}



#' Daily infections plot (RSV/VL simulator)
#'
#' Aggregates simulated household data to daily new infections by role.
#'
#' @param households List of data frames from the RSV/VL simulator; each must
#'   contain \code{role} and \code{infection_time}.
#'
#' @return A \code{ggplot} object.
#' @keywords internal
.rsv_plot_daily <- function(households) {
  df <- do.call(rbind, households)
  df %>%
    dplyr::mutate(if_infection = !is.na(infection_time)) %>%
    dplyr::group_by(role, day = infection_time) %>%
    dplyr::summarise(n_infections = sum(if_infection), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = day, y = n_infections, fill = role)) +
    ggplot2::geom_col(width = 1) +
    ggplot2::labs(x = "Day of infection", y = "New infections", fill = "Age group",
                  title = "Daily new infections by age group") +
    ggplot2::theme_classic(base_size = 14)
}


#' Weekly infections plot (RSV/VL simulator)
#'
#' Aggregates simulated household data to weekly new infections by role.
#'
#' @param households List of data frames from the RSV/VL simulator; each must
#'   contain \code{role} and \code{infection_time}.
#'
#' @return A \code{ggplot} object.
#' @keywords internal
.rsv_plot_weekly <- function(households) {
  df <- do.call(rbind, households)
  df %>%
    dplyr::filter(!is.na(infection_time)) %>%
    dplyr::mutate(week = floor(infection_time/7)) %>%
    dplyr::count(role, week, name = "n_infections") %>%
    ggplot2::ggplot(ggplot2::aes(x = week, y = n_infections, fill = role)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::labs(x = "Week", y = "New infections", fill = "Age group",
                  title = "Weekly new infections by age group") +
    ggplot2::theme_classic(base_size = 14)
}


#' Timeline plot of household epidemics (RSV/VL simulator)
#'
#' Shows infection and detection events per person across selected households.
#'
#' @param households List of data frames from the RSV/VL simulator; each must
#'   contain \code{hh_id}, \code{person_id}, \code{role}, \code{infection_time},
#'   and \code{detection_time}.
#' @param max_hh Integer. Maximum number of households to display (default 15).
#'
#' @return A \code{ggplot} object.
#' @keywords internal
.rsv_plot_timeline <- function(households, max_hh = 15) {
  df  <- do.call(rbind, households)
  sel <- unique(df$hh_id)[seq_len(min(length(unique(df$hh_id)), max_hh))]
  tl  <- df %>%
    dplyr::filter(hh_id %in% sel) %>%
    tidyr::pivot_longer(c(infection_time, detection_time),
                        names_to = "event_type", values_to = "day") %>%
    dplyr::filter(!is.na(day)) %>%
    dplyr::mutate(event_type = factor(event_type, levels = c("detection_time","infection_time")))

  ggplot2::ggplot(tl, ggplot2::aes(x = day, y = factor(person_id), color = event_type, shape = role)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::facet_wrap(~ hh_id, scales = "free_y") +
    ggplot2::scale_color_manual(
      name   = "Event",
      values = c(detection_time = "#1f77b4",  # blue
                 infection_time  = "#d62728") # red
    ) +
    ggplot2::labs(x = "Day", y = "Person ID", title = "Household Epidemic Timelines") +
    ggplot2::theme_bw(base_size = 12)
}



#' SAR by index viral load (RSV/VL simulator)
#'
#' Computes household secondary attack rate (SAR) and displays SAR by the
#' index case's viral load category on detection day.
#'
#' @param households List of data frames from the RSV/VL simulator; each must
#'   include \code{hh_id}, \code{infection_time}, \code{detection_time},
#'   and \code{viral_loads_test_days} (per-person list column).
#'
#' @return A \code{ggplot} object.
#' @keywords internal
.rsv_plot_sar_by_index_vl <- function(households) {
  df <- do.call(rbind, households)
  hh_sum <- df %>%
    dplyr::group_by(hh_id) %>%
    dplyr::summarize(n_total = dplyr::n(),
                     n_infected = sum(!is.na(infection_time)),
                     sar = ifelse(n_total > 1, (n_infected - 1)/(n_total - 1), NA_real_),
                     .groups = "drop") %>%
    dplyr::mutate(sar = ifelse(sar < 0, NA, sar))

  get_index_vl <- function(hh) {
    if (all(is.na(hh$infection_time))) return(NA_real_)
    tmin <- min(hh$infection_time, na.rm = TRUE)
    idx  <- which(hh$infection_time == tmin)[1]
    vl   <- hh$viral_loads_test_days[[idx]]
    tdet <- suppressWarnings(min(hh$detection_time, na.rm = TRUE))
    if (!is.finite(tdet) || length(vl) == 0) return(NA_real_)
    as.numeric(vl[as.character(tdet)])
  }

  idx_vl <- tibble::tibble(
    hh_id    = vapply(households, function(hh) unique(hh$hh_id)[1], character(1)),
    index_vl = vapply(households, get_index_vl, numeric(1))
  )

  hh_sum %>%
    dplyr::left_join(idx_vl, by = "hh_id") %>%
    dplyr::mutate(vl_category = cut(index_vl, breaks = c(-Inf,2,4,6,8,Inf),
                                    labels = c("0-2","2-4","4-6","6-8",">8"))) %>%
    dplyr::filter(!is.na(vl_category)) %>%
    ggplot2::ggplot(ggplot2::aes(x = vl_category, y = sar)) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.15), alpha = 0.5) +
    ggplot2::labs(x = "Index VL (log10 copies/mL, bins)", y = "Household SAR",
                  title = "Household SAR by index-case viral load category") +
    ggplot2::theme_classic(base_size = 14)
}



#' Make transmission plots (batch)
#'
#' Builds selected figures from either RSV/VL simulation outputs
#' (\code{results$raw_simulation}) or legacy summary data via
#' \code{.compute_plot_inputs()}.
#'
#' @param results Pipeline result list.
#' @param which Character vector of plot names among \code{"daily"},
#'   \code{"weekly"}, \code{"sar"}, \code{"timeline"}, or \code{"all"}.
#' @param print Logical; print each plot when \code{TRUE} (default).
#' @param index_vl_column Optional character; viral-load column in
#'   \code{person_day} (fallback \code{"vl_test"}).
#'
#' @return Named list of \code{ggplot} objects for the requested plots
#'   (a subset of \code{daily}, \code{weekly}, \code{timeline}, \code{sar}).
#' @keywords internal
.make_transmission_plots <- function(results,
                                     which = c("daily","weekly","sar","timeline"),
                                     print = TRUE,
                                     index_vl_column = NULL) {
  if (identical(which, "all")) which <- c("daily","weekly","sar","timeline")
  out <- list()

  # ===== RSV/VL path: plot directly from households =====
  if (!is.null(results$raw_simulation) &&
      is.list(results$raw_simulation) &&
      length(results$raw_simulation) &&
      is.data.frame(results$raw_simulation[[1]]) &&
      all(c("hh_id","person_id","role","infection_time","detection_time")
          %in% names(results$raw_simulation[[1]]))) {

    households <- results$raw_simulation

    if ("daily"    %in% which) out$daily    <- .rsv_plot_daily(households)
    if ("weekly"   %in% which) out$weekly   <- .rsv_plot_weekly(households)
    if ("timeline" %in% which) out$timeline <- .rsv_plot_timeline(households)
    if ("sar"      %in% which) out$sar      <- .rsv_plot_sar_by_index_vl(households)

  } else {
    # ===== Legacy path (unchanged) =====
    inp <- .compute_plot_inputs(results, index_vl_column = index_vl_column)
    if (isTRUE(inp$ok)) {
      if ("daily"    %in% which) out$daily    <- NULL
      if ("weekly"   %in% which) out$weekly   <- NULL
      if ("timeline" %in% which) out$timeline <- NULL
      if ("sar"      %in% which) out$sar      <- NULL
    }
  }

  if (print && length(out)) for (nm in names(out)) print(out[[nm]])
  out
}
