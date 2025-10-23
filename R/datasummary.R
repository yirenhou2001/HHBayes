#' Summarize infection episodes (per individual)
#'
#' Aggregates long-format testing records into one row per individual with
#' episode counts, first/last positive within the first episode, inferred
#' infectious-day span, last pre-positive negative, and optional covariate
#' summaries. Accepts either a single data frame \code{df} containing the
#' six required core columns, or the six vectors individually. Optionally
#' merges an external covariate data frame before summarizing.
#'
#' @param df Data frame with at least the columns
#'   \code{HH}, \code{individual_ID}, \code{role}, \code{test_date},
#'   \code{infection_status}, \code{community_risk}. When \code{df} is supplied,
#'   the vector arguments below are ignored.
#' @param Household_ID Integer vector. Household identifiers; used only when
#'   \code{df} is \code{NULL}.
#' @param Individual_ID Integer vector. Individual identifiers; used only when
#'   \code{df} is \code{NULL}.
#' @param Household_role Character vector. Household role for each row
#'   (e.g., "infant", "sibling", "adult", "elder"); used only when \code{df} is \code{NULL}.
#' @param Sample_test_days Integer or numeric vector. Testing day index for each
#'   record (relative day); used only when \code{df} is \code{NULL}.
#' @param Infectious_status Integer or logical vector (0/1). Test-time
#'   infection indicator; used only when \code{df} is \code{NULL}.
#' @param Community_rate_infection Numeric vector. Community risk/intensity at
#'   each record; used only when \code{df} is \code{NULL}.
#'
#' @param Covariate_DataFrame Data frame of additional covariates to merge prior
#'   to summarization. The join uses the best available keys among
#'   \code{c("HH","individual_ID","test_date")}. At minimum, \code{individual_ID}
#'   must be present (preferably with \code{HH} and/or \code{test_date}).
#' @param covariate_cols Character vector of covariate names to summarize
#'   (after merging). If \code{NULL} (default), all non-core columns are summarized.
#'
#' @return A data frame with one row per individual containing:
#' \itemize{
#'   \item \code{HH}: household identifier.
#'   \item \code{individual_ID}: individual identifier (within-household).
#'   \item \code{n.true.infection}: number of infection episodes (runs of 1's).
#'   \item \code{n.detected.infection}: number of detected episodes (same as \code{n.true.infection} given \code{infection_status} encodes detection).
#'   \item \code{infection.detected.start}: first positive day of the first episode.
#'   \item \code{infection.detected.end}: last positive day of the first episode.
#'   \item \code{infection.true.duration}: duration (days) of the first episode
#'     based on test days (\code{end - start + 1}).
#'   \item \code{last_negative}: last negative test day prior to the first positive (if any).
#'   \item \code{infection.infectious.day}: comma-separated list of test-day indices
#'     during the first episode.
#'   \item \code{community.risk}: mean of \code{community_risk} across this individual's records.
#'   \item \code{role}: household role (as observed).
#' }
#' If covariates are present, additional columns are appended per covariate (after
#' name normalization to snake_case):
#' \itemize{
#'   \item \code{<cov>_mode}: mode for categorical/logical or binary-numeric covariates.
#'   \item \code{<cov>_first}, \code{<cov>_mean}, \code{<cov>_last}: first observed, mean, and last observed value for numeric covariates.
#'   \item \code{<cov>_timevarying}: logical flag indicating whether the covariate varies over time for the individual.
#' }
#'
#' @details
#' \itemize{
#'   \item Either \code{df} or all six vector inputs must be provided; otherwise an error is thrown.
#'   \item When merging \code{Covariate_DataFrame}, name collisions with the six core columns are avoided by renaming the colliding covariate columns with a \code{"_cv"} suffix.
#'   \item Covariate names are normalized to snake_case prior to creating summary columns.
#'   \item Records are internally ordered by \code{HH}, \code{individual_ID}, \code{test_date}.
#'   \item Episode detection is based on runs of \code{infection_status == 1}; the first episode is used for \code{*_start}, \code{*_end}, \code{*_duration}, and \code{infection.infectious.day}.
#' }
#'
#' @examples
#' \dontrun{
#' # Minimal example with a data frame:
#' df <- data.frame(
#'   HH = c(1,1,1,1,1,1),
#'   individual_ID = c(1,1,1,2,2,2),
#'   role = c("infant","infant","infant","adult","adult","adult"),
#'   test_date = c(1,2,3,1,2,3),
#'   infection_status = c(0,1,1,0,0,1),
#'   community_risk = c(0.01,0.02,0.02,0.01,0.01,0.02)
#' )
#' data_summarization(df)
#'
#' # With external covariates to merge and summarize:
#' covdf <- data.frame(
#'   HH = c(1,1), individual_ID = c(1,2),
#'   vacc_status = c(1,0), bmi = c(18.5, 27.2)
#' )
#' data_summarization(df, Covariate_DataFrame = covdf)
#' }
#'
#' @seealso \code{\link{generate_synthetic_data_standardized}},
#'   \code{\link{summarize_individuals}},
#'   \code{\link{main_parameter_estimation_pipeline}}
data_summarization <- function(
    df = NULL,
    Household_ID = NULL, Individual_ID = NULL, Household_role = NULL,
    Sample_test_days = NULL, Infectious_status = NULL, Community_rate_infection = NULL,

    # User covariate data frame
    Covariate_DataFrame = NULL,
    #Allow user to specify which covariates to summarize
    covariate_cols = NULL
) {
  tryCatch({

    ##########################################################################
    #                                                                        #
    #                    Build and check data frame                          #
    #                                                                        #
    ##########################################################################
    core_cols <- c("HH","individual_ID","role","test_date","infection_status","community_risk")

    if (!is.null(Household_ID) && !is.null(Individual_ID) && !is.null(Household_role) &&
        !is.null(Sample_test_days) && !is.null(Infectious_status) && !is.null(Community_rate_infection)) {
      newdf <- data.frame(
        HH = Household_ID,
        individual_ID = Individual_ID,
        role = Household_role,
        test_date = Sample_test_days,
        infection_status = Infectious_status,
        community_risk = Community_rate_infection,
        stringsAsFactors = FALSE
      )
      df <- newdf[order(newdf$HH, newdf$individual_ID, newdf$test_date), ]
    } else if (!is.null(df)) {
      missing_core <- setdiff(core_cols, names(df))
      if (length(missing_core))
        stop("`df` is missing required columns: ", paste(missing_core, collapse=", "))
      df <- df[order(df$HH, df$individual_ID, df$test_date), ]
    } else {
      stop("Either a full data frame `df` or all six vector inputs must be provided.")
    }

    ##########################################################################
    #                                                                        #
    #                   Merge user covariate data frame                      #
    #                                                                        #
    ##########################################################################
    if (!is.null(Covariate_DataFrame)) {
      cv <- Covariate_DataFrame
      # Choose best join keys available
      join_keys <- intersect(c("HH","individual_ID","test_date"), names(cv))
      if (all(c("HH","individual_ID","test_date") %in% join_keys)) {
        keys <- c("HH","individual_ID","test_date")
      } else if (all(c("HH","individual_ID") %in% join_keys)) {
        keys <- c("HH","individual_ID")
      } else if ("individual_ID" %in% join_keys) {
        keys <- "individual_ID"
      } else {
        stop("`Covariate_DataFrame` must contain at least `individual_ID` (and ideally `HH`, and/or `test_date`).")
      }

      # Avoid overwriting base df columns
      keep_cols <- setdiff(names(cv), keys)
      if (!is.null(covariate_cols)) keep_cols <- intersect(keep_cols, covariate_cols)
      if (length(keep_cols) > 0) {
        collide <- intersect(keep_cols, names(df))
        if (length(collide)) {
          ren <- setNames(paste0(collide, "_cv"), collide)
          names(cv)[match(collide, names(cv))] <- ren
          keep_cols <- setdiff(names(cv), keys)  # refresh
        }
        df <- merge(df, cv[, c(keys, keep_cols), drop = FALSE], by = keys, all.x = TRUE, sort = FALSE)
      }
    }

    ##########################################################################
    #                                                                        #
    #                  Decide which covariates to summarize                  #
    #                                                                        #
    ##########################################################################
    # Anything beyond the 6 core columns counts as a covariate
    extra_cols <- setdiff(names(df), core_cols)
    if (!is.null(covariate_cols)) extra_cols <- intersect(extra_cols, covariate_cols)

    sanitize <- function(x) {
      x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
      gsub("_+", "_", gsub("^_|_$", "", x))
    }

    mode_val <- function(v) {
      v <- v[!is.na(v)]
      if (!length(v)) return(NA)
      tt <- sort(table(v), decreasing = TRUE)
      names(tt)[1]
    }

    ##########################################################################
    #                                                                        #
    #                     Split by person and summarize                      #
    #                                                                        #
    ##########################################################################
    split_df <- split(df, list(df$HH, df$individual_ID), drop = TRUE)

    res_list <- lapply(split_df, function(ind_df) {
      status <- ind_df$infection_status
      dates  <- ind_df$test_date
      role   <- unique(ind_df$role)
      comm_risk <- mean(ind_df$community_risk, na.rm = TRUE)

      r   <- rle(status)
      idx_end   <- cumsum(r$lengths)
      idx_start <- idx_end - r$lengths + 1

      starts <- dates[idx_start[r$values == 1]]
      ends   <- dates[idx_end[r$values == 1]]
      n_inf  <- sum(r$values == 1)
      dur    <- ends - starts + 1

      first_pos <- if (length(starts)) min(starts) else Inf
      has_pre_neg <- ind_df$infection_status == 0 & ind_df$test_date < first_pos
      last_negative <- if (any(has_pre_neg)) max(ind_df$test_date[has_pre_neg]) else NA_integer_

      infectious_day <- if (length(starts) > 0) paste(starts[1]:ends[1], collapse = ",") else NA_character_

      out <- list(
        HH = ind_df$HH[1],
        individual_ID = ind_df$individual_ID[1],
        n.true.infection = n_inf,
        n.detected.infection = n_inf,
        infection.detected.start = if (length(starts) > 0) starts[1] else NA_integer_,
        infection.detected.end   = if (length(ends) > 0)   ends[1]   else NA_integer_,
        infection.true.duration  = if (length(dur) > 0)    dur[1]    else NA_integer_,
        last_negative = last_negative,
        infection.infectious.day = infectious_day,
        community.risk = comm_risk,
        role = role
      )

      # Covariate summaries, if there are any
      if (length(extra_cols) > 0) {
        for (cn in extra_cols) {
          v <- ind_df[[cn]]
          sn <- sanitize(cn)

          if (all(is.na(v))) {
            out[[paste0(sn, "_timevarying")]] <- NA
            out[[paste0(sn, "_mode")]]  <- NA
            out[[paste0(sn, "_first")]] <- NA
            out[[paste0(sn, "_mean")]]  <- NA
            out[[paste0(sn, "_last")]]  <- NA
            next
          }

          is_num  <- is.numeric(v)
          is_logi <- is.logical(v)
          is_cat  <- is.factor(v) || is.character(v) || is_logi

          is_bin_num <- FALSE
          if (is_num) {
            u <- unique(v[!is.na(v)])
            is_bin_num <- length(u) <= 2 && all(u %in% c(0,1))
          }

          if (is_cat || is_bin_num) {
            mv <- mode_val(if (is_logi) as.integer(v) else v)
            out[[paste0(sn, "_mode")]] <- if (is_logi || is_bin_num) as.numeric(mv) else as.character(mv)
          } else if (is_num) {
            nz <- which(!is.na(v))
            out[[paste0(sn, "_first")]] <- v[nz[1]]
            out[[paste0(sn, "_mean")]]  <- mean(v, na.rm = TRUE)
            out[[paste0(sn, "_last")]]  <- v[nz[length(nz)]]
          }

          out[[paste0(sn, "_timevarying")]] <- length(unique(v[!is.na(v)])) > 1
        }
      }

      as.data.frame(out, stringsAsFactors = FALSE)
    })

    out_df <- do.call(rbind, res_list)
    rownames(out_df) <- NULL
    return(out_df)

  }, error = function(e) {
    message("An error occurred during summarization: ", conditionMessage(e))
    return(NULL)
  })
}
