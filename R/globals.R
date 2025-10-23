#' Internal globals for NSE checks
#' @keywords internal
#' @noRd
utils::globalVariables(c(
  ".", "..cols", "..keep", ".SD", "pp",
  "HH","ID_hh","ID_indiv","individual_ID",
  "infected","is_index",
  "age_cat",                # <-- add this line
  "agegrp2","agegrp3","agegrp4",
  "test_date","role",
  "T_FP_date","T_LP_date",
  "inf_date","inf_start_date","inf_end_date",
  "inf_win_start","inf_win_end",
  "infection.detected.start","infection.detected.end","infection.infectious.day",
  "last_neg_date","last_negative",
  "indiv.index","day"
))
