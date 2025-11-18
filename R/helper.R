#' Package imports and internal aliases
#'
#' Internal namespace declarations and lightweight helpers.
#'
#' @name imports
#' @keywords internal
#' @import data.table
#' @import tidyverse
#' @import rstan
#' @importFrom stats qlogis plogis rnorm rbinom rlnorm runif optim na.omit model.matrix setNames sd pgamma qgamma rgamma
#' @importFrom data.table setnames fifelse
#' @importFrom utils globalVariables modifyList
#' @importFrom dplyr %>% mutate group_by summarise filter count left_join
#' @importFrom ggplot2 ggplot aes geom_col labs theme_classic facet_wrap
#' @importFrom ggplot2 scale_color_manual theme_bw geom_point geom_boxplot position_jitter
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @importFrom utils head
#' @noRd
NULL



#' Null-coalescing operator
#'
#' Returns \code{a} unless it is \code{NULL}, otherwise returns \code{b}.
#'
#' @name %||%
#' @param a,b Objects to choose from.
#' @return \code{a} if non-\code{NULL}; otherwise \code{b}.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a



#' Clamp to the open unit interval
#'
#' Constrains numeric values to \code{(0, 1)} using a small epsilon to avoid
#' exact 0/1 (useful before logit/log computations).
#'
#' @param x Numeric vector.
#' @param eps Positive numeric tolerance (default: machine double epsilon).
#' @return Numeric vector with values in \code{(0, 1)}.
#' @keywords internal
#' @noRd
clamp01 <- function(x, eps = .Machine$double.eps) {
  pmin(pmax(x, eps), 1 - eps)
}



#' Conditionally modify a list
#'
#' Wrapper around \code{utils::modifyList()}: returns \code{x} unchanged when
#' \code{val} is \code{NULL}; otherwise applies \code{modifyList(x, val)}.
#'
#' @param x A list to update.
#' @param val A list of replacements, or \code{NULL}.
#' @return A list: either \code{x} (if \code{val} is \code{NULL}) or
#'   the result of \code{utils::modifyList(x, val)}.
#' @keywords internal
#' @noRd
modify_if_not_null <- function(x, val) {
  if (is.null(val)) x else utils::modifyList(x, val)
}

#' Internal alias for role normalization
#'
#' Thin wrapper calling \code{normalize_roles()}.
#'
#' @param x Character vector of role labels.
#' @return Character vector with normalized roles.
#' @keywords internal
#' @noRd
.normalize_roles4 <- function(x) {
  normalize_roles(x)
}
