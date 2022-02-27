#' Calculate the effect measures
#'
#' Calculate the effect measures.
#'
#' Calculate the effect measures and put them in a named numeric() vector.
#'
#' @param val0 Numeric, base value
#' @param val1 Numeric, treated value
#' @param log If TRUE (default) compute the effect measures on the log scale.
#' If FALSE compute the effect measures on the natural scale.
#' The risk difference measure (RD) is always on the natural scale.
#'
#' @return Named numeric vector.
#' @export
#'
#' @examples
#' val0 <- 0.25
#' val1 <- 0.5
#' # when log is required, we use default value
#' out <- effect_measures(val0, val1)
#' stopifnot(all(is.finite(out)))
#' # when natural scale is required, we use log = FALSE
#' out <- effect_measures(val0, val1, log = FALSE)
#' stopifnot(all(is.finite(out)))
effect_measures <- function(val0, val1, log = TRUE) {
  # both values must be finite
  stopifnot(is.finite(val0), is.finite(val1))

  # create a vector of names used later when creating output
  # this trick does not work on unamed variables (next step)
  # so it must be done now.
  nms <- c(deparse1(substitute(val0)), deparse1(substitute(val1)))
  # make sure the values are unnamed
  # otherwise their names get combined with the new ones
  val0 <- unname(val0)
  val1 <- unname(val1)

  # only val0 required not to be zero
  va0_not_zero <- !dplyr::near(val0, 0)
  # either values cannot be zero
  both_not_zero <- va0_not_zero & !dplyr::near(val1, 0)
  # almost positivity assumtion: both values must be >= zero but < 1
  both_gte_zero_lt_1 <- 0 <= val0 & val0 < 1 & 0 <= val1 & val1 < 1
  # the positivity assumption is required for both values
  is_positivity <- 0 < val0 & val0 < 1 & 0 < val1 & val1 < 1

  # Risk Difference
  RD <- val1 - val0

  RR <- NA_real_  # Risk Ratio
  RRstar <- NA_real_  # Other Risk Ratio
  OR <- NA_real_  # Odds Ratio
  if (log) {
    if (both_not_zero) RR <- log(val1) - log(val0)
    if (both_gte_zero_lt_1) RRstar <- log(1 - val0) - log(1 - val1)
    if (is_positivity) OR <- log(val1 / (1 - val1)) - log(val0 / (1 - val0))
    out <- c("RD" = RD, "logRR" = RR, "logRR*" = RRstar, "logOR" = OR)
  } else {
    if (va0_not_zero) RR <- val1 / val0
    if (both_gte_zero_lt_1) RRstar <- (1 - val0) / (1 - val1)
    if (is_positivity) OR <- (val1 / (1 - val1)) / (val0 / (1 - val0))
    out <- c("RD" = RD, "RR" = RR, "RR*" = RRstar, "OR" = OR)
  }

  # output only the measures that were calculated
  out <- out[!is.na(out)]

  # vector of named input values
  vals <- c(val0, val1)
  names(vals) <- nms

  c(vals, out)
}


#' Exponentiate the effect measures
#'
#' Exponentiate the effect measures.
#'
#' Exponentiate each selected measure in the dataframe and replace its name
#' accordingly.
#'
#' @param data Dataframe.
#' @param vars Character() of measure names.
#'
#' @return Dataframe of converted effects measures.
#' @export
effect_exp <- function(data,
                        vars = c("RR" = "logRR", "RR*"  = "logRR*",
                                 "OR" = "logOR")) {
  is_matched <- vars %in% data$name
  if (any(is_matched)) {
    # nomatch = 0 to exclude unmatched items
    pos <- match(vars, data$name, nomatch = 0L)
    within(data, {
      est[pos] <- exp(est[pos])
      lci[pos] <- exp(lci[pos])
      uci[pos] <- exp(uci[pos])
      name[pos] <- names(vars)[is_matched]
    })
  }
}

#' Inverse logit the effect measures
#'
#' Inverse logit the effect measures.
#'
#' Inverse logit each selected measure in the dataframe and replace its name
#' accordingly.
#'
#' @param data Dataframe.
#' @param vars Character() of measure names.
#'
#' @return Dataframe of converted effects measures.
#' @export
effect_expit <- function(data, vars = c("P" = "logitP")) {
  is_matched <- vars %in% data$name
  if (any(is_matched)) {
    # nomatch = 0 to exclude unmatched items
    pos <- match(vars, data$name, nomatch = 0L)
    within(data, {
      est[pos] <- plogis(est[pos])
      lci[pos] <- plogis(lci[pos])
      uci[pos] <- plogis(uci[pos])
      name[pos] <- names(vars)[is_matched]
    })
  }
}
