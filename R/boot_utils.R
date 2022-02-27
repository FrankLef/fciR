#' Extract the name of variables and coefficients from a formula
#'
#' @param formula Object of class \code{formula}.
#'
#' @importFrom stats terms
#'
#' @return List of variable names
#' @export
#'
#' @examples
#' formula2vars(Y ~ A + X1*X2)
formula2vars <- function(formula) {
  stopifnot(inherits(formula, "formula"))

  # x0 is the coefficient for intercept, used by lm, glm, geeglm, etc.
  x0 <- "(Intercept)"

  vars <- all.vars(formula)
  nvars <- length(vars)
  stopifnot(nvars >= 2)

  # outcome and treatmaent variables
  y <- vars[1]  # outcome variable
  t <- vars[2]  # treatment variable

  # if only 2 variables, t is the independent variable
  if (nvars == 2) return(list("y" = y, "t" = t, "ind" = t, "x0" = x0))

  # the condition variables
  h <- vars[3:nvars]

  # all independent variables including interactions
  ind <- attr(stats::terms(formula), which = "term.labels")
  # the condition variables including interaction with the treatment var
  ht <- ind[seq_along(ind)[-1]]

  # return results in a list
  list("y" = y, "t" = t, "h" = h, "ht" = ht, "ind" = ind, "x0" = x0)
}


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
#' out <- calc_effect_measures(val0, val1)
#' stopifnot(all(is.finite(out)))
#' # when natural scale is required, we use log = FALSE
#' out <- calc_effect_measures(val0, val1, log = FALSE)
#' stopifnot(all(is.finite(out)))
calc_effect_measures <- function(val0, val1, log = TRUE) {
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


#' Bootstrap and generate a dataframe of estimates with CI
#'
#' Bootstrap and generate a dataframe of estimates with CI.
#'
#' Generate a dataframe of estimates with the columns
#' \code{c("est", "conf", "lci", "uci")}.
#'
#' @param data Dataframe of raw data.
#' @param statistic Function applied to data by bootstrapping.
#' @param R Number of bootstrap replicates. Default is 1000.
#' @param conf Confidence interval width. Default is 0.95.
#' @param ... Other named arguments for \code{statistics}.
#'
#' @return Dataframe of estimates with CI.
#' @export
run_boot <- function(data, statistic, R = 1000, conf = 0.95, ...) {
  stopifnot(R >= 1, conf > 0, conf < 1)

  # run the bootstrapping
  boot.out <- boot::boot(data = data, statistic = statistic, R = R, ...)

  # extract the estimated values and confidence intervals from the boot object
  out <- sapply(X = seq_along(boot.out$t0), FUN = function(i) {
    est <- boot.out$t0[i]
    ci <- boot::boot.ci(boot.out, conf = conf, type = "norm", index = i)$normal
    out <- c(est, ci)
    names(out) <- c("est", "conf", "lci", "uci")
    out
  })

  # create the dataframe to hold the results
  out <- data.frame(t(out))
  # add the first column as the names of the results
  data.frame(name = names(boot.out$t0), out)
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
exp_effects <- function(data,
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
invlogit_effects <- function(data, vars = c("P" = "logitP")) {
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
