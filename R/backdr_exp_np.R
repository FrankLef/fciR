#' Compute standardized averages using exposure modeling, non-parametric
#'
#' Compute standardized averages using exposure modeling, non-parametric.
#'
#' Compute standardized averages using exposure modeling as described in
#' section 6.2.
#'
#' @param data Dataframe
#' @param formula Formula, must be in the format \code{Y ~ `T` + H}, i.e.
#' only 1 covariate H.
#' @param weights String. Name of the columns with the weights that will
#' be used to create probabilities summing up to 1.
#'
#' @return List with 3 elements: EY1, EY0, EY0T1. See section 6.2.
#' for more details.
#' @export
backdr_exp_np <- function(data, formula = Y ~ `T` + H, weights = "n") {

  # extract the variables names from the formula
  fvars <- formula2vars(formula)
  # there can be only one H
  stopifnot(length(fvars$h) == 1)

  # compute e(H=0)
  dat0 <- data[data[, fvars$h] == 0, ]
  eH0 <- sum(dat0[dat0[, fvars$t] == 1, weights]) / sum(dat0[, weights])
  # compute e(H=1)
  dat1 <- data[data[, fvars$h] == 1, ]
  eH1 <- sum(dat1[dat1[, fvars$t] == 1, weights]) / sum(dat1[, weights])
  # compute e(H) for all participants
  eH <- eH0 * (1 - data[, fvars$h]) + eH1 * data[, fvars$h]
  # compute the summands of the estimating equations
  s1 <- data[, fvars$t] * data[, fvars$y] / eH
  s0 <- (1 - data[, fvars$t]) * data[, fvars$y] / (1 - eH)

  # estimate the expected values of the potential outcomes
  probs <- data[, weights] / sum(data[, weights])  # the probabilities
  stopifnot(sum(probs) == 1)
  EY1 <- sum(s1 * probs)
  EY0 <- sum(s0 * probs)

  # ATT calculations
  # estimate P(T = 1)
  e0 <- sum(data[, fvars$t] * probs)
  # compute the summands of the estimating equation
  s <- data[, fvars$y] * (1 - data[, fvars$t]) * eH / (e0 * (1 - eH))
  # estimate E(Y0|T=1)
  EY0T1 <- sum(s * probs)

  list("EY1" = EY1, "EY0" = EY0, "EY0T1" = EY0T1)
}

#' @rdname backdr_exp_np
#' @export
calc_exposure <- backdr_exp_np
