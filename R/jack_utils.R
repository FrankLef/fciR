#' Compute the Confidence Interval Estimated with Jacknife
#'
#' Compute the confidence interval estimated with jacknife.
#'
#' This function is used to calculate the confidence interval using input
#' data estimated with the jacknife method.
#'
#' @param x Numerical vector of effect measure
#' @inheritParams jack_est
#'
#' @importFrom stats qt
#'
#' @return Numeric vector with estimates and confidence interval.
#' @export
jack_ci <- function(x, conf = 0.95) {
  n <- length(x)
  nsample <- n - 1
  m <- mean(x)
  v <- (nsample / n) * sum((x - m)^2)
  se <- sqrt(v)
  ci <- stats::qt(conf, df = nsample)
  c("est" = m, "se" = se, "conf" = conf,
    "lci" = m - ci * se, "uci" = m + ci * se)
}



#' Estimate of Effect Measure and CI With Jacknife (LOO)
#'
#' Estimate of effect measure and CI With jacknife (LOO).
#'
#' Compute the confidence interval of effect measures using the jacknife method.
#' It uses the \code{loo_cv} function from the \code{rsample} package. The
#' \code{jacknife} funciton from the \code{resample} is not maintained anymore
#' and the author of \code{resample} recommend using the \code{rsample} package.
#'
#' @inheritParams jack_est
#'
#' @importFrom rsample loo_cv analysis
#' @importFrom purrr map_dfr
#'
#' @return Dataframe of estimates with confidence interval.
#' @export
jack_run <- function(data, func, conf = 0.95, ...) {
  # get the leave-one-out samples
  the_samples <- rsample::loo_cv(data)

  # estimates the effect measures
  the_results <- purrr::map_dfr(.x = the_samples$splits, .f = function(x) {
    df <- rsample::analysis(x)
    func(df, ...)
  })

  # compute the confidence intervals
  purrr::map_dfr(.x = the_results, .f = ~jack_ci(., conf = conf), .id = "name")
}



#' Estimate of Effect Measure and CI With Jacknife (LOO)
#'
#' Estimate of effect measure and CI With jacknife (LOO).
#'
#' Compute the confidence interval of effect measures using the jacknife method.
#' It uses the \code{loo_cv} function from the \code{rsample} package. The
#' \code{jacknife} funciton from the \code{resample} is not maintained anymore
#' and the author of \code{resample} recommend using the \code{rsample} package.
#' The output is transformed with inverse function when required. See section
#' 9.3 of the book for more details.
#'
#' @param data Dataframe of raw data.
#' @param func Function to estimate the effect measure.
#' @param conf Confidence interval width. Default is 0.95.
#' @param inv Choice of inverse function to apply to the effect measure
#'  \code{exp} will exponentiate the result (Default), \code{expit} will apply
#'  the inverse logit and \code{none} will do nothing (identity function).
#' @param evars String. Should one of \code{c("standard", "modifier", "logit")}.
#' Default value is \code{"standard"}.
#' @param ... Other named arguments for \code{func}.
#'
#' @seealso jack_run jack_ci
#'
#' @return Dataframe of estimates with confidence interval.
#' @export
jack_est <- function(data, func, conf = 0.95,
                     inv = c("exp", "expit", "none"),
                     evars = c("standard", "modifier", "logit"),
                     ...) {
  inv <- match.arg(inv)
  evars <- match.arg(evars)

  out <- jack_run(data = data, func = func, conf = conf, ...)

  # inverse transform the result
  vars <- effect_vars(evars = evars)
  effect_inv(data = out, inv = inv, vars = vars)
}
