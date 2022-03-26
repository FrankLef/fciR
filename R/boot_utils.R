#' Get estimate and CI using a function as input
#'
#' Get estimate and CI using a function as input.
#'
#' Boostrap with a function and extra arguments as input. The function is called
#' \code{statistic} to remain constant with \code{boot::boot}.
#'
#' @inheritParams boot_run
#' @param func Function used to compute the estimate.
#' @param inv Choice of inverse function to apply to the effect measure
#'  \code{exp} will exponentiate the result (Default), \code{expit} will apply
#'  the inverse logit and \code{none} will do nothing (identity function).
#' @param evars String. Should one of \code{c("standard", "modifier", "logit")}.
#' Default value is \code{"standard"}.
#' @param ... Other named arguments for \code{func}.
#'
#' @return Dataframe of estimates with CI.
#' @export
boot_est <- function(data, func, R = 1000, conf = 0.95,
                     inv = c("exp", "expit", "none"),
                     evars = c("standard", "modifier", "logit"),
                     ...) {
  inv <- match.arg(inv)
  evars <- match.arg(evars)

  estimator <- function(data, ids) {
    dat <- data[ids, ]
    func(dat, ...)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # inverse transform the result
  vars <- effect_vars(evars = evars)
  effect_inv(data = out, inv = inv, vars = vars)
}

#' Bootstrap and generate a dataframe of estimates with CI
#'
#' Bootstrap and generate a dataframe of estimates with CI.
#'
#' Generate a dataframe of estimates with the columns
#' \code{c("est", "conf", "lci", "uci")}. The \code{boot::boot} function is used
#' for bootstraping and the function \code{boot::boot.ci} is used to compute the
#' confidence interval.
#'
#' @param data Dataframe of raw data.
#' @param statistic Function applied to data by bootstrapping.
#' @param R Number of bootstrap replicates. Default is 1000.
#' @param conf Confidence interval width. Default is 0.95.
#' @param ... Other named arguments for \code{statistic}.
#'
#' @seealso boot::boot boot::boot.ci
#'
#' @return Dataframe of estimates with CI.
#' @export
boot_run <- function(data, statistic, R = 1000, conf = 0.95, ...) {
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
