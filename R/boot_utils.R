#' Get estimate and CI using a function as input
#'
#' Get estimate and CI using a function as input.
#'
#' Boostrap with a function and extra arguments as input. The function is called
#' \code{statistic} to remain constant with \code{boot::boot}.
#'
#' @inheritParams boot_run
#' @param inv Choice of inverse function to apply to the effect measure
#'  \code{exp} will exponentiate the result (Default), \code{expit} will apply
#'  the inverse logit and \code{none} will do nothing (identity function).
#' @param evars String. Should one of \code{c("standard", "modifier", "logit")}.
#' Default value is \code{"standard"}.
#' @param ... Other named arguments for \code{func}.
#'
#' @return Dataframe of estimates with confidence interval..
#' @export
boot_est <- function(data, func, times = 1000, alpha = 0.05,
                     inv = c("exp", "expit", "none"),
                     evars = c("standard", "modifier", "logit"),
                     ...) {
  stopifnot(times >= 1, alpha > .Machine$double.eps^0.5, alpha < 0.5)
  inv <- match.arg(inv)
  evars <- match.arg(evars)

  out <- boot_run(data = data, func = func, times = times, alpha = alpha, ...)

  # compute the 4 effect measures
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
#' @param func Function applied to data by bootstrapping.
#' @param times Number of bootstrap replicates. Default is 1000.
#' @param alpha Alpha used by percentile to give interval in
#' \code{c(alpha, 1- alpha)}.
#' @param ... Other named arguments for \code{statistic}.
#'
#' @seealso boot::boot boot::boot.ci
#'
#' @return Dataframe of estimates with CI.
#' @export
boot_run <- function(data, func, times = 1000, alpha = 0.05, ...) {
  stopifnot(times >= 1, alpha > .Machine$double.eps^0.5, alpha < 0.5)

  # the function is for bootstrapping returns a named vector
  boot.func <- function(data, ids, ...) {
    dat <- data[ids, ]
    df <- func(dat, ...)
    out <- c(df$estimate)
    names(out) <- df$term
    out
  }

  # run the bootstrapping
  boot.out <- boot::boot(data = data, statistic = boot.func, R = times, ...)

  # the method used for intervals
  the_method <- "norm"

  # extract the estimated values and confidence intervals from the boot object
  out <- lapply(X = seq_along(boot.out$t0), FUN = function(i) {
    est <- boot.out$t0[i]
    ci <- boot::boot.ci(boot.out, conf = 1 - alpha, type = the_method, index = i)
    ci <- ci$normal
    data.frame(
      "term" = names(est),
      ".lower" = ci[2],
      ".estimate" = unname(est),
      ".upper" = ci[3],
      ".alpha" = alpha,
      ".method" = the_method
    )
  })

  # bind the data.frames together
  do.call(rbind, out)
}


#' Bootstrapping Confidence Intervals with Tidyverse
#'
#' Bootstrapping confidence intervals with tidyverse.
#'
#' Bootstrapping confidence intervals using the \code{rsample} package.
#'
#' @param data Dataframe of raw data.
#' @param func Function applied to data by bootstrapping.
#' @param times Number of bootstrap replicates. Default is 1000.
#' @param alpha Alpha used by percentile to give interval in
#' \code{c(alpha, 1- alpha)}.
#' @param ... Additional arguments used by \code{func}.
#'
#' @importFrom rsample bootstraps int_pctl analysis
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
#'
#' @return Dataframe with term, .lower, .estimate, .upper, .alpha, .method
#' @export
boot_run_td <- function(data, func, times = 1000, alpha = 0.05, ...) {
  stopifnot(times >= 1, alpha > .Machine$double.eps^0.5, alpha < 0.5)

  data |>
    rsample::bootstraps(times = times, apparent = FALSE) |>
    mutate(results = purrr::map_dfr(.data[["splits"]], function(x) {
      dat <- rsample::analysis(x)
      func(dat, ...)
    })) |>
    rsample::int_pctl(.data[["results"]], alpha = alpha)
}
