#' Get estimate and CI using a function as input
#'
#' Get estimate and CI using a function as input.
#'
#' Boostrap with a function and extra arguments as input. The function is called
#' \code{statistic} to remain constant with \code{boot::boot}.
#'
#' @inheritParams boot_run
#' @param transf Type of conversion. Must be one of
#' \code{c("identity", "exp", "expit")}, default is \code{identity}.
#' @param ... Other named arguments for \code{func}.
#'
#' @seealso effect_transf
#'
#' @return Dataframe of estimates with confidence interval.
#' @export
boot_est <- function(data, func, times = 1000, alpha = 0.05, seed = NULL,
                     transf = c("identity", "exp", "expit"), ...) {
  stopifnot(times >= 1, alpha > .Machine$double.eps^0.5, alpha < 0.5)

  transf <- match.arg(transf)

  out <- boot_run(data = data, func = func, times = times, alpha = alpha,
                  seed = seed, ...)

  # transform the results
  effect_transf(data = out, transf = transf)
}

#' Bootstrapping Confidence Intervals with Base R
#'
#' Bootstrapping confidence intervals with base R.
#'
#' Generate a dataframe of estimates using he \code{boot::boot} function for
#' bootstrapping and \code{boot::boot.ci} to compute the confidence intervals.
#'
#' @inheritParams boot_run
#' @param ... Other named arguments for \code{statistic}.
#'
#' @seealso boot::boot boot::boot.ci
#'
#' @return Dataframe with term, .lower, .estimate, .upper, .alpha, .method.
#' @export
bootR_run <- function(data, func, times = 1000, alpha = 0.05, seed = NULL, ...) {
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
  set.seed(seed)
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
#' @param seed Seed for random number generator. If \code{NULL} the seed itself
#' is random.
#' @param ... Additional arguments used by \code{func}.
#'
#' @importFrom rsample bootstraps int_pctl analysis
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
#'
#' @seealso rsample::bootstraps rsample::int_pctl
#'
#' @source \url{https://rsample.tidymodels.org/articles/Applications/Intervals.html}
#'
#' @return Dataframe with term, .lower, .estimate, .upper, .alpha, .method.
#' @export
boot_run <- function(data, func, times = 1000, alpha = 0.05, seed = NULL, ...) {
  stopifnot(times >= 1, alpha > .Machine$double.eps^0.5, alpha < 0.5)

  set.seed(seed)
  data |>
    rsample::bootstraps(times = times, apparent = FALSE) |>
    mutate(results = purrr::map(.data$splits, function(x) {
      dat <- rsample::analysis(x)
      func(dat, ...)
    })) |>
    rsample::int_pctl(.data$results, alpha = alpha) |>
    suppressWarnings()
}
