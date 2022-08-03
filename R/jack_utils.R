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
jack_ci <- function(x, alpha = 0.05) {
  checkmate::assertNumber(alpha, lower = 0.01, upper = 0.49)

  n <- length(x)
  nsample <- n - 1
  m <- mean(x)
  v <- (nsample / n) * sum((x - m)^2)
  se <- sqrt(v)
  ci <- qt(1 - alpha, df = nsample)
  c(".lower" = m - ci * se, ".estimate" = m, ".upper" = m + ci * se)
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
jack_run <- function(data, func, alpha = 0.05, ...) {
  checkmate::assertNumber(alpha, lower = 0.01, upper = 0.49)

  # get the leave-one-out samples
  the_samples <- rsample::loo_cv(data)

  # estimates the effect measures
  the_results <- purrr::map_dfr(.x = the_samples$splits, .f = function(x) {
    dat <- rsample::analysis(x)
    df <- func(dat, ...)
    out <- c(df$estimate)
    names(out) <- df$term
    out
  })

  # compute the confidence interval for each term
  out <- purrr::map_dfr(.x = the_results, .f = ~jack_ci(., alpha = alpha), .id = "term")

  # create the output dataframe
  data.frame(
    out,
    ".alpha" = alpha,
    ".method" = "qt"
  )
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
#' @param alpha Alpha used by percentile to give interval in
#' \code{c(alpha, 1- alpha)}.
#' @param transf Type of conversion. Must be one of
#' \code{c("identity", "exp", "expit")}, default is \code{identity}.
#' @param ... Other named arguments for \code{func}.
#'
#' @seealso jack_run jack_ci effect_transf
#'
#' @return Dataframe of estimates with confidence interval.
#' @export
jack_est <- function(data, func, alpha = 0.05,
                     transf = c("identity", "exp", "expit"), ...) {
  checkmate::assertDataFrame(data)
  checkmate::assertFunction(func)
  checkmate::assertNumber(alpha, lower = 0.01, upper = 0.49)

  transf <- match.arg(transf)

  out <- jack_run(data = data, func = func, alpha = alpha, ...)

  # inverse transform the result
  effect_transf(data = out, transf = transf)
}

#' @rdname jack_est
#' @export
jackiv.r <- jack_est
