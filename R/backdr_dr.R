#' Compute the doubly robust standardized estimates
#'
#' Compute the doubly robust standardized estimates.
#'
#' Compute the doubly robust standardized estimates using the code from section
#' 6.3.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + ...} see details above.
#' @param family Character. Name of the distribution. Default is "binomial".
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @importFrom stats lm glm fitted predict
#'
#' @return Dataframe of estimates
#' @export
backdr_dr <- function(data, formula = Y ~ `T` + H,
                    family = c("binomial", "poisson", "gaussian"),
                    R = 1000, conf = 0.95) {
  family <- match.arg(family)

  # extract the variables names from the formula
  fvars <- formula2vars(formula)

  # exposure model formula
  eformula <- formula(paste(fvars$t, paste(fvars$h, collapse = "+"), sep = "~"))

  estimator <- function(data, ids) {
    dat <- data[ids, ]

    # estimate the parametric exposure model
    e <- fitted(glm(formula = eformula, family = "binomial", data = dat))
    stopifnot(all(!dplyr::near(e, 0)))  # e must not equal zero

    # Fit the parametric outcome model
    lmod <- glm(formula = formula, family = family, data = dat)

    # predict potential outcome for each participant
    dat0 <- dat
    dat0[, fvars$t] <- 0
    EYhat0 <- predict(lmod, newdata = dat0, type = "response")
    dat1 <- dat
    dat1[, fvars$t] <- 1
    EYhat1 <- predict(lmod, newdata = dat1, type = "response")

    # Use the DR estimating equation to estimate the expected
    # potential outcome
    datY <- dat[, fvars$y]
    datT <- dat[, fvars$t]
    EY0 <- mean(datY * (1 - datT) / (1 - e) + EYhat0 * (e - datT) / (1 - e))
    EY1 <- mean(datY * (datT / e) - EYhat1 * (datT - e) / e)

    # estimate the effect measures
    effect_measures(EY0, EY1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  effect_exp(data = out)
}

#' @rdname backdr_dr
#' @export
standdr <- backdr_dr
