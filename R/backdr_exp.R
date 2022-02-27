#' Compute standardized estimates with parametric exposure model
#'
#' Compute standardized estimates with parametric exposure model.
#'
#' The standardized estimates are computed using the exposure model.
#' This method requires 2 different formulas which are created from the
#' arguments \code{formula}. The 2 formulas created are for the exposure model
#' and another one for the weighted linear model. lso \code{T} must always be
#' binary, if not it can be made binary "one can first recode the data so that
#' T = 1 when it is previously equaled, and T = 0 when it previously equaled
#' any value other than t", p. 113.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula must be in the form \code{Y ~ `T` + ...}
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @importFrom stats lm glm fitted predict
#'
#' @return Dataframe of estimates using exposure-model standardization
#' @export
backdr_exp <- function(data, formula = Y ~ `T` + H, att = FALSE,
                     R = 1000, conf = 0.95) {

  # extract the variables names from the formula
  fvars <- formula2vars(formula)

  # exposure model formula
  eformula <- formula(paste(fvars$t, paste(fvars$h, collapse = "+"), sep = "~"))
  # weighted linear model formula
  lformula <- formula(paste(fvars$y, fvars$t, sep = "~"))

  estimator <- function(data, ids) {
    dat <- data[ids, ]

    # estimate the parametric exposure model
    # NOTE: fitted() is the same as using predict(..., type = "response")
    #       BUT fitted only use the ORIGINAL data, there is no newdata.
    e <- fitted(glm(formula = eformula, family = "binomial", data = dat))
    stopifnot(all(!dplyr::near(e, 0)))  # e must not equal zero

    # compute the weights
    datT <- dat[, fvars$t]
    dat$W <- (1 / e) * datT + (1 / (1 - e)) * (1 - datT)

    # fit the weighted linear model
    coefs <- coef(glm(formula = lformula, data = dat, weights = W))

    # estimate the expected potential outcome
    EY0 <- coefs[fvars$x0]
    EY1 <- sum(coefs)

    # estimate the effect measures
    effect_measures(EY0, EY1)
  }

  # run the bootstrapping
  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  effect_exp(data = out)
}

#' @rdname backdr_exp
#' @export
standexp <- backdr_exp
