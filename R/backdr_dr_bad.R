#' Doubly robust standardized estimates with misspecified outcome model
#'
#' Doubly robust standardized estimates with misspecified outcome model.
#'
#' Compute the doubly robust standardized estimates using the code from section
#' 6.3.
#'
#' @inheritParams backdr_out
#'
#' @importFrom formulaic create.formula
#' @importFrom stats glm fitted predict
#'
#' @return Dataframe of estimates
#' @export
backdr_dr_bad <- function(data, outcome.name = "Y", exposure.name = "T",
                          confound.names = "H") {

  # exposure model formula
  eformula <- formulaic::create.formula(outcome.name = exposure.name,
                                        input.names = confound.names,
                                        dat = data)

  # weighted linear model formula
  lformula <- formulaic::create.formula(outcome.name = outcome.name,
                                        input.names = exposure.name,
                                        dat = data)

  # estimate the parametric exposure model
  eH <- fitted(glm(formula = eformula, family = "binomial", data = data))
  stopifnot(all(!dplyr::near(eH, 0)))  # eH must not equal zero

  # fit a nonparametric outcome model that we do not believe
  # i.e. a bad outcome model
  lmod <- glm(formula = lformula, family = "binomial", data = data)

  # predict potential outcome for each participant
  dat0 <- data
  dat0[, exposure.name] <- 0
  EYhat0 <- predict(lmod, newdata = dat0, type = "response")
  dat1 <- data
  dat1[, exposure.name] <- 1
  EYhat1 <- predict(lmod, newdata = dat1, type = "response")

  # Use the DR estimating equation to estimate the expected
  # potential outcome
  datY <- data[, outcome.name]
  datT <- data[, exposure.name]
  EY0 <- mean(datY * (1 - datT) / (1 - eH) + EYhat0 * (eH - datT) / (1 - eH))
  EY1 <- mean(datY * (datT / eH) - EYhat1 * (datT - eH) / eH)

  # estimate the effect measures
  effect_measures(EY0, EY1)
}


#' @rdname backdr_dr_bad
#' @export
badstanddr <- backdr_dr_bad


#' Doubly robust standardized estimates with misspecified outcome model
#'
#' Doubly robust standardized estimates with misspecified outcome model.
#'
#' Compute the doubly robust standardized estimates using the code from section
#' 6.3
#'
#' @param data Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + ...} see details above.
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @return Dataframe of estimates
#' @export
backdr_dr_badX <- function(data, formula = Y ~ `T` + H, R = 1000, conf = 0.95) {

  # extract the variables names from the formula
  fvars <- formula2vars(formula)

  # exposure model formula
  eformula <- formula(paste(fvars$t, paste(fvars$h, collapse = "+"), sep = "~"))
  # weighted linear model formula
  lformula <- formula(paste(fvars$y, fvars$t, sep = "~"))

  estimator <- function(data, ids) {
    dat <- data[ids, ]

    # estimate the parametric exposure model
    e <- fitted(glm(formula = eformula, family = "binomial", data = dat))
    stopifnot(all(!dplyr::near(e, 0)))  # e must not equal zero

    # fit a nonparametric outcome model that we do not believe
    # i.e. a bad outcome model
    lmod <- glm(formula = lformula, family = "binomial", data = dat)

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
