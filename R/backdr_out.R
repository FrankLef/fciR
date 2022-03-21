#' Compute standardized estimates with parametric outcome model
#'
#' Compute standardized estimates with parametric outcome model.
#'
#' The standardized estimates are computed using the outcome model. See chapter
#' 6 for details.
#'
#' @param data Dataframe of raw data.
#' @param outcome.name Name of outcome variable.
#' @param exposure.name Name of exposure variable.
#' @param confound.names Character vector of confound variable names.
#' @param family Name of distribution. Must be in
#'  \code{c("binomial", "poisson", "gaussian")}.
#'
#' @importFrom formulaic create.formula
#' @importFrom stats lm glm fitted predict
#'
#' @return Estimate using outcome-model standardization
#' @export
backdr_out <- function(data, outcome.name = "Y", exposure.name = "T",
                       confound.names = c("A", "H"),
                       family = c("binomial", "poisson", "gaussian")) {
  x0 <- "(Intercept)"  # name of intercept used by lm, glm, etc.
  family <- match.arg(family)

  input.names <- c(exposure.name, confound.names)
  a_formula <- formulaic::create.formula(outcome.name = outcome.name,
                                         input.names = input.names,
                                         dat = data)

  lmod.out <- glm(formula = a_formula, family = family, data = data)

  # dataset with everyone untreated
  dat0 <- data
  dat0[, exposure.name] <- 0

  # dataset with everyone treated
  dat1 <- data
  dat1[, exposure.name] <- 1

  # compute the expected potential outcome for
  # each participant if untreated
  # NOTE: fitted() is the same as using predict(..., type = "response")
  #       BUT fitted only use the ORIGINAL data, there is no newdata.
  EYhat0 <- predict(lmod.out, newdata = dat0, type = "response")
  # compute the expected potential outcome for
  # each participant if treated
  # NOTE: fitted() is the same as using predict(..., type = "response")
  #       BUT fitted only use the ORIGINAL data, there is no newdata.
  EYhat1 <- predict(lmod.out, newdata = dat1, type = "response")

  # estimate the average potential outcomes
  EY0 <- mean(EYhat0)
  EY1 <- mean(EYhat1)

  # estimate the effect measures
  effect_measures(val0 = EY0, val1 = EY1)
}

#' @rdname backdr_out
#' @export
standout <- backdr_out

#' Compute standardized estimates with parametric outcome model
#'
#' Compute standardized estimates with parametric outcome model.
#'
#' The standardized estimates are computed using the outcome model.
#' IMPORTANT: The formula must be in the format \code{Y ~ T + ...} where T
#' is the exposure.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + ...} see details above.
#' @param family Character. Name of the distribution. Default is "binomial".
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @importFrom stats lm glm fitted predict
#'
#' @return Estimate using outcome-model standardization
#' @export
backdr_outX <- function(data, formula = Y ~ `T` + H,
                       family = c("binomial", "poisson", "gaussian"),
                       R = 1000, conf = 0.95) {
  family <- match.arg(family)

  # extract the variables names from the formula
  fvars <- formula2vars(formula)


  estimator <- function(data, ids) {
    dat <- data[ids, ]

    lmod.out <- glm(formula = formula, family = family, data = dat)

    # dataset with everyone untreated
    dat0 <- dat
    dat0[, fvars$t] <- 0

    # dataset with everyone treated
    dat1 <- dat
    dat1[, fvars$t] <- 1

    # compute the expected potential outcome for
    # each participant if untreated
    # NOTE: fitted() is the same as using predict(..., type = "response")
    #       BUT fitted only use the ORIGINAL data, there is no newdata.
    EYhat0 <- predict(lmod.out, newdata = dat0, type = "response")
    # compute the expected potential outcome for
    # each participant if treated
    # NOTE: fitted() is the same as using predict(..., type = "response")
    #       BUT fitted only use the ORIGINAL data, there is no newdata.
    EYhat1 <- predict(lmod.out, newdata = dat1, type = "response")

    # estimate the average potential outcomes
    EY0 <- mean(EYhat0)
    EY1 <- mean(EYhat1)

    # estimate the effect measures
    effect_measures(val0 = EY0, val1 = EY1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  effect_exp(data = out)
}
