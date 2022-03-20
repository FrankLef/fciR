#' Compute estimates of the conditional association measures
#'
#' Compute estimates of the conditional association measures.
#'
#' Estimate the expected conditional outcomes and the
#' conditional effect or association measures. See exercise 3 of chapter 3
#' for a full example on how to use this function.
#'
#' @param data Dataframe of raw data.
#' @param outcome.name Name of outcome variable.
#' @param exposure.name Name of exposure variable.
#' @param confound.names Character vector of confound variable names.
#' @param condition.names Character vector of condition variable names.
#' @param family Name of distribution. Must be in
#'  \code{c("binomial", "poisson", "gaussian")}
#'
#' @importFrom formulaic create.formula
#'
#' @return Numeric vector of summarized results.
#' @export
meas_effect_cond <- function(data, outcome.name = "Y", exposure.name = "T",
                             confound.names = c("A", "H"),
                             condition.names = confound.names,
                             family = c("binomial", "poisson", "gaussian")) {
  stopifnot(all(condition.names %in% confound.names))
  x0 <- "(Intercept)"  # name of intercept used by lm, glm, etc.

  family <- match.arg(family)

  # all the input names
  input.names <- c(exposure.name, confound.names)

  a_formula <- formulaic::create.formula(outcome.name = outcome.name,
                                         input.names = input.names,
                                         dat = data)

  coefs <- coef(glm(formula = a_formula, family = family, data = data))
  # use variables from cond0 and cond1 to identify conditioned variables.
  xbeta0 <- sum(coefs[c(x0, condition.names)])
  xbeta1 <- sum(coefs[c(x0, exposure.name, condition.names)])

  P0 <- xbeta0
  P1 <- xbeta1

  if (family == "binomial") {
    P0 <- plogis(P0)  # plogis is the inverse of logit
    P1 <- plogis(P1)  # plogis is the inverse of logit
  } else if (family == "poisson") {
    P0 <- exp(P0)
    P1 <- exp(P1)
  }

  # calculate effect measures
  effect_measures(val0 = P0, val1 = P1)
}

#' @rdname meas_effect_cond
#' @export
bootc <- meas_effect_cond


#' Compute estimates of the conditional association measures
#'
#' Compute estimates of the conditional association measures.
#'
#' Estimate the expected conditional outcomes and the
#' conditional effect or association measures.
#' IMPORTANT: This is the function in chapter 3 called lmodboot.r
#'            It has been renamed bootc.r to avoid conflict with
#'            lmodboot.r of chapter 2
#'
#' @param data Dataframe of raw data.
#' @param formula Formula of linear model.
#' @param cond0 Formula of condition 0.
#' @param cond1 Formula of condition 1.
#' @param family Family used by \code{glm}. Default is "binomial".
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @return Dataframe of effect measures.
#' @export
meas_effect_condX <- function(data, formula = Y ~ `T` + A + H,
                             cond0 = Y ~ A + H,
                             cond1 = Y ~ `T` + A + H,
                             family = c("binomial", "poisson", "gaussian"),
                             R = 1000, conf = 0.95) {
  # the family used by glm
  family <- match.arg(family)

  # extract the variables names from the formula
  fvars0 <- formula2vars(cond0)
  fvars1 <- formula2vars(cond1)


  estimator <- function(data, ids) {
    dat <- data[ids, ]
    coefs <- coef(glm(formula = formula, family = family, data = dat))
    # use variables from cond0 and cond1 to identify conditioned variables.
    xbeta0 <- sum(coefs[c(fvars0$x0, fvars0$ind)])
    xbeta1 <- sum(coefs[c(fvars1$x0, fvars1$ind)])

    P0 <- plogis(xbeta0)  # plogis is the inverse of logit
    P1 <- plogis(xbeta1)  # plogis is the inverse of logit

    # calculate effect measures
    effect_measures(val0 = P0, val1 = P1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  effect_exp(out)  # exponentiate effect measures
}
