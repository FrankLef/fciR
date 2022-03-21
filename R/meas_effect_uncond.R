#' Compute estimates of the unconditional association measures
#'
#' Compute estimates of the unconditional association measures.
#'
#' Compute estimates of the unconditional association measures,
#' hence the u in \code{bootu()}, and their confidence intervals.
#' 1- We use Gaussian, Poisson and Binomial glm to solve the
#'    estimating equations of the measures, not their distributions
#' 2- The association measures' distributions (ci) are estimated by
#'    bootstrapping
#' Assumptions: We assume that (3.2) holds but not (3.1)
#' See p. 45 and 46 for more details.
#' @param data Dataframe of raw data.
#' @param outcome.name Name of outcome variable.
#' @param input.names Character vector of input variable names.
#'
#' @importFrom formulaic create.formula
#' @importFrom stats glm coef plogis
#'
#' @return Numeric vector of summarized results
#' @export
meas_effect_uncond <- function(data, outcome.name = "Y", input.names = c("T")) {

  x0 <- "(Intercept)"  # name of intercept used by lm, glm, etc.

  a_formula <- formulaic::create.formula(outcome.name = outcome.name,
                                         input.names = input.names,
                                         dat = data)

  # estimate the conditional probabilities
  coefs <- coef(glm(formula = a_formula, family = "gaussian", data = data))
  p0 <- coefs[x0]
  p1 <- sum(coefs)
  # estimate the risk difference
  rd <- p1 - p0

  # use loglinear model to estimate the log relative risk
  coefs <- coef(glm(formula = a_formula, family = "poisson", data = data))
  logrr <- coefs[input.names]

  # prepare data to estimate the log other relative risk
  ystar <- 1 - data[, outcome.name]
  xstar <- 1 - data[, input.names]

  # use loglinear model to estimate the log other relative risk
  coefs <- coef(glm(ystar ~ xstar, family = "poisson"))
  logrrstar <- coefs[2]

  # use logistic model to estimate the log of other risk
  coefs <- coef(glm(formula = a_formula, family = "binomial", data = data))
  logor <- coefs[input.names]

  # return the results
  out <- c(p0, p1, rd, logrr, logrrstar, logor)
  names(out) <- c("P0", "P1", "RD", "logRR", "logRR*", "logOR")
  out
}


#' @rdname meas_effect_uncond
#' @export
bootu <- meas_effect_uncond
