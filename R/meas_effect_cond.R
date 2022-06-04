#' Compute estimates of the conditional association measures
#'
#' Compute estimates of the conditional association measures.
#'
#' Estimate the expected conditional outcomes and the
#' conditional effect or association measures. See exercise 3 of chapter 3
#' for a full example on how to use this function.
#'
#' @param data Dataframe of raw data.
#' @param formula The model formula.
#' @param exposure.name Name of the exposure variable.
#' @param condition.names Character vector of conditioned variable names. If
#' \code{NULL} all independent variables exclusding the \code{exposure.name}
#' will be used.
#' @param family Name of distribution. Must be in
#'  \code{c("binomial", "poisson", "gaussian")}.
#'
#' @importFrom rlang f_rhs .data
#' @importFrom stats glm coef plogis
#'
#' @return Numeric vector of summarized results.
#' @export
meas_effect_cond <- function(data, formula = Y ~ `T` + A + H,
                             exposure.name = "T", condition.names = NULL,
                             family = c("binomial", "poisson", "gaussian")) {

  # independent variables from the formula
  indvars <- all.vars(rlang::f_rhs(formula))
  # if condition.names is NULL then use all independent variables
  # EXCLUDING exposure
  if (is.null(condition.names)) condition.names <- indvars[indvars != exposure.name]
  stopifnot(all(condition.names %in% indvars), exposure.name %in% indvars)
  # name of intercept used by lm, glm, etc.
  x0 <- "(Intercept)"


  # get the family
  family <- match.arg(family)

  coefs <- coef(glm(formula = formula, family = family, data = data))
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
