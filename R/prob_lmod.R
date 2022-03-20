#' Estimate s sampling distribution by Bootstrapping
#'
#' Estimate s sampling distribution by bootstrapping.
#'
#  Estimate a sampling distribution by bootstrapping using \code{boot::boot()}
#'
#' @param data Dataframe of raw data.
#' @param outcome.name Name of outcome variable.
#' @param input.names Character vector of input variable names.
#' @param condition.names Character vector of conditioned variable names.
#'
#' @importFrom formulaic create.formula
#'
#' @return Vector of summarized results
#' @export
prob_lmod <- function(data, outcome.name = "Y", input.names = c("T", "A", "H"),
                     condition.names = input.names) {
  x0 <- "(Intercept)"  # name of intercept used by lm, glm, etc.

  a_formula <- formulaic::create.formula(outcome.name = outcome.name,
                              input.names = input.names,
                              dat = data)
  coefs <- coef(glm(formula = a_formula, family = "binomial", data = data))
  coefs <- coefs[c(x0, condition.names)]
  p <- unname(sum(coefs))
  c("logitP" = p)
}

#' @rdname prob_lmod
#' @export
lmodboot <- prob_lmod

#' Estimate s sampling distribution by Bootstrapping
#'
#' Estimate s sampling distribution by bootstrapping.
#'
#  Estimate a sampling distribution by bootstrapping using \code{boot::boot()}
#'
#' @param data Dataframe of raw data.
#' @param formula Formula of linear model.
#' @param cond Formula to identify conditioned variables.
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @importFrom stats coef glm plogis coef glm
#'
#' @return Vector of summarized results
#' @export
prob_lmodX <- function(data, formula = Y ~ `T` + A + H,
                      cond = Y ~ `T` + A + H,
                      R = 1000, conf = 0.95) {
  # extract the variable names form the formula
  fvars <- formula2vars(cond)

  estimator <- function(data, ids) {
    dat <- data[ids, ]
    coefs <- coef(glm(formula = formula, family = "binomial", data = dat))
    coefs <- coefs[c(fvars$x0, fvars$ind)]
    p <- unname(sum(coefs))
    c("logitP" = p)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  effect_expit(out)
}
