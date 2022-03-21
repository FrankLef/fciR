#' Estimate s sampling distribution by Bootstrapping
#'
#' Estimate s sampling distribution by bootstrapping.
#'
#  Estimate a sampling distribution. See details in chapter 2.
#'
#' @param data Dataframe of raw data.
#' @param outcome.name Name of outcome variable.
#' @param input.names Character vector of input variable names.
#' @param condition.names Character vector of conditioned variable names.
#'
#' @importFrom formulaic create.formula
#' @importFrom stats glm coef plogis
#'
#' @return Numeric vector of summarized results
#' @export
prob_lmod <- function(data, outcome.name = "Y", input.names = c("T", "A", "H"),
                     condition.names = input.names) {
  stopifnot(all(condition.names %in% input.names))
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
