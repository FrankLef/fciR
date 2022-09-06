#' Fit the Propensity Score Model
#'
#' Fit the propensity score model.
#'
#' Fit the propensity score model to obtain the score for a given data set.
#'
#' @param data Data.frame.
#' @param formula Formula used to fit the propensity score.
#'
#' @source Section 6.2.2, p. 120-121.
#'
#' @return List with fitted values and the fit object.
#' @export
prop_scores <- function(data, formula) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)

  fit <- glm(formula = formula, family = "binomial", data = data)
  list(fitted = fitted(fit), fit = fit)
}
