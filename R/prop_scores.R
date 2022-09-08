#' Fit the Propensity Score Model
#'
#' Fit the propensity score model.
#'
#' Fit the propensity score model to obtain the score for a given data set.
#'
#' @param data Data.frame.
#' @param formula Formula used to fit the propensity score.
#'
#' @source Section 6.2.2, p. 120-121. Used throughout chapter 10.
#'
#' @return List with
#' \describe{
#'   \item{scores}{Vector of propensity scores.}
#'   \item{fit}{The object of class glm returned by glm().}
#' }
#' @export
prop_scores <- function(data, formula) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)

  fit <- glm(formula = formula, family = "binomial", data = data)
  scores <- fitted(fit)
  list("scores" = scores, "fit" = fit)
}

#' @rdname prop_scores
#' @export
prop.r <- prop_scores
