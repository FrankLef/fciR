#' Estimate the Natural Indirect Effect of a Mediator Variable
#'
#' Estimate the natural indirect effect of a mediator variable.
#'
#' Estimate the natural indirect effect of a mediator variable with parametric
#' assumptions.
#'
#' @inheritParams mediation_np
#'
#' @source section 12.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
mediation_NIE <- function(data, formula, exposure.name, mediator.name) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)

  outcome.name <- all.vars(formula[[2]])
  input.names <- all.vars(formula[[3]])

  # reduced formula, without mediator
  input.names.reduced <- input.names[input.names != mediator.name]
  formula_red <- as.formula(paste(outcome.name,
                                  paste(input.names.reduced, sep = "+"),
                                  sep = "~"))
  # mediator formula
  input.names.mediator <- input.names[input.names != mediator.name]
  formula_med <- as.formula(paste(mediator.name, exposure.name, sep = "~"))

  # the models
  the_models <- list(
    "full" = formula,
    "reduced" = formula_red,
    "mediator" = formula_med)

  # the coefficients
  the_coefs <- lapply(the_models, function(x) {
    lm(formula = x, data = data) |>
      coef()
  }) |>
    setNames(names(the_models))


  NIE_prod <- the_coefs$full[mediator.name] * the_coefs$mediator[exposure.name]

  NIE_diff <- the_coefs$reduced[exposure.name] - the_coefs$full[exposure.name]

  msg <- sprintf("NIE_prod of %f != NIE_diff of %f", NIE_prod, NIE_diff)
  assertthat::assert_that(dplyr::near(NIE_prod, NIE_diff), msg = msg)

  # output compatible with boostrap function
  data.frame(
    term = c("NIE_prod", "NIE_diff"),
    estimate = unname(c(NIE_prod, NIE_diff)),
    std.err = NA_real_
  )
}
