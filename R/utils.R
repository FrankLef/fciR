#' Audit the Formula Used for Causal Inference
#'
#' Audit the formula used for causal inference.
#'
#' The formula and the variables used must be validated to avoid problems.
#' All formula must have at least 1 outcome variable,
#' 1 exposure variable and 1 extra variable. The extra variables can be confounds,
#' instrumental variable, etc. depending on the context.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula with at least 1 exposure and 1 extra variable.
#' @param exposure.name The name of the exposure variable.
#' @param extra.names Character vector with the names of the extra variable.
#'
#' @return List with outcome.name, exposure.name and extra.names of variables.
#' @export
#' @examples
#' out <- audit_formula(
#'  iris, formula = Sepal.Length ~ Species * Sepal.Width,
#'  exposure.name = "Species", extra.names = "Sepal.Width")
#' # return a list with all 3 variable names
#' stopifnot(length(out) == 3)
audit_formula <- function(data, formula, exposure.name, extra.names) {
  checkmate::assert_names(exposure.name, subset.of = names(data))
  checkmate::assert_names(extra.names, subset.of = names(data))

  # the outcome variable must be in the data
  outcome.name <- all.vars(formula[[2]])
  assertthat::assert_that(length(outcome.name) == 1)
  checkmate::assert_names(outcome.name, subset.of = names(data))

  # the outcome, exposure and extra variables must be mutually exclusive
  checkmate::assert_names(outcome.name,
                          disjunct.from = c(exposure.name, extra.names))
  checkmate::assert_names(exposure.name,
                          disjunct.from = c(outcome.name, extra.names))
  checkmate::assert_names(extra.names,
                          disjunct.from = c(outcome.name, exposure.name))


  # the independent variables must be in the given exposure.name or extra.names
  ind_vars <- all.vars(formula[[3]])
  checkmate::assert_names(ind_vars,
                          permutation.of = c(exposure.name, extra.names))


  list(outcome.name = outcome.name,
       exposure.name = exposure.name,
       extra.names = extra.names)
}
