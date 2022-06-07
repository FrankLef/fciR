#' Audit the Formula Used for Causal Inference
#'
#' Audit the formula used for causal inference.
#'
#' The formula and the variables used must be validated to avoid many different
#' problems. All formula are assumed to have at least 1 outcome variable,
#' 1 exposure variable and 1 extra variable. The extra variables can be confounds,
#' instrumental variable, etc. depending on the context.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula with at least 1 exposure and 1 extra variable.
#' @param exposure.name The name of the exposure variable.
#' @param nvars The required number of extra variables. If \code{NA_integer_}
#' then the nb of variables must be >= 1. The default value is \code{NA_integer_}.
#'
#' @return List with outcome.name, exposure.name and extra.names of variables.
#' @export
audit_formula <- function(data, formula, exposure.name, nvars = NA_integer_) {
  stopifnot(is.na(nvars) | nvars >= 1)

  # the outcome variable must be in the data
  outcome.name <- all.vars(formula[[2]])
  stopifnot(length(outcome.name) == 1, outcome.name %in% names(data))

  # the independent variables must be in the data
  ind_vars <- all.vars(formula[[3]])
  stopifnot(all(ind_vars %in% names(data)))

  # the exposure variable must be in the formula
  stopifnot(length(exposure.name) == 1, exposure.name %in% ind_vars)

  # the nb extra independent variables is conditional on nvars
  extra.names <- ind_vars[ind_vars != exposure.name]
  if (is.na(nvars)) {
    # there must be at least one extra
    stopifnot(length(extra.names) >= 1)
  } else {
    # the required nb of extra variables must be met
    stopifnot(length(extra.names) == nvars)
  }


  list(outcome.name = outcome.name,
       exposure.name = exposure.name,
       extra.names = extra.names)
}
