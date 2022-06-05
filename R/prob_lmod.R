
#' Estimate s sampling distribution by Bootstrapping
#'
#' Estimate s sampling distribution by bootstrapping.
#'
#  Estimate a sampling distribution. See details in chapter 2.
#'
#' @param data Dataframe of raw data.
#' @param formula The model formula.
#' @param condition.names Character vector of conditioned variable names. If
#' \code{NULL} all independent variables will be used.
#'
#' @importFrom rlang f_rhs .data
#' @importFrom stats glm coef plogis
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
prob_lmod <- function(data, formula = Y ~ `T` + A + H,
                      condition.names = NULL) {
  # independent variables from the formula
  ind_vars <- all.vars(rlang::f_rhs(formula))
  # if condition.names is NULL then use all independent variables
  # which is the same as saying there is no condition
  if (is.null(condition.names)) condition.names <- ind_vars
  stopifnot(all(condition.names %in% ind_vars))
  # name of intercept used by lm, glm, etc.
  x0 <- "(Intercept)"

  coefs <- coef(glm(formula = formula, family = "binomial", data = data))
  coefs <- coefs[c(x0, condition.names)]
  p <- unname(sum(coefs))
  data.frame(
    term = "logitP",
    estimate = p,
    std.err = NA_real_
  )
}


#' @rdname prob_lmod
#' @export
lmodboot <- prob_lmod


#' Estimate s sampling distribution by Bootstrapping
#'
#' Estimate s sampling distribution by bootstrapping.
#'
#'  Estimate a sampling distribution. See details in chapter 2, section 2.4.
#'  The package \code{rsample} is used.
#'
#' @param data Dataframe of raw data.
#' @param formula The model formula.
#' @param condition.names Character vector of conditioned variable names. If
#' \code{NULL} all independent variables will be used.
#'
#' @importFrom rlang f_rhs .data
#' @importFrom stats glm plogis
#' @importFrom broom tidy
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
prob_lmod_td <- function(data, formula = Y ~ `T` + A + H,
                         condition.names = NULL) {
  # independent variables from the formula
  ind_vars <- all.vars(rlang::f_rhs(formula))
  # if condition.names is NULL then use all independent variables
  # which is the same as saying there is no condition
  if (is.null(condition.names)) condition.names <- ind_vars
  stopifnot(all(condition.names %in% ind_vars))
  # add intercept to conditions
  x0 <- "(Intercept)"  # name of intercept used by lm, glm, etc.
  condition.names <- c(x0, condition.names)

  fit <- glm(formula = formula, family = "binomial", data = data) |>
    broom::tidy()

  fit |>
    filter(.data$term %in% condition.names) |>
    summarize(term = "logitP",
              estimate = sum(.data$estimate),
              # don't know the std.err so no t-intervals
              std.err = NA_real_)
}
