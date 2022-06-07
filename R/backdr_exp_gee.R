#' Compute standardized estimates with parametric exposure model
#'
#' Compute standardized estimates with parametric exposure model.
#'
#' The standardized estimates are computed using the exposure model and the
#' \code{geeglm} from the \code{gee} package.
#' This method requires 2 different formulas which are created from the
#' arguments \code{formula}. The 2 formulas created are for the exposure model
#' and another one for the weighted linear model.
#'
#' @inheritParams backdr_out_np
#'
#' @importFrom stats formula lm glm fitted predict
#' @importFrom geepack geeglm
#'
#' @seealso backdr_exp
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
backdr_exp_gee <- function(data, formula = Y ~ `T` + H, exposure.name = "T") {

  x0 <- "(Intercept)"  # name of intercept used by geeglm

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name)
  outcome.name <- var_names$outcome.name
  confound.names <- var_names$extra.names

  # exposure model formula
  eformula <- paste(exposure.name, paste(confound.names, collapse = "+"),
                    sep = "~")
  eformula <- formula(eformula)

  # weighted linear model formula
  lformula <- paste(outcome.name, exposure.name, sep = "~")
  lformula <- formula(lformula)

  data$id <- seq_len(nrow(data))  # id column used by geeglm

  # estimate the parametric exposure model
  eH <- fitted(glm(formula = eformula, family = "binomial", data = data))
  stopifnot(all(!dplyr::near(eH, 0)))  # eH must not equal zero

  # compute the weights
  datT <- data[, exposure.name]
  data$W <- (1 / eH) * datT + (1 / (1 - eH)) * (1 - datT)

  # fit the weighted linear model
  coefs <- coef(geepack::geeglm(formula = lformula, data = data,
                                id = id, weights = W))

  # estimate the expected potential outcome
  EY0 <- coefs[x0]
  EY1 <- sum(coefs)

  # estimate the effect measures
  out <- effect_measures(EY0, EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname backdr_exp_gee
#' @export
standexpgee <- backdr_exp_gee
