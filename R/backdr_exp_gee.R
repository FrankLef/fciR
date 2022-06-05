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
#' @importFrom formulaic create.formula
#' @importFrom stats formula lm glm fitted predict
#' @importFrom geepack geeglm
#'
#' @seealso backdr_exp
#'
#' @return Dataframe of estimates
#' @export
backdr_exp_gee <- function(data, outcome.name = "Y", exposure.name = "T",
                           confound.names = "H") {
  x0 <- "(Intercept)"  # name of intercept used by geeglm

  # exposure model formula
  eformula <- formulaic::create.formula(outcome.name = exposure.name,
                                        input.names = confound.names,
                                        dat = data)

  # IMPORTANT: geepack::geeglm returns an error when using a formula
  #            created with formulaic::create.formula! Do it with paste().
  # weighted linear model formula
  lformula <- formula(paste(outcome.name, exposure.name, sep = "~"))

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
