#' Compute Standardized Estimates With Parametric Exposure Model
#'
#' Compute standardized estimates with parametric exposure model.
#'
#' The standardized estimates are computed using the exposure model.
#' This method requires 2 different formulas which are created from the
#' arguments \code{formula}. The 2 formulas created are for the exposure model
#' and another one for the weighted linear model. Also \code{T}, i.e. the
#' exposure, must always be binary, if not it can be made binary
#' "one can first recode the data so that T = 1 when it is previously equaled,
#' and T = 0 when it previously equaled any value other than t", p. 113.
#'
#' @inheritParams backdr_out_np
#'
#' @source Section 6.2.2
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
backdr_exp <- function(data, formula, exposure.name, confound.names,
                       att = FALSE) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)
  checkmate::assertFlag(att)

  x0 <- "(Intercept)"  # name of intercept used by lm, glm, etc.

  var_names <- audit_formula(data, formula, exposure.name, confound.names)
  outcome.name <- var_names$outcome.name

  # exposure model formula
  eformula <- paste(exposure.name, paste(confound.names, collapse = "+"),
                    sep = "~")
  eformula <- formula(eformula)

  # weighted linear model formula
  lformula <- paste(outcome.name, exposure.name, sep = "~")
  lformula <- formula(lformula)

  # estimate the parametric exposure model
  # NOTE: fitted() is the same as using predict(..., type = "response")
  #       BUT fitted only use the ORIGINAL data, there is no newdata.
  eH <- fitted(glm(formula = eformula, family = "binomial", data = data))
  assertthat::assert_that(all(!dplyr::near(eH, 0)),
                          msg = "eH must not equal zero")

  # compute the E(T) when ATT is required
  e0 <- NA_real_
  if (att) {
    e0 <- sum(data[, exposure.name] == 1) / nrow(data)
  }


  # compute the weights
  datT <- data[, exposure.name]
  if (!att) {
    data$W <- datT * (1 / eH) + (1 - datT) * (1 / (1 - eH))
  } else {
    data$W <- datT * (1 / eH) + (1 - datT) * (eH / (e0 * (1 - eH)))
    condT1 <- data[, exposure.name] == 1
    EYT1 <- mean(data[condT1, outcome.name])
  }

  # fit the weighted linear model
  coefs <- coef(glm(formula = lformula, data = data, weights = W))

  # estimate the expected potential outcome
  EY0 <- coefs[x0]
  EY1 <- sum(coefs)
  if (att) EY1 <- EYT1  # use att values if required

  # estimate the effect measures
  out <- effect_measures(EY0, EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname backdr_exp
#' @export
standexp.r <- backdr_exp
