#' Compute the doubly robust standardized estimates
#'
#' Compute the doubly robust standardized estimates.
#'
#' Compute the doubly robust standardized estimates using the code from section
#' 6.3.
#'
#' @inheritParams backdr_out_np
#' @param family Name of the model's family. Must be one of
#' \code{c("binomial", "poisson", "gaussian")}. default is \code{"binomial"}.
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @source Section 6.3.
#' @export
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
backdr_dr <- function(data, formula, exposure.name, confound.names,
                      family = c("binomial", "poisson", "gaussian")) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)

  # the model's family
  family <- match.arg(family)

  # audit the variables
  var_names <- audit_formula(data, formula, exposure.name, confound.names)
  outcome.name <- var_names$outcome.name

  # exposure model formula
  eformula <- paste(exposure.name, paste(confound.names, collapse = "+"),
                    sep = "~")
  eformula <- formula(eformula)

  # estimate the parametric exposure model
  eH <- fitted(glm(formula = eformula, family = "binomial", data = data))
  assertthat::assert_that(all(!dplyr::near(eH, 0)),
                          msg = "eH must not equal zero")

  # Fit the parametric outcome model
  lmod <- glm(formula = formula, family = family, data = data)

  # predict potential outcome for each participant
  dat0 <- data
  dat0[, exposure.name] <- 0
  EYhat0 <- predict(lmod, newdata = dat0, type = "response")
  dat1 <- data
  dat1[, exposure.name] <- 1
  EYhat1 <- predict(lmod, newdata = dat1, type = "response")

  # Use the DR estimating equation to estimate the expected
  # potential outcome
  datY <- data[, outcome.name]
  datT <- data[, exposure.name]
  EY0 <- mean(datY * (1 - datT) / (1 - eH) + EYhat0 * (eH - datT) / (1 - eH))
  EY1 <- mean(datY * (datT / eH) - EYhat1 * (datT - eH) / eH)

  # estimate the effect measures
  out <- effect_measures(EY0, EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname backdr_dr
#' @export
standdr <- backdr_dr
