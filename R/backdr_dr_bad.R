#' Doubly robust standardized estimates with misspecified outcome model
#'
#' Doubly robust standardized estimates with misspecified outcome model.
#'
#' Compute the doubly robust standardized estimates with erros in the outcome
#' model.
#'
#' @inheritParams backdr_out_np
#'
#' @source section 6.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
backdr_dr_bad <- function(data, formula, exposure.name, confound.names) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)

  # audit the variables
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
  eH <- fitted(glm(formula = eformula, family = "binomial", data = data))
  assertthat::assert_that(all(!dplyr::near(eH, 0)),
                          msg = "eH must not equal zero")

  # fit a nonparametric outcome model that we do not believe
  # i.e. a bad outcome model
  lmod <- glm(formula = lformula, family = "binomial", data = data)

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


#' @rdname backdr_dr_bad
#' @export
badstanddr <- backdr_dr_bad
