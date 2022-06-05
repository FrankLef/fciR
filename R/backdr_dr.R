#' Compute the doubly robust standardized estimates
#'
#' Compute the doubly robust standardized estimates.
#'
#' Compute the doubly robust standardized estimates using the code from section
#' 6.3.
#'
#' @inheritParams backdr_out_np
#' @param family family used for lm fit.
#'
#' @importFrom formulaic create.formula
#' @importFrom stats glm fitted predict
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
backdr_dr <- function(data, outcome.name = "Y", exposure.name = "T",
                      confound.names = c("A", "H"),
                      family = c("binomial", "poisson", "gaussian")) {
  family <- match.arg(family)

    # exposure model formula
  eformula <- formulaic::create.formula(outcome.name = exposure.name,
                                        input.names = confound.names,
                                        dat = data)

  # estimate the parametric exposure model
  eH <- fitted(glm(formula = eformula, family = "binomial", data = data))
  stopifnot(all(!dplyr::near(eH, 0)))  # eH must not equal zero

  input.names <- c(exposure.name, confound.names)
  a_formula <- formulaic::create.formula(outcome.name = outcome.name,
                                         input.names = input.names,
                                         dat = data)

  # Fit the parametric outcome model
  lmod <- glm(formula = a_formula, family = family, data = data)

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
