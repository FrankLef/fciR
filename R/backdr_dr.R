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
#' @importFrom stats glm fitted predict
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
backdr_dr <- function(data, formula = Y ~ T + A + H, exposure.name = "T",
                      family = c("binomial", "poisson", "gaussian")) {

  # the model's family
  family <- match.arg(family)

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name)
  outcome.name <- var_names$outcome.name
  confound.names <- var_names$extra.names

  # exposure model formula
  eformula <- paste(exposure.name, paste(confound.names, collapse = "+"),
                    sep = "~")
  eformula <- formula(eformula)

  # estimate the parametric exposure model
  eH <- fitted(glm(formula = eformula, family = "binomial", data = data))
  stopifnot(all(!dplyr::near(eH, 0)))  # eH must not equal zero

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
