#' Compute the DiD Estimator with a Loglinear Model
#'
#' Compute the DiD Estimator with a loglinear model.
#'
#' the DiD Estimator using the scripts from section 7.2, p. 141-142.
#'
#' @inheritParams did_linear
#'
#' @seealso did_longer
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
did_loglinear <- function(data, formula = Y1 ~ `T` + Y0, exposure.name = "T",
                          names_to = "var", timevar = "time") {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)
  checkmate::assertNames(exposure.name, subset.of = names(data))

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name)
  outcome.name <- var_names$outcome.name
  confound.names <- var_names$extra.names

  # convert data to long format
  outcomes <- c(confound.names, outcome.name)
  data <- did_longer(data, outcomes = outcomes, outcome = outcome.name,
                     names_to = names_to, timevar = timevar)

  # fit the did model
  dformula <- paste0(outcome.name, "~", exposure.name, "*", timevar)
  mod.out <- glm(formula = dformula, family = "poisson", data = data)
  coefs <- coef(mod.out)

  # extract the log relative risk
  logrr <- coefs[length(coefs)]

  # estimate E(Y(1)|A=1)
  sel <- data[, timevar] == 1 & data[, exposure.name] == 1
  EY1 <- mean(data[sel, outcome.name, drop = TRUE])

  # estimate E(Y(0)|A=1)
  EY0 <- EY1 / exp(logrr)

  EY0A1 <- EY0  # change to explicit name
  # estimate the effect measures
  out <- effect_measures(val0 = EY0A1, val1 = EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname did_loglinear
#' @export
didloglinear <- did_loglinear
