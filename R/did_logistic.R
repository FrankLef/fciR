#' Compute the DiD Estimator with a Lofistic Model
#'
#' Compute the DiD Estimator with a logistic model.
#'
#' the DiD Estimator using the scripts from section 7.2, p. 141-142.
#'
#' @inheritParams did_linear
#'
#' @seealso did_longer
#'
#' @source Section 7.1.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
did_logistic <- function(data, formula, exposure.name, confound.names,
                         names_to = "var", timevar = "time") {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name, confound.names)
  outcome.name <- var_names$outcome.name

  # convert data to long format
  outcomes <- c(confound.names, outcome.name)
  data <- did_longer(data, outcomes = outcomes, outcome = outcome.name,
                     names_to = names_to, timevar = timevar)

  # fit the did model
  dformula <- paste0(outcome.name, "~", exposure.name, "*", timevar)
  mod.out <- glm(formula = dformula, family = "binomial", data = data)
  coefs <- coef(mod.out)

  # extract the log relative risk
  logor <- coefs[length(coefs)]

  # estimate E(Y(1)|A=1)
  sel <- data[, timevar] == 1 & data[, exposure.name] == 1
  EY1 <- mean(data[sel, outcome.name, drop = TRUE])

  # estimate E(Y(0)|A=1)
  tmp <- log(EY1 / (1 - EY1)) - logor
  EY0 <- exp(tmp) / (1 + exp(tmp))

  EY0A1 <- EY0  # change to explicit name
  # estimate the effect measures
  out <- effect_measures(val0 = EY0A1, val1 = EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname did_logistic
#' @export
didlogistic <- did_logistic
