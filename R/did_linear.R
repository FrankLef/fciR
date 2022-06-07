#' Compute the DiD Estimator with a Linear Model
#'
#' Compute the DiD Estimator with a linear model.
#'
#' The DiD Estimator using the scripts from section 7.2, p. 141-142.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula representing the model.
#' @param exposure.name Name of exposure variable. All other independent
#' variables in the formula will be assumed to be confounds.
#' @param names_to Name that will be used for the name variable.
#' @param timevar Name that will be used for the time variable.
#'
#' @seealso did_longer
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
did_linear <- function(data, formula = Y1 ~ `T` + Y0, exposure.name = "T",
                       names_to = "var", timevar = "time") {

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
  mod.out <- glm(formula = dformula, family = "gaussian", data = data)
  coefs <- coef(mod.out)

  # extract the risk difference from the interaction coefficient
  rd <- coefs[length(coefs)]


  # estimate E(Y(1)|A=1)
  sel <- data[, timevar] == 1 & data[, exposure.name] == 1
  EY1 <- mean(data[sel, outcome.name, drop = TRUE])

  # estimate E(Y(0)|A=1)
  EY0 <- EY1 - rd

  EY0A1 <- EY0  # change to explicit name
  # estimate the effect measures
  out <- effect_measures(val0 = EY0A1, val1 = EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname did_linear
#' @export
didlinear <- did_linear
