#' Compute standardized estimates with the 2-parts model
#'
#' Compute standardized estimates with the 2-parts model.
#'
#' This method is detailed in exercise 4 of chapter 6. It is useful when there
#' are many zeros.  Note that the results don't match exactly those obtained
#' with outcome-mode or exposure-model standardization.
#'
#' @param data Dataframe of raw data.
#' @param outcome.name Name of outcome variable.
#' @param exposure.name Name of exposure variable.
#' @param confound.names Name of confound variable.
#' @param condition.name Character vector of confound variable names.
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
backdr_twoparts <- function(data, outcome.name = "Y", exposure.name = "T",
                            confound.names = "H", condition.name = "Z") {

  input.names <- c(exposure.name, confound.names)

  a_formula <- formulaic::create.formula(outcome.name = outcome.name,
                                         input.names = input.names,
                                         dat = data)
  data.cond <- data[data[, condition.name] == 0, ]
  mod.log <- glm(formula = a_formula, family = "poisson", data = data.cond)

  # formula.cond <- paste(cond, paste(fvars$ind, collapse = "+"), sep = "~")
  # formula.cond <- formula(formula.cond)
  formula.cond <- formulaic::create.formula(outcome.name = condition.name,
                                            input.names = input.names,
                                            dat = data)
  mod.logit <- glm(formula = formula.cond, family = "binomial", data = data)

  # dataset with everyone untreated
  dat0 <- data
  dat0[, exposure.name] <- 0

  # dataset with everyone treated
  dat1 <- data
  dat1[, exposure.name] <- 1


  EYhat0 <- 1 - predict(mod.logit, newdata = dat0, type = "response")
  EYhat0 <- EYhat0 * predict(mod.log, newdata = dat0, type = "response")

  EYhat1 <- 1 - predict(mod.logit, newdata = dat1, type = "response")
  EYhat1 <- EYhat1 * predict(mod.log, newdata = dat1, type = "response")

  # estimate the average potential outcomes
  EY0 <- mean(EYhat0)
  EY1 <- mean(EYhat1)

  # estimate the effect measures
  out <- effect_measures(EY0, EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}
