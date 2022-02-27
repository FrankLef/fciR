#' Compute standardized estimates with the 2-parts model
#'
#' Compute standardized estimates with the 2-parts model.
#'
#' This method is detailed in exercise 4 of chapter 6. It is useful when there
#' are many zeros.  Note that the results don't match exactly those obtained
#' with outcome-mode or exposure-model standardization.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + ...} T must represent the
#' exposure variable.
#' @param cond String. Name of the binary variable used by the 2-parts method
#' to differentiate the 2 parts of the model.
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @return Estimate using 2-parts standardization
#' @export
backdr_twoparts <- function(data, formula = Y ~ `T` + H, cond = "Z",
                            R = 1000, conf = 0.95) {
  stopifnot(is.character(cond), nchar(cond) != 0)

  # extract the variables names from the formula
  fvars <- formula2vars(formula)
  # the condition variable must not be in the formula
  stopifnot(!(cond %in% fvars))
  # the condition variable must be binary
  stopifnot(all(unique(data[, cond]) %in% 0:1))

  estimator <- function(data = data, cond = cond, ids) {
    dat <- data[ids, ]
    dat.cond <- dat[dat[, cond] == 0, ]

    mod.log <- glm(formula = formula, family = "poisson", data = dat.cond)
    formula.cond <- paste(cond, paste(fvars$ind, collapse = "+"), sep = "~")
    formula.cond <- formula(formula.cond)
    mod.logit <- glm(formula = formula.cond, family = "binomial", data = dat)

    # dataset with everyone untreated
    dat0 <- dat
    dat0[, fvars$t] <- 0

    # dataset with everyone treated
    dat1 <- dat
    dat1[, fvars$t] <- 1


    EYhat0 <- 1 - predict(mod.logit, newdata = dat0, type = "response")
    EYhat0 <- EYhat0 * predict(mod.log, newdata = dat0, type = "response")

    EYhat1 <- 1 - predict(mod.logit, newdata = dat1, type = "response")
    EYhat1 <- EYhat1 * predict(mod.log, newdata = dat1, type = "response")

    # estimate the average potential outcomes
    EY0 <- mean(EYhat0)
    EY1 <- mean(EYhat1)

    # estimate the effect measures
    effect_measures(val0 = EY0, val1 = EY1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf,
                  cond = cond)

  # exponentiate the log values
  effect_exp(data = out)
}
