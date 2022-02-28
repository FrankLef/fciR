#' Compute the DiD Estimator with a Loglinear Model
#'
#' Compute the DiD Estimator with a loglinear model.
#'
#' the DiD Estimator using the scripts from section 7.2, p. 141-142.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ A} where \code{A}
#' is the exposure.
#' @param varsY Names of the outcome variables.
#' @param R Number of bootstrap replicates. Default is 1000.
#' @param conf Confidence interval. Default is 0.95.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#'
#' @return Estimates using the difference in differences.
#' @export
did_loglinear <- function(data, formula = Y ~ A, varsY = c("VL0", "VL1"),
                         R = 1000, conf = 0.95) {

  # extract the variables names from the formula
  fvars <- formula2vars(formula)

  estimator <- function(data, ids, varsY) {
    nmvar <- "var"  # name of variable used to hold names
    outvar <- "outcome"  # name of variable used for outcome
    timevar <- "time"  # name of variable used for time

    dat <- data[ids, ]
    # make long format
    dat <- pivot_longer(dat, cols = all_of(varsY), names_to = nmvar, values_to = outvar)
    dat <- as.data.frame(dat)
    # create time variable
    dat[, timevar] <- ifelse(grepl(pattern = "0", x = dat$var), 0, 1)

    # fit the did model
    dformula <- paste0(outvar, "~", fvars$t, "*", timevar)
    mod.out <- glm(formula = dformula, family = "poisson", data = dat)
    coefs <- coef(mod.out)

    # extract the log relative risk
    logrr <- coefs[length(coefs)]

    # estimate E(Y(1)|A=1)
    sel <- dat[, timevar] == 1 & dat[, fvars$t] == 1
    EY1 <- mean(dat[sel, outvar, drop = TRUE])

    # estimate E(Y(0)|A=1)
    EY0 <- EY1 / exp(logrr)

    EY0A1 <- EY0  # change to explicit name
    # estimate the effect measures
    effect_measures(val0 = EY0A1, val1 = EY1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf,
                  varsY = varsY)

  # exponentiate the log values
  effect_exp(data = out)
}
