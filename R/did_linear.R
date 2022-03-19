#' Compute the DiD Estimator with a Linear Model
#'
#' Compute the DiD Estimator with a linear model.
#'
#' The DiD Estimator using the scripts from section 7.2, p. 141-142.
#'
#' @param data Dataframe of raw data.
#' @param outcomes Character vector of base and final variables.
#' @param outcome Name that will be used for the single outcome variable.
#' @param treatment Name of the treatment variable.
#' @param names_to Name that will be used for the name variable.
#' @param timevar Name that will be used for the time variable.
#' @param R Number of bootstrap replicates. Default is 1000.
#' @param conf Confidence interval. Default is 0.95.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#'
#' @seealso did_longer
#'
#' @return Estimates using the difference in differences.
#' @export
did_linear <- function(data, outcomes = c("Y0", "Y1"), outcome = "Y",
                       treatment = "T", names_to = "var", timevar = "time",
                       R = 1000, conf = 0.95) {

  estimator <- function(data, ids) {

    dat <- data[ids, ]

    # convert data to long format
    dat <- did_longer(dat, outcomes = outcomes, outcome = outcome,
                      names_to = names_to, timevar = timevar)

    # fit the did model
    dformula <- paste0(outcome, "~", treatment, "*", timevar)
    mod.out <- glm(formula = dformula, family = "gaussian", data = dat)
    coefs <- coef(mod.out)

    # extract the risk difference from the interaction coefficient
    rd <- coefs[length(coefs)]


    # estimate E(Y(1)|A=1)
    sel <- dat[, timevar] == 1 & dat[, treatment] == 1
    EY1 <- mean(dat[sel, outcome, drop = TRUE])

    # estimate E(Y(0)|A=1)
    EY0 <- EY1 - rd

    EY0A1 <- EY0  # change to explicit name
    # estimate the effect measures
    effect_measures(val0 = EY0A1, val1 = EY1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  effect_exp(data = out)
}
