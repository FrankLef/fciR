#' Compute estimates of the association measures for 2 strata
#'
#' Compute estimates of the association measures for 2 strata.
#'
#' IMPORTANT:
#'  The formula must always be in form Y ~ `T` + M, that is with only
#'  2 predictors: T as the treatment variable and M as the modifier
#'  variable
#'
#' @param data Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ `T` + M}
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval
#'
#' @return Dataframe of summarized results
meas_effect_modif <- function(data, formula = Y ~ `T` + M, R = 1000, conf = 0.95) {

  # extract the variables names from the formula
  fvars <- formula2vars(formula)
  # only one H (the modifier)
  stopifnot(length(fvars$h) == 1)

  estimator <- function(data, ids) {
    dat <- data[ids, ]
    # estimate the expected potential outcomes
    EYT0.M0 <- mean(dat[dat[, fvars$t] == 0 & dat[, fvars$h] == 0, fvars$y])
    EYT0.M1 <- mean(dat[dat[, fvars$t] == 0 & dat[, fvars$h] == 1, fvars$y])
    EYT1.M0 <- mean(dat[dat[, fvars$t] == 1 & dat[, fvars$h] == 0, fvars$y])
    EYT1.M1 <- mean(dat[dat[, fvars$t] == 1 & dat[, fvars$h] == 1, fvars$y])
    # estimate the effect measures
    RD.M0 <- EYT1.M0 - EYT0.M0
    RD.M1 <- EYT1.M1 - EYT0.M1
    logRR.M0 <- log(EYT1.M0 / EYT0.M0)
    logRR.M1 <- log(EYT1.M1 / EYT0.M1)
    logRRstar.M0 <- log((1 - EYT0.M0) / (1 - EYT1.M0))
    logRRstar.M1 <- log((1 - EYT0.M1) / (1 - EYT1.M1))
    logOR.M0 <- logRR.M0 + logRRstar.M0
    logOR.M1 <- logRR.M1 + logRRstar.M1
    # the effect measure difference
    EYT0.diff <- EYT0.M1 - EYT0.M0
    EYT1.diff <- EYT1.M1 - EYT1.M0
    RD.diff <- RD.M1 - RD.M0
    logRR.diff <- logRR.M1 - logRR.M0
    logRRstar.diff <- logRRstar.M1 - logRRstar.M0
    logOR.diff <- logOR.M1 - logOR.M0

    out <- c(EYT0.M0, EYT0.M1, EYT1.M0, EYT1.M1, RD.M0, RD.M1,
             logRR.M0, logRR.M1, logRRstar.M0, logRRstar.M1,
             logOR.M0, logOR.M1, EYT0.diff, EYT1.diff,
             RD.diff, logRR.diff, logRRstar.diff, logOR.diff)
    names(out) <- c("EYT0.M0", "EYT0.M1", "EYT1.M0", "EYT1.M1", "RD.M0", "RD.M1",
                    "logRR.M0", "logRR.M1", "logRR*.M0", "logRR*.M1",
                    "logOR.M0", "logOR.M1", "EYT0.diff", "EYT1.diff",
                    "RD.diff", "logRR.diff", "logRR*.diff", "logOR.diff")
    out
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # vector of variables to exponentiate
  vars <- c("RR.M0" = "logRR.M0", "RR.M1" = "logRR.M1", "RR.diff"  = "logRR.diff",
            "RR*.M0"  = "logRR*.M0", "RR*.M1"  = "logRR*.M1", "RR*.diff" = "logRR*.diff",
            "OR.M0" = "logOR.M0", "OR.M1" = "logOR.M1", "OR.diff" = "logOR.diff")
  out <- effect_exp(data = out, vars = vars)

  # split to estimator and stratum
  out %>%
    separate(col = "name", into = c("estimator", "group"), sep = "[.]")
}

#' @rdname meas_effect_modif
#' @export
boot <- meas_effect_modif
