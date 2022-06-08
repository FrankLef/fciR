#' Compute estimates of the association measures for 2 strata
#'
#' Compute estimates of the association measures for 2 strata.
#'
#' The formula must always be in form \code{Y ~ `T` + M}, that is with only
#' 2 predictors: \code{T} as the treatment variable and \code{M} as the modifier
#' variable. The exposure variable is identified by the argument
#' \code{exposure.name}, the other variable is assumed to be the modifier
#' variable.
#'
#' @inheritParams meas_effect_cond
#'
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
meas_effect_modif <- function(data, formula = Y ~ `T` + M, exposure.name = "T") {

  var_names <- audit_formula(data, formula, exposure.name, 1)
  outcome.name <- var_names$outcome.name
  modifier.name <- var_names$extra.names


  # estimate the expected potential outcomes
  Y_condT0_condM0 <- data[, exposure.name] == 0 & data[, modifier.name] == 0
  EYT0.M0 <- mean(data[Y_condT0_condM0, outcome.name])
  Y_condT0_condM1 <- data[, exposure.name] == 0 & data[, modifier.name] == 1
  EYT0.M1 <- mean(data[Y_condT0_condM1, outcome.name])
  Y_condT1_condM0 <- data[, exposure.name] == 1 & data[, modifier.name] == 0
  EYT1.M0 <- mean(data[Y_condT1_condM0, outcome.name])
  Y_condT1_condM1 <- data[, exposure.name] == 1 & data[, modifier.name] == 1
  EYT1.M1 <- mean(data[Y_condT1_condM1, outcome.name])

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

  out <- c("EYT0.M0" = EYT0.M0, "EYT0.M1" = EYT0.M1, "EYT1.M0" = EYT1.M0,
           "EYT1.M1" = EYT1.M1, "RD.M0" = RD.M0, "RD.M1" = RD.M1,
           "logRR.M0" = logRR.M0, "logRR.M1" = logRR.M1, "logRR*.M0" = logRRstar.M0,
           "logRR*.M1" = logRRstar.M1, "logOR.M0" = logOR.M0, "logOR.M1" = logOR.M1,
           "EYT0.diff" = EYT0.diff, "EYT1.diff" = EYT1.diff,
           "RD.diff" = RD.diff, "logRR.diff" = logRR.diff, "logRR*.diff" = logRRstar.diff,
           "logOR.diff" = logOR.diff)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname meas_effect_modif
#' @export
boot <- meas_effect_modif
