#' Estimate and CI using the Front-Door Method
#'
#' Estimate and CI using the front-door method.
#'
#' Estimate and CI using the front-door method as described in section 8.3
#' of chater 8.
#'
#' @inheritParams est_frontdr_np
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @return Dataframe of estimate and CI using the front-door method.
#' @export
frontdr_np <- function(data, outcome, exposure, surrogate, R = 1000, conf = 0.95) {

  estimator <- function(data, ids) {
    dat <- data[ids, ]

    dat.est <- est_frontdr_np(dat, outcome, exposure, surrogate)
    EY0 <- dat.est["EY0"]
    EY1 <- dat.est["EY1"]

    # estimate the effect measures
    effect_measures(val0 = EY0, val1 = EY1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  effect_exp(data = out)
}

#' Estimate the effect using Front-Door Method
#'
#' Estimate the effect using Front-door method.
#'
#' Estimate the effect using Front-door method. See chap. 8, section 8.3
#' for details.
#'
#' @param data Dataframe of raw data.
#' @param outcome Name of outcome variable.
#' @param exposure Name of exposure variable.
#' @param surrogate Name of surrogate variable.
#'
#' @return Numeric vector.
#' @export
est_frontdr_np <- function(data, outcome, exposure, surrogate) {
  probA1 <- mean(data[, exposure])
  probA0 <- 1 - probA1

  # compute EY0, expected Y with outcome 0
  condA0 <- data[, exposure] == 0
  probS1_condA0 <- mean(data[condA0, surrogate])
  probS0_condA0 <- 1 - probS1_condA0

  condS0_condA0 <- data[, surrogate] == 0 & data[, exposure] == 0
  Y_condS0_condA0 <- data[condS0_condA0, outcome]
  condS0_condA1 <- data[, surrogate] == 0 & data[, exposure] == 1
  Y_condS0_condA1 <- data[condS0_condA1, outcome]

  exY_out0_condA0 <- probS0_condA0 *
    (mean(Y_condS0_condA0) * probA0 + mean(Y_condS0_condA1) * probA1)

  condS1_condA0 <- data[, surrogate] == 1 & data[, exposure] == 0
  Y_condS1_condA0 <- data[condS1_condA0, outcome]
  condS1_condA1 <- data[, surrogate] == 1 & data[, exposure] == 1
  Y_condS1_condA1 <- data[condS1_condA1, outcome]

  exY_out0_condA1 <- probS1_condA0 *
    (mean(Y_condS1_condA0) * probA0 + mean(Y_condS1_condA1) * probA1)

  exY_out0 <- exY_out0_condA0 + exY_out0_condA1

  # compute EY1, expected Y with outcome 1
  condA1 <- !condA0
  probS1_condA1 <- mean(data[condA1, surrogate])
  probS0_condA1 <- 1 - probS1_condA1

  exY_out1_condA0 <- probS0_condA1 *
    (mean(Y_condS0_condA0) * probA0 + mean(Y_condS0_condA1) * probA1)

  exY_out1_condA1 <- probS1_condA1 *
    (mean(Y_condS1_condA0) * probA0 + mean(Y_condS1_condA1) * probA1)

  exY_out1 <- exY_out1_condA0 + exY_out1_condA1

  c("EY0" = exY_out0, "EY1" = exY_out1)
}
