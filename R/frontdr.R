#' Estimate the effect using Front-Door Method
#'
#' Estimate the effect using Front-door method.
#'
#' Estimate the effect using Front-door method. See chap. 8, section 8.3
#' for details.
#'
#' @param data Dataframe of raw data.
#' @param outcome.name Name of outcome variable.
#' @param exposure.name Name of exposure variable.
#' @param confound.names Name of surrogate variable.
#'
#' @return Numeric vector.
#' @export
frontdr_np <- function(data, outcome.name = "Y", exposure.name = "A",
                       confound.names = "S") {
  stopifnot(length(confound.names) == 1)

  probA1 <- mean(data[, exposure.name])
  probA0 <- 1 - probA1

  # compute EY0, expected Y with outcome 0
  condA0 <- data[, exposure.name] == 0
  probS1_condA0 <- mean(data[condA0, confound.names])
  probS0_condA0 <- 1 - probS1_condA0

  condS0_condA0 <- data[, confound.names] == 0 & data[, exposure.name] == 0
  Y_condS0_condA0 <- data[condS0_condA0, outcome.name]
  condS0_condA1 <- data[, confound.names] == 0 & data[, exposure.name] == 1
  Y_condS0_condA1 <- data[condS0_condA1, outcome.name]

  exY_out0_condA0 <- probS0_condA0 *
    (mean(Y_condS0_condA0) * probA0 + mean(Y_condS0_condA1) * probA1)

  condS1_condA0 <- data[, confound.names] == 1 & data[, exposure.name] == 0
  Y_condS1_condA0 <- data[condS1_condA0, outcome.name]
  condS1_condA1 <- data[, confound.names] == 1 & data[, exposure.name] == 1
  Y_condS1_condA1 <- data[condS1_condA1, outcome.name]

  exY_out0_condA1 <- probS1_condA0 *
    (mean(Y_condS1_condA0) * probA0 + mean(Y_condS1_condA1) * probA1)

  exY_out0 <- exY_out0_condA0 + exY_out0_condA1

  # compute EY1, expected Y with outcome 1
  condA1 <- !condA0
  probS1_condA1 <- mean(data[condA1, confound.names])
  probS0_condA1 <- 1 - probS1_condA1

  exY_out1_condA0 <- probS0_condA1 *
    (mean(Y_condS0_condA0) * probA0 + mean(Y_condS0_condA1) * probA1)

  exY_out1_condA1 <- probS1_condA1 *
    (mean(Y_condS1_condA0) * probA0 + mean(Y_condS1_condA1) * probA1)

  exY_out1 <- exY_out1_condA0 + exY_out1_condA1

  c("EY0" = exY_out0, "EY1" = exY_out1)
}
