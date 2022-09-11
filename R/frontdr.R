#' Estimate the Effect Using the Front-Door Method
#'
#' Estimate the effect using Front-door method.
#'
#' Estimate the effect using Front-door method. The exposure variable is used
#' as a confounder in the front-door method.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula representing the model.
#' @param exposure.name Name of exposure variable.
#' @param surrogate.name Name of the surrogate variable.
#'
#' @source Section 8.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
frontdr_np <- function(data, formula, exposure.name, surrogate.name) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name, surrogate.name)
  outcome.name <- var_names$outcome.name

  # P(A=1)
  probA1 <- mean(data[, exposure.name])
  # P(A=0)
  probA0 <- 1 - probA1


# estimate E(Y(0)) --------------------------------------------------------

  # The computations are:
  # E(Y(0)) =
  #   P(S=0|A=0)[E(Y|S=0,A=0)P(A=0) + E(Y|S=0,A=1)P(A=1)] +
  #   P(S=1|A=0)[E(Y|S=1,A=0)P(A=0) + E(Y|S=1,A=1)P(A=1)]


  # P(S=1|A=0)
  condA0 <- data[, exposure.name] == 0
  probS1_condA0 <- mean(data[condA0, surrogate.name])
  # P(S=0|A=0)
  probS0_condA0 <- 1 - probS1_condA0

  # E(Y|S=0,A=0)
  condS0_condA0 <- data[, surrogate.name] == 0 & data[, exposure.name] == 0
  Y_condS0_condA0 <- data[condS0_condA0, outcome.name]
  exY_condS0_condA0 <- mean(Y_condS0_condA0)

  # E(Y|S=0,A=1)
  condS0_condA1 <- data[, surrogate.name] == 0 & data[, exposure.name] == 1
  Y_condS0_condA1 <- data[condS0_condA1, outcome.name]
  exY_condS0_condA1 <- mean(Y_condS0_condA1)

  # the first component of E(Y(0))
  # P(S=0|A=0) * [E(Y|S=0,A=0)P(A=0) + E(Y|S=0,A=1)P(A=1)]
  exY0_probS0_condA0 <- probS0_condA0 *
    (exY_condS0_condA0 * probA0 + exY_condS0_condA1 * probA1)


  # E(Y|S=1,A=0)
  condS1_condA0 <- data[, surrogate.name] == 1 & data[, exposure.name] == 0
  Y_condS1_condA0 <- data[condS1_condA0, outcome.name]
  exY_condS1_condA0 <- mean(Y_condS1_condA0)

  # E(Y|S=1,A=1)
  condS1_condA1 <- data[, surrogate.name] == 1 & data[, exposure.name] == 1
  Y_condS1_condA1 <- data[condS1_condA1, outcome.name]
  exY_condS1_condA1 <- mean(Y_condS1_condA1)


  # the second component of E(Y(0))
  # P(S=1|A=0) * [E(Y|S=1,A=0)P(A=0) + E(Y|S=1,A=1)P(A=1)]
  exY0_probS1_condA0 <- probS1_condA0 *
    (exY_condS1_condA0 * probA0 + exY_condS1_condA1 * probA1)


  # E(Y(0)) = P(S=0|A=0)[E(Y|S=0 A=0)P(A=0) + E(Y|S=0,A=1)P(A=1)] +
  # P(S=1|A=0)[E(Y|S=0 A=0)P(A=0) + E(Y|S=1,A=1)P(A=1)]
  EY0 <- exY0_probS0_condA0 + exY0_probS1_condA0


# estimate E(Y(1)) --------------------------------------------------------

  # The computations are:
  # E(Y(1)) =
  #   P(S=0|A=1)[E(Y|S=0,A=0)P(A=0) + E(Y|S=0,A=1)P(A=1)] +
  #   P(S=1|A=1)[E(Y|S=1,A=0)P(A=0) + E(Y|S=1,A=1)P(A=1)]

  # P(S=1|A=1)
  condA1 <- data[, exposure.name] == 1
  probS1_condA1 <- mean(data[condA1, surrogate.name])
  # P(S=0|A=1)
  probS0_condA1 <- 1 - probS1_condA1

  # the first component of E(Y(1))
  # P(S=0|A=1) * [E(Y|S=0,A=0)P(A=0) + E(Y|S=0,A=1)P(A=1)]
  exY1_probS0_condA1 <- probS0_condA1 *
    (exY_condS0_condA0 * probA0 + exY_condS0_condA1 * probA1)

  # the second component of E(Y(1))
  # P(S=1|A=1) * [E(Y|S=1,A=0)P(A=0) + E(Y|S=1,A=1)P(A=1)]
  exY1_probS1_condA1 <- probS1_condA1 *
    (exY_condS1_condA0 * probA0 + exY_condS1_condA1 * probA1)

  # E(Y(1)) = P(S=0|A=1)[E(Y|S=0 A=0)P(A=0) + E(Y|S=0,A=1)P(A=1)] +
  # P(S=1|A=1)[E(Y|S=0 A=0)P(A=0) + E(Y|S=1,A=1)P(A=1)]
  EY1 <- exY1_probS0_condA1 + exY1_probS1_condA1

  # estimate the effect measures
  out <- effect_measures(val0 = EY0, val1 = EY1, log = FALSE)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname frontdr_np
#' @export
frontdoor.r <- frontdr_np
