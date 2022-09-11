#' Compute ITT, CACE and ATT from Instrument Variables
#'
#' Compute ITT, CACE and ATT from instrument variables.
#'
#' See section 9.3 on p. 164 for details. When the tolerance is not met, the
#'
#' @param data Dataframe of raw data.
#' @param formula Formula representing the model.
#' @param exposure.name Name of exposure variable.
#' @param instrument.name Name of instrument variable.
#' @param tol Numeric > 0. Tolerance used in estimation. Default is
#' .Machine$double.eps^0.5.
#'
#' @source Section 9.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
instr_vars <- function(data, formula, exposure.name, instrument.name,
                       tol = .Machine$double.eps^0.5) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name, instrument.name)
  outcome.name <- var_names$outcome.name

  # estimate the ITT
  dat0 <- data[, instrument.name] == 0
  dat1 <- data[, instrument.name] == 1
  ITT <- mean(data[dat1, outcome.name]) - mean(data[dat0, outcome.name])

  # estimate the denominator of the CAE and ATT with equation (9.5)
  denom <- mean(data[dat1, exposure.name]) - mean(data[dat0, exposure.name])
  denom <- mean(data[dat1, exposure.name]) - mean(data[dat0, exposure.name])
  msg <- sprintf("The variable \'%s\' is a weak instrument. Typically, no analysis
  should done with a weak instrument.", instrument.name)
  assertthat::assert_that(abs(denom) >= tol, msg = msg)
  IV <- ITT / denom

  out <- c("ITT" = ITT, "IV" = IV)
  data.frame(
    term = names(out),
    estimate = out,
    std.err = NA_real_
  )
}

#' @rdname instr_vars
#' @export
iv.r <- instr_vars
