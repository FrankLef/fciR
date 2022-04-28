
#' Compute ITT, CACE and ATT from Instrument Variables
#'
#' Compute ITT, CACE and ATT from instrument variables.
#'
#' See section 9.3 on p. 164 for details. When the tolerance is not met, the
#'
#' @param data Dataframe of raw data.
#' @param outcome.name Name of outcome variable.
#' @param exposure.name Name of exposure variable.
#' @param instrument.name Name of instrument variable.
#' @param tol Numeric > 0. Tolerance used in estimation.
#'
#' @return Numeric vector of estimates.
#' @export
instr_vars <- function(data, outcome.name = "Y", exposure.name = "A",
                       instrument.name = "T", tol = .Machine$double.eps^0.5) {

  # estimate the ITT
  dat0 <- data[, instrument.name] == 0
  dat1 <- data[, instrument.name] == 1
  ITT <- mean(data[dat1, outcome.name]) - mean(data[dat0, outcome.name])

  # estimate the denominator of the CAE and ATT with equation (9.5)
  denom <- mean(data[dat1, exposure.name]) - mean(data[dat0, exposure.name])
  # denominator should not be near zero to avoid numerical problems
  check <- abs(denom) >= tol
  if(check) {
    IV <- ITT / denom
  } else {
    IV <- NA_real_
  }
  c("ITT" = ITT, "IV" = IV)
}
