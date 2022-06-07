#' Compute ITT, CACE and ATT from Instrument Variables
#'
#' Compute ITT, CACE and ATT from instrument variables.
#'
#' See section 9.3 on p. 164 for details. When the tolerance is not met, the
#'
#' @param data Dataframe of raw data.
#' @param formula Formula representing the model.
#' @param exposure.name Name of exposure variable. The other independent
#' variables in the formula will be assumed to be the instrument variable.
#' There can be only one exposure variable and one instrument variable.
#' @param tol Numeric > 0. Tolerance used in estimation. Default is
#' .Machine$double.eps^0.5.
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
instr_vars <- function(data, formula = Y ~ A + `T`, exposure.name = "A",
                       tol = .Machine$double.eps^0.5) {

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name, nvars = 1)
  outcome.name <- var_names$outcome.name
  instrument.name <- var_names$extra.names

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

  out <- c("ITT" = ITT, "IV" = IV)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname instr_vars
#' @export
iv.r <- instr_vars
