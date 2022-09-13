#' Compute Precision efficiency
#'
#' Compute precision efficiency.
#'
#' Compare the variance obtained when using a precision variable or not.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula representing the model.
#' @param exposure.name Name of exposure variable.
#' @param precision.name Name of precision variable.
#' @param none.name Name of the term when no precision variable is used.
#'
#' @source Section 11.2
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
precision_eff <- function(data, formula, exposure.name, precision.name,
                          none.name = "None") {
  # Estimate the effect without the precision variable
  vars <- all.vars(formula)
  vars <- vars[vars != precision.name]  # remove the precision variable
  formula1 <- paste(vars[[1]], vars[[2]], sep = "~")
  fit_none <- lm(formula = formula1, data = data)
  # Estimate the effect with the precision variable
  fit <- lm(formula = formula, data = data)
  est <- c(coef(fit_none)[2], coef(fit)[2])
  names(est) <- c(none.name, precision.name)
  std_err <- c(sqrt(diag(stats::vcov(fit_none)))[2],
               sqrt(diag(stats::vcov(fit)))[2])
  # must return a data.frame of tidy results to use with bootstrap later
  data.frame(
    term = names(est),
    estimate = unname(est),
    std.err = std_err
  )
}
