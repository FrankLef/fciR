#' Calculate the mediation variables.
#'
#' Calculate the mediation variables.
#'
#' Calculate the mediation variables and put them in a dataframe that can be
#' used with bootstrapping.
#'
#' @param NDE Number. Natural direct effect.
#' @param NIE Number. Natural indirect effect.
#' @param CDE0 Number. Controlled direct effect setting M=0.
#' @param CDE1 Number. Controlled direct effect setting M=1.
#'
#' @seealso mediation
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' mediation_calc(NDE = -0.03552486, NIE = -0.11595150,
#'   CDE0 = 0.04240090, CDE1 = 0.04705889)
mediation_calc <- function(NDE, NIE, CDE0, CDE1) {
  # estimate the total effect
  TE <- NDE + NIE
  assertthat::assert_that(!dplyr::near(TE, 0),
                          msg = "Total effect must not be zero.")
  # estimate the proportion mediated
  PM <- NIE / TE
  # estimate the proportion eliminated setting M=1
  PE1 <- (TE - CDE1) / TE
  # estimate the proportion eliminated setting M=0
  PE0 <- (TE - CDE0) / TE

  # output format can be used with bootstrap
  data.frame(
    term = c("TE", "PM", "PE(0)", "PE(1)"),
    estimate = c(TE, PM, PE0, PE1),
    std.err = NA_real_
  )
}
