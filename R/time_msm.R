#' Estimate Using Marginal Structural Models
#'
#' Estimate using marginal structural models.
#'
#' Estimate using marginal structural models with he tidyverse way of coding.
#'
#' @param data Dataframe.
#' @param outcome.name Name of outcome variable.
#' @param exposure.names Name of exposure variables. There must be 2 of them.
#' @param confound.names Names of the confound variables.
#'
#' @source Section 13.1
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
time_msm <- function(data, outcome.name, exposure.names, confound.names) {
  # exposure names must be in sequential order as in c("A!", "A2")
  stopifnot(length(exposure.names) == 2)

  # estimate the probability of treatment at time 2
  eformula <- paste(exposure.names[2],
                    paste(c(exposure.names[1], confound.names), collapse = "*"),
                    sep = "~")
  eA1H <- fitted(glm(formula = eformula, data = data, family = "binomial"))
  # estimate the weights
  datA2 <- data[, exposure.names[2]]
  wghts <- (eA1H * datA2 + (1 - eA1H) * (1 - datA2))
  inv_wghts <- 1 / unname(wghts)
  # IMPORTANT: Must add the weights to data to avoid error
  #            "object 'inv_wghts' not found".
  data$inv_wghts <- inv_wghts
  # fit the marginal structural model
  msmformula <- paste(outcome.name,
                      paste(exposure.names, collapse = "*"),
                      sep = "~")
  msm_fit <- glm(formula = msmformula, data = data, family = "gaussian",
                 weights = inv_wghts)
  coefs <- coef(msm_fit)
  # return contrasts of the MSM parameters
  out <- c(coefs,
           sum(coefs[c("A1", "A1:A2")]),
           sum(coefs[c("A2", "A1:A2")]),
           sum(coefs[coefs != "(Intercept)"]))
  names(out) <- c(paste("beta", seq_along(coefs) - 1),
                  "beta 1+3", "beta 2+3", "beta 1+2+3")
  # output compatible with boostrap function
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_)
  }

#' @rdname time_msm
#' @export
msm.r <- time_msm
