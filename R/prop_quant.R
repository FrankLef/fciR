#' Stratifying on the Quantiles of the Propensity Score
#'
#' Stratifying on the quantiles of the propensity score.
#'
#' Stratifying on the quantiles is useful to identify the possible interactions
#' between the propensity scores and the exposure.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula representing the model.
#' @param exposure.name Name of exposure variable.
#' @param confound.names Names of the confound variables.
#' @param probs Vector of probability for the quantiles.
#' @param quant_var Name of the quantile variable. Default is "pquants".
#'
#' @source Section 10.3. p. 180.
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
prop_quant <- function(data, formula, exposure.name, confound.names,
                           probs = 0:4/4, quant_var = "pquants") {
  # audit the variables
  var_names <- audit_formula(data, formula, exposure.name, confound.names)
  outcome.name <- var_names$outcome.name


  # fit the propensity score model using the prop.r, alias fciR::prop_scores,
  #  from chapter 6
  eformula <- paste(exposure.name, paste(confound.names, collapse = "+"),
                    sep = "~")
  pscores <- fciR::prop_scores(data, formula = formula(eformula))$scores
  # put participants into quantiles
  pquants <- cut(pscores, quantile(pscores, prob = probs), include.lowest = T)
  # add the quantiles to the data
  data <- data |>
    mutate(!!quant_var := pquants)

  # estimate the average potential outcome within each quartile
  oformula <- paste(outcome.name, paste(exposure.name, quant_var, sep = "*"),
                    sep = "~")
  oformula <- paste(c(oformula, "1", exposure.name), collapse = "-")
  fit <- glm(oformula, data = data)
  coefs <- coef(fit)
  ncoefs <- length(coefs)
  msg <- "nb of coefs must be even. Changing the quantiles usually solves this."
  assertthat::assert_that(ncoefs %% 2 == 0, msg = msg)
  EY0 <- coefs[1:(ncoefs/2)]
  EY1 <- coefs[1:(ncoefs/2)] + coefs[(1 + ncoefs/2):ncoefs]
  meanRD <- mean(EY1 - EY0)
  # must return a data.frame of tidy results to use with bootstrap later
  data.frame(
    term = 'meanRD',
    estimate = meanRD,
    std.err = NA_real_)
}
