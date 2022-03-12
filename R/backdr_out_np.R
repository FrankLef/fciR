#' Standardized estimates via Outcome Modeling, Non-Parametric
#'
#' Standardized estimates via outcome modeling, non-parametric.
#'
#' The standardized estimates are computed using the non-parametric outcome
#' model. The calculations are performed parametricaly without regression.
#'
#' @param data Dataframe of raw data.
#' @param outcome Outcome variable
#' @param exposure Exposure variable
#' @param confound Confound variable
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @importFrom dplyr count group_by ungroup mutate summarise filter pull
#' @importFrom rlang .data enquo quo_name
#'
#' @return Estimate using outcome-model standardization.
#' @export
backdr_out_np <- function(data, outcome, exposure, confound, att = FALSE,
                            R = 1000, conf = 0.95) {

  estimator <- function(data, ids) {
    dat <- data[ids, ]
    # compute the frequencies
    summ <- dat %>%
      count({{outcome}}, {{exposure}}, {{confound}}, name = "n") %>%
      mutate(freq = n / sum(n))

    # the expected value of the outcome given the exposure and confounds
    EYcond <- summ %>%
      group_by({{exposure}}, {{confound}}) %>%
      summarise(EYcond = weighted.mean(x = {{outcome}}, w = n))

    # the probabilities of the confound
    if (!att) {
      PH <- summ %>%
        group_by({{confound}}) %>%
        summarize(prob = sum(.data$freq))
    } else {
      PH <- dat %>%
        filter({{exposure}} == 1) %>%
        count({{confound}}, name = "n") %>%
        mutate(prob = n / sum(n))
    }

    # multiply the conditional expectation by the confound probabilities
    cnf <- enquo(confound)  # this line create an enquo variable
    EY <- dplyr::inner_join(EYcond, PH, by = quo_name(cnf)) %>%
      mutate(EY = EYcond * .data$prob) %>%
      group_by({{exposure}}) %>%
      summarize(EY = sum(EY)) %>%
      arrange({{exposure}}) %>%
      pull(EY)

    EY0 <- EY[1]
    EY1 <- EY[2]

    # estimate the effect measures
    effect_measures(val0 = EY0, val1 = EY1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  effect_exp(data = out)
}

#' Standardized estimates via Outcome Modeling, Non-Parametric Regression
#'
#' Standardized estimates via outcome modeling, non-parametric regression.
#'
#' The standardized estimates are computed using the non-parametric outcome
#' model. IMPORTANT: The formula must be in the format \code{Y ~ T + ...} where the
#' exposure is in the first position \code{T}.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + ...}, see details.
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @return Estimate using outcome-model standardization.
#' @export
backdr_out_npr <- function(data, formula = Y ~ `T` + H + `T`*H, att = FALSE,
                          R = 1000, conf = 0.95) {

  # extract the variables names from the formula
  fvars <- formula2vars(formula)

  estimator <- function(data, ids) {
    dat <- data[ids, ]
    # marginal expected value of H
    if (!att) {
      EH <- mean(dat[, fvars$h])
    } else {
      # condition on treatment when ATT is requested
      EH <- mean(dat[dat[, fvars$t] == 1, fvars$h])
    }
    # fit the outcome model and extract the coefficients
    coefs <- coef(lm(formula = formula , data = dat))
    # compute the marginal expected potential outcomes
    EY0 <- coefs[fvars$x0] + coefs[fvars$h] * EH
    EY1 <- coefs[fvars$x0] + coefs[fvars$t] + sum(coefs[fvars$ht]) * EH

    # estimate the effect measures
    effect_measures(val0 = EY0, val1 = EY1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  effect_exp(data = out)
}

#' @rdname backdr_out_npr
#' @export
stand <- backdr_out_npr
