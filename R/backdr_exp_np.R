#' Compute standardized averages using exposure modeling, non-parametric
#'
#' Compute standardized averages using exposure modeling, non-parametric.
#'
#' Compute standardized averages using exposure modeling as described in
#' section 6.2.
#'
#' @param data Dataframe
#' @param outcome Outcome variable
#' @param exposure Exposure variable
#' @param confound Confound variable
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @importFrom dplyr count group_by ungroup mutate summarise filter pull near
#' @importFrom rlang .data
#'
#' @return Estimate using exposure-model standardization.
#' @export
backdr_exp_np <- function(data, outcome, exposure, confound, att = FALSE,
                          R = 1000, conf = 0.95) {

  estimator <- function(data, ids) {
    dat <- data[ids, ]

    # get the summarized data
    summ <- dat %>%
      count({{outcome}}, {{exposure}}, {{confound}}, name = "n") %>%
      mutate(freq = n / sum(n))
    stopifnot(dplyr::near(sum(summ$freq), 1))

    # compute e(H=0) and e(H=1)
    eH <- summ %>%
      group_by({{exposure}}, {{confound}}) %>%
      summarize(n = sum(n)) %>%
      group_by({{confound}}) %>%
      mutate(prob = n / sum(n)) %>%
      filter({{exposure}} == 1) %>%
      arrange({{confound}}) %>%
      pull(.data$prob)
    eH
    stopifnot(all(eH > .Machine$double.eps^0.5))
    eH0 <- eH[1]
    eH1 <- eH[2]

    # compute the E(T) when ATT is required
    e0 <- NA_real_
    if (att) {
      e0 <- summ %>%
        filter({{exposure}} == 1) %>%
        summarize(sum(.data$freq)) %>%
        pull()
    }

    # create the  eH variable
    EY <- summ %>%
      mutate(eH = (1 - {{confound}}) * eH0 + {{confound}} * eH1)

    # compute the summand of the estimating equations
    if (!att) {
      EY <- EY %>%
        mutate(s = (1 - {{exposure}}) * {{outcome}} / (1 - eH) +
                 {{exposure}} * {{outcome}} / eH)
    } else {
      EY <- EY %>%
        mutate(s = (1 - {{exposure}}) * {{outcome}} * eH / (e0 * (1 - eH)) +
                 {{exposure}} * {{outcome}} / eH )
      # E(Y(1)|T=1) = E(Y|T=1) is estimated as before
      # see very last paragraph of section 6.2.1
      EYT1 <- summ %>%
        filter({{exposure}} == 1) %>%
        group_by({{outcome}}) %>%
        summarize(n = sum(.data$n)) %>%
        mutate(prob = .data$n / sum(.data$n)) %>%
        summarize(EYT1 = sum({{outcome}} * .data$prob)) %>%
        pull(EYT1)
    }

    # Estimate the value of the potential outcome
    EY <- EY %>%
      group_by({{exposure}}) %>%
      summarize(EY = sum(.data$s * .data$freq)) %>%
      arrange({{exposure}}) %>%
      pull(EY)

    EY0 <- EY[1]
    EY1 <- EY[2]
    if (att) EY1 <- EYT1  # if ATT, compute E(Y|T=1) as before

    # estimate the effect measures
    effect_measures(val0 = EY0, val1 = EY1)
  }

  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  effect_exp(data = out)
}

#' @rdname backdr_exp_np
#' @export
calc_exposure <- backdr_exp_np


#' Compute standardized averages using exposure modeling, non-parametric
#'
#' Compute standardized averages using exposure modeling, non-parametric.
#'
#' Compute standardized averages using exposure modeling as described in
#' section 6.2.
#'
#' @param data Dataframe
#' @param formula Formula, must be in the format \code{Y ~ `T` + H}, i.e.
#' only 1 covariate H.
#' @param weights String. Name of the columns with the weights that will
#' be used to create probabilities summing up to 1.
#'
#' @return List with 3 elements: EY1, EY0, EY0T1. See section 6.2.
#' for more details.
#' @export
backdr_exp_bb <- function(data, formula = Y ~ `T` + H, weights = "n") {

  # extract the variables names from the formula
  fvars <- formula2vars(formula)
  # there can be only one H
  stopifnot(length(fvars$h) == 1)

  # compute e(H=0)
  dat0 <- data[data[, fvars$h] == 0, ]
  eH0 <- sum(dat0[dat0[, fvars$t] == 1, weights]) / sum(dat0[, weights])
  # compute e(H=1)
  dat1 <- data[data[, fvars$h] == 1, ]
  eH1 <- sum(dat1[dat1[, fvars$t] == 1, weights]) / sum(dat1[, weights])
  # compute e(H) for all participants
  eH <- eH0 * (1 - data[, fvars$h]) + eH1 * data[, fvars$h]
  # compute the summands of the estimating equations
  s1 <- data[, fvars$t] * data[, fvars$y] / eH
  s0 <- (1 - data[, fvars$t]) * data[, fvars$y] / (1 - eH)

  # estimate the expected values of the potential outcomes
  probs <- data[, weights] / sum(data[, weights])  # the probabilities
  stopifnot(sum(probs) == 1)
  EY1 <- sum(s1 * probs)
  EY0 <- sum(s0 * probs)

  # ATT calculations
  # estimate P(T = 1)
  e0 <- sum(data[, fvars$t] * probs)
  # compute the summands of the estimating equation
  s <- data[, fvars$y] * (1 - data[, fvars$t]) * eH / (e0 * (1 - eH))
  # estimate E(Y0|T=1)
  EY0T1 <- sum(s * probs)

  list("EY1" = EY1, "EY0" = EY0, "EY0T1" = EY0T1)
}
