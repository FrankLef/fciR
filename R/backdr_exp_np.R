#' Compute standardized averages using exposure modeling, non-parametric
#'
#' Compute standardized averages using exposure modeling, non-parametric.
#'
#' Compute standardized averages using exposure modeling as described in
#' section 6.2.
#'
#' @param data Dataframe of data.
#' @param outcome.name Name of outcome variable.
#' @param exposure.name Name of exposure variable.
#' @param confound.names Name of confound variable.
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#'
#' @importFrom formulaic create.formula
#' @importFrom dplyr count group_by ungroup mutate summarise filter pull near
#' @importFrom rlang .data
#'
#' @return Estimate using exposure-model standardization.
#' @export
backdr_exp_np <- function(data, outcome.name = "Y", exposure.name = "T",
                          confound.names = "H", att = FALSE) {
  stopifnot(length(confound.names) == 1)

  # get the summarized data
  summ <- data %>%
    count(.data[[outcome.name]], .data[[exposure.name]], .data[[confound.names]], name = "n") %>%
    mutate(freq = n / sum(n))
  stopifnot(dplyr::near(sum(summ$freq), 1))

  # compute e(H=0) and e(H=1)
  eH <- summ %>%
    group_by(.data[[exposure.name]], .data[[confound.names]]) %>%
    summarize(n = sum(n)) %>%
    group_by(.data[[confound.names]]) %>%
    mutate(prob = n / sum(n)) %>%
    filter(.data[[exposure.name]] == 1) %>%
    arrange(.data[[confound.names]]) %>%
    pull(.data$prob)
  eH
  stopifnot(all(eH > .Machine$double.eps^0.5))
  eH0 <- eH[1]
  eH1 <- eH[2]

  # compute the E(T) when ATT is required
  e0 <- NA_real_
  if (att) {
    e0 <- summ %>%
      filter(.data[[exposure.name]] == 1) %>%
      summarize(sum(.data$freq)) %>%
      pull()
  }

  # create the  eH variable
  EY <- summ %>%
    mutate(eH = (1 - .data[[confound.names]]) * eH0 + .data[[confound.names]] * eH1)

  # compute the summand of the estimating equations
  if (!att) {
    EY <- EY %>%
      mutate(s = (1 - .data[[exposure.name]]) * .data[[outcome.name]] / (1 - eH) +
               .data[[exposure.name]] * .data[[outcome.name]] / eH)
  } else {
    EY <- EY %>%
      mutate(s = (1 - .data[[exposure.name]]) * .data[[outcome.name]] * eH / (e0 * (1 - eH)) +
               .data[[exposure.name]] * .data[[outcome.name]] / eH )
    # E(Y(1)|T=1) = E(Y|T=1) is estimated as before
    # see very last paragraph of section 6.2.1
    EYT1 <- summ %>%
      filter(.data[[exposure.name]] == 1) %>%
      group_by(.data[[outcome.name]]) %>%
      summarize(n = sum(.data$n)) %>%
      mutate(prob = .data$n / sum(.data$n)) %>%
      summarize(EYT1 = sum(.data[[outcome.name]] * .data$prob)) %>%
      pull(EYT1)
  }

  # Estimate the value of the potential outcome
  EY <- EY %>%
    group_by(.data[[exposure.name]]) %>%
    summarize(EY = sum(.data$s * .data$freq)) %>%
    arrange(.data[[exposure.name]]) %>%
    pull(EY)

  EY0 <- EY[1]
  EY1 <- EY[2]
  if (att) EY1 <- EYT1  # if ATT, compute E(Y|T=1) as before

  # estimate the effect measures
  out <- effect_measures(val0 = EY0, val1 = EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
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
#' @param data Dataframe of data.
#' @param outcome.name Name of outcome variable.
#' @param exposure.name Name of exposure variable.
#' @param confound.names Name of confound variable.
#' @param weights String. Name of the columns with the weights that will
#' be used to create probabilities summing up to 1.
#'
#' @return List with 3 elements: EY1, EY0, EY0T1. See section 6.2.
#' for more details.
#' @export
backdr_exp_bb <- function(data, outcome.name = "Y", exposure.name = "T",
                          confound.names = "H", weights = "n") {
  stopifnot(length(confound.names) == 1)

  # compute e(H=0)
  condH0 <- data[, confound.names] == 0
  dat0 <- data[condH0, ]
  eH0 <- sum(dat0[dat0[, exposure.name] == 1, weights]) / sum(dat0[, weights])
  # compute e(H=1)
  condH1 <- data[, confound.names] == 1
  dat1 <- data[condH1, ]
  eH1 <- sum(dat1[dat1[, exposure.name] == 1, weights]) / sum(dat1[, weights])
  # compute e(H) for all participants
  eH <- eH0 * (1 - data[, confound.names]) + eH1 * data[, confound.names]
  # compute the summands of the estimating equations
  s1 <- data[, exposure.name] * data[, outcome.name] / eH
  s0 <- (1 - data[, exposure.name]) * data[, outcome.name] / (1 - eH)

  # estimate the expected values of the potential outcomes
  probs <- data[, weights] / sum(data[, weights])  # the probabilities
  stopifnot(abs(sum(probs) - 1) < .Machine$double.eps^0.5)
  EY1 <- sum(s1 * probs)
  EY0 <- sum(s0 * probs)

  # ATT calculations
  # estimate P(T = 1)
  e0 <- sum(data[, exposure.name] * probs)
  # compute the summands of the estimating equation
  s <- data[, outcome.name] * (1 - data[, exposure.name]) * eH / (e0 * (1 - eH))
  # estimate E(Y0|T=1)
  EY0T1 <- sum(s * probs)

  list("EY1" = EY1, "EY0" = EY0, "EY0T1" = EY0T1)
  }


#' Compute standardized averages using exposure modeling, non-parametric
#'
#' Compute standardized averages using exposure modeling, non-parametric.
#'
#' Compute standardized averages using exposure modeling as described in
#' section 6.2.
#'
#' @param data Dataframe of data.
#' @param outcome.name Name of outcome variable.
#' @param exposure.name Name of exposure variable.
#' @param confound.names Name of confound variable.
#' @param weights String. Name of the columns with the weights that will
#' be used to create probabilities summing up to 1.
#'
#' @return List with 3 elements: EY1, EY0, EY0T1. See section 6.2.
#' for more details.
#' @export
backdr_exp_bbX <- function(data, outcome.name = "Y", exposure.name = "T",
                           confound.names = "H", weights = "n") {
  stopifnot(length(confound.names) == 1)

  # compute e(H=0)
  dat0 <- data[data[, confound.names] == 0, ]
  eH0 <- sum(dat0[dat0[, exposure.name] == 1, weights]) / sum(dat0[, weights])
  # compute e(H=1)
  dat1 <- data[data[, confound.names] == 1, ]
  eH1 <- sum(dat1[dat1[, exposure.name] == 1, weights]) / sum(dat1[, weights])
  # compute e(H) for all participants
  eH <- eH0 * (1 - data[, confound.names]) + eH1 * data[, confound.names]
  # compute the summands of the estimating equations
  s1 <- data[, exposure.name] * data[, outcome.name] / eH
  s0 <- (1 - data[, exposure.name]) * data[, outcome.name] / (1 - eH)

  # estimate the expected values of the potential outcomes
  probs <- data[, weights] / sum(data[, weights])  # the probabilities
  stopifnot(sum(probs) == 1)
  EY1 <- sum(s1 * probs)
  EY0 <- sum(s0 * probs)

  # ATT calculations
  # estimate P(T = 1)
  e0 <- sum(data[, exposure.name] * probs)
  # compute the summands of the estimating equation
  s <- data[, outcome.name] * (1 - data[, exposure.name]) * eH / (e0 * (1 - eH))
  # estimate E(Y0|T=1)
  EY0T1 <- sum(s * probs)

  list("EY1" = EY1, "EY0" = EY0, "EY0T1" = EY0T1)
}
