#' Standardized estimates via Outcome Modeling, Non-Parametric
#'
#' Standardized estimates via outcome modeling, non-parametric.
#'
#' The standardized estimates are computed using the non-parametric outcome
#' model. The calculations are performed without regression.
#'
#' @inheritParams backdr_out
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#'
#' @importFrom dplyr count group_by ungroup mutate summarise filter pull near
#' @importFrom rlang .data enquo quo_name
#'
#' @return Estimate using outcome-model standardization.
#' @export
backdr_out_np <- function(data, outcome.name = "Y", exposure.name = "T",
                          confound.names = "H", att = FALSE) {
  stopifnot(length(confound.names) == 1)

  # get the summarized data
  summ <- data %>%
    count(.data[[outcome.name]], .data[[exposure.name]],
          .data[[confound.names]], name = "n") %>%
    mutate(freq = n / sum(n))
  stopifnot(near(sum(summ$freq), 1))

  # the expected value of the outcome given the exposure and confounds
  EYcond <- summ %>%
    group_by(.data[[exposure.name]], .data[[confound.names]]) %>%
    summarise(EYcond = weighted.mean(x = .data[[outcome.name]], w = n))


  # the confound distribution
  if (!att) {
    PH <- summ %>%
      group_by(.data[[confound.names]]) %>%
      summarize(prob = sum(.data$freq))
    # print(PH)
  } else {
    PH <- summ %>%
      filter(.data[[exposure.name]] == 1) %>%
      group_by(.data[[confound.names]]) %>%
      summarize(n = sum(n)) %>%
      mutate(prob = .data$n / sum(.data$n))
  }


  EY <- dplyr::inner_join(EYcond, PH, by = confound.names) %>%
    mutate(EY = EYcond * .data$prob) %>%
    group_by(.data[[exposure.name]]) %>%
    summarize(EY = sum(EY)) %>%
    arrange(.data[[exposure.name]]) %>%
    pull(EY)

  EY0 <- EY[1]
  EY1 <- EY[2]

  # estimate the effect measures
  effect_measures(val0 = EY0, val1 = EY1)
}

#' Standardized estimates via Outcome Modeling, Non-Parametric Regression
#'
#' Standardized estimates via outcome modeling, non-parametric regression.
#'
#' The standardized estimates are computed using the non-parametric outcome
#' model. IMPORTANT: The formula must be in the format \code{Y ~ T + ...} where the
#' exposure is in the first position \code{T}.
#'
#' @inheritParams backdr_out
#' @param interactions List of character vectors with the interactions.
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#'
#'
#' @return Estimate using outcome-model standardization.
#' @export
backdr_out_npr <- function(data, outcome.name = "Y", exposure.name = "T",
                           confound.names = "H",
                           interactions = list(c("T", "A")),
                           att = FALSE) {
  stopifnot(length(confound.names) == 1)

  x0 <- "(Intercept)"  # name of intercept used by lm, glm, etc.

  # marginal expected value of H
  if (!att) {
    EH <- mean(data[, confound.names])
  } else {
    # condition on treatment when ATT is requested
    condT1 <- data[, exposure.name] == 1
    EH <- mean(data[condT1, confound.names])
  }

  input.names <- c(exposure.name, confound.names)
  a_formula <- formulaic::create.formula(outcome.name = outcome.name,
                                         input.names = input.names,
                                         interactions = interactions,
                                         dat = data)

  # fit the outcome model and extract the coefficients
  coefs <- coef(lm(formula = a_formula , data = data))
  # compute the marginal expected potential outcomes
  EY0 <- coefs[x0] + coefs[confound.names] * EH
  # create strings of interactions as coefficients
  ht <- sapply(interactions, function(x) paste(x, collapse = ":"))
  EY1 <- coefs[x0] + coefs[exposure.name] +
    sum(coefs[c(confound.names, ht)]) * EH

  # estimate the effect measures
  effect_measures(val0 = EY0, val1 = EY1)
}

#' @rdname backdr_out_npr
#' @export
stand <- backdr_out_npr


#' Standardized estimates via Outcome Modeling, Non-Parametric
#'
#' Standardized estimates via outcome modeling, non-parametric.
#'
#' The standardized estimates are computed using the non-parametric outcome
#' model. The calculations are performed without regression.
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
#' @importFrom dplyr count group_by ungroup mutate summarise filter pull near
#' @importFrom rlang .data enquo quo_name
#'
#' @return Estimate using outcome-model standardization.
#' @export
backdr_out_npX <- function(data, outcome, exposure, confound, att = FALSE,
                            R = 1000, conf = 0.95) {

  estimator <- function(data, ids) {
    dat <- data[ids, ]
    # get the summarized data
    summ <- dat %>%
      count({{outcome}}, {{exposure}}, {{confound}}, name = "n") %>%
      mutate(freq = n / sum(n))
    stopifnot(near(sum(summ$freq), 1))

    # the expected value of the outcome given the exposure and confounds
    EYcond <- summ %>%
      group_by({{exposure}}, {{confound}}) %>%
      summarise(EYcond = weighted.mean(x = {{outcome}}, w = n))


    # the confound distribution
    if (!att) {
      PH <- summ %>%
        group_by({{confound}}) %>%
        summarize(prob = sum(.data$freq))
      # print(PH)
    } else {
      PH <- summ %>%
        filter({{exposure}} == 1) %>%
        group_by({{confound}}) %>%
        summarize(n = sum(n)) %>%
        mutate(prob = .data$n / sum(.data$n))
    }

    # multiply the conditional expectation by the confound probabilities
    cnf <- enquo(confound)  # the variable name used to join the tables
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
backdr_out_nprX <- function(data, formula = Y ~ `T` + H + `T`*H, att = FALSE,
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
