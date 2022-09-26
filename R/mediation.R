#' Estimate Non-parametric Mediation Effect
#'
#' Estimate non-parametric mediation effect.
#'
#' Estimate non-parametric mediation effect using the \code{tidyverse} way
#' of coding.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula representing the model.
#' @param exposure.name Name of exposure variable.
#' @param mediator.name Name of mediator variable.
#' @param confound.names Names of the confound variables.
#'
#' @source section 12.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
mediation_np <- function(data, formula, exposure.name, mediator.name,
                              confound.names) {
  # character vectors of names used below
  outcome.name <- all.vars(formula[[2]])
  exposure.confound.names <- c(exposure.name, confound.names)
  mediator.confound.names <- c(mediator.name, confound.names)
  input.names <- c(exposure.name, mediator.name, confound.names)
  all.names <- c(outcome.name, exposure.name, mediator.name, confound.names)

  # P(H)
  probH <- data |>
    fciR::calc_prob(var.name = confound.names, prob.name = "probH")
  assertthat::assert_that(sum(probH$probH) == 1,
                          msg = "Sum of prob. H must equal 1.")

  # P(M = m | A, H)
  probMcondAH <- data |>
    fciR::calc_prob_cond(condition.names = exposure.confound.names,
                         var.name = mediator.name,
                         prob.name = "probM") |>
    tidyr::pivot_wider(names_from = exposure.name, values_from = "probM",
                       names_prefix = "probMA") |>
    # add P(H) to th data, it will be used later for standardization by H
    dplyr::inner_join(y = probH, by = confound.names)

  # E(Y | A, M, H)
  expYcondAMH <- data |>
    group_by(across(all_of(input.names))) |>
    summarize(expYcondAMH = mean(.data[[outcome.name]])) |>
    tidyr::pivot_wider(names_from = exposure.name, values_from = expYcondAMH,
                       names_prefix = "EYA")

  # NOTE: This is the final dataframe used to make all computations
  #       It is an important dataframe when debuging
  processed_data <- inner_join(x = expYcondAMH, y = probMcondAH,
                               by = mediator.confound.names) |>
    ungroup()

  NDE <- processed_data |>
    mutate(NDEH = (.data$EYA1 - .data$EYA0) * .data$probMA0) |>
    # standardize over H
    summarize(NDE = sum(.data$NDEH * probH)) |>
    pull()

  NIE <- processed_data |>
    mutate(NIEH = .data$EYA1 * (.data$probMA1 - .data$probMA0)) |>
    # standardize over H
    summarize(NIE = sum(.data$NIEH * probH)) |>
    pull()

  CDE <- processed_data |>
    group_by(across(all_of(mediator.name))) |>
    summarize(CDE = sum((.data$EYA1 - .data$EYA0) * probH)) |>
    mutate(term = paste0("CDE(", .data[[mediator.name]], ")"))

  # calculated variables
  calc <- fciR::mediation_calc(NDE = NDE, NIE = NIE,
                              CDE0 = CDE$CDE[CDE$term == "CDE(0)"],
                              CDE1 = CDE$CDE[CDE$term == "CDE(1)"])

  # output format can be used with bootstrap
  out <- data.frame(
    term = c("NDE", "NIE", CDE$term),
    estimate = c(NDE, NIE, CDE$CDE),
    std.err = NA_real_) |>
    dplyr::bind_rows(calc)
  # relocate the columns
  the_terms <- c("TE", "CDE(0)", "CDE(1)", "NDE", "NIE", "PE(0)", "PE(1)", "PM")
  pos <- match(the_terms, out$term)
  out <- out[pos, ]
  out
}

#' Estimate Mediation Effect with Parametric Assumptions
#'
#' Estimate mediation effect with parametric assumptions.
#'
#' Estimate mediation effect with parametric assumptions using the
#' \code{tidyverse} way of coding.
#'
#' @inheritParams mediation_np
#'
#' @source section 12.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
mediation <- function(data, formula, exposure.name, mediator.name,
                           confound.names) {
  # character vectors of names used below
  outcome.name <- all.vars(formula[[2]])
  exposure.confound.names <- c(exposure.name, confound.names)
  input.names <- c(exposure.name, mediator.name, confound.names)

  # fit a saturated model including the mediator
  formula_out <- paste(outcome.name, paste(input.names, collapse = "*"),
                       sep = "~")
  fit_out <- glm(formula = formula_out, family = "binomial", data = data)
  # fit a saturated model for the mediator
  formula_med <- paste(mediator.name, paste(exposure.confound.names, collapse = "*"),
                       sep = "~")
  fit_med <- glm(formula = formula_med, family = "binomial", data = data)

  # create new data sets
  newdat <- list()
  newdat$A0M0 <- data |>
    mutate(
      !!exposure.name := 0L,
      !!mediator.name := 0L)
  newdat$A1M0 <- data |>
    mutate(
      !!exposure.name := 1L,
      !!mediator.name := 0L)
  newdat$A0M1 <- data |>
    mutate(
      !!exposure.name := 0L,
      !!mediator.name := 1L)
  newdat$A1M1 <- data |>
    mutate(
      !!exposure.name := 1L,
      !!mediator.name := 1L)

  # estimate the first term  of equation (1) as a funciton of H
  EY1M0H <-
    predict(fit_out, newdata = newdat$A1M0, type = "response") *
    (1 - predict(fit_med, newdata = newdat$A0M0, type = "response")) +
    predict(fit_out, newdata = newdat$A1M1, type = "response") *
    predict(fit_med, newdata = newdat$A0M1, type = "response")
  # standardize with respect to marginal distribution of H
  EY1M0 <- mean(EY1M0H)
  # estimate the second term  of equation (1) as a funciton of H
  EY0M0H <-
    predict(fit_out, newdata = newdat$A0M0, type = "response") *
    (1 - predict(fit_med, newdata = newdat$A0M0, type = "response")) +
    predict(fit_out, newdata = newdat$A0M1, type = "response") *
    predict(fit_med, newdata = newdat$A0M1, type = "response")
  # standardize with respect to marginal distribution of H
  EY0M0 <- mean(EY0M0H)
  # estimate the first term  of equation (2) as a funciton of H
  EY1M1H <-
    predict(fit_out, newdata = newdat$A1M0, type = "response") *
    (1 - predict(fit_med, newdata = newdat$A1M0, type = "response")) +
    predict(fit_out, newdata = newdat$A1M1, type = "response") *
    predict(fit_med, newdata = newdat$A1M1, type = "response")
  # standardize with respect to marginal distribution of H
  EY1M1 <- mean(EY1M1H)

  NDE <- EY1M0 - EY0M0
  NIE <- EY1M1 - EY1M0

  # estimate the controlled direct effects
  CDE1H <- predict(fit_out, newdata = newdat$A1M1, type = "response") -
    predict(fit_out, newdata = newdat$A0M1, type = "response")
  # standardize with respect to marginal distribution of H
  CDE1 <- mean(CDE1H)

  # estimate the controlled direct effects
  CDE0H <- predict(fit_out, newdata = newdat$A1M0, type = "response") -
    predict(fit_out, newdata = newdat$A0M0, type = "response")
  # standardize with respect to marginal distribution of H
  CDE0 <- mean(CDE0H)

  # calculated variables
  calc <- fciR::mediation_calc(NDE = NDE, NIE = NIE, CDE0 = CDE0, CDE1 = CDE1)

  # output format can be used with bootstrap
  out <- data.frame(
    term = c("NDE", "NIE", "CDE(0)", "CDE(1)"),
    estimate = c(NDE, NIE, CDE0, CDE1),
    std.err = NA_real_) |>
    dplyr::bind_rows(calc)
  # relocate the columns
  the_terms <- c("TE", "CDE(0)", "CDE(1)", "NDE", "NIE", "PE(0)", "PE(1)", "PM")
  pos <- match(the_terms, out$term)
  out <- out[pos, ]
  out
}
