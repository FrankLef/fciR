#' Standardized estimates via Outcome Modeling, Non-Parametric
#'
#' Standardized estimates via outcome modeling, non-parametric.
#'
#' The standardized estimates are computed using the non-parametric outcome
#' model. The calculations are performed without regression.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula representing the model.
#' @param exposure.name Name of exposure variable.
#' @param confound.names Name of confound variable. Must have length of 1.
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#'
#' @importFrom dplyr count group_by mutate summarize filter pull relocate
#' @importFrom rlang .data
#' @importFrom stats setNames
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
backdr_out_np <- function(data, formula = Y ~ `T` + H, exposure.name = "T",
                          confound.names = "H", att = FALSE) {

  # must have only one exposure
  stopifnot(length(exposure.name) == 1)

  # all independent variables must be accounted for
  ind_vars <- all.vars(rlang::f_rhs(formula))
  stopifnot(all(ind_vars %in% c(exposure.name, confound.names)))

  # the name of the outcome variable
  outcome.name <- all.vars(rlang::f_lhs(formula))

  # compute the frequencies, this table is then used for all computations
  summ <- data |>
    count(.data[[outcome.name]], .data[[exposure.name]], .data[[confound.names]]) |>
    mutate(freq = n / sum(n))
  stopifnot(abs(sum(summ$freq) - 1) < .Machine$double.eps^0.5)

  # the expected value of the outcome given the exposure and confounds
  out_cond_mean <- summ |>
    group_by(.data[[exposure.name]], .data[[confound.names]]) |>
    summarize(EY = weighted.mean(.data[[outcome.name]], w = n)) |>
    # add and id column to be able to join the confounds variables later
    unite(col = "id", .data[[confound.names]], remove = FALSE)


  # the confound distribution
  if (!att) {
    confound_dist <- summ %>%
      group_by(.data[[confound.names]]) |>
      summarize(prob = sum(.data$freq))
  } else {
    confound_dist <- summ %>%
      filter(.data[[exposure.name]] == 1) |>
      group_by(.data[[confound.names]]) |>
      summarize(n = sum(n)) |>
      mutate(prob = .data$n / sum(.data$n))
  }
  # add and id column to be able to join the confounds variables later
  confound_dist <- confound_dist |>
    unite(col = "id", .data[[confound.names]], remove = FALSE)


  # multiply the conditional expectation by the confound probabilities
  EY <- dplyr::inner_join(out_cond_mean, confound_dist, by = "id") |>
    group_by(.data[[exposure.name]]) |>
    summarize(EY = sum(.data$EY * .data$prob)) |>
    # create the output vector
    arrange(.data[[exposure.name]]) |>
    pull(EY) |>
    stats::setNames(c("EY0", "EY1"))

  EY0 <- EY[1]
  EY1 <- EY[2]

  # estimate the effect measures
  out <- effect_measures(EY0, EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}
