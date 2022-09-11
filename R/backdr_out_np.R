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
#' @param confound.names Names of the confound variables.
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#'
#' @source Section 6.1
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
backdr_out_np <- function(data, formula, exposure.name, confound.names,
                          att = FALSE) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)
  checkmate::assertFlag(att)

  # audit the variables
  var_names <- audit_formula(data, formula, exposure.name, confound.names)
  outcome.name <- var_names$outcome.name

  # compute the frequencies, this table is then used for all computations
  summ <- data |>
    count(.data[[outcome.name]], .data[[exposure.name]], .data[[confound.names]]) |>
    mutate(freq = n / sum(n))
  assertthat::assert_that(dplyr::near(abs(sum(summ$freq)), 1),
                          msg = "total freq must equal 1")

  # the expected value of the outcome given the exposure and confounds
  out_cond_mean <- summ |>
    group_by(.data[[exposure.name]], .data[[confound.names]]) |>
    summarize(EY = weighted.mean(.data[[outcome.name]], w = n)) |>
    # add and id column to be able to join the confounds variables later
    unite(col = "id", .data[[confound.names]], remove = FALSE)


  # the confound distribution
  if (!att) {
    confound_dist <- summ |>
      group_by(.data[[confound.names]]) |>
      summarize(prob = sum(.data$freq))
  } else {
    confound_dist <- summ |>
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
    pull(EY)

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
